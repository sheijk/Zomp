(*

#directory "../v3";;
#load "lexer2.cmo";;
#load "parser2.cmo";;
#load "lang.cmo";;
#load "ast2.cmo";;
#load "common.cmo";;
#load "bindings.cmo";;

*)

open Lang
open Semantic
open Ast2
open Common
open Bindings
open Printf

let macroVar = "var"
and macroFunc = "std:base:func"
and macroAssign = "assign"
and macroSequence = "seq"
and macroTypedef = "type"
and macroRecord = "record"
and macroField = "field"
and macroPtr = "ptr"
and macroReturn = "std:base:ret"
and macroLabel = "label"
and macroBranch = "branch"
and macroMacro = "macro"
and macroReplacement = "macroReplace"
and macroFieldptr = "fieldptr"
and macroLoad = "load"
and macroStore = "store"
and macroNullptr = "nullptr"
and macroPtradd = "ptradd"
and macroMalloc = "malloc"
and macroGetaddr = "ptr"
and macroCast = "cast"
and macroInclude = "include"
and macroRest = "postop..."
and macroJuxOp = "opjux"
and macroSeqOp = "opseq"
and macroCallOp = "opcall"

let rec findTypeName bindings = function
  | #Lang.integralType | `Array _ as integralType -> Lang.typeName integralType
  | `Pointer targetType -> findTypeName bindings targetType ^ "*"
  | `Function ft ->
      sprintf "%s -> %s"
        (Common.combine ", " (List.map (findTypeName bindings) ft.argTypes))
        (findTypeName bindings ft.returnType)
  | `TypeRef name -> name
  | `Record record as typ ->
      try
        record.rname
      with Not_found ->
        Lang.typeName typ

let typeErrorMessage bindings (fe, msg, foundType, expectedType) =
  let typeName = function
    | `Any description -> description
    | #Lang.typ as t -> findTypeName bindings t
  in
  sprintf "Type error: %s, expected %s but found %s in expression %s"
    msg
    (typeName expectedType)
    (typeName foundType)
    (match fe with
       | Semantic.Form form -> Lang.formToString form
       | Semantic.Ast ast -> Ast2.toString ast)

exception IllegalExpression of sexpr * string

let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

let raiseIllegalExpressionFromTypeError expr (formOrExpr, msg, found, expected) =
  raiseIllegalExpression expr
    (typeErrorMessage Bindings.defaultBindings (formOrExpr, msg,found,expected))

let raiseInvalidType typeExpr =
  raise (Typesystems.Zomp.CouldNotParseType (Ast2.expression2string typeExpr))

(* TODO: use in translateDefineVar and translateGlobalVar + test *)
let determineStorage typ =
  match typ with
    | `Pointer _ -> MemoryStorage
    | _ -> RegisterStorage

let lookupType bindings name =
  try
    Some (Lang.parseType name)
  with
    | Typesystems.Zomp.CouldNotParseType _ ->
        match lookup bindings name with
          | TypedefSymbol t -> Some t
          | _ -> None

let rec translateType bindings typeExpr =
  let translatePtr targetTypeExpr =
    match translateType bindings targetTypeExpr with
      | Some t -> Some (`Pointer t)
      | None -> None
  in
  let translateArray memberTypeExpr sizeExpr =
    let potentialSize =
      match sizeExpr with
        | { id = number; args = [] } ->
            begin try
              Some(int_of_string number)
            with Failure _ ->
              None
            end
        | _ -> None
    in
    match translateType bindings memberTypeExpr, potentialSize with
      | Some t, Some size -> Some (`Array(t, size))
      | _, _ -> None
  in
  match typeExpr with
    | { id = "opjux"; args = {id = "fptr"; args = []} :: returnTypeExpr :: argTypeExprs } ->
        begin try
          let translate typ =
            match translateType bindings typ with
              | Some t -> t
              | None -> failwith ""
          in
          begin
            Some (`Pointer (`Function {
                              returnType = translate returnTypeExpr;
                              argTypes = List.map translate argTypeExprs;
                            }))
          end
        with Failure _ ->
          None
        end
    | { id = "opjux"; args = args } (* when jux = macroJuxOp *) ->
        translateType bindings (shiftId args)
    | { id = "postop*"; args = [targetTypeExpr] }
    | { id = "ptr"; args = [targetTypeExpr]; } ->
        translatePtr targetTypeExpr
    | { id = "postop[]"; args = [memberTypeExpr; sizeExpr] }
    | { id = "array"; args = [memberTypeExpr; sizeExpr] } ->
        translateArray memberTypeExpr sizeExpr
    | { id = name; args = [] } ->
        begin
          lookupType bindings name
        end
    | _ -> None

(* let translateType = Common.sampleFunc2 "translateType" translateType *)

type formWithTLsEmbedded = [`ToplevelForm of toplevelExpr | form]

let formWithTLsEmbeddedToString = function
  | #Lang.form as form -> Lang.formToString form
  | `ToplevelForm tlform -> Lang.toplevelFormToString tlform

type exprTranslateF = bindings -> sexpr -> bindings * formWithTLsEmbedded list

let rec extractToplevelForms = function
  | [] -> [], []
  | head :: rem ->
      begin
        let remTL, remImpl = extractToplevelForms rem in
        match head with
          | `ToplevelForm #toplevelExpr as tlform -> tlform::remTL, remImpl
          | #Lang.form as implF -> remTL, implF::remImpl
      end


let translatelst translateF bindings expr =
  let rec worker bindings = function
    | [] -> bindings, []
    | expr :: tail ->
        let newBindings, sf = translateF bindings expr in
        let resultingBindings, sfuncs = worker newBindings tail in
        resultingBindings, (sf @ sfuncs)
  in
  worker bindings expr

let translateSeq translateF bindings = function
  | { id = id; args = sequence } when id = macroSequence || id = macroSeqOp ->
      Some (translatelst translateF bindings sequence)
  | _ ->
      None

let rec expr2value typ expr =
  match typ with
    | #integralType -> begin
        match expr with
          | { id = value; args = [] } -> string2integralValue value
          | _ -> raiseIllegalExpression expr
              (Printf.sprintf "expected value of type %s" (Lang.typeName typ))
      end
    | `Record components -> begin
        raiseIllegalExpression expr "records not supported"
      end
    | _ -> raiseIllegalExpression expr "unsupported value expression"


let translateDefineVar (translateF :exprTranslateF) (bindings :bindings) expr =
  let transform id name typeExpr valueExpr =
    let declaredType = match typeExpr with
      | Some e ->
          begin
            match translateType bindings e with
              | Some t -> Some t
              | None -> raise (CouldNotParseType (Ast2.expression2string e))
          end
      | None -> None
    in
    let valueType, toplevelForms, implForms =
      match valueExpr with
        | Some valueExpr ->
            begin
              let _, simpleform = translateF bindings valueExpr in
              let toplevelForms, implForms = extractToplevelForms simpleform in
              match typeCheck bindings (`Sequence implForms) with
                | TypeOf t -> Some t, toplevelForms, implForms
                | TypeError (fe, m,f,e) -> raiseIllegalExpressionFromTypeError valueExpr (fe,m,f,e)
            end
        | None -> None, [], []
    in
    let varType =
      match declaredType, valueType with
        | Some declaredType, Some valueType when equalTypes bindings declaredType valueType ->
            declaredType
        | Some declaredType, Some valueType ->
            raiseIllegalExpressionFromTypeError expr
              (Semantic.Ast expr, "Types do not match",declaredType,valueType)
        | None, Some valueType -> valueType
        | Some declaredType, None -> declaredType
        | None, None ->
            raiseIllegalExpression expr "var needs either a default value or declare a type"
    in
    match varType with
      | #integralType | `Pointer _ | `Function _ as typ ->
          begin
            let var = variable name typ (defaultValue typ) MemoryStorage false in
            let defvar = `DefineVariable (var, Some (`Sequence implForms))
            in
            match typeCheck bindings defvar with
              | TypeOf _ -> Some( addVar bindings var, toplevelForms @ [defvar] )
              | TypeError (fe,m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
          end
      | `Array(memberType, size) as typ ->
          begin
            let var = variable name typ (defaultValue typ) MemoryStorage false in
            let defvar = `DefineVariable (var, match implForms with [] -> None | _ -> Some (`Sequence implForms)) in
            match typeCheck bindings defvar with
              | TypeOf _ -> Some( addVar bindings var, toplevelForms @ [defvar] )
              | TypeError (fe,m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
          end
      | (`Record _ as typ) ->
          begin
            match valueExpr with
              | None ->
                  let var = variable name typ (defaultValue typ) MemoryStorage false in
                  Some( addVar bindings var, [ `DefineVariable (var, None) ] )
              | Some valueExpr ->
                  let var = variable name typ (defaultValue typ) MemoryStorage false in
                  Some( addVar bindings var, [ `DefineVariable (var, Some (`Sequence implForms)) ] )
          end
      | `TypeRef _ ->
          raiseIllegalExpression expr "Internal error: received unexpected type ref"
  in
  match expr with
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          valueExpr
        ] }
        when (id = macroVar) ->
        transform id name (Some typeExpr) (Some valueExpr)

    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
        ] }
        when (id = macroVar) ->
        transform id name (Some typeExpr) None

    | { id = id; args = [
          { id = name; args = [] };
          valueExpr
        ] }
        when id = "var2" ->
        transform id name None (Some valueExpr)

    | _ ->
        None

let expr2VarOrConst (bindings :bindings) = function
  | { id = name; args = [] } -> begin
      match lookup bindings name with
        | VarSymbol v ->
            Some (`Variable v)
        | _ ->
            match string2integralValue name with
              | Some c -> Some (`Constant c)
              | None -> None
    end
  | { id = "preop&"; args = [{id = name; args = []}] } ->
      begin
        match lookup bindings name with
          | FuncSymbol f ->
              let name = f.fname in
              let typ = `Function {
                returnType = f.rettype;
                argTypes = List.map snd f.Lang.fargs;
              } in
              let var = variable name (`Pointer typ) (defaultValue typ) RegisterStorage true in
              Some (`Variable var)
          | _ ->
              None
      end
  | _ -> None


let lastTempVarNum = ref 0

let getUnusedName ?(prefix = "temp") bindings =
  let rec tryTempVar num =
    let name = (Printf.sprintf "%s_%d" prefix num) in
    match lookup bindings name with
      | UndefinedSymbol -> name
      | _ -> tryTempVar (num + 1)
  in
  incr lastTempVarNum;
  tryTempVar !lastTempVarNum

let getNewGlobalVar bindings typ =
  let newVarName = getUnusedName ~prefix:"tempvar" bindings in
  let default = defaultValue typ in
  let var = globalVar ~name:newVarName ~typ ~default in
  (addVar bindings var, var)

let getNewLocalVar bindings typ =
  let newVarName = getUnusedName bindings in
  (* let var = localVar ~name:newVarName ~typ in *)
  let var = Lang.variable
    ~name:newVarName
    ~typ
    ~storage:MemoryStorage
    ~global:false
    ~default:(Typesystems.Zomp.defaultValue typ)
  in
  (addVar bindings var, var)

let translateSimpleExpr (_ :exprTranslateF) (bindings :bindings) expr =
  match expr2VarOrConst bindings expr with
    | Some varOrConst ->
        begin match varOrConst with
          | `Constant StringLiteral string ->
              begin
                let newBindings, var = getNewGlobalVar bindings (`Pointer `Char) in
                let var = { var with vdefault = StringLiteral string } in
                Some( newBindings, [`ToplevelForm (`GlobalVar var); `Variable var] )
              end
          | _ -> Some (bindings, [varOrConst] )
        end
    | None -> None


let translateToForms (translateF :exprTranslateF) bindings expr =
  let newBindings, xforms = translateF bindings expr in
  let tlforms, forms = extractToplevelForms xforms in
  newBindings, toSingleForm forms, tlforms

let rec flattenLeft = function
  | [] -> [], []
  | (list, x) :: rem ->
      let remList, remX = flattenLeft rem in
      (list @ remList, x :: remX)

let translateFuncCall (translateF :exprTranslateF) (bindings :bindings) expr =
  let buildCall name rettype argTypes isPointer bindings args =
    let evalArg (argExpr :Ast2.sexpr) =
      let _, xforms = translateF bindings argExpr in
      let toplevelForms, forms = extractToplevelForms xforms in
      toplevelForms, toSingleForm forms
    in
    let x = List.map evalArg args in
    let toplevelForms, argForms = flattenLeft x in
    let funccall = `FuncCall {
      fcname = name;
      fcrettype = rettype;
      fcparams = argTypes;
      fcargs = argForms;
      fcptr = isPointer;
    }
    in
    match typeCheck bindings funccall with
      | TypeOf _ ->
          Some( bindings, toplevelForms @ [funccall] )
      | TypeError (fe,msg,f,e) ->
          raiseIllegalExpression expr (typeErrorMessage bindings (fe,msg,f,e))
  in
  match expr with
    | { id = name; args = args; } ->
        match lookup bindings name with
          | FuncSymbol func ->
              begin
                buildCall name func.rettype (List.map snd func.fargs) `NoFuncPtr bindings args
              end
          | VarSymbol var ->
              begin
                match var.typ with
                  | `Pointer `Function ft ->
                      begin
                        buildCall name ft.returnType ft.argTypes `FuncPtr bindings args
                      end
                  | _ ->
                      raiseIllegalExpression
                        expr
                        (sprintf "Variable should be a function pointer but has type %s"
                           (typeName var.typ))
              end
          | _ -> None


let translateMacro translateF (bindings :bindings) expr =
  match expr with
    | { id = macroName; args = args; } as expr ->
        match lookup bindings macroName with
          | MacroSymbol macro ->
              begin try
                let transformedExpr = macro.mtransformFunc bindings args in
                Some (translateF bindings transformedExpr)
              with
                | Failure msg ->
                    raiseIllegalExpression expr ("Could not expand macro: " ^ msg)
              end
          | _ -> None

let astType = `Record {
  rname = "ast";
  fields = [
    "id", `Pointer `Char;
    "childCount", `Int32;
    "childs", `Pointer (`Pointer (`TypeRef "ast"))
  ] }
let astPtrType = `Pointer astType

let flattenNestedTLForms = List.map (fun (`ToplevelForm tlform) -> tlform)

let macroFuncs = ref []

let rec listContains element = function
  | [] -> false
  | e :: rem when e = element -> true
  | _ :: rem -> listContains element rem


(* let createNativeMacro translateF bindings macroName argNames impl = *)
(*   let sexprImpl = impl in *)
(*   let bindings = *)
(*     List.fold_left *)
(*       (fun bindings name -> *)
(*          Bindings.addVar bindings *)
(*            (Lang.variable *)
(*               ~name *)
(*               ~typ:astPtrType *)
(*               ~default:(PointerVal (astPtrType, None)) *)
(*               ~storage:Lang.RegisterStorage *)
(*               ~global:false) ) *)
(*       bindings argNames *)
(*   in *)
(*   let _, xforms = translateF bindings sexprImpl in *)
(*   let initForms, implforms = extractToplevelForms xforms in *)
(*   let initForms = flattenNestedTLForms initForms in *)
(*   let localVar ~typ ~name = *)
(*     Lang.variable ~typ ~name ~storage:MemoryStorage *)
(*     ~global:false ~default:(Typesystems.Zomp.defaultValue typ) *)
(*   in *)
(*   let astType = *)
(*     match lookupType bindings "ast" with *)
(*       | Some t -> t *)
(*       | None -> raiseIllegalExpression (Ast2.idExpr "ast") "Could not find prelude type 'ast'" *)
(*   in *)
(*   let argParamName = "__macro_args" in *)
(*   let extractParamExprs : Lang.form list = *)
(*     let makeExtractCode argNum argName = *)
(*       let argNum = Int32.of_int argNum in *)
(*       let argName = argName ^ "_dummy" in *)
(*       (\* var ast* argName (ptradd __macro_args argNum) *\) *)
(*       let astPtr = `Pointer astType in *)
(*       let argVar = localVar ~typ:astPtr ~name:argName in *)
(*       let macroArgs = `Variable (localVar ~typ:(`Pointer astPtr) ~name:argParamName) in *)
(*       let argNumConstant = `Constant (Int32Val argNum) in *)
(*       `DefineVariable (argVar, Some (`LoadIntrinsic (`PtrAddIntrinsic *)
(*                                                        (macroArgs, *)
(*                                                         argNumConstant)))) *)
(*     in *)
(*     Common.listMapi makeExtractCode argNames *)
(*   in *)
(*   (\* let argsDummy : Lang.form = *\) *)
(*   (\*   `DefineVariable (localVar *\) *)
(*   (\*                      ~typ:(`Pointer (`Pointer astType)) *\) *)
(*   (\*                      ~name:argParamName, *\) *)
(*   (\*                    Some (`NullptrIntrinsic (`Pointer astType))) *\) *)
(*   (\* in *\) *)
(* (\* TODO: wieder alte methode von unten einbauen, neue special form "build macro impl ast" *\) *)
(* (\*   crash: weil __macro_args mit null initialisiert wird *\) *)
(*   (\* let implforms = argsDummy :: (extractParamExprs @ implforms) in *\) *)
(*   let implforms = extractParamExprs @ implforms in *)
(*   let macroFunc = { *)
(*     Lang.fname = macroName; *)
(*     rettype = astPtrType; *)
(*     fargs = [argParamName, `Pointer (`Pointer astType)]; *)
(*     (\* fargs = List.map (fun name -> (name, astPtrType)) argNames; *\) *)
(*     impl = Some (toSingleForm implforms); *)
(*   } in *)
(*   printf "----- %s\n" macroName; *)
(*   (\* printf "%s\n" (formToString argsDummy); *\) *)
(*   List.iter (printf "%s\n" ++ formToString) extractParamExprs; *)
(*   printf "=> %s\n" (funcToString macroFunc); *)
(*   List.iter (printf "%s\n" ++ formToString) implforms; *)
(*   printf "-----\n"; *)
(*   flush stdout; *)
(*   let tlforms = initForms @ [`DefineFunc macroFunc] in *)
(*   let llvmCodes = List.map Genllvm.gencodeTL tlforms in *)
(*   let llvmCode = Common.combine "\n" llvmCodes in *)
(*   Zompvm.evalLLVMCodeB *)
(*     ~targetModule:Zompvm.Runtime *)
(*     (if listContains macroName !macroFuncs then *)
(*        [macroName] *)
(*      else begin *)
(*        macroFuncs := macroName :: !macroFuncs; *)
(*        [] *)
(*      end) *)
(*     (tlforms @ [`DefineFunc macroFunc]) *)
(*     llvmCode *)

let createNativeMacro translateF bindings macroName argNames impl =
  let sexprImpl = impl in
  let bindings =
    List.fold_left
      (fun bindings name ->
         Bindings.addVar bindings
           (Lang.variable
              ~name
              ~typ:astPtrType
              ~default:(PointerVal (astPtrType, None))
              ~storage:Lang.RegisterStorage
              ~global:false) )
      bindings argNames
  in
  let _, xforms = translateF bindings sexprImpl in
  let initForms, implforms = extractToplevelForms xforms in
  let initForms = flattenNestedTLForms initForms in
  let macroFunc = {
    Lang.fname = macroName;
    rettype = astPtrType;
    fargs = List.map (fun name -> (name, astPtrType)) argNames;
    impl = Some (toSingleForm implforms);
  } in
  let tlforms = initForms @ [`DefineFunc macroFunc] in
  let llvmCodes = List.map Genllvm.gencodeTL tlforms in
  let llvmCode = Common.combine "\n" llvmCodes in
  Zompvm.evalLLVMCodeB
    ~targetModule:Zompvm.Runtime
    (if listContains macroName !macroFuncs then
       [macroName]
     else begin
       macroFuncs := macroName :: !macroFuncs;
       []
     end)
    (tlforms @ [`DefineFunc macroFunc])
    llvmCode

let log message =
  (* () *)
  eprintf "%s\n" message;
  flush stderr

let trace message f =
  log ("-> " ^ message);
  let result = f() in
  log ("<- " ^ message);
  result

let foldString str f init =
  let strLength = String.length str in
  let rec worker index v =
    if index >= strLength then v
    else worker (index+1) (f v str.[index])
  in
  worker 0 init

(** Utilites to convert between Ast2.sexpr and Zomp native AST *)
module NativeAst =
struct

  let isValidId name =
    foldString
      name (fun wasValid chr -> wasValid && Char.code chr < 128) true

  let rec extractSExprFromNativeAst astAddress =
    if Zompvm.zompAstIsNull astAddress then
      Ast2.idExpr "error, macro returned NULL"
    else
      let name =
        let extracted = Machine.zompAstId astAddress in
        if isValidId extracted then extracted
        else sprintf "compiler:error:invalidId '%s'" extracted
      in
      let childCount = Machine.zompAstChildCount astAddress in
      let childs =
        let rec getChilds num =
          if num < childCount then
            let childAddress = Machine.zompAstChild astAddress num in
            let child = extractSExprFromNativeAst childAddress in
            child :: getChilds (num+1)
          else
            []
        in
        getChilds 0
      in
      Ast2.expr name childs

  let rec buildNativeAst = function
    | {id = id; args = []} ->
        Zompvm.zompSimpleAst id
    | ast ->
        let childs = List.map buildNativeAst ast.args in
        let nativeAst = Zompvm.zompSimpleAst ast.id in
        List.iter (fun child -> Zompvm.zompAddChild ~parent:nativeAst ~child) childs;
        nativeAst
end

let translateFuncMacro (translateNestedF :exprTranslateF) name bindings argNames args impl =
  let expectedArgCount = List.length argNames in
  let foundArgCount = List.length args in
  if expectedArgCount <> foundArgCount then
    raiseIllegalExpression
      (Ast2.expr name args)
      (sprintf "Could not invoke macro: expected %d arguments but found %d" expectedArgCount foundArgCount);

  let rec repeatedList element count =
    if count > 0 then element :: repeatedList element (count-1)
    else []
  in

  let constructCallerFunction args =
    log "building arg expr";
    let argAstExprs = List.map Genllvm.sexpr2codeasis args in
    List.iter (fun expr -> log (Ast2.expression2string expr)) argAstExprs;
    log "translating to simpleform";
    let tlforms, argAstForms = List.fold_left
      (fun (prevTLForms, prevForms) expr ->
         let _, xforms = translateNestedF bindings expr in
         let tlforms, simpleforms = extractToplevelForms xforms in
         prevTLForms @ tlforms, prevForms @ [(toSingleForm simpleforms)] )
      ([], [])
      argAstExprs
    in
    let resultVar =
      Lang.variable
        ~name:"result"
        ~typ:astPtrType
        ~storage:MemoryStorage
        ~global:false
        ~default:(Typesystems.Zomp.defaultValue astPtrType)
    in
    log "building function impl";
    let implForms = [
      `DefineVariable(resultVar,
                      Some (`FuncCall { fcname = name;
                                        fcrettype = astPtrType;
                                        fcparams = repeatedList astPtrType (List.length argAstForms);
                                        fcargs = argAstForms;
                                        fcptr = `NoFuncPtr;
                                      }) );
      `CastIntrinsic (`Int32, `Variable resultVar);
    ]
    in
    let func = `DefineFunc {
      Lang.fname = "macroExec";
      rettype = `Int32;
      fargs = [];
      impl = Some (`Sequence implForms);
    } in
    log "...";
    let alltlforms = (flattenNestedTLForms tlforms) @ [func] in
    let llvmCodeLines = List.map Genllvm.gencodeTL alltlforms in
    let llvmCode = Common.combine "\n\n\n" llvmCodeLines in
    Zompvm.evalLLVMCodeB
      ~targetModule:Zompvm.Runtime
      ["macroExec"]
      alltlforms
      llvmCode
  in

  if false then constructCallerFunction [];

  let createArgs() =
    List.map NativeAst.buildNativeAst args;
  in

  let callMacro args =
    Zompvm.zompResetArgs();
    List.iter (fun astAddr -> Zompvm.zompAddPointerArg astAddr) args;
    Zompvm.zompRunFunctionPointerWithArgs name
  in

  log "Creating args";
  let argsAddresses = createArgs() in
  log "Calling macro";
  let astAddress = callMacro argsAddresses in

  (* log (sprintf "extracting result from address %d" astAddress); *)
  let sexpr = NativeAst.extractSExprFromNativeAst astAddress in
  (* log (sprintf "extracted:\n%s" (Ast2.toString sexpr)); *)
  sexpr

let translateVariadicFuncMacro
    (translateNestedF :exprTranslateF)
    name
    bindings
    argNames
    args
    impl
    =
  let internalArgCount = List.length argNames in
  let inflatedArgs =
    let regularArgs, variadicArgs = Common.splitAfter (internalArgCount-1) args in
    regularArgs @ [Ast2.seqExpr variadicArgs]
  in
  translateFuncMacro translateNestedF name bindings argNames inflatedArgs impl

let translateDefineMacro translateNestedF translateF (bindings :bindings) = function
  | { id = id; args =
        { id = name; args = [] }
        :: paramImpl
    } when id = macroMacro or id = macroReplacement ->
      let result =
        begin match List.rev paramImpl with
          | [] -> None
          | impl :: args ->
              begin
                let args = List.rev args in
                let argNames, isVariadic =
                  let rec worker accum = function
                    | [] ->
                        accum, false
                    | [{ id = id; args = [{ id = name; args = [] }] }] when id = macroRest ->
                        (name :: accum), true
                    | { id = name; args = [] } :: rem ->
                        worker (name :: accum) rem
                    | (_ as arg) :: _ ->
                        raiseIllegalExpression arg "only identifiers allowed as macro parameter"
                  in
                  let reversed, isVariadic = worker [] args in
                  List.rev reversed, isVariadic
                in
                let docstring =
                  Common.combine " " argNames ^ if isVariadic then "..." else ""
                in
                let macroF =
                  if id = macroMacro then begin
                    createNativeMacro translateNestedF bindings name argNames impl;
                    if isVariadic then
                      translateVariadicFuncMacro translateNestedF name
                    else
                      translateFuncMacro translateNestedF name
                  end else
                    (fun (_ :bindings) exprs -> Ast2.replaceParams exprs)
                in
                Some( Bindings.addMacro bindings name docstring
                        (fun bindings args -> macroF bindings argNames args impl), [] )
              end
        end
      in
      if not (id = macroReplacement) then begin
        printf "Warning: Invoked old translateDefineMacro for '%s'\n" name;
        flush stdout;
      end;
      result
  | _ ->
      None

let translateReturn (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [expr] } when id = macroReturn ->
      begin
        let _, form, toplevelExprs = translateToForms translateF bindings expr in
        Some( bindings, toplevelExprs @ [`Return form] )
      end
  | _ -> None

let translateTypedef translateF (bindings :bindings) =
  let translateRecordTypedef typeName componentExprs expr =
    let tempBindings = Bindings.addTypedef bindings typeName (`TypeRef typeName) in
    let expr2component =
      let translate name typeExpr =
        match translateType tempBindings typeExpr with
          | Some typ -> name, typ
          | None -> raise (CouldNotParseType typeName)
      in
      function
        | { id = typeName; args = [{ id = componentName; args = []}] } ->
            translate componentName (Ast2.idExpr typeName)
        | { id = seq; args = [typeExpr; { id = componentName; args = [] }] }
            when seq = macroSequence || seq = macroJuxOp ->
            translate componentName typeExpr
        | _ -> raiseIllegalExpression expr "(type typeName (typeExpression componentName)* ) expected"
    in
    let components = List.map expr2component componentExprs in
    let recordType = `Record { rname = typeName; fields = components } in
    Some (addTypedef bindings typeName recordType, [`Typedef (typeName, recordType)] )
  in
  function
    | { id = id; args = [
          { id = typeName; args = [] };
          { id = opseq; args = componentExprs }
        ] } as expr
        when id = macroTypedef && opseq = macroSeqOp ->
        translateRecordTypedef typeName componentExprs expr
    | { id = id; args = [
          { id = newTypeName; args = [] };
          targetTypeExpr;
        ] }
        when id = macroTypedef ->
        (** type foo typeExpr *)
        begin
          match translateType bindings targetTypeExpr with
            | None -> raiseInvalidType targetTypeExpr
            | Some t -> Some (addTypedef bindings newTypeName t, [`Typedef (newTypeName, t)] )
        end
    | { id = id; args =
          { id = typeName; args = [] }
          :: componentExprs
      } as expr
        when id = macroTypedef ->
        (** record typedef *)
        translateRecordTypedef typeName componentExprs expr
    | _ -> None

let translateRecord (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args =
        { id = name; args = []; }
        :: componentExprs
    } as expr
      when id = macroRecord ->
      begin
        match lookup bindings name with
          | TypedefSymbol `Record record ->
              begin
                let recordInitFunc name components =
                  { fname = name ^ "_init";
                    rettype = `Void;
                    fargs = components;
                    impl = None;
                  }
                in
                let initFunc = recordInitFunc name record.fields in
                let translate param compExpr = match (param, compExpr) with
                  | ((argName, argType), ({ id = compName; args = [argExpr] }) ) ->
                      begin
                        if argName = compName then begin
                          let _, form, toplevelForms = translateToForms translateF bindings argExpr in
                          if List.length toplevelForms > 0 then
                            raiseIllegalExpression expr "No nested toplevel forms allowed inside record";
                          form
                        end else
                          raiseIllegalExpression compExpr (sprintf "expected %s as id" argName)
                      end
                  | _, invalidCompExpr ->
                      raiseIllegalExpression invalidCompExpr "expected (componentName expr)"
                in
                let call = {
                  fcname = initFunc.fname;
                  fcrettype = initFunc.rettype;
                  fcparams = List.map snd initFunc.fargs;
                  fcargs = List.map2 translate initFunc.fargs componentExprs;
                  fcptr = `NoFuncPtr;
                } in
                Some( bindings, [`FuncCall call] )
              end
          | UndefinedSymbol -> raiseIllegalExpression expr (sprintf "%s is undefined" name)
          | _ -> raiseIllegalExpression expr (sprintf "%s is not a record" name)
      end
  | _ -> None

let translateGenericIntrinsic (translateF :exprTranslateF) (bindings :bindings) expr =
  let convertSimple typeExpr constructF =
    match translateType bindings typeExpr with
      | Some typ -> Some (bindings, [constructF typ] )
      | None -> None
  in
  let buildMallocInstruction typeExpr countExpr =
    let typ =
      match translateType bindings typeExpr with
        | Some t -> t
        | None -> raiseIllegalExpression typeExpr "Could not translate type"
    in
    let _, rightHandForm, toplevelForms = translateToForms translateF bindings countExpr in
    let mallocForm = `MallocIntrinsic (typ, rightHandForm) in
    match typeCheck bindings mallocForm with
      | TypeOf _ -> Some( bindings, toplevelForms @ [mallocForm] )
      | TypeError (fe,m,f,e) ->
          raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
  in
  let buildStoreInstruction ptrExpr rightHandExpr =
    let _, ptrForm, toplevelForms = translateToForms translateF bindings ptrExpr in
    let _, rightHandForm, toplevelForms2 = translateToForms translateF bindings rightHandExpr in
    let storeInstruction = `StoreIntrinsic (ptrForm, rightHandForm) in
    match typeCheck bindings storeInstruction with
      | TypeOf _ ->
          Some( bindings, toplevelForms @ toplevelForms2 @ [storeInstruction] )
      | TypeError (fe,m,f,e) ->
          raiseIllegalExpressionFromTypeError rightHandExpr (fe,m,f,e)
  in
  let buildLoadInstruction ptrExpr =
    let _, ptrForm, toplevelForms = translateToForms translateF bindings ptrExpr in
    let loadForm = `LoadIntrinsic ptrForm in
    match typeCheck bindings loadForm with
      | TypeOf _ ->
          Some( bindings, toplevelForms @ [loadForm] )
      | TypeError (fe,m,f,e) ->
          raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
  in
  match expr with
    | { id = id; args = [typeExpr] } when id = macroNullptr ->
        convertSimple typeExpr (fun t -> `NullptrIntrinsic t)
    | { id = id; args = [typeExpr] } when id = macroMalloc ->
        buildMallocInstruction typeExpr (Ast2.idExpr "1")
    | { id = id; args = [typeExpr; countExpr] } when id = macroMalloc ->
        buildMallocInstruction typeExpr countExpr
    | { id = id; args = [{ id = varName; args = [] }] } when id = macroGetaddr ->
        begin
          match lookup bindings varName with
            | VarSymbol var -> Some (bindings, [`GetAddrIntrinsic var] )
            | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" varName)
        end
    | { id = id; args = [ptrExpr; rightHandExpr] } when id = macroStore ->
        begin
          buildStoreInstruction ptrExpr rightHandExpr
        end
    | { id = id; args = [ ptrExpr ] } when id = macroLoad ->
        begin
          buildLoadInstruction ptrExpr
        end
    | { id = id; args = [ptrExpr; indexExpr] } when id = macroPtradd ->
        begin
          let _, ptrForm, toplevelForms = translateToForms translateF bindings ptrExpr in
          let _, indexForm, toplevelForms2 = translateToForms translateF bindings indexExpr in
          let ptradd = `PtrAddIntrinsic (ptrForm, indexForm) in
          match typeCheck bindings ptradd with
            | TypeOf _ -> Some( bindings, toplevelForms @ toplevelForms2 @ [ptradd] )
            | TypeError (fe,m,f,e) ->
                raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
        end
    | { id = id; args = [
          recordExpr;
          { id = fieldName; args = [ ] };
        ] } when id = macroFieldptr ->
        begin
          let _, recordForm, toplevelForms = translateToForms translateF bindings recordExpr in
          let (moreForms : formWithTLsEmbedded list), (recordForm' :Lang.form) =
            match recordForm with
              | `Variable ({ typ = `Record _; } as recordVar) ->
                  [], `GetAddrIntrinsic recordVar
              | _ ->
                  begin match typeCheck bindings recordForm with
                    | TypeOf (`Record _ as typ) ->
                        let newBindings, tempVar = getNewLocalVar bindings typ in
                        [((`DefineVariable (tempVar, Some recordForm)) :> formWithTLsEmbedded)],
                        `GetAddrIntrinsic tempVar
                    | TypeOf `Pointer `Record _ -> [], recordForm
                    | TypeOf invalidType ->
                        raiseIllegalExpression expr
                          (sprintf "%s but found %s"
                             ("Can only access struct members from a var of [pointer to] record type")
                             (typeName invalidType))
                    | TypeError (fe,m,f,e) ->
                        raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
                  end
          in
          let fieldptr = `GetFieldPointerIntrinsic (recordForm', fieldName) in
          match typeCheck bindings fieldptr with
            | TypeOf _ -> Some( bindings, toplevelForms @ moreForms @ [fieldptr] )
            | TypeError (fe,m,f,e) ->
                begin
                  raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
                end
        end
    | { id = id; args = [targetTypeExpr; valueExpr] } when id = macroCast ->
        begin
          match translateType bindings targetTypeExpr with
            | Some typ ->
                let _, valueForm, toplevelForms = translateToForms translateF bindings valueExpr in
                let castForm = `CastIntrinsic( typ, valueForm ) in
                Some( bindings, toplevelForms @ [castForm] )
            | None ->
                raiseIllegalExpression targetTypeExpr "Expected a valid type"
        end
    | _ ->
        None

let translateLabel (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [{ id = name; args = [] }] } when id = macroLabel ->
      begin
        Some( addLabel bindings name, [`Label { lname = name; }] )
      end
  | _ -> None

let translateBranch (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [{ id = labelName; args = [] }] } when id = macroBranch ->
      begin
        Some( bindings, [`Jump { lname = labelName; }] )
      end
  | { id = id; args = [
        { id = condVarName; args = [] };
        { id = trueLabelName; args = [] };
        { id = falseLabelName; args = [] };
      ] } as expr when id = macroBranch ->
      begin
        match lookup bindings condVarName with
          | VarSymbol var when var.typ = `Bool ->
              begin
                Some( bindings, [`Branch {
                                   bcondition = { var with typ = `Bool };
                                   trueLabel = { lname = trueLabelName};
                                   falseLabel = { lname = falseLabelName}; }] )
              end
          | _ ->
              raiseIllegalExpression expr "Branch requires a bool variable as condition"
      end
  | _ -> None


type toplevelExprTranslateF = bindings -> sexpr -> bindings * toplevelExpr list

let translateGlobalVar (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        typeExpr;
        { id = name; args = [] };
        { id = valueString; args = [] }
      ] } as expr
      when id = macroVar ->
      begin
        match translateType bindings typeExpr with
          | Some ctyp -> begin
              match ctyp with
                | (#integralType as typ) | (`Pointer `Char as typ) ->
                    let value = parseValue typ valueString in
                    let var = globalVar name typ value in
                    let newBindings = addVar bindings var in
                    Some( newBindings, [ `GlobalVar var ] )
                | `Pointer targetType ->
                    if valueString = "null" then
                      let var = globalVar name (`Pointer targetType) (PointerVal (targetType, None)) in
                      let newBindings = addVar bindings var in
                      Some( newBindings, [`GlobalVar var] )
                    else
                      raiseIllegalExpression expr "only null values supported for global pointers currently"
                | `Array (targetType, size) ->
                    if valueString = "0" then
                      let var = globalVar name (`Array (targetType,size)) (ArrayVal (targetType, [])) in
                      let newBindings = addVar bindings var in
                      Some( newBindings, [`GlobalVar var] )
                    else
                      raiseIllegalExpression expr "only 0 supported to init global pointers currently"
                | `Record recordT ->
                    if valueString = "0" then
                      let var = globalVar name (`Record recordT) (RecordVal (recordT.rname, [])) in
                      let newBindings = addVar bindings var in
                      Some( newBindings, [`GlobalVar var] )
                    else
                      raiseIllegalExpression expr "only 0 supported to init global structs currently"
                | _ -> raiseIllegalExpression expr
                    "only integral types legal for global variables at this time"
            end
          | None -> raiseInvalidType typeExpr
      end
  | _ ->
      None

(* type env = { *)
(*   bindings :Bindings.t; *)
(*   translateF :exprTranslateF; *)
(* } *)

type 'translateF env = {
  bindings :Bindings.t;
  translateF :'translateF;
  parseF :string -> Ast2.t list option;
}

type 'a mayfail =
  | Result of 'a
  | Error of string list

let combineErrors msg = function
    | [] -> msg ^ ": no error message given"
    | errors -> msg ^ ": " ^ Common.combine "\n  " errors

type translationResult = (bindings * (formWithTLsEmbedded list)) mayfail
type toplevelTranslationResult = (bindings * (toplevelExpr list)) mayfail

let translateDummy (env :exprTranslateF env) (expr :Ast2.sexpr)  :translationResult =
  Error ["Not supported at all"]

module Macros =
struct
  let buildNativeMacroFunc translateF bindings
      macroName argNames implExprs (isVariadic : [`IsVariadic | `IsNotVariadic])
      =
    let bindings = Bindings.addVar bindings
      (Lang.variable
         ~name:"macro_args"
         ~typ:(`Pointer astPtrType)
         ~default:(PointerVal (`Pointer astPtrType, None))
         ~storage:Lang.RegisterStorage
         ~global:false)
    in
    let argParamName = "macro_args" in
    let buildParamFetchExpr num name =
      Ast2.expr "std:base:localVar" [Ast2.idExpr name;
                                     Ast2.expr "load" [
                                       Ast2.expr "ptradd" [Ast2.idExpr argParamName;
                                                           Ast2.idExpr (sprintf "%d" num)]
                                     ]]
    in
    let fetchParamExprs = Common.listMapi buildParamFetchExpr argNames in
    let sexprImpl = Ast2.seqExpr (fetchParamExprs @ implExprs) in
    let _, xforms = translateF bindings sexprImpl in
    let initForms, implforms = extractToplevelForms xforms in
    let initForms = flattenNestedTLForms initForms in
    let astType =
      match lookupType bindings "ast" with
        | Some t -> t
        | None -> raiseIllegalExpression (Ast2.idExpr "ast") "Could not find prelude type 'ast'"
    in
    let macroFunc = {
      Lang.fname = macroName;
      rettype = astPtrType;
      fargs = [argParamName, `Pointer (`Pointer astType)];
      (* fargs = List.map (fun name -> (name, astPtrType)) argNames; *)
      impl = Some (toSingleForm implforms);
    } in
    let tlforms = initForms @ [`DefineFunc macroFunc] in
    tlforms, macroFunc

  let translateMacroCall name paramCount isVariadic =
    let nativeFuncAddr = Machine.zompAddressOfMacroFunction ~name in
    (fun bindings args ->
       begin
         let invokeMacro args =
           let nativeArgs = List.map NativeAst.buildNativeAst args in
           Machine.zompResetMacroArgs();
           List.iter (fun ptr -> Machine.zompAddMacroArg ~ptr) nativeArgs;
           let nativeResultAst = Machine.zompCallMacro nativeFuncAddr in
           let resultAst = NativeAst.extractSExprFromNativeAst nativeResultAst in
           resultAst
         in
         let argCount = List.length args in
         match isVariadic with
           | `IsNotVariadic -> begin
               if argCount <> paramCount then begin
                 raiseIllegalExpression
                   (Ast2.expr name args)
                   (sprintf "Expected %d args but found %d" paramCount argCount);
               end else begin
                 invokeMacro args
               end
             end
           | `IsVariadic -> begin
               if argCount < paramCount-1 then begin
                 raiseIllegalExpression
                   (Ast2.expr name args)
                   (sprintf "Expected at least %d args but found only %d"
                      (paramCount-1) argCount)
               end;
               let declaredArgs, variadicArgs = Common.splitAfter (paramCount-1) args in
               let inflatedArgs = declaredArgs @ [Ast2.seqExpr variadicArgs] in
               invokeMacro inflatedArgs
             end
               (* raiseIllegalExpression *)
               (*   {Ast2.id = name; args = args} *)
               (*   "Variadic arg macros cannot be called, yet" *)
       end : bindings -> Ast2.sexpr list -> Ast2.sexpr)

  let translateDefineMacro translateNestedF env expr =
    let decomposeMacroDefinition expr =
      let nameParamImplOption =
        match expr with
          | {Ast2.args = {Ast2.id = name; args = []} :: paramsAndImpl} ->
              begin match List.rev paramsAndImpl with
                | {Ast2.id = seqId; args = implExprs} :: params ->
                    Some (name, List.rev params, implExprs)
                | _ ->
                    None
              end
          | _ ->
              None
      in

      match nameParamImplOption with
        | Some (name, params, implExprs) ->
            begin
              let decomposeMacroParam = function
                | {Ast2.id = paramName; Ast2.args = []} ->
                    Result (paramName, `IsNotVariadic)
                | {Ast2.id = id; args = [{Ast2.id = paramName; Ast2.args=[]}]}
                    when id = macroRest ->
                    Result (paramName, `IsVariadic)
                | (_ as invalidParam) ->
                    Error [sprintf "Invalid macro param: %s" (Ast2.toString invalidParam)]
              in
              match List.rev params with
                | [] ->
                    Result (name, ([] :string list), implExprs, `IsNotVariadic)
                | lastParam :: frontParamsRev ->
                    let combineParams names param =
                      match names, param with
                        | Result names, Result (paramName, `IsNotVariadic) ->
                            Result (paramName :: names)
                        | Result names, Result (paramName, `IsVariadic) ->
                            Error [sprintf
                                     "param %s is variadic but not the last param"
                                     paramName]
                        | Error msg, _ | _, Error msg ->
                            Error msg
                    in
                    let frontParamNamesOpt =
                      List.fold_left combineParams
                        (Result [])
                        (List.map decomposeMacroParam frontParamsRev)
                    in
                    match frontParamNamesOpt with
                      | Result frontParamNames ->
                          begin match decomposeMacroParam lastParam with
                            | Result (lastParamName, isVariadic) ->
                                Result (name,
                                        frontParamNames @ [lastParamName],
                                        implExprs,
                                        isVariadic)
                            | Error msg ->
                                Error msg
                          end
                      | Error msg ->
                          Error msg
            end
        | _ ->
            Error [sprintf "Expecting '%s name paramName* lastParamVariadic...? seq" expr.id]
    in
    match decomposeMacroDefinition expr with
      | Result (name, paramNames, implExprs, isVariadic) ->
          begin
            let tlexprs, func =
              buildNativeMacroFunc
                translateNestedF env.bindings
                name paramNames implExprs isVariadic
            in

            let llvmCodeFragments = List.map Genllvm.gencodeTL tlexprs in

            Zompvm.evalLLVMCodeB
              ~targetModule:Zompvm.Runtime
              (if listContains name !macroFuncs then
                 [name]
               else begin
                 macroFuncs := name :: !macroFuncs;
                 []
               end)
              []
              (Common.combine "\n" llvmCodeFragments);

            flush stdout;

            let docstring =
              Common.combine " " paramNames ^
                match isVariadic with | `IsVariadic -> "..." | _ -> ""
            in

            let newBindings = Bindings.addMacro env.bindings name docstring
              (translateMacroCall name (List.length paramNames) isVariadic)
            in
            Result (newBindings, [])
          end
      | Error reasons ->
          Error [combineErrors "Could not define macro: " reasons]

end

(** A module defining various zomp transformations *)
module type Zomp_transformer =
sig
  val register : (string -> (exprTranslateF env -> Ast2.t -> translationResult) -> unit) -> unit
end

module Base : Zomp_transformer =
struct
  let translateDefineLocalVar env expr =
    let transform id name typeExpr valueExpr =
      let declaredType = match typeExpr with
        | Some e ->
            begin
              match translateType env.bindings e with
                | Some t -> Some t
                | None -> raise (CouldNotParseType (Ast2.expression2string e))
            end
        | None -> None
      in
      let valueType, toplevelForms, implForms =
        match valueExpr with
          | Some valueExpr ->
              begin
                let _, simpleform = env.translateF env.bindings valueExpr in
                let toplevelForms, implForms = extractToplevelForms simpleform in
                match typeCheck env.bindings (`Sequence implForms) with
                  | TypeOf t ->
                      Some t, toplevelForms, implForms
                  | TypeError (fe,m,f,e) ->
                      raiseIllegalExpressionFromTypeError valueExpr (fe,m,f,e)
              end
          | None -> None, [], []
      in
      let varType =
        match declaredType, valueType with
          | Some declaredType, Some valueType
              when equalTypes env.bindings declaredType valueType
                -> declaredType
          | Some declaredType, Some valueType ->
              raiseIllegalExpressionFromTypeError
                expr (Semantic.Ast expr, "Types do not match",declaredType,valueType)
          | None, Some valueType ->
              valueType
          | Some declaredType, None ->
              declaredType
          | None, None ->
              raiseIllegalExpression
                expr "var needs either a default value or declare a type"
      in
      match varType with
        | #integralType | `Pointer _ | `Function _ as typ ->
            begin
              let var = variable name typ (defaultValue typ) MemoryStorage false in
              let defvar = `DefineVariable (var, Some (`Sequence implForms))
              in
              match typeCheck env.bindings defvar with
                | TypeOf _ -> Result( addVar env.bindings var, toplevelForms @ [defvar] )
                | TypeError (fe, m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe, m,f,e)
            end
        | (`Record _ as typ) ->
            begin
              match valueExpr with
                | None ->
                    let var = variable name typ (defaultValue typ) MemoryStorage false in
                    Result( addVar env.bindings var, [ `DefineVariable (var, None) ] )
                | Some valueExpr -> raiseIllegalExpression valueExpr "Record type var must not have a default value"
            end
        | `TypeRef _ ->
            raiseIllegalExpression expr "Internal error: received unexpected type ref"
        | `Array _ ->
            raiseIllegalExpression expr "Array vars not supported, yet"
    in
    match expr with
      | { args = [
            { id = name; args = [] };
            valueExpr
          ] } ->
          transform id name None (Some valueExpr)

      | _ ->
          Error [sprintf "Expecting '%s name valueExpr'" expr.id]

  (** exprTranslateF env -> Ast2.sexpr -> translationResult *)
  let translateAssignVar (env :exprTranslateF env) expr =
    let doTranslation id varName rightHandExpr =
      let _, rightHandForm, toplevelForms =
        translateToForms env.translateF env.bindings rightHandExpr
      in
      match lookup env.bindings varName with
        | VarSymbol lhsVar ->
            begin
              match typeCheck env.bindings rightHandForm with
                | TypeOf rhsType when rhsType = lhsVar.typ ->
                    Result (env.bindings,
                            toplevelForms @ [`AssignVar (lhsVar, rightHandForm)] )
                | TypeOf invalidRhsType ->
                    Error [sprintf "Type error: cannot assign %s to %s"
                             (typeName invalidRhsType) (typeName lhsVar.typ)]
                | TypeError (fe,m,f,e) ->
                    Error [typeErrorMessage env.bindings (fe,m,f,e)]
            end
        | _ -> Error [sprintf "Could not find variable %s" varName]
    in

    match expr with
      | { id = id; args = [
            { id = varName; args = [] };
            rightHandExpr;
          ] } when id = macroAssign ->
          doTranslation id varName rightHandExpr
      | _ ->
          Error ["Expected 'assign varName valueExpr'"]

  let register addF =
    addF "std:base:localVar" translateDefineLocalVar;
    addF macroAssign translateAssignVar;
end

module Array : Zomp_transformer =
struct
  let arraySize (env: exprTranslateF env) expr =
    match expr with
      | { id = _; args = [arrayExpr] } ->
          begin
            let _, rightHandForm, toplevelForms =
              translateToForms env.translateF env.bindings arrayExpr
            in
            match typeCheck env.bindings rightHandForm with
              | TypeOf `Array(_, size) ->
                  Result (env.bindings,
                          toplevelForms @ [(rightHandForm :> formWithTLsEmbedded);
                                           `Constant (Int32Val (Int32.of_int size))])
              | TypeOf invalidType ->
                  Error [typeErrorMessage
                           env.bindings
                           (Semantic.Ast expr,
                            "Cannot get size of array",
                            invalidType,
                            `Any "array")]
              | TypeError (fe,m,f,e) ->
                  Error [typeErrorMessage env.bindings (fe,m,f,e)]
          end
      | _ ->
          Error ["Expected 'zmp:array:size arrayExpr'"]

  let arrayAddr (env: exprTranslateF env) = function
    | {args = [arrayPtrExpr]} as expr ->
        begin
          let _, arrayPtrForm, tlforms =
            translateToForms env.translateF env.bindings arrayPtrExpr
          in
          match typeCheck env.bindings arrayPtrForm with
            | TypeOf `Pointer `Array(memberType,_) ->
                begin
                  Result(
                    env.bindings, tlforms @ [
                      `CastIntrinsic(`Pointer memberType, arrayPtrForm)])
                end
            | TypeOf invalidType ->
                Error [typeErrorMessage env.bindings
                         (Semantic.Ast expr,
                          "Cannot get address of first element",
                          invalidType, `Any "Pointer to array")]
            | TypeError (fe, m,f,e) ->
                Error [typeErrorMessage env.bindings (fe,m,f,e)]
        end
    | _ ->
        Error ["Expected 'zmp:array:addrOf arrayExpr indexExpr'"]

  let register addF =
    addF "zmp:array:size" arraySize;
    addF "zmp:array:addr" arrayAddr

end

module Overloaded_ops : Zomp_transformer =
struct
  (** creates a macro which turns baseName(l,r) into
    * baseName_ltype_rtype(l,r). Has special handling for op+/op- and pointer
    * arguments *)
  let overloadedOperator baseName (env :exprTranslateF env) = function
    | {args = [leftExpr; rightExpr]} ->
        begin
          let _, leftForm, toplevelFormsLeft =
            translateToForms env.translateF env.bindings leftExpr
          in
          let _, rightForm, toplevelFormsRight =
            translateToForms env.translateF env.bindings rightExpr
          in
          let tc = typeCheck env.bindings in
          match baseName, tc leftForm, tc rightForm with
            | "op+", TypeOf `Pointer _, TypeOf `Int32 ->
                begin
                  Result(env.bindings,
                         toplevelFormsLeft @ toplevelFormsRight @
                         [`PtrAddIntrinsic (leftForm, rightForm)])
                end
            | "op-", TypeOf `Pointer _, TypeOf `Int32 ->
                begin
                  Result(env.bindings,
                         toplevelFormsLeft @ toplevelFormsRight @
                           [`PtrAddIntrinsic (leftForm,
                                              `FuncCall { fcname = "u32:neg";
                                                          fcrettype = `Int32;
                                                          fcparams = [`Int32];
                                                          fcargs = [rightForm];
                                                          fcptr = `NoFuncPtr })])
                end
            | _, TypeOf leftType, TypeOf rightType ->
                begin
                  let postfixAndImplConv form = function
                    | `Pointer _ ->
                        "ptr", `CastIntrinsic(`Pointer `Void, form)
                    | typ ->
                        typeName typ, form
                  in
                  let typenameL, castFormL = postfixAndImplConv leftForm leftType in
                  let typenameR, castFormR = postfixAndImplConv rightForm rightType in
                  let funcName = baseName ^ "_" ^ typenameL ^ "_" ^ typenameR in
                  match Bindings.lookup env.bindings funcName with
                    | FuncSymbol func -> begin
                        Result(env.bindings,
                               toplevelFormsLeft @ toplevelFormsRight @
                                 [`FuncCall { fcname = func.fname;
                                              fcrettype = func.rettype;
                                              fcparams = List.map snd func.fargs;
                                              fcargs = [castFormL; castFormR];
                                              fcptr = `NoFuncPtr; }])
                      end
                    | _ -> begin
                        Error [sprintf "No overload for %s(%s, %s) found (expected function %s)"
                                 baseName typenameL typenameR funcName]
                      end
                end
            | _, lresult, rresult ->
                let typeErrorMessagePotential potError =
                  match potError with
                    | TypeError (fe,msg,found,expected) ->
                        typeErrorMessage env.bindings (fe, msg, found, expected)
                    | _ ->
                        ""
                in
                Error [typeErrorMessagePotential lresult ^
                         typeErrorMessagePotential rresult]
        end
    | _ ->
        Error ["Expected two arguments"]

  let overloadedFunction baseName (env :exprTranslateF env) = function
    | {args = [argExpr]} ->
        begin
          let _, argForm, toplevelForms =
            translateToForms env.translateF env.bindings argExpr
          in
          match typeCheck env.bindings argForm with
            | TypeOf argType ->
                begin
                  let funcName = baseName ^ "_" ^ typeName argType in
                  match Bindings.lookup env.bindings funcName with
                    | FuncSymbol func ->
                        Result(env.bindings,
                               toplevelForms @
                                 [`FuncCall { fcname = func.fname;
                                              fcrettype = func.rettype;
                                              fcparams = List.map snd func.fargs;
                                              fcargs = [argForm];
                                              fcptr = `NoFuncPtr; }])
                    | _ ->
                        Error [sprintf "No overload for %s(%s) found (expected function %s)"
                                 baseName (typeName argType) funcName]
                end
            | TypeError (fe,m,f,e) ->
                Error [typeErrorMessage env.bindings (fe,m,f,e)]
        end
    | _ ->
        Error ["Expected one argument"]

  let register addF =
    let addOp (name, op) =
      addF ("zmp:cee:" ^ name) (overloadedOperator op)
    in
    List.iter addOp [
      "add", "op+";
      "sub", "op-";
      "mul", "op*";
      "div", "op/";
      "equal", "op==";
      "notEqual", "op!=";
      "greater", "op>";
      "greaterEqual", "op>=";
      "less", "op<";
      "lessEqual", "op<=";
      "and", "op&";
      "or", "op|";
      "xor", "op^";
      "shl", "op<<";
      "shr", "op>>";
      "pow", "op**";
    ];
    let addFun name = addF ("zmp:cee:" ^ name) (overloadedFunction name) in
    List.iter addFun
      ["print"; "write";
       "toInt"; "toFloat"; "toDouble"; "toBool"; "toChar"; "toCString";
       "neg"; "not"];
end

let baseInstructions =
  let table = Hashtbl.create 32 in
  let add = Hashtbl.add table in
  add "dummy" translateDummy;
  Base.register add;
  Array.register add;
  Overloaded_ops.register add;
  table

let translateFromDict
    baseInstructions
    translateF
    (bindings :bindings)
    (expr :Ast2.sexpr)
    =
  try
    let handler = Hashtbl.find baseInstructions expr.id in
    let env = {
      bindings = bindings;
      translateF = translateF;
      parseF = Parseutils.parseIExprsOpt;
    } in
    match handler env expr with
      | Error errors ->
          print_string (combineErrors "Swallowed errors: " errors);
          print_newline();
          flush stdout;
          None
      | Result (bindings, tlexprs) ->
          Some (bindings, tlexprs)
  with Not_found ->
    None

let rec translate errorF translators bindings expr =
  let rec t = function
    | [] -> errorF expr "No translator matched expression"
    | f :: remf -> begin
        Zompvm.currentBindings := bindings;
        match f (translate errorF translators) bindings expr with
          | Some (newBindings, result) -> (newBindings, result)
          | None -> t remf
      end
  in
  t translators

let rec translateNested translateF bindings = translate raiseIllegalExpression
  [
    translateFromDict baseInstructions;
    translateSeq;
    translateDefineVar;
    translateSimpleExpr;
    translateFuncCall;
    translateMacro;
    translateDefineMacro translateNested;
    translateGenericIntrinsic;
    translateRecord;
    translateReturn;
    translateLabel;
    translateBranch;
  ]
  translateF bindings
let translateNested = sampleFunc2 "translateNested" translateNested

let translateAndEval handleLLVMCodeF env exprs =
  collectTimingInfo "translateAndEval"
    (fun () ->
       let newBindings, tlexprsFromFile =
         List.fold_left
           (fun (bindings,prevExprs) sexpr ->
              let newBindings, newExprs = env.translateF bindings sexpr in
              List.iter
                (fun form ->
                   let llvmCode =
                     collectTimingInfo "gencode"
                       (fun () ->
                          Genllvm.gencodeTL form) in
                   collectTimingInfo "Zompvm.evalLLVMCode"
                     (fun () ->
                        Zompvm.evalLLVMCode bindings [form] llvmCode);
                   collectTimingInfo "handleLLVMCodeF"
                     (fun () ->
                        handleLLVMCodeF llvmCode))
                newExprs;
              newBindings, prevExprs @ newExprs )
           (env.bindings, [])
           exprs
       in
       Result (newBindings, []))

let translateSeqTL handleLLVMCodeF env expr =
  translateAndEval handleLLVMCodeF env expr.args

let () =
  Hashtbl.add baseInstructions "macro" (Macros.translateDefineMacro translateNested);
  ()

let toplevelBaseInstructions =
  let table : (string, toplevelExprTranslateF env -> Ast2.sexpr -> toplevelTranslationResult) Hashtbl.t =
    Hashtbl.create 32
  in
  (* Hashtbl.add table "macro" (Macros.translateDefineMacro translateNested); *)
  Hashtbl.add table "macro" (sampleFunc2 "macro(dict)" (Macros.translateDefineMacro translateNested));
  (* Hashtbl.add table "seq" translateSeqTL; *)
  table

let translateCompileTimeVar (translateF :toplevelExprTranslateF) (bindings :bindings) = function
  | { id = "antiquote"; args = [quotedExpr] } ->
      begin
        let newBindings, simpleforms = translateF bindings quotedExpr in
        match Common.lastElement simpleforms with
          | Some `GlobalVar var ->
              begin
                let llvmCodes = List.map Genllvm.gencodeTL simpleforms in
                let llvmCode = Common.combine "\n" llvmCodes in
                printf "sending code to zompvm\n"; flush stdout;
                Zompvm.evalLLVMCode ~targetModule:Zompvm.Compiletime bindings simpleforms llvmCode;
                Some( bindings, [] )
              end
          | _ ->
              raiseIllegalExpression quotedExpr "Did not evaluate to a variable declaration"
      end
  | _ ->
      None

let matchFunc =
  let convertParam = function
    | { id = opjux; args = [typeExpr; {id = paramName; args = []}] as param }
        when opjux = macroJuxOp ->
        Ast2.shiftLeft param
    | _ -> failwith ""
  in
  function
      (** func def from sexpr **)
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = seq1; args = paramExprs };
          { id = seq2; args = _ } as implExpr;
        ] }
        when id = macroFunc && seq1 = macroSequence && seq2 = macroSequence ->
        `FuncDef (name, typeExpr, paramExprs, implExpr)

    (** func decl from sexpr **)
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = seq; args = paramExprs };
        ] }
        when id = macroFunc && seq = macroSequence ->
        `FuncDecl (name, typeExpr, paramExprs)

    (** func decl from iexpr with multiple/0 arguments **)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args = { id = name; args = [] } :: paramExprs }
        ] } as expr
        when id = macroFunc && opcall = macroCallOp ->
        begin try
          `FuncDecl (name, typeExpr, List.map convertParam paramExprs)
        with Failure _ ->
          `NotAFunc expr
        end

    (** func decl from iexpr with one argument **)
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = jux; args = _ } as paramExpr;
        ] } as expr
        when id = macroFunc && jux = macroJuxOp ->
        begin try
          `FuncDecl (name, typeExpr, List.map convertParam [paramExpr])
        with Failure _ ->
          `NotAFunc expr
        end

    (** func def from iexpr with multiple/0 arguments **)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args =
              { id = name; args = [] }
              :: paramExprs };
          { id = opseq; args = _ } as implExpr
        ] } as expr
        when id = macroFunc && opcall = macroCallOp && opseq = macroSeqOp ->
        begin try
          `FuncDef (name, typeExpr, List.map convertParam paramExprs, implExpr)
        with Failure _ ->
          `NotAFunc expr
        end

    (** func def from iexpr with one argument **)
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = jux; args = _ } as paramExpr;
          { id = seq; args = _ } as implExpr
        ] } as expr
        when id = macroFunc && jux = macroJuxOp && seq = macroSeqOp ->
        begin try
          `FuncDef (name, typeExpr, List.map convertParam [paramExpr], implExpr)
        with Failure _ ->
          `NotAFunc expr
        end

    | expr ->
        `NotAFunc expr

let rec translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) expr =
  let sanityChecks returnType name params =
    (* (match returnType with *)
    (*    | `Record _ -> raiseIllegalExpression expr "Functions cannot return records, yet" *)
    (*    | _ -> ()); *)
    let module StringSet = Set.Make(String) in
    let _ =
      List.fold_left
        (fun prevNames (nextName, _) ->
           if StringSet.mem nextName prevNames then
             raiseIllegalExpression expr
               (sprintf "Each argument needs a distinct name. %s used more than once" nextName);
           StringSet.add nextName prevNames)
        StringSet.empty
        params
    in
    let nameRE = "^[a-zA-Z0-9][^\"]*$" in
    if not (name =~ nameRE) then
      raiseIllegalExpression expr
        (sprintf "Function names must match the following regexp: %s" nameRE);
    ()
  in
  let buildFunction bindings typ uncheckedName paramExprs implExprOption =
    let name = removeQuotes uncheckedName in
    let expr2param argExpr =
      let translate varName typeExpr =
        match translateType bindings typeExpr with
          | Some typ -> (varName, typ)
          | None -> raiseInvalidType typeExpr
      in
      match argExpr with
        | { id = "seq"; args = [typeExpr; { id = varName; args = []};] } ->
            translate varName typeExpr
        | { id = typeName; args = [{ id = varName; args = [] }] } ->
            translate varName (Ast2.idExpr typeName)
        | _ as expr ->
            raiseIllegalExpression expr "Expected 'typeName varName' for param"
    in
    let rec localBinding bindings = function
      | [] -> bindings
      | (name, typ) :: tail ->
          let var =
            variable
              ~name
              ~typ
              (** structs are copied into a local var by genllvm *)
              ~storage:(match typ with | `Record _ -> MemoryStorage | _ -> RegisterStorage)
              ~global:false
              ~default:(Typesystems.Zomp.defaultValue typ)
          in
          localBinding (addVar bindings var) tail
    in

    let params = List.map expr2param paramExprs in
    sanityChecks typ name params;
    let innerBindings = localBinding bindings params in
    let nestedTLForms, impl = match implExprOption with
      | Some implExpr ->
          let nestedForms = snd (translateNested innerBindings implExpr) in
          let nestedTLForms, implForms = extractToplevelForms nestedForms in
          let nestedTLForms = List.map (fun (`ToplevelForm f) -> f) nestedTLForms in
          let implFormsWithFixedVars = moveLocalVarsToEntryBlock (`Sequence implForms) in
          nestedTLForms, Some (`Sequence implFormsWithFixedVars)
      | None -> [], None
    in
    let f = func name typ params impl in
    match Semantic.functionIsValid f with
      | `Ok ->
          let newBindings =
            match impl with
              | None -> addFunc bindings f
              | _ -> bindings
          in
          let funcDef = `DefineFunc f in
          newBindings, nestedTLForms, funcDef
      | `Errors messages -> raiseIllegalExpression
          expr (Common.combine "\n" ((sprintf "Could not translate function %s:" name)::messages))
  in
  match matchFunc expr with
    | `FuncDef (name, typeExpr, paramExprs, implExpr) ->
        begin
          match translateType bindings typeExpr with
            | Some typ -> begin
                let tempBindings, _, _ = buildFunction bindings typ name paramExprs None in
                let newBindings, toplevelForms, funcDef =
                  buildFunction tempBindings typ name paramExprs (Some implExpr)
                in
                match typeCheckTL newBindings funcDef with
                  | TypeOf _ -> Some( newBindings, toplevelForms @ [funcDef] )
                  | TypeError (fe, msg, declaredType, returnedType) ->
                      raiseIllegalExpression
                        expr
                        (typeErrorMessage bindings (fe, msg, returnedType, declaredType))
              end
            | None -> raiseInvalidType typeExpr
        end
    | `FuncDecl (name, typeExpr, paramExprs) ->
        begin
          match translateType bindings typeExpr with
            | Some typ ->
                begin
                  let newBindings, _, funcDecl = buildFunction bindings typ name paramExprs None in
                  Some (newBindings, [funcDecl] )
                end
            | None ->
                raiseInvalidType typeExpr
        end
    | _ ->
        None

and translateTL bindings expr = translate raiseIllegalExpression
  [
    sampleFunc3 "translateFromDict" (translateFromDict toplevelBaseInstructions);
    sampleFunc3 "translateGlobalVar" translateGlobalVar;
    sampleFunc3 "translateFunc" translateFunc;
    sampleFunc3 "translateTypedef" translateTypedef;
    sampleFunc3 "translateDefineMacro" (translateDefineMacro translateNested);
    sampleFunc3 "translateMacro" translateMacro;
    sampleFunc3 "translateCompileTimeVar" translateCompileTimeVar;
  ]
  bindings expr

let translateTL = Common.sampleFunc2 "translateTL" translateTL

let translateInclude includePath handleLLVMCodeF env expr =
  let importFile fileName =
    let fileContent =
      collectTimingInfo "readFileContent"
        (fun () -> Common.readFile ~paths:!includePath fileName)
    in
    let exprs =
      collectTimingInfo "parse"
        (fun () -> Parseutils.parseIExprsNoCatch fileContent)
    in
    collectTimingInfo "translateAndEval"
      (fun () -> translateAndEval handleLLVMCodeF env exprs)
  in
  collectTimingInfo "translateInclude"
    (fun () ->
       match expr with
         | { id = id; args = [{ id = quotedFileName; args = []}] } when id = macroInclude ->
             begin
               let fileName =
                 let fileName = Common.removeQuotes quotedFileName in
                 if Common.endsWith fileName ".zomp" then fileName
                 else fileName ^ ".zomp"
               in
               try
                 importFile fileName
               with
                 | Indentlexer.UnknowToken(loc,token,reason) -> begin
                     let msg = sprintf "%s: unknown token '%s' (%s)"
                       (Indentlexer.locationToString {
                          loc with Indentlexer.fileName = fileName })
                       token
                       reason
                     in
                     Error [msg]
                   end
                 | error ->
                     let msg = Printexc.to_string error in
                     Error [sprintf "%s, while compiling included file %s" msg fileName]
             end
         | _ ->
             Error ["Expecting 'include \"fileName.zomp\"'"])


