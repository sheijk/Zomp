open Lang
open Semantic
open Ast2
open Common
open Bindings
open Printf
open Basics

module Utilities =
struct
  let log message =
    eprintf "%s\n" message;
    flush stderr

  let trace message f =
    log ("-> " ^ message);
    let result = f() in
    log ("<- " ^ message);
    result
end

open Utilities

let typeErrorMessage bindings (fe, msg, foundType, expectedType) =
  let typeName = function
    | `Any description -> description
    | #Lang.typ as t -> typeName t
  in
  sprintf "type error: %s, expected %s but found %s in expression %s"
    msg
    (typeName expectedType)
    (typeName foundType)
    (match fe with
    | Semantic.Form form -> Lang.formToString form
    | Semantic.Ast ast -> Ast2.toString ast)

(** Single error, to avoid name clash with Error constructor in mayfail type *)
module Serror = struct
  type t = {
    emsg: string;
    eloc: Basics.location option;
    eexpr: Ast2.t option;
  }

  (** TODO: add 'in $expr' if location is missing *)
  let diagnosticsToString kind error =
    let loc =
      match error.eloc, error.eexpr with
        | Some location, _ ->
          location
        | None, Some { location = Some location } ->
          location
        | None, Some { location = None }
        | None, None ->
          Basics.fakeLocation
    in
    Basics.formatDiagnostics kind loc error.emsg

  let toString error =
    diagnosticsToString "error" error

  let check funcName error =
    let warn msg = printf "alert, %s (in %s, msg = %s)!\n" msg funcName (toString error) in
    if error.emsg =~ ".*\\.zomp:[0-9]+: error:" then begin
      warn "error location has been added twice";
      if error.eloc = None then
        warn "error location missing";
      Printexc.print_backtrace stdout;
      flush stdout;
    end;
    if String.length error.emsg = 0 then
      warn "empty error message"
    else if (error.emsg.[0] = Char.uppercase error.emsg.[0]) then
      warn "error message begins with upper case letter";
    error

  let checkAll funcName errors =
    ignore (List.map (check funcName) errors);
    ()

  let fromMsg eloc emsg = { emsg; eloc; eexpr = None }

  let fromExpr expr msg =
    let eexpr, emsg =
      match expr.location with
        | Some location ->
          None, msg
        | None ->
          Some expr, sprintf "%s in %s" msg (Ast2.toString expr)
    in
    { emsg; eloc = expr.location; eexpr }

  let fromTypeError bindings expr (fe,m,f,e) =
    let msg = typeErrorMessage bindings (fe,m,f,e) in
    check "fromTypeError" (fromExpr expr msg)

  let fromMsg eloc emsg = check "fromMsg" (fromMsg eloc emsg)
  let fromExpr expr msg = check "fromExpr" (fromExpr expr msg)
end

exception IllegalExpression of sexpr * Serror.t list

module Translation_utils =
struct
  (* TODO: use in translateDefineVar and translateGlobalVar + test *)
  let determineStorage typ =
    match typ with
      | `Pointer _ -> MemoryStorage
      | _ -> RegisterStorage

  let lookupType bindings name =
    try
      (** TODO: bug, this prevents built-in types to be shadowed *)
      Some (Lang.parseType name)
    with
      | Typesystems.Zomp.CouldNotParseType _ ->
          match lookup bindings name with
            | TypedefSymbol t -> Some t
            | _ -> None

  let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, [Serror.fromExpr expr msg]))
  let raiseIllegalExpressions expr errors = raise (IllegalExpression (expr, errors))

  let raiseIllegalExpressionFromTypeError expr (formOrExpr, msg, found, expected) =
    raiseIllegalExpression expr
      (typeErrorMessage Bindings.defaultBindings (formOrExpr, msg,found,expected))

  let raiseInvalidType typeExpr =
    raise (Typesystems.Zomp.CouldNotParseType (Ast2.expression2string typeExpr))

  type formWithTLsEmbedded = [`ToplevelForm of toplevelExpr | form]

  let formWithTLsEmbeddedToString = function
    | #Lang.form as form -> Lang.formToString form
    | `ToplevelForm tlform -> Lang.toplevelFormToString tlform

  type 'a someTranslateF = bindings -> sexpr -> bindings * 'a list
  type exprTranslateF = formWithTLsEmbedded someTranslateF

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

  let expr2VarOrConst (bindings :bindings) =
    let tryGetFunctionAddress name =
      match lookup bindings name with
        | FuncSymbol f ->
            let name = f.fname in
            let typ = `Function {
              returnType = f.rettype;
              argTypes = List.map snd f.Lang.fargs;
            } in
            let var = variable name (`Pointer typ) RegisterStorage true None in
            Some (`Variable var)
        | _ ->
            None
    in
    function
    | { id = name; args = [] } -> begin
        match lookup bindings name with
          | VarSymbol v ->
              Some (`Variable v)
          | FuncSymbol f ->
              tryGetFunctionAddress name
          | _ ->
              match string2integralValue name with
                | Some c -> Some (`Constant c)
                | None -> None
      end
    | { id = "preop&"; args = [{id = name; args = []}] } ->
        begin
          tryGetFunctionAddress name
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
    let var = globalVar ~name:newVarName ~typ ~location:None in
    (addVar bindings var, var)

  let getNewLocalVar bindings typ =
    let newVarName = getUnusedName bindings in
    (* let var = localVar ~name:newVarName ~typ in *)
    let var = Lang.variable
      ~name:newVarName
      ~typ
      ~storage:MemoryStorage
      ~global:false
      ~location:None
    in
    (addVar bindings var, var)

  let translateToForms (translateF :exprTranslateF) bindings expr =
    let newBindings, xforms = translateF bindings expr in
    let tlforms, forms = extractToplevelForms xforms in
    newBindings, toSingleForm forms, tlforms

  let rec flattenLeft = function
    | [] -> [], []
    | (list, x) :: rem ->
        let remList, remX = flattenLeft rem in
        (list @ remList, x :: remX)

  let flattenNestedTLForms = List.map (fun (`ToplevelForm tlform) -> tlform)

  type toplevelExprTranslateF = toplevelExpr someTranslateF
end

open Translation_utils

type 'a mayfail =
  | Result of 'a
  | Error of Serror.t list

let errorFromStringDeprecated emsg = Error [{ Serror.emsg; eloc = None; eexpr = None }]
let errorFromString location msg = Error [Serror.fromMsg (Some location) msg]
let errorFromTypeError bindings expr (fe,m,f,e) =
  Error [Serror.fromTypeError bindings expr (fe,m,f,e)]
let errorFromExpr expr msg = Error [Serror.fromExpr expr msg]
let singleError error = Error [error]
let multipleErrors errors = Error errors
let result r = Result r

let combineResults mayfails =
  let errors = ref [] in
  let addTo listRef e = listRef := e :: !listRef in
  let results = mapFilter
    (function Error msgs -> List.iter (addTo errors) msgs; None | Result r -> Some r)
    mayfails
  in
  match !errors with
    | [] -> Result results
    | messages -> Error messages

let combineErrors msg =
  let msgPrefix =
    if String.length msg = 0 then ""
    else sprintf "%s: " msg
  in
  function
  | [] -> msgPrefix ^ "no error message given"
  | errors -> msgPrefix ^ Common.combine "\n  " errors

let mapResult f = function
  | Result r -> Result (f r)
  | Error errors -> Error errors

let extractErrors = function
  | Result _ -> []
  | Error errors -> errors

exception MayfailError of Serror.t list

let rec translateType bindings emitWarning typeExpr : Lang.typ mayfail =
  let error msg = errorFromExpr typeExpr msg in
  let instantiateType parametricType argumentType =
    let rec inst = function
      | `TypeParam ->
        argumentType
      | `TypeRef _
      | #integralType as t ->
        t
      | `Record rt ->
        `Record { rt with fields = List.map (map2nd inst) rt.fields }
      | `Pointer t ->
        `Pointer (inst t)
        (* | `ParametricType t -> *)
        (*     `ParametricType (inst t) *)
      | `Array (memberType, size) ->
        `Array (inst memberType, size)
      | `Function ft ->
        `Function { returnType = inst ft.returnType;
                    argTypes = List.map inst ft.argTypes }
      | `ParametricType `Record rt ->
        `Record { rt with fields = List.map (map2nd inst) rt.fields }
      | `ParametricType `Pointer t ->
        `Pointer (inst t)
      | `ErrorType _ as t ->
        t
    in
    inst parametricType
  in

  let translatePtr targetTypeExpr =
    match translateType bindings emitWarning targetTypeExpr with
      | Result t -> Result (`Pointer t)
      | error -> error
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
    match translateType bindings emitWarning memberTypeExpr, potentialSize with
      | Result t, Some size -> Result (`Array(t, size))
      | errorM, _ -> errorM
  in
  let translateFunction returnTypeExpr argTypeExprs =
    try
      let translate typ =
        match translateType bindings emitWarning typ with
          | Result t -> t
          | Error errors ->
            raise (MayfailError errors)
      in
      begin
        Result (`Function {
          returnType = translate returnTypeExpr;
          argTypes = List.map translate argTypeExprs;
        })
      end
    with MayfailError errors ->
      Error errors
  in
  match typeExpr with
    (** function pointers *)
    | { id = "opcall"; args = returnTypeExpr :: argTypeExprs } ->
      translateFunction returnTypeExpr argTypeExprs
    | { id = "opjux"; args = {id = "fptr"; args = []} :: returnTypeExpr :: argTypeExprs } ->
      begin match translateFunction returnTypeExpr argTypeExprs with
        | Result typ ->
          let () = (* fix return type *)
            emitWarning typeExpr (sprintf "fptr is deprecated. Use %s instead" (Typesystems.Zomp.typeName typ))
          in
          Result (`Pointer typ)
        | Error _ as errors ->
          errors
      end

    | { id = "opjux"; args = args } (* when jux = macroJuxOp *) ->
      translateType bindings emitWarning (shiftId args)

    | { id = "op!"; args =
        [{ id = paramTypeName; args = [] };
         argumentTypeExpr ] } ->
      begin
        match lookup bindings paramTypeName, translateType bindings emitWarning argumentTypeExpr with
          | TypedefSymbol ((`ParametricType _) as t), Result `TypeParam ->
            Result t
          | TypedefSymbol (`ParametricType t), Result argumentType ->
            Result (instantiateType (t :> typ) argumentType)
          | paramTypeResult, argumentResult ->
            let otherErrors = extractErrors argumentResult in
            let error = Serror.fromExpr typeExpr "could not translate type" in
            Error (error :: otherErrors)
      end

    | { id = "postop*"; args = [targetTypeExpr] }
    | { id = "ptr"; args = [targetTypeExpr]; } ->
      translatePtr targetTypeExpr

    | { id = "postop[]"; args = [memberTypeExpr; sizeExpr] }
    | { id = "array"; args = [memberTypeExpr; sizeExpr] } ->
      translateArray memberTypeExpr sizeExpr

    | { id = name; args = [] } ->
      begin
        match lookupType bindings name with
          | Some t -> Result t
          | None -> error (sprintf "could not look up type %s" name)
      end
    | _ ->
      errorFromExpr typeExpr "don't know how to interprete as type"

module Translators_deprecated_style =
struct
  let reportWarning expr msg =
    eprintf "%s\n" (Serror.diagnosticsToString "warning" (Serror.fromExpr expr msg))

  let translateType bindings expr =
    translateType bindings reportWarning expr

  let translateSimpleExpr (_ :exprTranslateF) (bindings :bindings) expr =
    match expr2VarOrConst bindings expr with
      | Some varOrConst ->
          begin match varOrConst with
            | `Constant StringLiteral string ->
                begin
                  let newBindings, var = getNewGlobalVar bindings (`Pointer `Char) in
                  let value = StringLiteral string in
                  let gvar = {
                    gvVar = var;
                    gvInitialValue = value;
                    gvDefinitionLocation = expr.location;
                  } in
                  Some( newBindings, [`ToplevelForm (`GlobalVar gvar); `Variable var] )
                end
            | _ -> Some (bindings, [varOrConst] )
          end
      | None -> None

  let translateTypedef translateF (bindings :bindings) =
    let translateRecordTypedef bindings typeName componentExprs expr =
      let tempBindings = Bindings.addTypedef bindings typeName (`TypeRef typeName) in
      let expr2component =
        let translate name typeExpr =
          match translateType tempBindings typeExpr with
            | Result typ ->
              name, typ
            | Error errors ->
              raiseIllegalExpressions typeExpr errors
        in
        function
          | { id = typeName; args = [{ id = componentName; args = []}] } ->
              translate componentName (Ast2.idExpr typeName)
          | { id = seq; args = [typeExpr; { id = componentName; args = [] }] }
              when seq = macroSequence || seq = macroJuxOp ->
              translate componentName typeExpr
          | _ ->
            raiseIllegalExpression
              expr
              "(type typeName (typeExpression componentName)* ) expected"
      in
      let components = List.map expr2component componentExprs in
      let recordType = `Record { rname = typeName; fields = components } in
      Some recordType
    in
    let returnRecordTypedef bindings name componentExprs expr =
      match translateRecordTypedef bindings name componentExprs expr with
        | Some rt ->
            Some (addTypedef bindings name rt, [`Typedef (name, rt)])
        | None ->
            None
    in
    function
      (** record with only one member *)
      | { id = id; args = [
            { id = typeName; args = [] };
            { id = opseq; args = componentExprs }
          ] } as expr
          when id = macroTypedef && opseq = macroSeqOp ->
          returnRecordTypedef bindings typeName componentExprs expr
      (** parametric record *)
      | { id = id; args = [
            { id = opcall; args = [
                { id = typeName; args = [] };
                { id = "T"; args = [] } ] };
            { id = opseq; args = componentExprs }
          ] } as expr
          when id = macroTypedef && opcall = macroParamType && opseq = macroSeqOp ->
          begin
            let paramBindings = addTypedef bindings "T" `TypeParam in
            match translateRecordTypedef paramBindings typeName componentExprs expr with
              | Some recordType ->
                  let parametricType = `ParametricType recordType in
                  Some (
                    addTypedef bindings typeName parametricType,
                    [`Typedef (typeName, parametricType)])
              | None ->
                  None
          end
      (** type foo typeExpr *)
      | { id = id; args = [
            { id = newTypeName; args = [] };
            targetTypeExpr;
          ] }
          when id = macroTypedef ->
          begin
            match translateType bindings targetTypeExpr with
              | Error _ -> raiseInvalidType targetTypeExpr
              | Result t -> Some (addTypedef bindings newTypeName t,
                                  [`Typedef (newTypeName, t)] )
          end
      (** record typedef *)
      | { id = id; args =
            { id = typeName; args = [] }
            :: componentExprs
        } as expr
          when id = macroTypedef ->
          returnRecordTypedef bindings typeName componentExprs expr
      | _ -> None

  (** TODO: check if this can be removed *)
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
                  let recordInitFunc name components = func (name ^ "_init") `Void components None Basics.fakeLocation in
                  let initFunc = recordInitFunc name record.fields in
                  let translate param compExpr = match (param, compExpr) with
                    | ((argName, argType), ({ id = compName; args = [argExpr] }) ) ->
                        begin
                          if argName = compName then begin
                            let _, form, toplevelForms = translateToForms translateF bindings argExpr in
                            if List.length toplevelForms > 0 then
                              raiseIllegalExpression expr "no nested toplevel forms allowed inside record";
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
                    fcvarargs = initFunc.cvarargs
                  } in
                  Some( bindings, [`FuncCall call] )
                end
            | UndefinedSymbol -> raiseIllegalExpression expr (sprintf "%s is undefined" name)
            | _ -> raiseIllegalExpression expr (sprintf "%s is not a record" name)
        end
    | _ -> None

  (** Support legacy macros which generate ASTs calling functions without using
   * opcall/opjux *)
  let translateRestrictedFunCall (translateF :exprTranslateF) (bindings :bindings) expr =
    match lookup bindings expr.id with
      | FuncSymbol _ when List.length expr.args >= 1 ->
          Some( translateF bindings
                  { expr with id = macroFunCall; args = idExpr expr.id :: expr.args } )
      | _ ->
          None
end

let astType = `Record {
  rname = "ast";
  fields = [
    "id", `Pointer `Char;
    "childCount", `Int32;
    "childs", `Pointer (`Pointer (`TypeRef "ast"))
  ] }
let astPtrType = `Pointer astType

let macroFuncs = ref []

module Old_macro_support =
struct
  (* make it easy to find places where this code is still used *)
  let trace functionName location = ()
    (* printf "[TRACE] Old_macro_support.%s @%s\n" *)
    (*   functionName *)
    (*   (applyOptOrDefault location locationToString "???") *)

  (** TODO: this can result in quadratic run time, fix source locations directly
   * after transferring from native code *)
  let rec withDefaultSourceLocation location expr =
    let fixedArgs = List.map (withDefaultSourceLocation location) expr.args in
    match expr.location with
      | Some _ ->
        { expr with args = fixedArgs }
      | None ->
        { expr with location = Some location; args = fixedArgs }

  let translateMacroCall translateF (bindings :bindings) expr =
    trace "translateMacroCall" expr.location;
    match expr with
      | { id = macroName; args = args; } ->
        match lookup bindings macroName with
          | MacroSymbol macro ->
            begin
              try
                let transformedExpr =
                  let withBrokenLocations = macro.mtransformFunc bindings expr in
                  match expr.location with
                    | Some location ->
                      withDefaultSourceLocation location withBrokenLocations
                    | None ->
                      withBrokenLocations
                in
                Some (translateF bindings transformedExpr)
              with
                | Failure msg ->
                  raiseIllegalExpression expr ("could not expand macro: " ^ msg)
            end
          | _ -> None

  let createNativeMacro translateF bindings macroName argNames impl =
    let sexprImpl = impl in
    let bindings =
      List.fold_left
        (fun bindings name ->
           Bindings.addVar bindings
             (Lang.variable
                ~name
                ~typ:astPtrType
                ~storage:Lang.RegisterStorage
                ~global:false
                ~location:None) )
        bindings argNames
    in
    let _, xforms = translateF bindings sexprImpl in
    let initForms, implforms = extractToplevelForms xforms in
    let initForms = flattenNestedTLForms initForms in
    let macroFunc =
      let fargs = List.map (fun name -> (name, astPtrType)) argNames
      and impl = Some (toSingleForm implforms) in
      func macroName astPtrType fargs impl Basics.fakeLocation
    in
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

  let translateFuncMacro (translateNestedF :exprTranslateF) name bindings argNames args impl =
    let expectedArgCount = List.length argNames in
    let foundArgCount = List.length args in
    if expectedArgCount <> foundArgCount then
      raiseIllegalExpression
        (Ast2.expr name args)
        (sprintf "could not invoke macro: expected %d arguments but found %d" expectedArgCount foundArgCount);

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
          ~location:None
      in
      log "building function impl";
      let implForms = [
        `DefineVariable(resultVar,
                        Some (`FuncCall { fcname = name;
                                          fcrettype = astPtrType;
                                          fcparams = repeatedList astPtrType (List.length argAstForms);
                                          fcargs = argAstForms;
                                          fcptr = `NoFuncPtr;
                                          fcvarargs = false;
                                        }) );
        `CastIntrinsic (`Int32, `Variable resultVar);
      ]
      in
      let func = `DefineFunc (func "macroExec" `Int32 [] (Some (`Sequence implForms)) Basics.fakeLocation) in
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
      List.map Zompvm.NativeAst.buildNativeAst args;
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
    let sexpr = Zompvm.NativeAst.extractSExprFromNativeAst astAddress in
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

  let translateDefineMacro translateNestedF translateF (bindings :bindings) expr =
    trace "translateDefineMacro" expr.location;
    match expr with
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
                          (fun bindings expr -> macroF bindings argNames expr.args impl), [] )
                end
          end
        in
        if not (id = macroReplacement) then begin
          printf "warning: Invoked old translateDefineMacro for '%s'\n" name;
          flush stdout;
        end;
        result
    | _ ->
        None
end

(** "new" compiler types *)

type 'translateF env = {
  bindings :Bindings.t;
  translateF :'translateF;
  translateExprOld : bindings -> Ast2.t -> bindings * formWithTLsEmbedded list;
  translateExpr : 'translateF env -> Ast2.t -> (bindings * formWithTLsEmbedded list) mayfail;
  parseF : fileName:string -> string -> Ast2.t list option;
}

let envBindings env = env.bindings

type toplevelEnv = Translation_utils.toplevelExprTranslateF env
let tlReturnNoExprs env = Result (env.bindings, [])

type 'a translationResultV = (bindings * ('a list)) mayfail
type translationResult = formWithTLsEmbedded translationResultV
type toplevelTranslationResult = toplevelExpr translationResultV

module Macros =
struct
  let buildNativeMacroFunc translateF bindings
      macroName argNames implExprs (isVariadic : [`IsVariadic | `IsNotVariadic])
      =
    let bindings = Bindings.addVar bindings
      (Lang.variable
         ~name:"macro_args"
         ~typ:(`Pointer astPtrType)
         ~storage:MemoryStorage
         ~global:false
         ~location:None)
    in
    let argParamName = "macro_args" in
    let buildParamFetchExpr num name =
      Ast2.expr "std:base:localVar" [Ast2.idExpr name;
                                     Ast2.expr "load" [
                                       Ast2.expr "ptradd" [
                                         Ast2.idExpr argParamName;
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
        | None -> raiseIllegalExpression (Ast2.idExpr "ast") "could not find prelude type 'ast'"
    in
    let macroFunc =
      let fargs = [argParamName, `Pointer (`Pointer astType)]
      and impl = Some (toSingleForm implforms) in
      func macroName astPtrType fargs impl Basics.fakeLocation
    in
    let tlforms = initForms @ [`DefineFunc macroFunc] in
    tlforms, macroFunc

  let translateMacroCall name paramCount isVariadic =
    let nativeFuncAddr = Machine.zompAddressOfMacroFunction ~name in
    (fun bindings expr ->
       let result =
         let args = expr.args in
         begin
           let invokeMacro args =
             let nativeArgs = List.map Zompvm.NativeAst.buildNativeAst args in
             Machine.zompResetMacroArgs();
             List.iter (fun ptr -> Machine.zompAddMacroArg ~ptr) nativeArgs;
             let nativeResultAst = Machine.zompCallMacro nativeFuncAddr in
             let resultAst = Zompvm.NativeAst.extractSExprFromNativeAst nativeResultAst in
             resultAst
           in
           let argCount = List.length args in
           match isVariadic with
             | `IsNotVariadic -> begin
                 if argCount <> paramCount then begin
                   raiseIllegalExpression
                     (Ast2.expr name args)
                     (sprintf "expected %d args but found %d" paramCount argCount);
                 end else begin
                   invokeMacro args
                 end
               end
             | `IsVariadic -> begin
                 if argCount < paramCount-1 then begin
                   raiseIllegalExpression
                     (Ast2.expr name args)
                     (sprintf "expected at least %d args but found only %d"
                        (paramCount-1) argCount)
                 end;
                 let declaredArgs, variadicArgs = Common.splitAfter (paramCount-1) args in
                 let inflatedArgs = declaredArgs @ [Ast2.seqExpr variadicArgs] in
                 invokeMacro inflatedArgs
               end
         end
       in
       match expr.location, result.location with
         | Some loc, None ->
             { result with Ast2.location = Some loc }
         | Some _, Some _
         | None, _ ->
             result : bindings -> Ast2.sexpr -> Ast2.sexpr)

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
                  errorFromExpr invalidParam
                    "invalid macro parameter, expected id or id..."
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
                            errorFromExpr expr
                              (sprintf "parameter %s is variadic but is not the last parameter" paramName)
                        | Error msg, _
                        | _, Error msg ->
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
            errorFromExpr expr (sprintf "expecting '%s name paramName* lastParamVariadic...? seq" expr.id)
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
          Error reasons

end

(** A module defining various zomp transformations *)
module type Zomp_transformer =
sig
  (** name, doc, translateFunc *)
  val register : (string -> (string * (exprTranslateF env -> Ast2.t -> translationResult)) -> unit) -> unit
  val registerTL : (string -> (string * (toplevelExprTranslateF env -> Ast2.t -> toplevelTranslationResult)) -> unit) -> unit
end

module Base : Zomp_transformer =
struct
  let reportError error =
    eprintf "%s\n" (Serror.toString error)

  let reportErrorE expr msg = reportError (Serror.fromExpr expr msg)
  let reportErrorM messages = List.iter (fun msg -> reportError (Serror.fromMsg None msg)) messages

  let translateType bindings expr =
    translateType bindings Translators_deprecated_style.reportWarning expr

  (** translates expressions of the form x = xExpr, y = yExpr etc. *)
  let translateStructLiteralArgs fields fieldExprs (translateExprF : Ast2.t -> 'a mayfail) =
    let rec handleFieldExprs errors fieldValues undefinedFields =
      let continueWithErrors messages remArgs =
        handleFieldExprs (messages@errors) fieldValues undefinedFields remArgs
      in
      function
        | [] ->
          begin match errors with
            | [] -> Result (List.rev fieldValues)
            | _ -> Error errors
          end
        | { id = "op=";
            args = [
              {id = fieldName; args= []};
              rhs]
          }
            as fieldValueExpr :: remArgs ->
          begin
            match List.partition ((=) fieldName) undefinedFields with
              | [_], undefinedFields ->
                begin match translateExprF rhs with
                  | Error rhsErrors ->
                    continueWithErrors rhsErrors remArgs
                  | Result value ->
                    handleFieldExprs
                      errors
                      ((fieldName, value) :: fieldValues)
                      undefinedFields
                      remArgs
                end
              | _, _ ->
                continueWithErrors
                  [Serror.fromExpr fieldValueExpr
                      (if listContains fieldName fields then
                          sprintf "field %s specified several times" fieldName
                       else
                          sprintf "field %s does not exist" fieldName)]
                  remArgs
          end
        | unexpectedExpr :: remArgs ->
          continueWithErrors
            [Serror.fromExpr unexpectedExpr "expected expression of the form 'id = expr'"]
            remArgs
    in
    if List.length fields <> List.length fieldExprs then
      Error [Serror.fromMsg None "not all fields have been defined"]
    else
      handleFieldExprs [] [] fields fieldExprs

  let translateConstantToValue env expr : value mayfail =
    match expr2VarOrConst env.bindings expr with
      | Some `Constant value -> Result value
      | _ -> errorFromExpr expr "must be a constant expression"

  (** exprTranslateF env -> Ast2.sexpr -> translationResult *)
  let translateGlobalVar env expr =
    match expr with
      | { id = _; args = [
        typeExpr;
        { id = name; args = [] };
        valueExpr
      ] } ->
        begin
          let typ =
            match translateType env.bindings typeExpr with
              | Result (#integralType as typ)
              | Result (`Pointer _ as typ)
              | Result (`Array (_, _) as typ)
              | Result (`Record _ as typ) ->
                typ
              | Result `ErrorType _
              | Result `Function _
              | Result `ParametricType _
              | Result `TypeParam
              | Result `TypeRef _ ->
                reportErrorE typeExpr "type not supported for global variables";
                `ErrorType "translateGlobalVar"
              | Error errors ->
                List.iter reportError errors;
                `ErrorType "translateGlobalVar"
          in
          let newBindings, tlforms, value =
            match typ, valueExpr with
              (** env.translateExpr always introduces temporary for string literals *)
              | `Pointer `Char, { id = stringlit; args = [] } ->
                env.bindings, [], parseValue (`Pointer `Char) stringlit
              (** legacy special case *)
              | `Pointer _, { id = "null"; args = [] } ->
                env.bindings, [], NullpointerVal typ
              (** legacy special case *)
              | `Record recordT, { id = "0"; args = [] } ->
                env.bindings, [], RecordVal (recordT.rname, [])
              (** legacy special case *)
              | `Array _, { id = "0"; args = [] } ->
                env.bindings, [], ArrayVal (typ, [])
              (** TODO: remove special cases above *)
              | _ ->
                match env.translateExpr env valueExpr with
                  | Result( newBindings, formsWTL ) ->
                    begin
                      let tlforms, forms = extractToplevelForms formsWTL in
                      match forms with
                        | [`Constant value] ->
                          newBindings, tlforms, value
                        | _ ->
                          reportErrorE expr "expecting a constant expression";
                          List.iter (fun f -> eprintf "  %s\n" $ Lang.formToString f) forms;
                          newBindings, tlforms, ErrorVal "translateGlobalVar"
                    end
                  | Error messages ->
                    List.iter reportError messages;
                    env.bindings, [], ErrorVal "translateGlobalVar"
          in
          let typeEquivalent lt rt =
            match lt, rt with
              | `Record { rname = rname; fields = [] }, `Record { rname = lname; fields = _ }
              | `Record { rname = rname; fields = _ }, `Record { rname = lname; fields = [] } ->
                (lname :string) = rname
              | _, _ ->
                lt = rt
          in
          if typeEquivalent typ (typeOf value) then begin
            let tlforms = List.map (fun (`ToplevelForm f) -> f) tlforms in
            let var = globalVar name typ expr.location in
            let newBindings = addVar newBindings var in
            let gvar = {
              gvVar = var;
              gvInitialValue = value;
              gvDefinitionLocation = expr.location;
            }
            in
            Result( newBindings, tlforms @ [ (`GlobalVar gvar :> toplevelExpr) ] )
          end else
            errorFromExpr expr
              (sprintf "expected initial value to have type %s but found %s"
                 (typeDescr typ) (typeDescr (typeOf value)))
        end
      | _ ->
        errorFromExpr expr "expected 'var typeExpr name initExpr'"
  let translateGlobalVarD = "type, name, value", translateGlobalVar

  let translateDefineLocalVar env expr =
    let transform id name typeExpr valueExpr =
      let declaredType = match typeExpr with
        | Some e ->
            begin
              match translateType env.bindings e with
                | Result t -> Some t
                | Error _ -> raise (CouldNotParseType (Ast2.expression2string e))
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
                expr (Semantic.Ast expr, "types do not match",declaredType,valueType)
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
              let var = variable name typ MemoryStorage false expr.location in
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
                    let var = variable name typ MemoryStorage false expr.location in
                    Result( addVar env.bindings var, [ `DefineVariable (var, None) ] )
                | Some valueExpr ->
                  raiseIllegalExpression
                    valueExpr
                    "record type var must not have a default value"
            end
        | `TypeRef _ ->
            raiseIllegalExpression expr "internal error: received unexpected type ref"
        | `Array _ ->
            raiseIllegalExpression expr "array vars not supported, yet"
        | `TypeParam ->
            raiseIllegalExpression expr "cannot define local variable of type parameter type"
        | `ParametricType _ ->
            raiseIllegalExpression expr "cannot define local variable of parametric type"
        | `ErrorType _ as t ->
          raiseIllegalExpression expr (sprintf "cannot define local variable of %s"
                                         (typeName t))
    in
    match expr with
      | { args = [
            { id = name; args = [] };
            valueExpr
          ] } ->
          transform id name None (Some valueExpr)

      | _ ->
          errorFromExpr expr (sprintf "expecting '%s name valueExpr'" expr.id)
  let translateDefineLocalVarD = "name, value", translateDefineLocalVar

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
                    errorFromExpr rightHandExpr
                      (sprintf "type error: cannot assign %s to %s"
                         (typeName invalidRhsType) (typeName lhsVar.typ))
                | TypeError (fe,m,f,e) ->
                    errorFromTypeError env.bindings rightHandExpr (fe,m,f,e)
            end
        | _ ->
            errorFromExpr expr (sprintf "could not find variable %s" varName)
    in

    match expr with
      | { id = id; args = [
            { id = varName; args = [] };
            rightHandExpr;
          ] } when id = macroAssign ->
          doTranslation id varName rightHandExpr
      | _ ->
        errorFromExpr expr "expected 'assign varName valueExpr'"
  let translateAssignVarD = "var, value", translateAssignVar

  let translateSeq (env :exprTranslateF env) expr =
    Result (translatelst env.translateF env.bindings expr.args)
  let translateSeqD = "ast...", translateSeq

  let translateReturn (env :exprTranslateF env) = function
    | { id = id; args = [expr] } ->
        begin
          let _, form, toplevelExprs = translateToForms env.translateF env.bindings expr in
          Result (env.bindings, toplevelExprs @ [`Return form])
        end
    | { args = [] } ->
      begin
        Result (env.bindings, [`Return (`Constant VoidVal)])
      end
    | expr ->
      errorFromExpr expr
        (sprintf "expected zero or one argument instead of %d" (List.length expr.args))
  let translateReturnD = "[value]", translateReturn

  let translateLabel (env :exprTranslateF env) expr =
    match expr.args with
      | [ {id = name; args = [] }] ->
          Result( addLabel env.bindings name, [`Label { lname = name; }] )
      | _ ->
        errorFromExpr expr ("expecting one argument which is an identifier")
  let translateLabelD = "name", translateLabel

  let translateBranch (env :exprTranslateF env) expr =
    match expr.args with
      | [{ id = labelName; args = [] }] ->
          begin
            Result( env.bindings, [`Jump { lname = labelName; }] )
          end
      | [invalidBranchTarget] ->
          errorFromExpr expr
            "expecting 'branch labelName' where labelName is an identifier denoting an existing label"
      | [
          { id = condVarName; args = [] };
          { id = trueLabelName; args = [] };
          { id = falseLabelName; args = [] };
        ] ->
          begin
            match lookup env.bindings condVarName with
              | VarSymbol var when var.typ = `Bool ->
                  begin
                    Result( env.bindings, [`Branch {
                                             bcondition = { var with typ = `Bool };
                                             trueLabel = { lname = trueLabelName};
                                             falseLabel = { lname = falseLabelName}; }] )
                  end
              | _ ->
                errorFromExpr expr
                  "first argument of conditional branch should be bool variable"
          end
      | _ ->
        errorFromExpr expr
          "expected either 'branch labelName' or 'branch boolVar labelOnTrue labelOnFalse'"
  let translateBranchD = "label | boolValue labelOnTrue labelOnFalse", translateBranch

  let translateLoad (env :exprTranslateF env) expr =
    match expr.args with
      | [ptrExpr] ->
          begin
            let _, ptrForm, toplevelForms = translateToForms env.translateF env.bindings ptrExpr in
            let loadForm = `LoadIntrinsic ptrForm in
            match typeCheck env.bindings loadForm with
              | TypeOf _ ->
                  Result( env.bindings, toplevelForms @ [loadForm] )
              | TypeError (fe,m,f,e) ->
                  errorFromTypeError env.bindings ptrExpr (fe,m,f,e)
          end
      | _ ->
          errorFromExpr expr "expecting only one argument"
  let translateLoadD = "pointer", translateLoad

  let translateStore (env :exprTranslateF env) expr =
    match expr.args with
      | [ptrExpr; rightHandExpr]->
          begin
            let _, ptrForm, toplevelForms = translateToForms env.translateF env.bindings ptrExpr in
            let _, rightHandForm, toplevelForms2 = translateToForms env.translateF env.bindings rightHandExpr in
            let storeInstruction = `StoreIntrinsic (ptrForm, rightHandForm) in
            match typeCheck env.bindings storeInstruction with
              | TypeOf _ ->
                  Result( env.bindings, toplevelForms @ toplevelForms2 @ [storeInstruction] )
              | TypeError (fe,m,f,e) ->
                  errorFromTypeError env.bindings rightHandExpr (fe,m,f,e)
          end
      | _ ->
          errorFromExpr expr "expected two arguments: 'store ptrExpr valueExpr'"
  let translateStoreD = "pointer, value", translateStore

  let translateMalloc (env :exprTranslateF env) expr =
    let buildMallocInstruction typeExpr countExpr =
      match translateType env.bindings typeExpr with
        | Error _ as err ->
          err
        | Result typ ->
            begin
              let _, rightHandForm, toplevelForms = translateToForms env.translateF env.bindings countExpr in
              let mallocForm = `MallocIntrinsic (typ, rightHandForm) in
              match typeCheck env.bindings mallocForm with
                | TypeOf _ -> Result( env.bindings, toplevelForms @ [mallocForm] )
                | TypeError (fe,m,f,e) ->
                    errorFromTypeError env.bindings expr (fe,m,f,e)
            end
    in
    match expr with
      | { id = id; args = [typeExpr] } when id = macroMalloc ->
          buildMallocInstruction typeExpr (Ast2.idExpr "1")
      | { id = id; args = [typeExpr; countExpr] } when id = macroMalloc ->
          buildMallocInstruction typeExpr countExpr
      | _ ->
          errorFromExpr expr "expected 'malloc typeExpr countExpr', countExpr being optional"
  let translateMallocD = "type count?", translateMalloc

  let translateNullptr (env :exprTranslateF env) expr =
    match expr.args with
      | [typeExpr] ->
          begin match translateType env.bindings typeExpr with
            | Result typ -> Result (env.bindings, [`Constant (NullpointerVal (`Pointer typ))] )
            | Error msgs -> Error msgs
          end
      | _ ->
          errorFromExpr expr "expected one argument denoting a type: 'nullptr typeExpr'"
  let translateNullptrD = "type", translateNullptr

  let translateGetaddr (env :exprTranslateF env) expr =
    match expr with
      | { args = [{ id = varName; args = [] }] } ->
          begin
            match lookup env.bindings varName with
              | VarSymbol var -> Result (env.bindings, [`GetAddrIntrinsic var] )
              | _ -> raiseIllegalExpression expr (sprintf "could not find variable %s" varName)
          end
      | { id = "ptr"; args = [] } ->
          (** could actually be a variable called 'ptr', handle this for backwards compatibility *)
          begin match lookup env.bindings "ptr" with
            | VarSymbol v -> Result (env.bindings, [`Variable v])
            | _ -> errorFromExpr expr "meh. ptr is not a variable"
          end
      | _ ->
          errorFromExpr expr "expected one argument denoting an lvalue: 'ptr lvalueExpr'"
  let translateGetaddrD = "lvalue", translateGetaddr

  let translatePtradd (env :exprTranslateF env) expr =
    match expr.args with
      | [ptrExpr; indexExpr] ->
          begin
            let _, ptrForm, toplevelForms = translateToForms env.translateF env.bindings ptrExpr in
            let _, indexForm, toplevelForms2 = translateToForms env.translateF env.bindings indexExpr in
            let ptradd = `PtrAddIntrinsic (ptrForm, indexForm) in
            match typeCheck env.bindings ptradd with
              | TypeOf _ -> Result( env.bindings, toplevelForms @ toplevelForms2 @ [ptradd] )
              | TypeError (fe,m,f,e) ->
                  errorFromTypeError env.bindings expr (fe,m,f,e)
          end
      | _ ->
          errorFromExpr expr "expected two arguments: 'ptradd ptrExpr intExpr'"
  let translatePtraddD = "ptr, intExpr", translatePtradd

  let translatePtrDiff (env :exprTranslateF env) expr =
    match expr.args with
      | [lhsExpr; rhsExpr] ->
        begin
          let _, lhsForm, lhsTLForms =
            translateToForms env.translateF env.bindings lhsExpr
          in
          let _, rhsForm, rhsTLForms =
            translateToForms env.translateF env.bindings rhsExpr
          in
          let ptrdiff = `PtrDiffIntrinsic (lhsForm, rhsForm) in
          match typeCheck env.bindings ptrdiff with
            | TypeOf _ -> Result( env.bindings, lhsTLForms @ rhsTLForms @ [ptrdiff] )
            | TypeError (fe,m,f,e) ->
              errorFromTypeError env.bindings expr (fe,m,f,e)
        end
      | _ ->
        errorFromExpr expr "expected two pointers as arguments"
  let translatePtrDiffD = "ptrExpr, ptrExpr", translatePtrDiff

  let translateGetfieldptr (env :exprTranslateF env) expr =
    match expr.args with
      | [
          recordExpr;
          { id = fieldName; args = [ ] };
        ] ->
          begin
            let _, recordForm, toplevelForms = translateToForms env.translateF env.bindings recordExpr in
            let (moreForms : formWithTLsEmbedded list), (recordForm' :Lang.form) =
              match recordForm with
                | `Variable ({ typ = `Record _; } as recordVar) ->
                    [], `GetAddrIntrinsic recordVar
                | _ ->
                    begin match typeCheck env.bindings recordForm with
                      | TypeOf (`Record _ as typ) ->
                          let newBindings, tempVar = getNewLocalVar env.bindings typ in
                          [((`DefineVariable (tempVar, Some recordForm)) :> formWithTLsEmbedded)],
                          `GetAddrIntrinsic tempVar
                      | TypeOf `Pointer `Record _ -> [], recordForm
                      | TypeOf invalidType ->
                          raiseIllegalExpression expr
                            (sprintf "%s but found %s"
                               ("can only access struct members from a var of [pointer to] record type")
                               (typeName invalidType))
                      | TypeError (fe,m,f,e) ->
                          raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
                    end
            in
            let fieldptr = `GetFieldPointerIntrinsic (recordForm', fieldName) in
            match typeCheck env.bindings fieldptr with
              | TypeOf _ -> Result( env.bindings, toplevelForms @ moreForms @ [fieldptr] )
              | TypeError (fe,m,f,e) ->
                  begin
                    raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
                  end
          end
      | _ ->
          errorFromExpr expr "expected two arguments: 'fieldptr structExpr id'"
  let translateGetfieldptrD = "structExpr, fieldName", translateGetfieldptr

  let translateCast (env :exprTranslateF env) expr =
    match expr.args with
      | [targetTypeExpr; valueExpr] ->
          begin
            match translateType env.bindings targetTypeExpr with
              | Result targetType ->
                begin
                  let _, valueForm, toplevelForms = translateToForms env.translateF env.bindings valueExpr in
                  match typeCheck env.bindings valueForm with
                    | TypeOf valueType ->
                      if Semantic.equalTypes env.bindings targetType valueType then
                        Result( env.bindings, toplevelForms @ [(valueForm :> formWithTLsEmbedded)] )
                      else
                        let castForm = `CastIntrinsic( targetType, valueForm ) in
                        Result( env.bindings, toplevelForms @ [castForm] )
                    | TypeError (fe,m,f,e) ->
                      errorFromTypeError env.bindings valueExpr (fe,m,f,e)
                end
              | Error msgs ->
                Error msgs
          end
      | _ ->
          errorFromExpr expr "expected 'cast typeExpr valueExpr'"
  let translateCastD = "typeExpr, valueExpr", translateCast

  let translateDefineVar (env :exprTranslateF env) expr :translationResult =
    let transformUnsafe id name typeExpr valueExpr :translationResult =
      let declaredType = match typeExpr with
        | Some e ->
            begin
              match translateType env.bindings e with
                | Result t -> Some t
                | Error _ -> raise (CouldNotParseType (Ast2.expression2string e))
            end
        | None ->
          None
      in
      let valueType, toplevelForms, implForms =
        match valueExpr with
          | Some valueExpr ->
              begin
                let _, simpleform = env.translateF env.bindings valueExpr in
                let toplevelForms, implForms = extractToplevelForms simpleform in
                match typeCheck env.bindings (`Sequence implForms) with
                  | TypeOf t -> Some t, toplevelForms, implForms
                  | TypeError (fe, m,f,e) -> raiseIllegalExpressionFromTypeError valueExpr (fe,m,f,e)
              end
          | None -> None, [], []
      in
      let varType =
        match declaredType, valueType with
          | Some declaredType, Some valueType
            when equalTypes env.bindings declaredType valueType ->
              declaredType
          | Some declaredType, Some valueType ->
              raiseIllegalExpressionFromTypeError expr
                (Semantic.Ast expr, "types do not match",declaredType,valueType)
          | None, Some valueType -> valueType
          | Some declaredType, None -> declaredType
          | None, None ->
              raiseIllegalExpression expr "var needs either a default value or declare a type"
      in
      match varType with
        | #integralType | `Pointer _ | `Function _ as typ ->
            begin
              let var = variable name typ MemoryStorage false expr.location in
              let defvar = `DefineVariable (var, Some (`Sequence implForms))
              in
              match typeCheck env.bindings defvar with
                | TypeOf _ -> Result( addVar env.bindings var, toplevelForms @ [defvar] )
                | TypeError (fe,m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
            end
        | `Array(memberType, size) as typ ->
            begin
              let var = variable name typ MemoryStorage false expr.location in
              let defvar = `DefineVariable (var, match implForms with [] -> None | _ -> Some (`Sequence implForms)) in
              match typeCheck env.bindings defvar with
                | TypeOf _ -> Result( addVar env.bindings var, toplevelForms @ [defvar] )
                | TypeError (fe,m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
            end
        | (`Record _ as typ) ->
            begin
              let value =
                match valueExpr with
                  | None -> None
                  | Some valueExpr -> Some (`Sequence implForms)
              in
              let var = variable name typ MemoryStorage false expr.location in
              Result( addVar env.bindings var, toplevelForms @ [`DefineVariable (var, value)] )
            end
        | `TypeRef _ ->
            raiseIllegalExpression expr "internal error: received unexpected type ref"
        | `TypeParam ->
            raiseIllegalExpression expr "cannot define local variable of type parameter type"
        | `ParametricType _ ->
            raiseIllegalExpression expr "cannot define local variable of parametric type"
        | `ErrorType _ as t ->
          raiseIllegalExpression expr (sprintf "cannot define local variable of %s"
                                         (typeName t))
    in
    let transform id name typeExpr valueExpr :translationResult =
      try
        begin
          if name = "funcInt" then
            match expr.location with
              | Some loc -> printf "var %s @ %s\n" name (locationToString loc)
              | None -> printf "var %s no loc\n" name
        end;
        match lookup env.bindings name with
          | VarSymbol { vglobal = false } ->
              (* todo: move somewhere else *)
              let module Errors =
                struct
                  let lexerBaseCode = 100
                  and parserBaseCode = 200
                  and expanderBaseCode = 300

                  let makeErrorString localErrorCode expr msg =
                    let errorCode = expanderBaseCode + localErrorCode in
                    match expr.location with
                      | Some loc ->
                          sprintf "error %d: %s: %d: %s" errorCode loc.fileName loc.line msg
                      | None ->
                          sprintf "error %d: %s in\n  %s" errorCode msg (Ast2.toString expr)

                  let localVarDefinedTwice loc varName =
                    errorFromExpr expr
                      (sprintf "redefinition of local variable '%s'" name)
                end
              in
              Errors.localVarDefinedTwice expr name
          | _ ->
              transformUnsafe id name typeExpr valueExpr
      with
          IllegalExpression (_, errors) ->
            Error errors
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
          if expr.id == "var" then
            errorFromExpr expr "expected var typeExpr nameId valueExpr"
          else if expr.id == "var2" then
            errorFromExpr expr "expected var2 nameId valueExpr"
          else
            errorFromExpr expr
              (sprintf "%s, invoked handler for '%s' but can only handle %s and %s"
                 "internal compiler error"
                 expr.id macroVar macroVar2)
  let translateDefineVarD = "type, name, [value] | var2 name, value", translateDefineVar

  let translateFuncCall (env : 'a env)  expr :translationResult =
    let buildCall name rettype argTypes isPointer hasVarArgs bindings args =
      let evalArg (argExpr :Ast2.sexpr) paramType =
        let _, xforms = env.translateF bindings argExpr in
        let toplevelForms, forms = extractToplevelForms xforms in
        let argForm =
          match paramType with
            | Some t when isTypeParametric t ->
                (* | Some ((`ParametricType _) as t) -> *)
              printf "parametric type param %s\n" (typeDescr (t :> typ));
              `CastIntrinsic (t, toSingleForm forms)
            | _ ->
              toSingleForm forms
        in
        (** TODO add cast if the parameter is a parametric type *)
        toplevelForms, argForm
      in
      let rec evalArgs argExprs paramTypes =
        match argExprs, paramTypes with
          | argExpr :: remArgs, paramType :: remParams ->
            evalArg argExpr (Some paramType) :: evalArgs remArgs remParams
          | argExpr :: remArgs, [] ->
            evalArg argExpr None :: evalArgs remArgs []
          | [], []->
            []
          | [], _ ->
            failwith "too many argument types in translateFuncCall"
      in
      let paramCount = List.length argTypes
      and argCount = List.length args in
      if (hasVarArgs && argCount < paramCount) then
        errorFromExpr expr
          (sprintf "expected %d parameters or more, but called with %d args"
             paramCount argCount)
      else if (not hasVarArgs && argCount != paramCount) then
        errorFromExpr expr
          (sprintf "expected %d parameters, but called with %d args"
             paramCount argCount)
      else
        let x = evalArgs args argTypes in
        let toplevelForms, argForms = flattenLeft x in
        let funccall = `FuncCall {
          fcname = name;
          fcrettype = rettype;
          fcparams = argTypes;
          fcargs = argForms;
          fcptr = isPointer;
          fcvarargs = hasVarArgs
        }
        in
        match typeCheck bindings funccall with
          | TypeOf _ ->
            Result( bindings, toplevelForms @ [funccall] )
          | TypeError (fe,msg,f,e) ->
            errorFromExpr expr (typeErrorMessage bindings (fe,msg,f,e))
    in
    match expr with
      | { id = funcall;
          args = { id = name; args = [] } :: args }
          when funcall = macroFunCall ->
        begin
          match lookup env.bindings name with
            | FuncSymbol func ->
              begin
                buildCall name func.rettype
                  (List.map snd func.fargs)
                  `NoFuncPtr
                  func.cvarargs
                  env.bindings
                  args
              end
            | VarSymbol var ->
              begin
                match var.typ with
                  | `Pointer `Function ft ->
                    begin
                      buildCall name ft.returnType ft.argTypes
                        `FuncPtr false env.bindings args
                    end
                  | _ ->
                    errorFromExpr
                      expr
                      (sprintf "trying to call variable '%s' as a function but has type %s"
                         var.vname
                         (typeName var.typ))
              end
            | _ ->
              errorFromExpr expr (sprintf "%s is neither a function nor a macro" name)
        end
      | _ ->
        errorFromExpr expr "unrecognized AST shape"
  let translateFuncCallD = "name, args...", translateFuncCall

  (** called by translateApply, not registered under any name *)
  let translateRecordLiteral (env :exprTranslateF env) typeExpr argExprs :translationResult =
    let translate recordType =
      let translateField expr : 'a mayfail =
        let bindings, formsWTL = env.translateF env.bindings expr in
        let tlforms, forms = extractToplevelForms formsWTL in
        Result (tlforms, forms)
      in
      let fieldNames = List.map fst recordType.fields in
      begin match translateStructLiteralArgs fieldNames argExprs translateField with
        | Result fieldFormsTL ->
          let alltlformsLst, fieldForms = List.split
            (List.map (fun (name, (tl, f)) -> tl, (name, f)) fieldFormsTL)
          in
          let alltlforms = List.flatten alltlformsLst in

          let translate nameAndFormList =
            let rec onlyConstantsSoFar nameAndFormList accum =
              match nameAndFormList with
                | [] ->
                  `AllConstant (List.rev accum)
                | (name, [`Constant value]) :: rem ->
                  onlyConstantsSoFar rem ((name, value) :: accum)
                | _ ->
                  let valueToFormList (name, value) =
                    name, [(`Constant value :> Lang.form)]
                  in
                  hadComplexExprs nameAndFormList (List.map valueToFormList accum)
            and hadComplexExprs nameAndFormList accum =
              match nameAndFormList with
                | [] ->
                  `ComplexExprs accum
                | nameAndForm :: rem ->
                  hadComplexExprs rem (nameAndForm :: accum)
            in
            onlyConstantsSoFar nameAndFormList []
          in

          begin match translate fieldForms with
            | `AllConstant nameAndValueList ->
              let c : formWithTLsEmbedded =
                `Constant (RecordVal (typeName (`Record recordType), nameAndValueList))
              in
              Result (env.bindings, alltlforms @ [c])

            | `ComplexExprs fieldsAndExprs ->
              let newBindings, recordVar =
                getNewLocalVar env.bindings (`Record recordType)
              in
              let recordVarAddress = `GetAddrIntrinsic recordVar in
              let makeFieldAssignment (name, forms) =
                let ptr = `GetFieldPointerIntrinsic (recordVarAddress, name) in
                `StoreIntrinsic (ptr, toSingleForm forms)
              in
              let assignments = List.map makeFieldAssignment fieldsAndExprs in
              Result (newBindings,
                      alltlforms
                      @ [`DefineVariable (recordVar, None)]
                      @ assignments
                      @ [`Variable recordVar])
          end
        | Error msgs ->
          Error msgs
      end
    in
    let fail () =
      errorFromExpr
        (Ast2.expr "(record literal)" (typeExpr :: argExprs))
        (sprintf "%s is not a struct" (Ast2.toString typeExpr))
    in
    match lookup env.bindings typeExpr.id with
      | TypedefSymbol (`Record recordType) ->
        translate recordType
      | UndefinedSymbol when typeExpr.id = "op!" ->
        begin match translateType env.bindings typeExpr with
          | Result (`Record recordType) ->
            translate recordType
          | _ ->
            fail()
        end
      | _ ->
        fail()

  let alwaysFail env typeExpr argExprs = errorFromExpr
    (Ast2.expr "(record literal)" (typeExpr :: argExprs)) "not supported"

  let translateApply translateRecordF (env :('a someTranslateF) env) expr =
    match expr with
      | { args = firstArg :: remArgs } -> begin
        match firstArg.args, lookup env.bindings firstArg.id with
          | [], FuncSymbol _
          | [], VarSymbol { typ = `Pointer `Function _ } ->
            Result( env.translateF env.bindings { expr with id = Lang.macroFunCall } )

          (** Can't combine bodies due to pattern guard *)
          | [], TypedefSymbol _ ->
            translateRecordF env firstArg remArgs
          | _, UndefinedSymbol when firstArg.id = "op!" ->
            translateRecordF env firstArg remArgs

          | [], _ ->
            let r = env.translateF env.bindings { (Ast2.shiftLeft expr.args) with location = expr.location } in
            Result r

          | (_::_), _ ->
            let tmpName = getUnusedName ~prefix:"opcall_func" env.bindings in
            let tempVar = Ast2.expr "std:base:localVar" [Ast2.idExpr tmpName; firstArg] in
            let callExpr = Ast2.expr "std:base:apply" (Ast2.idExpr tmpName :: remArgs) in
            let r = env.translateF env.bindings { (Ast2.seqExpr [tempVar; callExpr]) with location = expr.location } in
            Result r
      end
      | { args = [] } ->
        errorFromExpr expr (sprintf "expected 'std:base:apply expr args?'")

  let register addF =
    addF "std:base:localVar" translateDefineLocalVarD;
    addF macroAssign translateAssignVarD;
    addF macroSequence translateSeqD;
    addF macroSeqOp translateSeqD;
    addF macroReturn translateReturnD;
    addF macroLabel translateLabelD;
    addF macroBranch translateBranchD;
    addF macroLoad translateLoadD;
    addF macroStore translateStoreD;
    addF macroMalloc translateMallocD;
    addF macroCast translateCastD;
    addF macroFieldptr translateGetfieldptrD;
    addF macroPtradd translatePtraddD;
    addF macroPtrDiff translatePtrDiffD;
    addF macroGetaddr translateGetaddrD;
    addF macroNullptr translateNullptrD;
    addF macroVar translateDefineVarD;
    addF macroVar2 translateDefineVarD;
    addF macroFunCall translateFuncCallD;
    addF macroApply ("ast...", translateApply translateRecordLiteral)

  let registerTL addF =
    addF "var" translateGlobalVarD;
    addF macroApply ("ast...", translateApply alwaysFail)
end

module Compiler_environment : Zomp_transformer =
struct
  let translateFileName (env :exprTranslateF env) (expr :Ast2.sexpr)  :translationResult =
    let newBindings, var = getNewGlobalVar env.bindings (`Pointer `Char) in
    let value = StringLiteral (Ast2.fileName expr) in
    let gvar = {
      gvVar = var;
      gvInitialValue = value;
      gvDefinitionLocation = expr.location;
    } in
    Result (newBindings, [`ToplevelForm (`GlobalVar gvar); `Variable var])

  let translateLineNumber (env :exprTranslateF env) (expr :Ast2.sexpr)  :translationResult =
    Result (env.bindings, [`Constant (Int32Val (Int32.of_int (Ast2.lineNumber expr)))])

  let register addF =
    addF "std:env:file" ("char*", translateFileName);
    addF "std:env:line" ("int", translateLineNumber)

  let registerTL addF = ()
end

module Array : Zomp_transformer =
struct
  let arraySize (env: exprTranslateF env) expr =
    match expr with
      | { id = _; args = [arrayExpr] } ->
        let _, rightHandForm, toplevelForms =
          translateToForms env.translateF env.bindings arrayExpr
        in
        begin match typeCheck env.bindings rightHandForm with
          | TypeOf `Array(_, size) ->
            Result (env.bindings,
                    toplevelForms @ [(rightHandForm :> formWithTLsEmbedded);
                                     `Constant (Int32Val (Int32.of_int size))])
          | TypeOf invalidType ->
            errorFromTypeError env.bindings arrayExpr
              (Semantic.Ast expr,
               "cannot get size of array",
               invalidType,
               `Any "array")
          | TypeError (fe,m,f,e) ->
            errorFromTypeError env.bindings arrayExpr (fe,m,f,e)
        end
      | _ ->
        errorFromExpr expr "expected 'zmp:array:size arrayExpr'"
  let arraySizeD = "array", arraySize

  let arrayAddr (env: exprTranslateF env) = function
    | {args = [arrayPtrExpr]} as expr ->
      let _, arrayPtrForm, tlforms =
        translateToForms env.translateF env.bindings arrayPtrExpr
      in
      begin match typeCheck env.bindings arrayPtrForm with
        | TypeOf `Pointer `Array(memberType,_) ->
          begin
            Result(
              env.bindings, tlforms @ [
                `CastIntrinsic(`Pointer memberType, arrayPtrForm)])
          end
        | TypeOf invalidType ->
          errorFromTypeError env.bindings arrayPtrExpr
            (Semantic.Ast expr,
             "cannot get address of first element",
             invalidType, `Any "Pointer to array")
        | TypeError (fe, m,f,e) ->
          errorFromTypeError env.bindings expr (fe,m,f,e)
      end
    | invalidExpr ->
      errorFromExpr invalidExpr "expected 'zmp:array:addr arrayExpr'"
  let arrayAddrD = "arrayPtr", arrayAddr

  let register addF =
    addF "zmp:array:size" arraySizeD;
    addF "zmp:array:addr" arrayAddrD

  let registerTL addF = ()
end

module Overloaded_ops : Zomp_transformer =
struct
  (** creates a macro which turns baseName(l,r) into
    * baseName_ltype_rtype(l,r). Has special handling for op+/op- and pointer
    * arguments *)
  let overloadedOperator baseName (env :exprTranslateF env) = function
    | {args = [leftExpr; rightExpr]} as expr ->
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
                                                          fcptr = `NoFuncPtr;
                                                          fcvarargs = false })])
                end
            | "op-", TypeOf `Pointer _, TypeOf `Pointer _ ->
              begin
                Result(env.bindings,
                       toplevelFormsLeft @ toplevelFormsRight @
                       [`PtrDiffIntrinsic (leftForm, rightForm)])
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
                                              fcptr = `NoFuncPtr;
                                              fcvarargs = false }])
                      end
                    | _ -> begin
                      errorFromExpr expr
                        (sprintf "no overload for %s(%s, %s) found (expected function %s)"
                           baseName typenameL typenameR funcName)
                      end
                end
            | _, lresult, rresult ->
                let typeErrorMessagePotential potError =
                  match potError with
                    | TypeError (fe,msg,found,expected) ->
                      Some (Serror.fromTypeError env.bindings expr (fe, msg, found, expected))
                    | _ ->
                      None
                in
                Error (mapFilter typeErrorMessagePotential [lresult; rresult])
        end
    | invalidExpr ->
      errorFromExpr invalidExpr "expected two arguments"

  let overloadedFunction baseName (env :exprTranslateF env) = function
    | {args = [argExpr]} as expr ->
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
                                          fcptr = `NoFuncPtr;
                                          fcvarargs = false }])
                  | _ ->
                    let genericFuncName = baseName ^ "_ptr" in
                    match Bindings.lookup env.bindings genericFuncName with
                      | FuncSymbol func ->
                        Result(env.bindings,
                               toplevelForms @
                                 [`FuncCall { fcname = func.fname;
                                              fcrettype = func.rettype;
                                              fcparams = List.map snd func.fargs;
                                              fcargs = [`CastIntrinsic
                                                           (`Pointer `Void, argForm)];
                                              fcptr = `NoFuncPtr;
                                              fcvarargs = false }])
                      | _ ->
                        errorFromExpr expr
                          (sprintf
                              "no overload for %s(%s) found (expected function %s or %s)"
                                  baseName (typeName argType) funcName genericFuncName)
              end
            | TypeError (fe,m,f,e) ->
                errorFromTypeError env.bindings expr (fe,m,f,e)
        end
    | invalidExpr ->
      errorFromExpr invalidExpr "expected one argument"

  let register addF =
    let addOp (name, op) =
      addF ("zmp:cee:" ^ name) ("T, T", overloadedOperator op)
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
    let addFun name = addF ("zmp:cee:" ^ name) ("T", overloadedFunction name) in
    List.iter addFun
      ["print"; "write";
       "toInt"; "toFloat"; "toDouble"; "toBool"; "toChar"; "toCString";
       "neg"; "not"]

  let registerTL addF = ()
end

let traceMacroExpansion = ref (Some (fun (_ :string) (_ :sexpr) -> ()))
let setTraceMacroExpansion f = traceMacroExpansion := f

let translateFromDict
    baseInstructions
    translateExprOld
    translateF
    (bindings :bindings)
    (expr :Ast2.sexpr)
    =
  try
    begin match !traceMacroExpansion with
      | Some f -> f (sprintf "dict/%s" expr.id) expr
      | None -> ()
    end;
    let handler = Hashtbl.find baseInstructions expr.id in
    let env = {
      bindings = bindings;
      translateF = translateF;
      translateExprOld = translateExprOld;
      translateExpr = (fun env expr ->
        let newBindings, formsWTL = translateExprOld env.bindings expr in
        Result (newBindings, formsWTL));
      parseF = Parseutils.parseIExprsOpt;
    } in
    match handler env expr with
      | Error errors ->
        let printError error =
          printf "%s\n" (Serror.toString error)
        in
        List.iter printError errors;
        print_newline();
        flush stdout;
        None
      | Result (bindings, tlexprs) ->
        Some (bindings, tlexprs)
  with Not_found ->
    None

let translateBaseInstruction, addBaseInstruction, foreachBaseInstructionDoc =
  let table = Hashtbl.create 32 in
  let documentation = Hashtbl.create 32 in
  let add name (doc, f) =
    Hashtbl.add documentation name doc;
    Hashtbl.add table name f
  in
  Base.register add;
  Array.register add;
  Overloaded_ops.register add;
  Compiler_environment.register add;
  let addBaseInstruction name doc f = add name (doc, f) in
  translateFromDict table, addBaseInstruction, (fun f -> Hashtbl.iter f documentation)

let rec translate errorF translators bindings (expr :Ast2.t) =
  (* let s = Ast2.toString expr in *)
  (* let s = expr.id in *)
  (* begin match expr.location with *)
  (*   | Some loc -> *)
  (*       printf "Translate @%s %s\n" (Ast2.locationToString loc) s; *)
  (*       flush stdout; *)
  (*   | None -> *)
  (*       printf "Translate! %s\n" s; *)
  (*       flush stdout; *)
  (* end; *)
  let rec t = function
    | f :: remf ->
      begin
        Zompvm.currentBindings := bindings;
        match f (translate errorF translators) bindings expr with
          | Some ((newBindings : Bindings.t), result) -> (newBindings, result)
          | None -> t remf
      end
    | [] ->
      errorF expr (sprintf "no translator matched expression, id=%s" expr.id)
  in
  t translators

let rec translateNested (bindings :bindings) (expr :Ast2.sexpr) =
  begin match !traceMacroExpansion with
    | Some f -> f "nested/???" expr
    | None -> ()
  end;
  translate raiseIllegalExpression
    [
      translateBaseInstruction translateNested;
      Translators_deprecated_style.translateRestrictedFunCall;
      Translators_deprecated_style.translateSimpleExpr;
      Old_macro_support.translateMacroCall;
      Old_macro_support.translateDefineMacro translateNested;
      Translators_deprecated_style.translateRecord;
    ]
  bindings expr

let translateNested = sampleFunc2 "translateNested" translateNested

(* let translateNew bindings expr = *)
(*   try *)
(*     Result (translateNested bindings expr) *)
(*   with IllegalExpression (expr, msg) -> *)
(*     errorFromExpr expr "could not find handler for expression" *)

(* let translateDict *)
(*     baseInstructions *)
(*     translateF *)
(*     (bindings :bindings) *)
(*     (expr :Ast2.sexpr) *)
(*     = *)
(*   match Bindings.lookup bindings expr.id with *)
(*     | VarSymbol var -> *)
(*         None *)
(*     | FuncSymbol func -> *)
(*         None *)
(*     | MacroSymbol macro -> *)
(*         None *)
(*     | TypedefSymbol typedef -> *)
(*         None *)
(*     | LabelSymbol label -> *)
(*         None *)
(*     | UndefinedSymbol -> *)
(*         begin try *)
(*           let handler = Hashtbl.find baseInstructions expr.id in *)
(*           let env = { *)
(*             bindings = bindings; *)
(*             translateF = translateF; *)
(*             parseF = Parseutils.parseIExprsOpt; *)
(*           } in *)
(*           match handler env expr with *)
(*             | Error errors -> *)
(*                 print_string (combineErrors "swallowed errors: " errors); *)
(*                 print_newline(); *)
(*                 flush stdout; *)
(*                 None *)
(*             | Result (bindings, tlexprs) -> *)
(*                 Some (bindings, tlexprs) *)
(*         with Not_found -> *)
(*           None *)
(*         end *)

let translateAndEval handleLLVMCodeF translateTL env exprs =
  let genLLVMCode form =
    collectTimingInfo "gencode"
      (fun () ->
         Genllvm.gencodeTL form)
  in
  let evalLLVMCode bindings forms llvmCode =
    collectTimingInfo "Zompvm.evalLLVMCode"
      (fun () ->
         Zompvm.evalLLVMCode bindings forms llvmCode)
  in
  let llvmCodeCallback llvmCode =
    collectTimingInfo "handleLLVMCodeF"
      (fun () ->
         handleLLVMCodeF llvmCode)
  in
  let impl() =
    let newBindings, tlexprsFromFile =
      List.fold_left
        (fun (bindings,prevExprs) sexpr ->
           let newBindings, newExprs = translateTL bindings sexpr in
           List.iter
             (fun form ->
                let llvmCode = genLLVMCode form in
                evalLLVMCode bindings [form] llvmCode;
                llvmCodeCallback llvmCode)
             newExprs;
           newBindings, prevExprs @ newExprs )
        (env.bindings, [])
        exprs
    in
    Result (newBindings, [])
  in
  collectTimingInfo "translateAndEval" impl

let translateSeqTL handleLLVMCodeF translateTL env expr =
  translateAndEval handleLLVMCodeF translateTL env expr.args

let () =
  addBaseInstruction macroMacro "name, args..., body"
    (Macros.translateDefineMacro translateNested)

let translateBaseInstructionTL, addToplevelInstruction, foreachToplevelBaseInstructionDoc =
  let table : (string, toplevelExprTranslateF env -> Ast2.sexpr -> toplevelTranslationResult) Hashtbl.t =
    Hashtbl.create 32
  in
  let documentation = Hashtbl.create 32 in
  let add name doc f =
    Hashtbl.add documentation name doc;
    Hashtbl.add table name f
  in

  add macroMacro "name, args..., body"
    (sampleFunc2 "macro(dict)" (Macros.translateDefineMacro translateNested));
  Base.registerTL (fun name (doc, f) -> add name doc f);

  translateFromDict table, add, (fun f -> Hashtbl.iter f documentation)

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
              raiseIllegalExpression quotedExpr "did not evaluate to a variable declaration"
      end
  | _ ->
      None

let matchFunc =
  let rec scanParams = function
    | [{ id = "postop..."; args = [{ id = "cvarargs"; args = [] }] }] ->
        [], true
    | [] ->
        [], false
    | { id = opjux; args = [typeExpr; {id = paramName; args = []}] as param } :: remArgs
        when opjux = macroJuxOp ->
        let result, hasvarargs = scanParams remArgs in
        { (Ast2.shiftLeft param) with location = typeExpr.location } :: result, hasvarargs
    | _ -> failwith ""
  in
  function
    (** func def from iexpr for polymorphic function *)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args =
              { id = paramop; args =
                  { id = name; args = [] }
                  :: paramTypeExprs }
              :: paramExprs };
          { id = opseq; args = _ } as implExpr
        ] } as expr
        when id = macroFunc &&
          opcall = macroCallOp &&
          paramop = macroParamType &&
          opseq = macroSeqOp ->
        begin try
          let getParametricTypeName = function
            | { id = name; args = [] } -> name
            | _ -> failwith ""
          in
          let params, hasvararg = scanParams paramExprs in
          let parametricTypes = List.map getParametricTypeName paramTypeExprs in
          `FuncDef (name, typeExpr, params, hasvararg, implExpr, parametricTypes)
        with Failure _ ->
          `NotAFunc expr
        end

    (** func decl from iexpr with multiple/0 arguments **)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args = { id = name; args = [] } :: paramExprs }
        ] } as expr
        when id = macroFunc && opcall = macroCallOp ->
        begin try
          let params, hasvararg = scanParams paramExprs in
          `FuncDecl (name, typeExpr, params, hasvararg)
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
          let params, hasvararg = scanParams paramExprs in
          `FuncDef (name, typeExpr, params, hasvararg, implExpr, [])
        with Failure _ ->
          `NotAFunc expr
        end

    | { id = "std:base:func"; args = _ } as expr ->
        `NotAFunc expr
    | expr ->
        `NotAFunc expr

let rec translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) expr =
  let translateType bindings expr =
    translateType bindings Translators_deprecated_style.reportWarning expr
  in
  let doSanityChecks returnType name params =
    let module StringSet = Set.Make(String) in
    let checkForDuplicateParameterName() =
      let _ =
        List.fold_left
          (fun prevNames (nextName, _) ->
            if StringSet.mem nextName prevNames then
              raiseIllegalExpression expr
                (sprintf "each argument needs a distinct name. %s used more than once" nextName);
            StringSet.add nextName prevNames)
          StringSet.empty
          params
      in ()
    in
    checkForDuplicateParameterName();

    let nameRE = "^[a-zA-Z0-9][^\"]*$" in
    if not (name =~ nameRE) then
      raiseIllegalExpression expr
        (sprintf "function names must match the following regexp: %s" nameRE);
    ()
  in
  let buildFunction bindings typ uncheckedName paramExprs hasvarargs implExprOption =
    let name = removeQuotes uncheckedName in

    let translateParam argExpr =
      let translate varName typeExpr =
        match translateType bindings typeExpr with
          | Result typ ->
              if name = "same" then
                printf "func arg '%s' : '%s (%s)'\n" varName (typeName typ) (Ast2.toString typeExpr);
              (varName, typ)
          | Error _ -> raiseInvalidType typeExpr
      in
      match argExpr with
        | { id = "seq"; args = [typeExpr; { id = varName; args = []};] } ->
            translate varName typeExpr
        | { id = typeExpr; args = [{ id = varName; args = [] }] } ->
            translate varName (Ast2.idExpr typeExpr)
        | _ as expr ->
            raiseIllegalExpression expr "expected 'typeName varName' for param"
    in

    let rec bindingsWithParams bindings params =
      let addParam bindings (name, typ) =
        let var =
          variable
            ~name
            ~typ
            (** all parameters are copied into a local var by genllvm *)
            ~storage:MemoryStorage
            ~global:false
            ~location:None
        in
        addVar bindings var
      in
      List.fold_left addParam bindings params
    in

    let params = List.map translateParam paramExprs in
    doSanityChecks typ name params;

    let innerBindings = bindingsWithParams bindings params in
    let nestedTLForms, impl = match implExprOption with
      | Some implExpr ->
          let nestedForms = snd (translateNested innerBindings implExpr) in
          let nestedTLForms, implForms = extractToplevelForms nestedForms in
          let nestedTLForms = List.map (fun (`ToplevelForm f) -> f) nestedTLForms in
          let implFormsWithFixedVars = moveLocalVarsToEntryBlock (`Sequence implForms) in
          nestedTLForms, Some (`Sequence implFormsWithFixedVars)
      | None -> [], None
    in
    let f = (if hasvarargs then varargFunc else func) name typ params impl Basics.fakeLocation in
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
          expr (Common.combine "\n"
                  (let msg = sprintf "could not translate function %s:" name in
                   msg :: messages))
  in
  match matchFunc expr with
    | `FuncDef (name, typeExpr, paramExprs, hasvarargs, implExpr, parametricTypes) ->
        begin
          let bindingsWParametricTypes =
            List.fold_left
              (fun bindings parametricTypeName ->
                 addTypedef bindings parametricTypeName `TypeParam)
              bindings
              parametricTypes
          in
          match translateType bindings typeExpr with
            | Result typ -> begin
                let tempBindings, _, _ =
                  buildFunction bindingsWParametricTypes typ name paramExprs hasvarargs None
                in
                let newBindings, toplevelForms, funcDef =
                  buildFunction tempBindings typ name paramExprs hasvarargs (Some implExpr)
                in
                match typeCheckTL newBindings funcDef with
                  | TypeOf _ -> Some( newBindings, toplevelForms @ [funcDef] )
                  | TypeError (fe, msg, declaredType, returnedType) ->
                      raiseIllegalExpression
                        expr
                        (typeErrorMessage bindings (fe, msg, returnedType, declaredType))
              end
            | Error _ -> raiseInvalidType typeExpr
        end
    | `FuncDecl (name, typeExpr, paramExprs, hasvarargs) ->
        begin
          match translateType bindings typeExpr with
            | Result typ ->
                begin
                  let newBindings, _, funcDecl = buildFunction bindings typ name paramExprs hasvarargs None in
                  Some (newBindings, [funcDecl] )
                end
            | Error _ ->
                raiseInvalidType typeExpr
        end
    | `NotAFunc _ ->
        None

and translateTLNoErr bindings expr =
  begin match !traceMacroExpansion with
    | Some f -> f "tl/???" expr
    | None -> ()
  end;
  translate raiseIllegalExpression
  [
    sampleFunc3 "translateBaseInstructionTL" (translateBaseInstructionTL translateNested);
    sampleFunc3 "translateFunc" translateFunc;
    sampleFunc3 "translateTypedef" Translators_deprecated_style.translateTypedef;
    sampleFunc3 "translateDefineMacro" (Old_macro_support.translateDefineMacro translateNested);
    sampleFunc3 "translateMacroCall" Old_macro_support.translateMacroCall;
    sampleFunc3 "translateCompileTimeVar" translateCompileTimeVar;
  ]
  bindings expr

let translateTLNoErr = Common.sampleFunc2 "translateTL" translateTLNoErr

let translateTL bindings expr = Result (translateTLNoErr bindings expr)

type toplevelTranslationFunction =
    toplevelEnv -> Ast2.sexpr -> toplevelTranslationResult

let translateInclude includePath handleLLVMCodeF translateTL (env : toplevelExprTranslateF env) expr =
  let importFile fileName =
    let source =
      collectTimingInfo "readFileContent"
        (fun () -> Common.readFile ~paths:!includePath fileName)
    in
    let exprs =
      collectTimingInfo "parse"
        (fun () -> Parseutils.parseIExprsNoCatch ~fileName source)
    in
    let exprs = List.map (Parseutils.fixFileName fileName) exprs in
    collectTimingInfo "translateAndEval"
      (fun () -> translateAndEval handleLLVMCodeF translateTL env exprs)
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
          | Indentlexer.UnknowToken(loc,token,reason) ->
            errorFromExpr expr
              (Indentlexer.unknownTokenToErrorMsg
                 (Some {loc with fileName = fileName}, token, reason))
          | IllegalExpression (expr, errors) ->
            Error errors
          | Sys_error _ ->
            Error [Serror.fromExpr expr
                      (Common.combine "\n  "
                         (sprintf "file '%s' could not be found" fileName
                          :: sprintf "pwd = %s" (Sys.getcwd())
                          :: List.map (sprintf "zomp-include-dir %s") !includePath))]
          | error ->
            let msg = Printexc.to_string error in
            errorFromExpr expr
              (sprintf "%s, while compiling included file %s" msg fileName)
        end
      | invalidExpr ->
        errorFromExpr invalidExpr ("expecting 'include \"fileName.zomp\"'"))

let makeTranslateSeqFunction handleLLVMCodeF =
  translateSeqTL handleLLVMCodeF translateTLNoErr
let makeTranslateIncludeFunction includePath handleLLVMCodeF =
  translateInclude includePath handleLLVMCodeF translateTLNoErr



