(* #directory "../v3";; *)
(* #load "lexer2.cmo";; *)
(* #load "parser2.cmo";; *)
(* #load "lang.cmo";; *)
(* #load "ast2.cmo";; *)
(* #load "common.cmo";; *)
(* #load "bindings.cmo";; *)

open Lang
open Ast2
open Common
open Bindings
open Printf

let macroVar = "var"
and macroMutableVar = "mvar"
and macroFunc = "func"
and macroIfThenElse = "ifthenelse"
and macroLoop = "loop"
and macroAssign = "assign"
and macroSequence = "seq"
and macroTypedef = "type"
and macroRecord = "record"
and macroField = "field"
and macroPtr = "ptr"
and macroReturn = "ret"
and macroLabel = "label"
and macroBranch = "branch"
and macroMacro = "macro"
and macroFieldptr = "fieldptr"
and macroLoad = "load"
and macroStore = "store"
and macroNullptr = "nullptr"
and macroPtradd = "ptradd"
and macroMalloc = "malloc"
and macroGetaddr = "ptr"
  
exception IllegalExpression of expression * string
  
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))
  
let raiseIllegalExpressionFromTypeError expr (msg, found, expected) =
  raiseIllegalExpression expr (sprintf "Expected type %s but found %s" (typeName expected) (typeName found))

    
let raiseInvalidType typeExpr = raise (Typesystems.Zomp.CouldNotParseType (Ast2.expression2string typeExpr))


(* TODO: use in translateDefineVar and translateGlobalVar + test *)        
let determineStorage typ =
  match typ with
    | `Pointer _ -> MemoryStorage
    | _ -> RegisterStorage
        
let getNewVar bindings typ =
  let newVarName =
    let rec tryTempVar num =
      let name = (Printf.sprintf "tempvar_%d" num) in
      match lookup bindings name with
        | UndefinedSymbol -> name
        | _ -> tryTempVar (num + 1)
    in
    tryTempVar 0
  in
  let value = defaultValue typ in
  let var = variable newVarName typ value (determineStorage typ) false in
  (addVar bindings var, var)


let rec translateType bindings typeExpr =
  let lookupType bindings name =
    try
      Some (Lang.parseType name)
    with
      | Typesystems.Zomp.CouldNotParseType _ ->
          match lookup bindings name with
            | TypedefSymbol t -> Some t
            | _ -> None
  in
  match typeExpr with
    | { id = id; args = [targetTypeExpr]; } when id = macroPtr ->
        begin
          match translateType bindings targetTypeExpr with
            | Some t -> Some (`Pointer t)
            | None -> None
        end
    | { id = name; args = [] } ->
        begin
          lookupType bindings name
        end
    | _ -> None

        
type exprTranslateF = bindings -> expression -> bindings * expr list

let translatelst (translateF :exprTranslateF) bindings expr =
  let rec worker bindings = function
    | [] -> bindings, []
    | expr :: tail ->
        let newBindings, sf = translateF bindings expr in
        let resultingBindings, sfuncs = worker newBindings tail in
        resultingBindings, (sf @ sfuncs)
  in
  worker bindings expr
    
let translateSeq (translateF : exprTranslateF) bindings = function
  | { id = id; args = sequence } when id = macroSequence ->
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
    match translateType bindings typeExpr with
      | Some (#integralType as typ)
      | Some (`Pointer _ as typ) ->
          begin
            let _, simpleform = translateF bindings valueExpr in
            match typ, typeCheck (`Sequence simpleform) with
              | leftHandType, TypeOf rightHandType when leftHandType = rightHandType ->
                  begin
                    let value = defaultValue typ in
                    let storage = MemoryStorage
                    in
                    let var = variable name typ value storage false in
                    Some( addVar bindings var, [ `DefineVariable (var, Some (`Sequence simpleform)) ] )
                  end
              | leftHandType, TypeOf rightHandType ->
                  raiseIllegalExpression expr
                    (sprintf "types %s and %s do not match"
                       (Lang.typeName leftHandType) (Lang.typeName rightHandType))
              | _, TypeError (msg, _, _) -> raiseIllegalExpression valueExpr msg
          end
      | Some (`Record _ as typ) ->
          begin
            let var = variable name typ (defaultValue typ) MemoryStorage false in
            Some( addVar bindings var, [ `DefineVariable (var, None) ] )
          end
      | _ ->
          raise (CouldNotParseType (Ast2.expression2string typeExpr))
  in
  match expr with
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          valueExpr
        ] }
        when (id = macroVar) || (id = macroMutableVar) ->
        transform id name typeExpr valueExpr
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
        ] }
        when (id = macroVar) || (id = macroMutableVar) ->
        transform id name typeExpr { id = ""; args = []}
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
  | _ -> None

let translateSimpleExpr (_ :exprTranslateF) (bindings :bindings) expr =
  match expr2VarOrConst bindings expr with
    | Some varOrConst ->
        begin match varOrConst with
          | `Constant StringLiteral _ -> raiseIllegalExpression expr "String literals are not allowed, yet"
          | _ -> Some (bindings, [varOrConst] )
        end
    | None -> None
          

let translateFuncCall (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = args; } as expr ->
      match lookup bindings name with
        | FuncSymbol func ->
            begin
              let evalArg arg =
                match translateF bindings arg with
                  | _, [expr] -> expr
                  | _, exprList -> `Sequence exprList
              in
              let argExprs = List.map evalArg args in
              let funccall = `FuncCall {
                fcname = name;
                fcrettype = func.rettype;
                fcparams = List.map snd func.fargs;
                fcargs = argExprs;
              }
              in
              match typeCheck funccall with
                | TypeOf _ -> Some( bindings, [funccall] )
                | TypeError (msg, _, _) -> raiseIllegalExpression expr ("Type error: " ^ msg)
            end
        | _ -> None
            
let translateMacro translateF (bindings :bindings) = function
  | { id = macroName; args = args; } as expr ->
      match lookup bindings macroName with
        | MacroSymbol macro ->
            begin try
              let transformedExpr = macro.mtransformFunc args in
              Some (translateF bindings transformedExpr)
            with
              | Failure msg ->
                  raiseIllegalExpression expr ("Could not expand macro: " ^ msg)
            end
        | _ -> None
            
let translateDefineMacro translateF (bindings :bindings) = function
  | { id = id; args =
        { id = name; args = [] }
        :: paramImpl
    } when id = macroMacro ->
      begin match List.rev paramImpl with
        | [] -> None
        | impl :: args ->
            begin
              let args = List.rev args in
              let argNames = List.map
                (function
                   | { id = name; args = [] } -> name
                   | _ as arg -> raiseIllegalExpression arg "only identifiers allowed as macro parameter")
                args
              in
              Some( Bindings.addMacro bindings name (fun args -> Ast2.replaceParams argNames args impl), [] )
            end
      end
  | _ ->
      None

let translateReturn (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [expr] } when id = macroReturn ->
      begin
        match translateF bindings expr with
          | _, [form] -> Some( bindings, [`Return form] )
          | _, _ -> None
      end
  | _ -> None
      
let translateAssignVar (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        { id = varName; args = [] };
        rightHandExpr;
      ] } when id = macroAssign -> begin
      let _, rightHandSimpleform = translateF bindings rightHandExpr in
      match lookup bindings varName with
        | VarSymbol v ->
            Some (bindings, [`AssignVar (v, `Sequence rightHandSimpleform)] )
        | _ -> None
    end
  | _ -> None

let translateTypedef translateF (bindings :bindings) = function
  | { id = id; args = [
        { id = newTypeName; args = [] };
        targetTypeExpr;
      ] }
      when id = macroTypedef ->
      begin
        match translateType bindings targetTypeExpr with
          | None -> raiseInvalidType targetTypeExpr
          | Some t -> Some (addTypedef bindings newTypeName t, [] )
      end
  | { id = id; args =
        { id = typeName; args = [] }
        :: componentExprs
    } as expr
      when id = macroTypedef ->
      begin
        let expr2comp =
          let translate name typeExpr = 
            match translateType bindings typeExpr with
              | Some typ -> name, typ
              | None -> raise (CouldNotParseType typeName)
          in
          function
            | { id = typeName; args = [{ id = componentName; args = []}] } ->
                translate componentName { id = typeName; args = [] }
            | { id = seq; args = [typeExpr; { id = componentName; args = [] }] } when seq = macroSequence ->
                translate componentName typeExpr
            | _ -> raiseIllegalExpression expr "(type typeName (typeExpression componentName)* ) expected"
        in
        let components = List.map expr2comp componentExprs in
        Some (addTypedef bindings typeName (`Record components), [] )
      end
  | _ -> None

let translateRecord (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args =
        { id = name; args = []; }
        :: componentExprs
    } as expr
      when id = macroRecord ->
      begin
        match lookup bindings name with
          | TypedefSymbol `Record components ->
              begin
                let recordInitFunc name components =
                  { fname = name ^ "_init";
                    rettype = `Void;
                    fargs = components;
                    impl = None;
                  }
                in
                let initFunc = recordInitFunc name components in
                let translate param compExpr = match (param, compExpr) with
                  | ((argName, argType), ({ id = compName; args = [argExpr] }) ) ->
                      begin
                        if argName = compName then begin
                          `Sequence (snd (translateF bindings argExpr))
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
  let buildStoreInstruction ptrExpr rightHandExpr =
    let newBindings, ptrForm = translateF bindings ptrExpr >>= apply2nd toSingleForm in
    let newBindings, rightHandForm = translateF newBindings rightHandExpr >>= apply2nd toSingleForm in
    let storeInstruction = `StoreIntrinsic (ptrForm, rightHandForm) in
    match typeCheck storeInstruction with
      | TypeOf _ ->
          Some( newBindings, [storeInstruction] )
      | TypeError (m,f,e) ->
          raiseIllegalExpressionFromTypeError rightHandExpr (m,f,e)
  in
  let buildMallocInstruction typeExpr countExpr =
    let typ =
      match translateType bindings typeExpr with
        | Some t -> t
        | None -> raiseIllegalExpression typeExpr "Could not translate type"
    in
    let newBindings, rightHandForm = translateF bindings countExpr >>= apply2nd toSingleForm in
    let mallocForm = `MallocIntrinsic (typ, rightHandForm) in
    match typeCheck mallocForm with
      | TypeOf _ -> Some( newBindings, [mallocForm] )
      | TypeError (m,f,e) ->
          raiseIllegalExpressionFromTypeError expr (m,f,e)
  in
  let buildLoadInstruction ptrExpr =
    let _, simpleForms = translateF bindings ptrExpr in
    let loadForm = `LoadIntrinsic (`Sequence simpleForms) in
    match typeCheck loadForm with
      | TypeOf _ -> Some( bindings, [loadForm] )
      | TypeError (m,f,e) ->
          raiseIllegalExpressionFromTypeError expr (m,f,e)
  in
  match expr with
    | { id = id; args = [typeExpr] } when id = macroNullptr ->
        convertSimple typeExpr (fun t -> `NullptrIntrinsic t)
    | { id = id; args = [typeExpr] } when id = macroMalloc ->
        buildMallocInstruction typeExpr { id = "1"; args = [] }
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
          let newBindings, ptrForm = translateF bindings ptrExpr >>= apply2nd toSingleForm in
          let newBindings, indexForm = translateF newBindings indexExpr >>= apply2nd toSingleForm in
          let ptradd = `PtrAddIntrinsic (ptrForm, indexForm) in
          match typeCheck ptradd with
            | TypeOf _ -> Some( newBindings, [ptradd] )
            | TypeError (m,f,e) ->
                raiseIllegalExpressionFromTypeError expr (m,f,e)
        end
    | { id = id; args = [
          recordExpr;
          { id = fieldName; args = [ ] };
        ] } when id = macroFieldptr ->
        begin
          let newBindings, recordForm = translateF bindings recordExpr >>= apply2nd toSingleForm in
          let fieldptr = `GetFieldPointerIntrinsic (recordForm, fieldName) in
          match typeCheck fieldptr with
            | TypeOf _ -> Some( newBindings, [fieldptr] )
            | TypeError (m,f,e) ->
                raiseIllegalExpressionFromTypeError expr (m,f,e)
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

      
let translateNested = translate raiseIllegalExpression
  [
    translateSeq;
    translateDefineVar;
    translateSimpleExpr;
    translateFuncCall;
    translateMacro;
    translateDefineMacro;
    translateAssignVar;
    translateTypedef;
    translateGenericIntrinsic;
    translateRecord;
    translateReturn;
    translateLabel;
    translateBranch;
  ]

type toplevelExprTranslateF = bindings -> expression -> bindings * toplevelExpr list

let translateGlobalVar (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        typeExpr;
        { id = name; args = [] };
        { id = valueString; args = [] }
      ] } as expr
      when id = macroVar || id = macroMutableVar ->
      begin
        match translateType bindings typeExpr with
          | Some ctyp -> begin
              match ctyp with
                | (#integralType as typ) | (`Pointer `Char as typ) ->
                    let value = parseValue typ valueString in
                    let var = globalVar name typ value in
                    let newBindings = addVar bindings var in
                    Some( newBindings, [ GlobalVar var ] )
                | `Pointer targetType ->
                    if valueString = "null" then
                      let var = globalVar name (`Pointer targetType) (PointerVal (targetType, None)) in
                      let newBindings = addVar bindings var in
                      Some( newBindings, [GlobalVar var] )
                    else
                      raiseIllegalExpression expr "only null values supported for pointers currently"
                | _ -> raiseIllegalExpression expr
                    "only integral types legal for variables at this time"
            end
          | None -> raiseInvalidType typeExpr
      end
  | _ ->
      None

let translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) expr =
  let buildFunction typ name paramExprs implExprOption =
    (*     let typ = match lookupType bindings typeName with *)
    (*       | Some t -> t *)
    (*       | None -> raise (UnknownType typeName) *)
    (*     in *)
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
            translate varName { id = typeName; args = [] }
        | _ as expr ->
            raiseIllegalExpression expr "Expected 'typeName varName' for param"
    in
    let params = List.map expr2param paramExprs in
    let rec localBinding bindings = function
      | [] -> bindings
      | (name, typ) :: tail ->
          let var = localVar name typ (defaultValue typ) in
          localBinding (addVar bindings var) tail
    in
    let innerBindings = localBinding bindings params in
    let impl = match implExprOption with
      | Some implExpr -> Some (`Sequence (snd (translateNested innerBindings implExpr)))
      | None -> None
    in
    let f = func name typ params impl in
    let newBindings = addFunc bindings f in
    let funcDef = DefineFunc f in
    newBindings, funcDef
  in
  match expr with
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = seq1; args = paramExprs };
          { id = seq2; args = _ } as implExpr;
        ] } when id = macroFunc && seq1 = macroSequence && seq2 = macroSequence ->
        begin
          match translateType bindings typeExpr with
            | Some typ -> begin
                let newBindings, funcDef = buildFunction typ name paramExprs (Some implExpr) in
                match typeCheckTL funcDef with
                  | TypeOf _ -> Some( newBindings, [ funcDef ] )
                  | TypeError (msg, declaredType, returnedType) ->
                      raiseIllegalExpression
                        expr
                        (Printf.sprintf "Function has return type %s but returns %s: %s"
                           (Lang.typeName declaredType)
                           (Lang.typeName returnedType)
                           msg)
              end
            | None -> raiseInvalidType typeExpr
        end
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = seq; args = paramExprs };
        ] } when id = macroFunc && seq = macroSequence ->
        begin
          match translateType bindings typeExpr with
            | Some typ ->
                begin
                  let newBindings, funcDecl = buildFunction typ name paramExprs None in
                  Some (newBindings, [funcDecl] )
                end
            | None ->
                raiseInvalidType typeExpr
        end
    | _ ->
        None


let translateTL = translate raiseIllegalExpression [
  translateGlobalVar;
  translateFunc;
  translateTypedef;
  translateDefineMacro;
  translateMacro;
]
  
  
