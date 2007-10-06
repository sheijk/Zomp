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
  
exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

let raiseInvalidType typeExpr = raise (Typesystems.Zomp.CouldNotParseType (Ast2.expression2string typeExpr))

type typecheckResult =
  | TypeOf of composedType
      (** error message, found type, expected type *)
  | TypeError of string * composedType * composedType
      
let rec typeOf = function
  | Sequence [] -> TypeOf `Void
  | Sequence [expr] -> typeOf expr
  | Sequence (_ :: tail) -> typeOf (Sequence tail)
  | DefineVariable (var, expr) -> begin
      match expr with
        | Some expr ->
            begin
              match typeOf expr with
                | TypeOf exprType when exprType = var.typ -> TypeOf var.typ
                | TypeOf wrongType -> TypeError (
                    "variable definition requires same type for var and default value", wrongType, var.typ)
                | _ as e -> e
            end
        | None -> TypeOf var.typ
    end
  | Variable var -> TypeOf var.typ
  | Constant value -> TypeOf (Lang.typeOf value)
  | FuncCall call ->
      begin
        let paramCount = List.length call.fcparams
        and argCount = List.length call.fcargs in
        if paramCount != argCount then
          TypeError (sprintf "Expected %d params, but used with %d args" paramCount argCount, `Void, `Void)
        else
          List.fold_left2
            (fun prevResult typ arg ->
               match typeOf arg with
                 | TypeOf argType when typ = argType -> prevResult
                 | TypeOf invalidType -> TypeError ("Argument type does not match", invalidType, typ)
                 | TypeError(msg, invalidType, expectedType) ->
                     TypeError ("Argument type is invalid: " ^ msg, invalidType, expectedType)
            )
            (TypeOf (call.fcrettype :> composedType))
            call.fcparams call.fcargs
      end
  | AssignVar (v, expr) -> begin
      match typeOf expr with
        | TypeOf exprType when exprType = v.typ -> TypeOf `Void
        | TypeOf exprType -> TypeError (
            "Cannot assign result of expression to var because types differ",
            exprType, v.typ)
        | _ as typeError -> typeError
    end
  | Return expr -> typeOf expr
  | Label _ -> TypeOf `Void
  | Jump _ -> TypeOf `Void
  | Branch _ -> TypeOf `Void
  | GenericIntrinsic NullptrIntrinsic typ -> TypeOf (`Pointer typ)
  | GenericIntrinsic MallocIntrinsic typ -> TypeOf (`Pointer typ)
  | GenericIntrinsic DerefIntrinsic var ->
      begin
        match var.typ with
          | `Pointer typ -> TypeOf typ
          | nonPointerType ->
              TypeError (
                "deref applied to non-pointer type",
                nonPointerType,
                `Pointer nonPointerType)
      end
  | GenericIntrinsic GetAddrIntrinsic var ->
      begin
        match var.vstorage with
          | MemoryStorage -> TypeOf (`Pointer var.typ)
          | RegisterStorage -> TypeError ("Cannot get address of variable with register storage", var.typ, var.typ)
      end
  | GenericIntrinsic StoreIntrinsic (valueVar, ptrVar) ->
      begin
        match valueVar.typ, ptrVar.typ with
          | valueType, `Pointer ptrTargetType when valueType = ptrTargetType -> TypeOf `Void
          | valueType, invalidPointerType ->
              TypeError ("tried to store value to pointer of mismatching type",
                         invalidPointerType,
                         `Pointer valueType)
      end
  | GenericIntrinsic LoadIntrinsic ptrVar ->
      begin
        match ptrVar.typ with
          | `Pointer t -> TypeOf t
          | _ as invalid -> TypeError ("Expected pointer", invalid, `Pointer `Void)
      end
  | GenericIntrinsic GetFieldPointerIntrinsic (recordVar, fieldName) ->
      begin
        match recordVar.typ with
          | `Pointer `Record components ->
              begin
                match componentType components fieldName with
                  | Some t -> TypeOf (`Pointer t)
                  | None -> TypeError("Component not found", `Void, `Void)
              end
          | _ as invalidType ->
              TypeError ("Expected a record type", invalidType, `Record [])
      end
        
(*   | GenericIntrinsic SetFieldIntrinsic (typ, recordVar, componentName, valueExpr) -> *)
(*       begin *)
(*         match recordVar.typ with *)
(*           | `Record components | `Pointer `Record components -> *)
(*               begin *)
(*                 match componentType components componentName, typeOf valueExpr with *)
(*                   | None, _ -> *)
(*                       TypeError (sprintf "component %s does not exists" componentName, `Void, `Void) *)
(*                   | Some targetType, TypeOf valueType when targetType = valueType -> *)
(*                       TypeOf `Void *)
(*                   | Some targetType, TypeOf wrongType -> *)
(*                       TypeError ("mismatching types in setField", wrongType, targetType) *)
(*                   | _, (TypeError _ as error) -> *)
(*                       error *)
(*               end *)
(*           | _ as wrongType -> *)
(*               TypeError ("setField requires record variable", wrongType, `Record []) *)
(*       end *)
(*   | GenericIntrinsic GetFieldIntrinsic (typ, recordVar, componentName) -> *)
(*       begin *)
(*         match recordVar.typ with *)
(*           | `Record components -> *)
(*               begin *)
(*                 match componentType components componentName with *)
(*                   | Some t when t = typ -> TypeOf t *)
(*                   | Some t -> TypeError (sprintf "internal error, get field claims wrong type", typ, t) *)
(*                   | None -> TypeError (sprintf "component %s does not exist" componentName, `Void, `Void) *)
(*               end *)
(*           | _ as invalidType -> *)
(*               TypeError ("expected record type", invalidType, `Record []) *)
(*       end *)

let rec typeOfTL = function
  | GlobalVar var -> TypeOf var.typ
  | DefineFunc f ->
      match f.impl with
        | None -> TypeOf f.rettype
        | Some impl ->
            match typeOf impl with
              | TypeOf implType when implType = f.rettype -> TypeOf f.rettype
              | TypeOf wrongType -> TypeError (
                  "Function's return type is not equal to it's implementation",
                  f.rettype,
                  wrongType)
              | TypeError _ as e -> e


let rec translateType bindings typeExpr =
  let lookupType bindings name =
    try
      Some (Lang.parseType name)
    with
      | CouldNotParseType _ ->
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
(*         let translateField (name, fieldValueExpr) = *)
(*           name, expr2value `Int fieldValueExpr *)
(*         in *)
(*         let componentValues = List.map translateField components in *)
(*         `RecordVal componentValues *)
        raiseIllegalExpression expr "records not supported"
      end
    | _ -> raiseIllegalExpression expr "unsupported value expression"

(* TODO: use in translateDefineVar and translateGlobalVar + test *)        
let determineStorage typ =
  match typ with
    | `Pointer _ -> MemoryStorage
    | _ -> RegisterStorage
        
let translateDefineVar (translateF :exprTranslateF) (bindings :bindings) expr =
  let transform id name typeExpr valueExpr =
    match translateType bindings typeExpr with
      | Some (#integralType as typ)
      | Some (`Pointer _ as typ) ->
          begin
            let _, simpleform = translateF bindings valueExpr in
            match typ, typeOf (Sequence simpleform) with
              | leftHandType, TypeOf rightHandType when leftHandType = rightHandType ->
                  begin
                    let value = defaultValue typ in
                    let storage = MemoryStorage
(*                       match typ with *)
(*                         | `Pointer _ -> *)
(*                             MemoryStorage *)
(*                         | _ -> *)
(*                             if id = macroVar *)
(*                             then RegisterStorage *)
(*                             else MemoryStorage *)
                    in
                    let var = variable name typ value storage false in
                    Some( addVar bindings var, [ DefineVariable (var, Some (Sequence simpleform)) ] )
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
            Some( addVar bindings var, [ DefineVariable (var, None) ] )
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
        
let translateFuncCall (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = args; } as expr ->
      match lookup bindings name with
        | FuncSymbol func ->
            begin
              let evalArg arg =
                match translateF bindings arg with
                  | _, [expr] -> expr
                  | _, exprList -> Sequence exprList
              in
              let argExprs = List.map evalArg args in
              let funccall = FuncCall {
                fcname = name;
                fcrettype = func.rettype;
                fcparams = List.map snd func.fargs;
                fcargs = argExprs;
              }
              in
              match typeOf funccall with
                | TypeOf _ -> Some( bindings, [funccall] )
                | TypeError (msg, _, _) -> raiseIllegalExpression expr ("Type error: " ^ msg)
            end
        | _ -> None

let translateMacro (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = macroName; args = args; } ->
      match lookup bindings macroName with
        | MacroSymbol macro ->
            let transformedExpr = macro.mtransformFunc args in
            Some (translateF bindings transformedExpr)
        | _ -> None
            
let translateSimpleExpr (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = [] } -> begin
      match lookup bindings name with
        | VarSymbol v ->
            Some (bindings, [Variable v])
        | _ ->
            match string2integralValue name with
              | Some c -> Some( bindings, [Constant c] )
              | None -> None
    end
  | _ -> None

let translateReturn (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [expr] } when id = macroReturn ->
      begin
        match translateF bindings expr with
          | _, [form] -> Some( bindings, [Return form] )
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
            Some (bindings, [AssignVar (v, Sequence rightHandSimpleform)])
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
          | Some t -> Some (addTypedef bindings newTypeName t, [])
      end
  | { id = id; args =
        { id = typeName; args = [] }
        :: componentExprs
    } as expr
      when id = macroTypedef ->
      begin
        let expr2comp = function
          | { id = componentName; args = [typeExpr] } ->
              begin
                match translateType bindings typeExpr with
                  | Some typ -> componentName, typ
                  | None -> raise (CouldNotParseType typeName)
              end
          | _ -> raiseIllegalExpression expr "(type typeName (componentName typeExpression)* ) expected"
        in
        let components = List.map expr2comp componentExprs in
        Some (addTypedef bindings typeName (`Record components), [])
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
                          Sequence (snd (translateF bindings argExpr))
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
                Some (bindings, [FuncCall call])
              end
          | UndefinedSymbol -> raiseIllegalExpression expr (sprintf "%s is undefined" name)
          | _ -> raiseIllegalExpression expr (sprintf "%s is not a record" name)
      end
  | _ -> None

let translateGenericIntrinsic (translateF :exprTranslateF) (bindings :bindings) expr =
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
  in
  let newVarFromExpr binding expr =
    match typeOf (Sequence (snd (translateF binding expr))) with
      | TypeOf newVarType ->
          let newBindings, newVar = getNewVar bindings newVarType in
          let _, simpleforms = translateF bindings expr in
          let rightHandExpr = DefineVariable (newVar, Some (Sequence simpleforms)) in
          (newBindings, newVar, rightHandExpr)
      | _ -> raiseIllegalExpression expr "expression is ill typed"
  in
  let convertSimple typeExpr constructF =
    match translateType bindings typeExpr with
      | Some typ -> Some (bindings, [GenericIntrinsic (constructF typ)])
      | None -> None
  in
  match expr with
    | { id = "nullptr"; args = [typeExpr] } -> convertSimple typeExpr (fun t -> NullptrIntrinsic t)
    | { id = "malloc"; args = [typeExpr] } -> convertSimple typeExpr (fun t -> MallocIntrinsic t)
    | { id = "deref"; args = [rightHandExpr] } ->
        begin
          let newBindings, newVar, rightHandExpr = newVarFromExpr bindings rightHandExpr in
          Some (newBindings, rightHandExpr :: [GenericIntrinsic (DerefIntrinsic newVar)])
        end
    | { id = "ptr"; args = [{ id = varName; args = [] }] } ->
        begin
          match lookup bindings varName with
            | VarSymbol var -> Some (bindings, [GenericIntrinsic (GetAddrIntrinsic var)])
            | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" varName)
        end
    | { id = "store"; args = [
          { id = ptrName; args = [] };
          rightHandExpr
        ] } ->
        begin
          let isPointer = function
            | `Pointer _ -> true
            | _ -> false
          in
          match lookup bindings ptrName with
            | VarSymbol ptrVar when isPointer ptrVar.typ ->
                begin
                  let newBindings, newVar, rightHandSimpleform = newVarFromExpr bindings rightHandExpr
                  in
                  match ptrVar.typ, typeOf rightHandSimpleform with
                    | `Pointer ptrTargetType, TypeOf rightHandType when rightHandType = ptrTargetType ->
                        begin
                          let intrinsic = GenericIntrinsic (StoreIntrinsic (newVar, ptrVar)) in
                          Some (newBindings, rightHandSimpleform :: [intrinsic])
                        end
                    | ptrType, TypeOf rightHandType ->
                        begin
                          raiseIllegalExpression expr
                            (sprintf "first parameter (%s) should have type %s* instead of %s"
                               ptrVar.vname
                               (Lang.typeName ptrType)
                               (Lang.typeName rightHandType))
                        end
                    | _, TypeError (msg, _, _) ->
                        raiseIllegalExpression rightHandExpr msg
                end
            | _ -> raiseIllegalExpression expr "'store ptr var' requires a pointer type var for ptr"
        end
    | { id = "load"; args = [ { id = ptrVarName; args = [] } ] } ->
        begin
          let ptrVar = match lookup bindings ptrVarName with
            | VarSymbol v -> v
            | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" ptrVarName)
          in
          match ptrVar.typ with
            | `Pointer _ ->
                Some( bindings, [GenericIntrinsic (LoadIntrinsic ptrVar)] )
            | _ as invalidType ->
                raiseIllegalExpression expr
                  (sprintf "Expected %s to be of pointer type instead of %s"
                     ptrVarName (typeName invalidType))
        end
    | { id = "fieldptr"; args = [
          { id = recordVarName; args = [] };
          { id = fieldName; args = [] };
        ] } ->
        begin
          let recordVar = match lookup bindings recordVarName with
            | VarSymbol v -> v
            | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" recordVarName)
          in
          Some( bindings, [GenericIntrinsic (GetFieldPointerIntrinsic (recordVar, fieldName))] )
        end
(*     | { id = "setField"; args = [ *)
(*           { id = recordName; args = [] }; *)
(*           { id = componentName; args = [] }; *)
(*           { id = _; args = [] } as valueExpr; *)
(*         ] } -> *)
(*         begin *)
(*           match lookup bindings recordName with *)
(*             | VarSymbol recordVar -> *)
(*                 begin *)
(*                   match recordVar.typ with *)
(*                     | `Record components | `Pointer `Record components -> *)
(*                         begin match componentType components componentName with *)
(*                           | Some componentType -> *)
(*                               begin *)
(*                                 let _, valueSimpleform = translateF bindings valueExpr in *)
(*                                 Some( bindings, [GenericIntrinsic (SetFieldIntrinsic *)
(*                                                                      (componentType, *)
(*                                                                       recordVar, *)
(*                                                                       componentName, *)
(*                                                                       Sequence valueSimpleform))] ) *)
(*                               end *)
(*                           | None -> *)
(*                               raiseIllegalExpression expr *)
(*                                 (sprintf "component with name %s not found" componentName) *)
(*                         end *)
(*                     | _ -> *)
(*                         raiseIllegalExpression expr *)
(*                           ("'setField recordName componentName valueExpr' expects a record variable") *)
(*                 end *)
(*             | _ -> raiseIllegalExpression expr (sprintf "could not find variable %s" recordName) *)
(*         end *)
(*     | { id = "field"; args = [ *)
(*           { id = recordName; args = [] }; *)
(*           { id = componentName; args = [] }; *)
(*         ] } -> *)
(*         begin *)
(*           match lookup bindings recordName with *)
(*             | VarSymbol var -> *)
(*                 begin match var.typ with *)
(*                   | `Record components -> *)
(*                       begin match componentType components componentName with *)
(*                         | Some typ -> *)
(*                             Some( bindings, [GenericIntrinsic (GetFieldIntrinsic(typ, var, componentName))] ) *)
(*                         | None -> raiseIllegalExpression expr *)
(*                             (sprintf "could not find component %s" componentName) *)
(*                       end *)
(*                   | illegalType -> *)
(*                       raiseIllegalExpression expr *)
(*                         (sprintf "%s should be a record but is a %s" *)
(*                            componentName (Lang.typeName illegalType)) *)
(*                 end *)
(*             | _ -> *)
(*                 raiseIllegalExpression expr (sprintf "could not find variable %s" recordName) *)
(*         end *)
    | _ ->
        None

let translateLabel (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [{ id = name; args = [] }] } when id = macroLabel ->
      begin
        Some( addLabel bindings name, [Label { lname = name; }] )
      end
  | _ -> None

let translateBranch (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [{ id = labelName; args = [] }] } when id = macroBranch ->
      begin
        Some( bindings, [Jump { lname = labelName; }] )
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
                Some( bindings, [Branch {
                                   bcondition = var;
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
                | #integralType as typ ->
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
    let expr2param = function
      | { id = typeName; args = [{ id = varName; args = [] }] } ->
          begin
            let typeExpr = { id = typeName; args = [] } in
            match translateType bindings typeExpr with
              | Some typ -> (varName, typ)
              | None -> raiseInvalidType typeExpr
          end
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
      | Some implExpr -> Some (Sequence (snd (translateNested innerBindings implExpr)))
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
                match typeOfTL funcDef with
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
                  Some (newBindings, [funcDecl])
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
]
  
  
