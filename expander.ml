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
  
exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

let raiseInvalidType typeExpr = raise (Typesystems.Zomp.CouldNotParseType (Ast2.expression2string typeExpr))

type typecheckResult =
  | TypeOf of composedType
      (** error message, found type, expected type *)
  | TypeError of string * composedType * composedType
      
let rec typeOf : Lang.expr -> typecheckResult = function
  | `Sequence [] -> TypeOf `Void
  | `Sequence [expr] -> typeOf expr
  | `Sequence (_ :: tail) -> typeOf (`Sequence tail)
  | `DefineVariable (var, expr) -> begin
      match expr with
        | Some expr ->
            begin
              match typeOf expr with
                | TypeOf exprType when exprType = var.typ -> TypeOf `Void
                | TypeOf wrongType -> TypeError (
                    "variable definition requires same type for var and default value", wrongType, var.typ)
                | TypeError _ as typeError -> typeError
            end
        | None -> TypeOf `Void
    end
  | `Variable var -> TypeOf var.typ
  | `Constant value -> TypeOf (Lang.typeOf value)
  | `FuncCall call ->
      begin
        let paramCount = List.length call.fcparams
        and argCount = List.length call.fcargs in
        if paramCount != argCount then
          TypeError (sprintf "Expected %d params, but used with %d args" paramCount argCount, `Void, `Void)
        else
          List.fold_left2
            (fun prevResult typ arg ->
               match typeOf (arg :> Lang.expr) with
                 | TypeOf argType when typ = argType -> prevResult
                 | TypeOf invalidType -> TypeError ("Argument type does not match", invalidType, typ)
                 | TypeError(msg, invalidType, expectedType) ->
                     TypeError ("Argument type is invalid: " ^ msg, invalidType, expectedType)
            )
            (TypeOf (call.fcrettype :> composedType))
            call.fcparams call.fcargs
      end
  | `AssignVar (v, expr) -> begin
      match typeOf expr with
        | TypeOf exprType when exprType = v.typ -> TypeOf `Void
        | TypeOf exprType -> TypeError (
            "Cannot assign result of expression to var because types differ",
            exprType, v.typ)
        | _ as typeError -> typeError
    end
  | `Return expr -> typeOf expr
  | `Label _ -> TypeOf `Void
  | `Jump _ -> TypeOf `Void
  | `Branch _ -> TypeOf `Void
  | `GenericIntrinsic NullptrIntrinsic typ -> TypeOf (`Pointer typ)
  | `GenericIntrinsic MallocIntrinsic (typ, _) -> TypeOf (`Pointer typ)
  | `GenericIntrinsic DerefIntrinsic var ->
      begin
        let `Pointer targetType = var.typ in
        TypeOf targetType
      end
  | `GenericIntrinsic GetAddrIntrinsic var ->
      begin
        match var.vstorage with
          | MemoryStorage -> TypeOf (`Pointer var.typ)
          | RegisterStorage -> TypeError ("Cannot get address of variable with register storage", var.typ, var.typ)
      end
  | `GenericIntrinsic StoreIntrinsic (valueVar, ptrVar) ->
      begin
        match valueVar.typ, ptrVar.typ with
          | valueType, `Pointer ptrTargetType when valueType = ptrTargetType -> TypeOf `Void
          | valueType, invalidPointerType ->
              TypeError ("tried to store value to pointer of mismatching type",
                         (invalidPointerType :> Lang.typ),
                         `Pointer valueType)
      end
  | `GenericIntrinsic LoadIntrinsic ptrVar ->
      begin
        match ptrVar.typ with
          | `Pointer t -> TypeOf t
          | _ as invalid -> TypeError ("Expected pointer", invalid, `Pointer `Void)
      end
  | `GenericIntrinsic GetFieldPointerIntrinsic (recordVar, fieldName) ->
      begin
        let `Pointer `Record components = recordVar.typ in
        match componentType components fieldName with
          | Some t -> TypeOf (`Pointer t)
          | None -> TypeError("Component not found", `Void, `Void)
      end
  | `GenericIntrinsic PtrAddIntrinsic (ptrVar, _) ->
      begin
        TypeOf (ptrVar.typ :> Lang.typ)
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


(* type exprTranslateF = bindings -> expression -> bindings * expr list * typ variable option *)

(* let translatelst (translateF :exprTranslateF) bindings expr = *)
(*   let rec worker lastVar bindings = function *)
(*     | [] -> bindings, [], lastVar *)
(*     | expr :: tail -> *)
(*         let newBindings, sf, var = translateF bindings expr in *)
(*         let resultingBindings, sfuncs, var = worker var newBindings tail in *)
(*         resultingBindings, (sf @ sfuncs), var *)
(*   in *)
(*   worker None bindings expr *)
        
type exprTranslateF = bindings -> expression -> bindings * expr list

let translatelst (translateF :exprTranslateF) bindings expr =
  let rec worker lastVar bindings = function
    | [] -> bindings, []
    | expr :: tail ->
        let newBindings, sf = translateF bindings expr in
        let resultingBindings, sfuncs = worker lastVar newBindings tail in
        resultingBindings, (sf @ sfuncs)
  in
  worker None bindings expr
        
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
            match typ, typeOf (`Sequence simpleform) with
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
    | Some varOrConst -> Some (bindings, [varOrConst])
    | None -> None

(* let rec flattenExpr (translateF :exprTranslateF) bindings expr = *)
(*   match translateF bindings expr with *)
(*     | Some( *)
(* let rec flattenExpr bindings expr = *)
(*   let { id = name; args = args } = expr in *)
(*   match lookup bindings name with *)
(*     | VarSymbol _ | UndefinedSymbol -> *)
(*         begin match expr2VarOrConst bindings expr with *)
(*           | Some varOrConst -> Some (([] :Lang.expr list), varOrConst ) *)
(*           | None -> None *)
(*         end *)
(*     | FuncSymbol func -> *)
(*         begin *)
(*           let rec flattenArgs initCode args = function *)
(*             | [] -> initCode, args *)
(*             | expr :: remainingExprs -> *)
(*                 begin *)
(*                   match flattenExpr bindings expr with *)
(*                     | Some (newInitCode, flatForm) -> flattenArgs (initCode @ newInitCode) (args @ [flatForm]) remainingExprs *)
(*                     | None -> raiseIllegalExpression expr "Could not be translated to flat arg" *)
(*                 end *)
(*           in *)
(*           let initCode, flatArgs = flattenArgs [] [] args in *)
(* (\*           let initCodes, flatArgs = List.split (List.map (flattenExpr bindings) args) in *\) *)
(*           let funccall = `FuncCall { *)
(*             fcname = func.fname; *)
(*             fcrettype = func.rettype; *)
(*             fcargs = flatArgs; *)
(*             fcparams = List.map snd func.fargs; *)
(*           } in *)
(*           Some (initCode, funccall) *)
(*         end *)
(*     | _ -> None *)
        
(* let rec expr2funcall (translateF :exprTranslateF) (bindings :bindings) expr = *)
(*   let { id = name; args = args } = expr in *)
(*   match lookup bindings name with *)
(*     | FuncSymbol func -> *)
(*         begin *)
(* (\*           let flatten expr = *\) *)
(* (\*             let forms, arg = *\) *)
(* (\*               match expr2VarOrConst bindings expr with *\) *)
(* (\*                 | Some (#Lang.flatArgExpr as e) -> [], e *\) *)
(* (\*                 | None -> *\) *)
(* (\*                     begin match expr2funcall translateF bindings expr with *\) *)
(* (\*                       | Some (#Lang.funcCallExpr as e) -> [], e *\) *)
(* (\*                       | None -> raiseIllegalExpression expr "Not allowed as a nested expression" *\) *)
(* (\*                     end *\) *)
(* (\*             in *\) *)
(* (\*             forms, arg *\) *)
(* (\*           in *\) *)
(* (\*           let rec flattenArgs preCode args = function *\) *)
(* (\*             | [] -> preCode, args *\) *)
(* (\*             | hd :: tl -> *\) *)
(* (\*                 let newPreCode, arg = flatten hd in *\) *)
(* (\*                 flattenArgs (newPreCode @ newPreCode) (args @ [arg]) tl *\) *)
(* (\*           in *\) *)
(* (\*           let preCode, argExprs = flattenArgs [] [] args in *\) *)
(* (\*                         let evalArg arg = *\) *)
(* (\*                           match expr2VarOrConst bindings arg with *\) *)
(* (\*                             | Some flatForm -> flatForm *\) *)
(* (\*                             | None -> raiseIllegalExpression arg "Could not be translated to var or constant" *\) *)
(* (\*                         in *\) *)
          
(* (\*           let flatten expr = *\) *)
(* (\*             match translateF bindings expr with *\) *)
(* (\*               | _, [#Lang.flatArgExpr as e] -> e *\) *)
(* (\*               | _ -> raiseIllegalExpression expr "Could not be translated to flat arg" *\) *)
(* (\*           in *\) *)
(* (\*           let evalArg = flatten in *\) *)
(* (\*           let argExprs = List.map evalArg args in *\) *)
(*           let rec flattenArgs initCode args = function *)
(*             | [] -> initCode, args *)
(*             | expr :: remainingExprs -> *)
(*                 begin *)
(*                   match flattenExpr bindings expr with *)
(*                     | Some (newInitCode, flatForm) -> flattenArgs (initCode @ newInitCode) (args @ [flatForm]) remainingExprs *)
(*                     | None -> raiseIllegalExpression expr "Could not be translated to flat arg" *)
(*                 end *)
(*           in *)
(*           let initCode, argExprs = flattenArgs [] [] args in *)
(*           let funccall = `FuncCall { *)
(*             fcname = name; *)
(*             fcrettype = func.rettype; *)
(*             fcparams = List.map snd func.fargs; *)
(*             fcargs = argExprs; *)
(*           } *)
(*           in *)
(*           match typeOf (funccall :> Lang.expr) with *)
(*             | TypeOf _ -> Some funccall *)
(*             | TypeError (msg, _, _) -> raiseIllegalExpression expr ("Type error: " ^ msg) *)
(*         end *)
(*     | _ -> *)
(*         None *)
            
(* let translateFuncCall (translateF :exprTranslateF) (bindings :bindings) expr = *)
(*   match expr2funcall translateF bindings expr with *)
(*       | Some funcCall -> Some( bindings, [funcCall] ) *)
(*       | None -> None *)

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
              match typeOf funccall with
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
              | Failure msg -> raiseIllegalExpression expr ("Could not expand macro: " ^ msg)
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
            Some (bindings, [`AssignVar (v, `Sequence rightHandSimpleform)])
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
                Some (bindings, [`FuncCall call])
              end
          | UndefinedSymbol -> raiseIllegalExpression expr (sprintf "%s is undefined" name)
          | _ -> raiseIllegalExpression expr (sprintf "%s is not a record" name)
      end
  | _ -> None

let translateGenericIntrinsic (translateF :exprTranslateF) (bindings :bindings) expr =
  let intFlatForm countExpr errorMsg =
    match translateF bindings countExpr with
      | _, [`Constant IntVal _ as c] -> c
      | _, [`Variable var] ->
          begin match var.typ with
            | `Int -> `Variable { var with typ = `Int }
            | _ -> raiseIllegalExpression countExpr errorMsg
          end
      | _, _ -> raiseIllegalExpression countExpr errorMsg
  in
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
    match typeOf (`Sequence (snd (translateF binding expr))) with
      | TypeOf newVarType ->
          let newBindings, newVar = getNewVar bindings newVarType in
          let _, simpleforms = translateF bindings expr in
          let rightHandExpr = `DefineVariable (newVar, Some (`Sequence simpleforms)) in
          (newBindings, newVar, rightHandExpr, typeOf (`Sequence simpleforms))
      | _ -> raiseIllegalExpression expr "expression is ill typed"
  in
  let convertSimple typeExpr constructF =
    match translateType bindings typeExpr with
      | Some typ -> Some (bindings, [`GenericIntrinsic (constructF typ)])
      | None -> None
  in
  let buildStoreInstruction ptrName rightHandExpr =
    let isPointer = function
      | `Pointer _ -> true
      | _ -> false
    in
    match lookup bindings ptrName with
      | VarSymbol ptrVar when isPointer ptrVar.typ ->
          begin
            let newBindings, newVar, rightHandSimpleform, rightHandType = newVarFromExpr bindings rightHandExpr
            in
            match ptrVar.typ, rightHandType with
              | `Pointer ptrTargetType, TypeOf rightHandType when rightHandType = ptrTargetType ->
                  begin
                    let ptrVar = { ptrVar with typ = `Pointer ptrTargetType } in
                    let intrinsic = `GenericIntrinsic (StoreIntrinsic (newVar, ptrVar)) in
                    Some (newBindings, rightHandSimpleform :: [intrinsic])
                  end
              | ptrType, TypeOf rightHandType ->
                  begin
                    raiseIllegalExpression expr
                      (sprintf "first parameter (%s) should have type %s instead of %s"
                         ptrVar.vname
                         (Lang.typeName rightHandType)
                         (Lang.typeName ptrType))
                  end
              | _, TypeError (msg, _, _) ->
                  raiseIllegalExpression rightHandExpr msg
          end
      | _ -> raiseIllegalExpression expr "'store ptr var' requires a pointer type var for ptr"
  in
  let buildLoadInstruction ptrVarName =
    let ptrVar = match lookup bindings ptrVarName with
      | VarSymbol v -> v
      | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" ptrVarName)
    in
    match ptrVar.typ with
      | `Pointer _ ->
          Some( bindings, [`GenericIntrinsic (LoadIntrinsic ptrVar)] )
      | _ as invalidType ->
          raiseIllegalExpression expr
            (sprintf "Expected %s to be of pointer type instead of %s"
               ptrVarName (typeName invalidType))
  in
  match expr with
    | { id = "nullptr"; args = [typeExpr] } -> convertSimple typeExpr (fun t -> NullptrIntrinsic t)
    | { id = "malloc"; args = [typeExpr] } ->
        convertSimple typeExpr (fun t -> MallocIntrinsic (t, `Constant (IntVal 1)))
    | { id = "malloc"; args = [typeExpr; countExpr] } ->
        let errorMsg = "malloc expects an int constant/var as 2nd argument" in
        convertSimple typeExpr (fun t -> MallocIntrinsic (t, intFlatForm countExpr errorMsg))
    | { id = "deref"; args = [rightHandExpr] } ->
        begin
          let newBindings, newVar, rightHandExpr, _ = newVarFromExpr bindings rightHandExpr in
          let newVar = match newVar.typ with
            | `Pointer _ as typ -> { newVar with typ = typ }
            | _ -> raiseIllegalExpression expr "Can only deref pointers"
          in
          Some (newBindings, rightHandExpr :: [`GenericIntrinsic (DerefIntrinsic newVar)])
        end
    | { id = "ptr"; args = [{ id = varName; args = [] }] } ->
        begin
          match lookup bindings varName with
            | VarSymbol var -> Some (bindings, [`GenericIntrinsic (GetAddrIntrinsic var)])
            | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" varName)
        end
    | { id = "store"; args = [{ id = ptrName; args = [] }; rightHandExpr] } ->
        begin
          buildStoreInstruction ptrName rightHandExpr
        end
    | { id = "load"; args = [ { id = ptrVarName; args = [] } ] } ->
        begin
          buildLoadInstruction ptrVarName
        end
    | { id = "ptr.add"; args = [
          { id = ptrVarName; args = [] };
          indexExpr
        ] } ->
        begin
          let errorMsg = "Expected a constant/var as second argument" in
          let indexForm = intFlatForm indexExpr errorMsg in
          match lookup bindings ptrVarName with
            | VarSymbol ({ typ = `Pointer _;  } as ptrVar) ->
                Some( bindings, [`GenericIntrinsic (PtrAddIntrinsic (ptrVar, indexForm))] )
            | VarSymbol _ ->
                raiseIllegalExpression expr "Expected first argument to be a pointer"
            | _ ->
                raiseIllegalExpression expr (sprintf "Could not find variable %s" ptrVarName)
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
          let recordVar = match recordVar.typ with
            | `Pointer `Record _ as typ -> { recordVar with typ = typ }
            | _ -> raiseIllegalExpression expr (sprintf "Expected %s to be a pointer to a record" recordVarName)
          in
          Some( bindings, [`GenericIntrinsic (GetFieldPointerIntrinsic (recordVar, fieldName))] )
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
  translateDefineMacro;
  translateMacro;
]
  
  
