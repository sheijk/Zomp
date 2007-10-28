
open Printf
open Lang

type typecheckResult =
  | TypeOf of composedType
      (** error message, found type, expected type *)
  | TypeError of string * composedType * composedType

let rec equalTypes bindings ltype rtype =
  let rec lookupType name = 
    match Bindings.lookup bindings name with
      | Bindings.TypedefSymbol typ -> Some typ
      | _ -> None
  in
  match ltype, rtype with
    | `TypeRef lname, `TypeRef rname -> lname = rname
    | `TypeRef _, _ -> equalTypes bindings rtype ltype
    | _, `TypeRef rname ->
        begin match lookupType rname with
          | Some t -> equalTypes bindings ltype t
          | None -> false
        end
    | `Record l, `Record r ->
        let rec componentsEqual = function
          | [], [] -> true
          | (leftName, leftType) :: lrem, (rightName, rightType) :: rrem
              when leftName = rightName && equalTypes bindings leftType rightType ->
              componentsEqual (lrem, rrem)
          | _, _ ->
              false
        in
        componentsEqual (l, r)
    | `Pointer l, `Pointer r when equalTypes bindings l r -> true
    | _, _ -> ltype = rtype
      
let rec typeCheck bindings : form -> typecheckResult =
  let expectPointerType form =
    match typeCheck bindings form with
      | TypeOf (`Pointer _ as pointerType) -> TypeOf pointerType
      | TypeOf invalidType -> TypeError ("Expected pointer type", invalidType, `Pointer `Void)
      | _ as e -> e
  in
  let expectType form expectType =
    match typeCheck bindings form with
      | TypeOf typ when typ = expectType -> TypeOf typ
      | TypeOf invalidType -> TypeError ("Invalid type in 2nd parameter", invalidType, `Int)
      | _ as e -> e
  in
  let (>>) l r =
    match l with
      | TypeError _ as e -> e
      | TypeOf _ -> r
  in
  function
    | `Sequence [] -> TypeOf `Void
    | `Sequence [expr] -> typeCheck bindings expr
    | `Sequence (_ :: tail) -> typeCheck bindings (`Sequence tail)
    | `DefineVariable (var, expr) -> begin
        match expr with
          | Some expr ->
              begin
                match typeCheck bindings expr with
                  | TypeOf exprType when equalTypes bindings exprType var.typ -> TypeOf `Void
                  | TypeOf wrongType -> TypeError (
                      "variable definition requires same type for var and default value", wrongType, var.typ)
                  | TypeError _ as typeError -> typeError
              end
          | None -> TypeOf `Void
      end
    | `Variable var -> TypeOf var.typ
    | `Constant value -> TypeOf (typeOf value)
    | `FuncCall call ->
        begin
          let paramCount = List.length call.fcparams
          and argCount = List.length call.fcargs in
          if paramCount != argCount then
            TypeError (sprintf "Expected %d params, but used with %d args" paramCount argCount, `Void, `Void)
          else
            List.fold_left2
              (fun prevResult typ arg ->
                 match typeCheck bindings (arg :> form) with
                   | TypeOf argType when equalTypes bindings typ argType -> prevResult
                   | TypeOf invalidType -> TypeError ("Argument type does not match", invalidType, typ)
                   | TypeError(msg, invalidType, expectedType) ->
                       TypeError ("Argument type is invalid: " ^ msg, invalidType, expectedType)
              )
              (TypeOf (call.fcrettype :> composedType))
              call.fcparams call.fcargs
        end
    | `AssignVar (v, expr) -> begin
        match typeCheck bindings expr with
          | TypeOf exprType when equalTypes bindings exprType v.typ -> TypeOf `Void
          | TypeOf exprType -> TypeError (
              "Cannot assign result of expression to var because types differ",
              exprType, v.typ)
          | _ as typeError -> typeError
      end
    | `Return expr -> typeCheck bindings expr
    | `Label _ -> TypeOf `Void
    | `Jump _ -> TypeOf `Void
    | `Branch _ -> TypeOf `Void
    | `NullptrIntrinsic typ -> TypeOf (`Pointer typ)
    | `MallocIntrinsic (typ, _) -> TypeOf (`Pointer typ)
    | `GetAddrIntrinsic var ->
        begin
          match var.vstorage with
            | MemoryStorage ->
                TypeOf (`Pointer var.typ)
            | RegisterStorage ->
                TypeError ("Cannot get address of variable with register storage",
                           var.typ, var.typ)
        end
    | `StoreIntrinsic (ptrExpr, valueExpr) ->
        begin
          match typeCheck bindings ptrExpr, typeCheck bindings valueExpr with
            | TypeOf `Pointer ptrTargetType, TypeOf valueType
                when equalTypes bindings ptrTargetType valueType
                  ->
                TypeOf `Void
            | TypeOf (#typ as invalidPointerType), TypeOf valueType ->
                TypeError ("tried to store value to pointer of mismatching type",
                           invalidPointerType,
                           `Pointer valueType)
            | (_ as l), (_ as r) ->
                l >> r
        end
    | `LoadIntrinsic expr ->
        begin
          match typeCheck bindings expr with
            | TypeOf `Pointer targetType -> TypeOf targetType
            | TypeOf invalid -> TypeError ("Expected pointer", invalid, `Pointer `Void) 
            | TypeError _ as t -> t
        end
    | `GetFieldPointerIntrinsic (recordForm, fieldName) ->
        begin
          match typeCheck bindings recordForm with
            | TypeOf `Pointer `Record components ->
                begin
                  match componentType components fieldName with
                    | Some t -> TypeOf (`Pointer t)
                    | None -> TypeError("Component not found", `Void, `Void)
                end
            | TypeOf nonPtrToRecord ->
                TypeError
                  ("Expected pointer to record type", nonPtrToRecord, `Pointer (`Record []))
            | _ as typeError ->
                typeError
        end
    | `PtrAddIntrinsic (ptrExpr, offsetExpr) ->
        begin
          expectType offsetExpr `Int
          >> expectPointerType ptrExpr
        end
    | `CastIntrinsic (targetType, valueExpr) ->
        begin
          TypeOf targetType
        end
        
  
let rec typeCheckTL bindings = function
  | `GlobalVar var -> TypeOf var.typ
  | `DefineFunc f ->
      match f.impl with
        | None -> TypeOf f.rettype
        | Some impl ->
            match typeCheck bindings impl with
              | TypeOf implType when implType = f.rettype ->
                  TypeOf f.rettype
              | TypeOf wrongType ->
                  TypeError (
                    "Function's return type is not equal to it's implementation",
                    f.rettype,
                    wrongType)
              | TypeError _ as e ->
                  e
  
  
let typeOfForm ~onError bindings form =
  match typeCheck bindings form with
    | TypeOf typ -> typ
    | TypeError (msg, found, expected) ->
        onError ~msg ~found ~expected
          
