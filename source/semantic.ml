
open Printf
open Lang
open Types
open Common

type formOrAst = Form of Lang.form | Ast of Ast2.t
type typeRequirement = [
| `Any of string
| typ ]

let typeRequirementToString = function
  | `Any description -> sprintf "<%s>" description
  | #typ as t -> typeName t

type typecheckResult =
  | TypeOf of typ
      (** faulty expression, error message, found type, expected type *)
  | TypeError of formOrAst * string * typ * typeRequirement

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
        l.rname = r.rname &&
            componentsEqual (l.fields, r.fields)
    | `Pointer l, `Pointer r when equalTypes bindings l r -> true
    | _, _ -> ltype = rtype

let typeCheckFuncCall typeCheck bindings funcallForm call =
  let paramCount = List.length call.fcparams
  and argCount = List.length call.fcargs in
  if (if call.fcvarargs then argCount < paramCount else argCount != paramCount) then
    TypeError (
      Form funcallForm,
      sprintf "expected %d params, but used with %d args" paramCount argCount,
      `Void,
      `Void)
  else
    let errors = ref [] in
    let addError formOrAst msg found expected =
      errors := (formOrAst, msg, found, expected) :: !errors
    in

    let parametricTypeBinding = ref (None : typ option) in

    let checkArgument argNum paramType argForm =
      match typeCheck bindings argForm with
        | TypeOf argType ->
            begin match paramType with
              | `Pointer `TypeParam ->
                  begin match !parametricTypeBinding, argType with
                    | None, `Pointer t ->
                        parametricTypeBinding := Some t
                    | None, wrongType ->
                        addError
                          (Form funcallForm)
                          (sprintf "argument %d has invalid type" argNum)
                          wrongType
                          (paramType :> typeRequirement)
                    | Some t1, `Pointer t2 when equalTypes bindings t1 t2 ->
                        ()
                    | Some t, invalidType ->
                        addError
                          (Form funcallForm)
                          "parametric type is already bound"
                          argType
                          ((`Pointer t) :> typeRequirement)
                  (* if !parametricTypeBinding == None then *)
                  (*   parametricTypeBinding := argType *)
                  end
              | `Pointer `ParametricType _
              | `ParametricType _ ->
                  addError
                    (Form funcallForm)
                    "parametric function parameters not supported, yet"
                    argType
                    (`TypeParam :> typeRequirement)
              | _ ->
                  if not (equalTypes bindings paramType argType) then
                    addError
                      (Form argForm)
                      (sprintf "argument %d has invalid type" argNum)
                      argType
                      (paramType :> typeRequirement)
            end
        | TypeError (formOrAst, msg, found, expected) ->
            errors := (formOrAst, msg, found, expected) :: !errors
    in

    let rec check argNum = function
      | [], [] -> ()
      | (param :: remParams), (arg :: remArgs) ->
          checkArgument argNum param arg;
          check (argNum + 1) (remParams, remArgs)
      | [], _ | _, [] ->
          failwith "internal error, non-matching arg/param lists"
    in

    check 0 (call.fcparams, (fst (splitAfter paramCount call.fcargs)));
    match !errors with
      | [] -> TypeOf call.fcrettype
      | (errorForm, msg, found, expected) :: _ ->
          TypeError (
            errorForm,
            msg,
            found,
            expected)

let rec typeCheck bindings (form :Lang.form) : typecheckResult =
  let result =
    let expectPointerType form =
      match typeCheck bindings form with
        | TypeOf (`Pointer _ as pointerType) ->
            TypeOf pointerType
        | TypeOf invalidType ->
            TypeError (Form form,
                       "expected pointer type",
                       invalidType,
                       `Pointer `Void)
        | _ as e -> e
    in
    let expectType form expectType =
      match typeCheck bindings form with
        | TypeOf typ when typ = expectType -> TypeOf typ
        | TypeOf invalidType ->
            TypeError (
              Form form, "invalid type in 2nd parameter", invalidType, `Int32)
        | _ as e -> e
    in
    let (>>) l r =
      match l with
        | TypeError _ as e -> e
        | TypeOf _ -> r
    in
    match form with
      | `Sequence (_, []) -> TypeOf `Void
      | `Sequence (_, [expr]) -> typeCheck bindings expr
      | `Sequence (_, (_ :: tail)) -> typeCheck bindings (Lang.sequence tail)
      | `DefineVariable (_, var, maybeForm) -> begin
          match maybeForm with
            | Some form ->
                begin
                  match typeCheck bindings form with
                    | TypeOf exprType when equalTypes bindings exprType var.typ ->
                        TypeOf `Void
                    | TypeOf wrongType ->
                        TypeError (
                          Form form,
                          "variable definition requires same type for var " ^
                            "and default value",
                          wrongType,
                          (var.typ :> typeRequirement))
                    | TypeError _ as typeError -> typeError
                end
            | None -> TypeOf `Void
        end
      | `Variable (_, var) -> TypeOf var.typ
      | `Constant (_, value) -> TypeOf (typeOf value)
      | `FuncCall (_, call) as funcallForm ->
          typeCheckFuncCall typeCheck bindings funcallForm call
      | `AssignVar (_, v, expr) as assignVarForm -> begin
          match typeCheck bindings expr with
            | TypeOf exprType when equalTypes bindings exprType v.typ -> TypeOf `Void
            | TypeOf exprType -> TypeError (
                Form assignVarForm,
                "cannot assign result of expression to var because types differ",
                exprType,
                (v.typ :> typeRequirement))
            | _ as typeError -> typeError
        end
      | `Return (_, expr) ->
         typeCheck bindings expr >> TypeOf `Void
      | `Label _ -> TypeOf `Void
      | `Jump _ -> TypeOf `Void
      | `Branch _ -> TypeOf `Void
      | `SizeofIntrinsic (_, typ) ->
         (match Bindings.lookup bindings "size_t" with
            | Bindings.TypedefSymbol typ -> TypeOf typ
            | _ -> failwith "typeCheck")
      | `GetAddrIntrinsic (_, var) as getaddrForm ->
          begin
            match var.vstorage with
              | MemoryStorage ->
                  TypeOf (`Pointer var.typ)
              | RegisterStorage ->
                  TypeError (
                    Form getaddrForm,
                    "cannot get address of variable with register storage",
                    var.typ,
                    `Any "pointer")
          end
      | `StoreIntrinsic (_, ptrExpr, valueExpr) as storeForm ->
          begin
            match typeCheck bindings ptrExpr, typeCheck bindings valueExpr with
              | TypeOf `Pointer ptrTargetType, TypeOf valueType
                  when equalTypes bindings ptrTargetType valueType ->
                  TypeOf `Void
              | TypeOf (#typ as invalidPointerType), TypeOf valueType ->
                  TypeError (
                    Form storeForm,
                    "tried to store value to pointer of mismatching type",
                    invalidPointerType,
                    `Pointer valueType)
              | (_ as l), (_ as r) ->
                  l >> r
          end
      | `LoadIntrinsic (_, expr) as loadform ->
          begin
            match typeCheck bindings expr with
              | TypeOf `Pointer targetType ->
                  TypeOf targetType
              | TypeOf invalid ->
                  TypeError (
                    Form loadform, "expected pointer", invalid, `Any "pointer")
              | TypeError _ as t -> t
          end
      | `GetFieldPointerIntrinsic (_, recordForm, fieldName) as getfieldForm ->
          begin
            match typeCheck bindings recordForm with
              | TypeOf `Pointer `Record record ->
                  begin
                    match componentType record.fields fieldName with
                      | Some t ->
                          TypeOf (`Pointer t)
                      | None ->
                          TypeError(
                            Form getfieldForm,
                            "component not found",
                            `Void, `Void)
                  end
              | TypeOf nonPtrToRecord ->
                  TypeError (
                    Form getfieldForm,
                    "expected pointer to record type",
                    nonPtrToRecord, `Any "pointer to record")
              | _ as typeError ->
                  typeError
          end
      | `PtrAddIntrinsic (_, ptrExpr, offsetExpr) ->
          begin
            expectType offsetExpr `Int32
            >> expectPointerType ptrExpr
          end
      | `PtrDiffIntrinsic (_, lhsExpr, rhsExpr) ->
        begin
          let lhsTypeR = typeCheck bindings lhsExpr in
          let rhsTypeR = typeCheck bindings rhsExpr in
          match lhsTypeR, rhsTypeR with
            | (TypeOf (`Pointer _ as lhsType)), (TypeOf (`Pointer _ as rhsType)) ->
              if (equalTypes bindings lhsType rhsType) then
                TypeOf `Int32
              else
                TypeError (Form form, "expected pointers to same type", rhsType, lhsType)
            | _, _ ->
              TypeError (Form form, "expected two pointers", `Void, `Void)
        end
      | `CastIntrinsic (_, targetType, valueExpr) ->
          TypeOf targetType
      | `EmbeddedComment _ ->
          TypeOf `Void
  in
  let rec removeTypeRefsParam : typ parameterizableType -> typ parameterizableType = function
    | `Pointer t ->
        `Pointer (removeTypeRefs bindings t)
    | `Record _ as t ->
        t
    (** breaks stuff, not sure why. might uncover another error *)
    (* | `Record rt -> *)
    (*     `Record { rt with fields = List.map (map2nd (removeTypeRefs bindings)) rt.fields } *)
  and removeTypeRefs bindings = function
    | `TypeRef name as typeref ->
        begin match Bindings.lookup bindings name with
          | Bindings.TypedefSymbol t -> t
          | _ -> typeref
        end
    | `TypeParam
    | `ErrorType _
    | #integralType as t ->
        t
    | `ParametricType t ->
        `ParametricType (removeTypeRefsParam t)
    | `Pointer _ | `Record _ as t ->
        (removeTypeRefsParam t :> typ)
    | `Array (memberType, size) ->
        `Array (removeTypeRefs bindings memberType, size)
    | `Function ft ->
        `Function { returnType = removeTypeRefs bindings ft.returnType;
                    argTypes = List.map (removeTypeRefs bindings) ft.argTypes }
  in
  match result with
    | TypeOf t ->
        TypeOf (removeTypeRefs bindings t)
    | _ ->
        result

let rec collectVars (form :Lang.form) =
  let returnTransformed form f =
    let newForm, newVars = collectVars form in
    f newForm, newVars
  in
  let returnTransformed2 form1 form2 f =
    let newForm1, vars1 = collectVars form1 in
    let newForm2, vars2 = collectVars form2 in
    f newForm1 newForm2, vars1 @ vars2
  in
  match form with
    | `Variable _
    | `Constant _
    | `SizeofIntrinsic _
    | `GetAddrIntrinsic _
    | `Jump _
    | `Label _
    | `EmbeddedComment _
        as simple ->
        simple, []

    | `StoreIntrinsic (info, ptrForm, valueForm) ->
        returnTransformed2 ptrForm valueForm
          (fun newPtrForm newValueForm -> `StoreIntrinsic (info, newPtrForm, newValueForm))

    | `LoadIntrinsic (info, ptrForm) ->
        returnTransformed ptrForm (fun f -> `LoadIntrinsic (info, f))

    | `PtrAddIntrinsic (info, ptrForm, offsetForm) ->
        returnTransformed2 ptrForm offsetForm (fun p o -> `PtrAddIntrinsic (info, p, o))

    | `PtrDiffIntrinsic (info, lhsForm, rhsForm) ->
      returnTransformed2 lhsForm rhsForm (fun l r -> `PtrDiffIntrinsic (info, l, r) )

    | `GetFieldPointerIntrinsic (info, recordForm, fieldName) ->
        returnTransformed recordForm (fun r -> `GetFieldPointerIntrinsic (info, r, fieldName))

    | `CastIntrinsic (info, targetType, valueForm) ->
        returnTransformed valueForm (fun v -> `CastIntrinsic (info, targetType, v))

    | `Sequence (info, forms) ->
        let formsAndVars = List.map collectVars forms in
        let forms, vars = List.split formsAndVars in
        `Sequence (info, forms), List.flatten vars

    | `DefineVariable (info, var, valueFormOption) ->
        begin match var.typ with
          | `Void -> `Sequence (info, []), []
          | _ ->
              let assignForm =
                match valueFormOption with
                  | Some valueForm -> `AssignVar (info, var, valueForm)
                  | None -> `Sequence (info, [])
              in
              assignForm, [var]
        end

    | `FuncCall (info, call) ->
        let argFormsAndVars = List.map collectVars call.fcargs in
        let argForms, vars = List.split argFormsAndVars in
        `FuncCall (info, Lang.changeFuncCallArgs call argForms), List.flatten vars

    | `AssignVar (info, var, valueForm) ->
        returnTransformed valueForm (fun f -> `AssignVar(info, var, f))

    | `Return (info, form) ->
        returnTransformed form (fun f -> `Return (info, f))

    | `Branch (info, branch) ->
        `Branch (info, branch), []

let moveLocalVarsToEntryBlock implForm =
  let info = Lang.info implForm in
  let formWithoutVars, vars = collectVars implForm in
  let varDefs = List.map (fun var -> `DefineVariable (info, var, None)) vars
  in
  let forms =
    match formWithoutVars with
      | `Sequence (_, forms) -> forms
      | other -> [other]
  in
  varDefs @ forms

let test_moveLocalVarsToEntryBlock () =
  let fakeInfo = Lang.formInfo Basics.fakeLocation in
  let identityTestCases : form list = [
    `Sequence (fakeInfo, [`Return (fakeInfo, `Constant (fakeInfo, VoidVal))]);
    `Sequence (fakeInfo, [`Return (fakeInfo, `Constant (fakeInfo, Int32Val 100l))]);
    `Sequence (fakeInfo, [
      `Constant (fakeInfo, Int32Val 4l);
      `Return (fakeInfo, `Constant (fakeInfo, Int32Val 1000l))
    ]);
  ] in
  let testCases =
    List.map (fun form -> (form, [form])) identityTestCases
  in
  let results =
    List.map
      (fun (input, expected) ->
         let result = moveLocalVarsToEntryBlock input in
         if result <> expected then `Error (input, expected, result)
         else `Ok)
      testCases
  in
  let rec filterErrors acc = function
    | `Ok :: rem -> filterErrors acc rem
    | (`Error (_ as e)) :: rem -> filterErrors (e::acc) rem
    | [] -> acc
  in
  let errors = filterErrors [] results in
  let formsToString forms =
    let formStrings = List.map Lang.formToString forms in
    Common.combine "\n" formStrings
  in
  List.iter
    (fun (input, expected, result) ->
       printf "Error!\n Input %s \n Result %s \n Expected %s \n\n"
         (Lang.formToString input)
         (formsToString result)
         (formsToString expected))
    errors

let lookupTypeInBindings bindings typeName =
  match Bindings.lookup bindings typeName with
    | Bindings.TypedefSymbol typ -> `Found typ
    | _ -> `NotFound

let typesEquivalent bindings type1 type2 =
  try
    let canonicType = canonicType (lookupTypeInBindings bindings) in
    let canonic1, canonic2 = canonicType type1, canonicType type2 in
    canonic1 = canonic2
  with
    | Failure _ -> false

let raiseInvalidAst (loc :Basics.location) msg = failwith msg

let rec typeCheckTL bindings = function
  | `GlobalVar var -> TypeOf var.typ
  | `DefineFunc f ->
     TypeOf f.rettype

let rec typeOfForm sizeT (form :Lang.form) =
  match form with
    | `CastIntrinsic (_, typ, _) -> typ
    | `GetAddrIntrinsic (_, var) -> `Pointer var.typ
    | `SizeofIntrinsic (_, typ) ->
       sizeT
    | `GetFieldPointerIntrinsic (info, structForm, fieldName) ->
       begin
         match typeOfForm sizeT structForm with
           | `Pointer `Record record ->
              `Pointer (List.assoc fieldName record.fields)
           | invalidType ->
              raiseInvalidAst (Lang.flocation form) @@ sprintf "invalid type %s in fieldptr" (typeName invalidType)
       end
    | `LoadIntrinsic (info, ptrForm) ->
       begin match typeOfForm sizeT ptrForm with
               | `Pointer typ -> typ
               | invalidType ->
                  raiseInvalidAst (Lang.flocation form) @@ sprintf "invalid type %s in load" (typeName invalidType)
       end
    | `PtrAddIntrinsic (info, ptrForm, _) -> typeOfForm sizeT ptrForm
    | `PtrDiffIntrinsic (info, _, _) -> `Int32
    | `StoreIntrinsic (info, _, valueForm) -> typeOfForm sizeT valueForm

    | `DefineVariable _
    | `AssignVar _
    | `EmbeddedComment _
    | `Branch _
    | `Jump _
    | `Label _
      ->
       `Void

    | `Constant (_, value) -> Types.typeOf value
    | `Variable (_, var) -> var.typ
    | `FuncCall (_, call) -> call.fcrettype
    | `Return (_, form) -> `Void
    | `Sequence (_, []) -> `Void
    | `Sequence (_, args) ->
       typeOfForm sizeT @@ listLast args

(** Types of forms that are valid as an argument in three address form. *)
type argumentForm = [
| `Constant of formInfo * Types.value
| `Variable of formInfo * (typ variable)
]

let voidForm info =
  `Constant (info, VoidVal)

let splitBasicBlocks sizeT functionLoc returnType (form :Lang.form) : int * ((string * Lang.form list) list) =
  let localVariableCount = ref 0 in
  let registerVariable var =
    Lang.setLocalIndex var !localVariableCount;
    incr localVariableCount;
  in

  let addInstruction, newBasicBlock, basicBlocks =
    let revInstructions = ref ([] : Lang.form list) in
    let blocks = ref [] in
    let currentName = ref "" in

    let addInstruction form =
      revInstructions := form :: !revInstructions
    in
    let newBasicBlock name =
      blocks := (!currentName, List.rev !revInstructions) :: !blocks;
      currentName := name;
      revInstructions := [];
    in
    let basicBlocks () =
      if !revInstructions <> [] || !blocks = [] || !currentName <> "" then
        newBasicBlock "unreachable-end-of-function";
      !blocks
    in
    addInstruction, newBasicBlock, basicBlocks
  in

  let returnResultInVar form =
    let info = Lang.info form in
    let typ = typeOfForm sizeT form in
    let varForm =
      match typ with
        | `Void ->
           addInstruction form;
           voidForm info
        | typ ->
           let var =
             Lang.variable
               ~name:"temp"
               ~typ
               ~storage:RegisterStorage
               ~global:false
               ~location:None
           in
           registerVariable var;
           let defvar = `DefineVariable (info, var, Some form) in
           addInstruction (defvar :> Lang.form);
           `Variable (info, var)
    in
    varForm
  in

  let rec visitForm form :argumentForm =
    match form with
      | #argumentForm as form ->
         form

      | `DefineVariable (info, var, initialValue) ->
         registerVariable var;

         (match initialValue with
            | Some value ->
               let value = visitForm value in
               addInstruction (`DefineVariable (info, var, None));
               addInstruction (`AssignVar (info, var, (value :> Lang.form)));
            | None ->
               addInstruction form);
         voidForm info

      | `AssignVar (info, var, value) ->
         let flatValue = (visitForm value :> Lang.form) in
         addInstruction (`AssignVar (info, var, flatValue));
         voidForm info

      | `Sequence (info, forms) ->
         begin
           match lastElement (List.map visitForm forms) with
             | Some lastForm -> lastForm
             | None -> voidForm info
         end

      | `FuncCall (info, call) ->
         let flatArgs = List.map visitForm call.fcargs in
         let flatCall =
           Lang.funcCall
             ~name:call.fcname
             ~rettype:call.fcrettype
             ~params:call.fcparams
             ~args:(flatArgs :> Lang.form list)
             ~ptr:call.fcptr
             ~varargs:call.fcvarargs
         in
         returnResultInVar @@ `FuncCall (info, flatCall)

      | `Return (info, valueForm) ->
         let value = visitForm valueForm in
         addInstruction (`Return (info, (value :> Lang.form)));
         newBasicBlock "";
         voidForm info

      | `Branch (info, _) as branch ->
         addInstruction branch;
         newBasicBlock "";
         voidForm info

      | `Jump (info, _) as jump ->
         addInstruction jump;
         newBasicBlock "";
         voidForm info

      | `Label (info, label) ->
         (** labels are not added! *)
         newBasicBlock label.lname;
         voidForm info

      | `SizeofIntrinsic (_, _) ->
         returnResultInVar form

      | `LoadIntrinsic (info, ptrForm) ->
         let flatPtr = (visitForm ptrForm :> Lang.form) in
         returnResultInVar @@ `LoadIntrinsic (info, flatPtr)

      | `StoreIntrinsic (info, ptrForm, valueForm) ->
         let flatPtr = (visitForm ptrForm :> Lang.form) in
         let flatValue = (visitForm valueForm :> Lang.form) in
         addInstruction @@ `StoreIntrinsic (info, flatPtr, flatValue);
         voidForm info

      | `PtrAddIntrinsic (info, ptrLhs, ptrRhs) ->
         let flatPtrLhs = (visitForm ptrLhs :> Lang.form) in
         let flatPtrRhs = (visitForm ptrRhs :> Lang.form) in
         returnResultInVar @@ `PtrAddIntrinsic (info, flatPtrLhs, flatPtrRhs)

      | `PtrDiffIntrinsic (info, ptrLhs, ptrRhs) ->
         let flatPtrLhs = (visitForm ptrLhs :> Lang.form) in
         let flatPtrRhs = (visitForm ptrRhs :> Lang.form) in
         returnResultInVar @@ `PtrDiffIntrinsic (info, flatPtrLhs, flatPtrRhs)

      | `CastIntrinsic (info, typ, value) ->
         let flatValue = (visitForm value :> Lang.form) in
         returnResultInVar @@ `CastIntrinsic (info, typ, flatValue)

      | `GetAddrIntrinsic _ ->
         returnResultInVar form

      | `GetFieldPointerIntrinsic (info, structForm, fieldName) ->
         let flatStructForm = (visitForm structForm :> Lang.form) in
         returnResultInVar @@ `GetFieldPointerIntrinsic (info, flatStructForm, fieldName)

      (* todo make sure this function is complete *)
      | _ ->
         failwith (sprintf "splitBasicBlocks found unsupported instruction\n  %s" (Lang.formToString form))
  in
  ignore (visitForm form);
  let unterminatedBlocks =
    let unfiltered = List.rev @@ basicBlocks() in
    List.rev @@ match unfiltered with
        | [] -> failwith "splitBasicBlocks"
        | entryBB :: remBBs ->
           entryBB :: List.filter (fun (name, _) -> String.length name > 0) remBBs
  in

  let lastBlock = List.hd unterminatedBlocks in
  let revBlocksButLast = List.tl unterminatedBlocks in
  let nextBlockName = ref (fst lastBlock) in
  let startsWithControlFlowInstruction forms =
    match forms with
      | `Return _ :: _
      | `Jump _ :: _
      | `Branch _ :: _ ->
         true
      | _ -> false
  in
  let info = Lang.formInfo Basics.fakeLocation in
  let fixedLastBlock =
    let revInstructions = List.rev @@ snd lastBlock in
    if startsWithControlFlowInstruction revInstructions then
      lastBlock
    else begin
      if (returnType <> `Void) then begin
        let loc =
          match revInstructions with
            | [] -> functionLoc
            | instr :: _ -> Lang.flocation instr
        in
        raiseInvalidAst loc "missing return at end of function";
      end;
      let ret = ((`Return (info, `Constant (info, VoidVal))) :> Lang.form) in
      let name = fst lastBlock in
      name, List.rev (ret :: revInstructions)
    end
  in
  let terminatedBlocksRev =
    fixedLastBlock ::
    List.map (fun (name, (instructions :Lang.form list)) ->
              let fixedBlock =
                let revInstructions = List.rev instructions in
                if startsWithControlFlowInstruction revInstructions then
                  (name, instructions)
                else
                  (name, List.rev (((`Jump (info, Lang.label !nextBlockName)) :> Lang.form) :: revInstructions))
              in
              nextBlockName := name;
              fixedBlock)
             revBlocksButLast
  in
  !localVariableCount, List.rev terminatedBlocksRev

let functionIsValid sizeT bindings func =
  match func.impl with
    | Some funcImpl ->
        begin
          let add place value = place := value :: !place in
          let errors = ref [] in
          let labels = ref [] in
          let targets = ref [] in

          let rec collectLabels (form :form) =
            match form with
              | `Sequence (_, forms) ->
                 List.iter collectLabels forms
              | `Label (_, l) ->
                 add labels (l.lname)
              | `Jump (_, label) ->
                 add targets (label.lname, Lang.flocation form)
              | `Branch (_, { trueLabel = t; falseLabel = f }) ->
                 let loc = Lang.flocation form in
                 add targets (t.lname, loc);
                 add targets (f.lname, loc);
              | `Return (_, expr) ->
                 let exprType = typeOfForm sizeT expr in
                 if not (equalTypes bindings func.rettype exprType) then
                   add errors @@ Serror.fromMsg (Some (Lang.flocation form))
                                                (sprintf "function's return type is %s but found %s"
                                                         (Types.typeName func.rettype)
                                                         (Types.typeName exprType))
              | _ ->
                 ()
          in
          collectLabels funcImpl;

          let checkTarget (target, loc) =
            if not (List.mem target !labels) then
              add errors @@ Serror.fromMsg (Some loc) @@ sprintf "label %s does not exist" target
          in
          List.iter checkTarget !targets;
          
          match !errors with
            | [] -> Mayfail.Result ()
            | errors -> Mayfail.Error errors
        end
    | None ->
        Mayfail.Result ()

let rec sideEffectFree = function
  | `Variable _
  | `Constant _
  | `EmbeddedComment _
  | `SizeofIntrinsic _
  | `GetAddrIntrinsic (_, { vname = _ })
    -> true

  | `Sequence (_, forms)
    -> List.for_all sideEffectFree forms

  | `CastIntrinsic (_, _, arg)
  | `GetFieldPointerIntrinsic (_, arg, _)
  | `LoadIntrinsic (_, arg)
    -> sideEffectFree arg

  | `PtrAddIntrinsic (_, arg1, arg2)
  | `PtrDiffIntrinsic (_, arg1, arg2)
    -> sideEffectFree arg1 && sideEffectFree arg2

  | `StoreIntrinsic _
  | `MallocIntrinsic _
  | `AssignVar _
  | `Branch _
  | `DefineVariable _
  | `FuncCall _
  | `Jump _
  | `Label _
  | `Return _
    -> false

