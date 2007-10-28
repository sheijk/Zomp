
open Ast2
open Lang
open Printf
open Bindings

let combine = Common.combine
  
exception CodeGenError of string
let raiseCodeGenError ~msg = raise (CodeGenError msg)

let typeOfForm = Semantic.typeOfForm
  ~onError:(fun ~msg ~found ~expected ->
              raiseCodeGenError ~msg:(sprintf "%s: expected %s but found %s"
                                   msg
                                   (Typesystems.Zomp.typeName expected)
                                   (Typesystems.Zomp.typeName found) ) )
    
let llvmName name =
  if name.[0] = '%' then name
  else "%" ^ name

let llvmGlobalName name = name
let llvmLocalName = llvmName
  
let isLocalVar name = String.length name <= 2 || name.[1] <> '$'
  
let rec llvmTypeName : Lang.typ -> string = function
  | `Void -> "void"
  | `Int -> "i32"
  | `Bool -> "i1"
  | `Char -> "i8"
  | `Float -> "float"
  | `TypeRef name -> "%" ^ name
  | `Pointer `Void -> "i8*"
  | `Pointer targetType -> (llvmTypeName targetType) ^ "*"
  | `Record components ->
      let componentNames = List.map (fun (_, t) -> llvmTypeName t) components in
      "{ " ^ combine ", " componentNames ^ "}"
      
let paramTypeName = function
  | `Char -> "i8 signext"
  | other -> llvmTypeName other

      
type resultvar = {
  rvname :string;
  rvtypename :string;
}

let resultVar var = {
  rvname = if var.vglobal then "@" ^ var.vname else "%" ^ var.vname;
  rvtypename = llvmTypeName var.typ;
}
  
let noVar = { rvname = ""; rvtypename = "" }
    
let lastTempVarNum = ref 0
let nextUID () = incr lastTempVarNum; !lastTempVarNum
let newGlobalTempVar, newLocalTempVar =
  let newVar isGlobal (typ :Lang.typ) =
    let id = nextUID () in
    let name = (sprintf "temp%d" id) in
    resultVar (variable name typ (defaultValue typ) RegisterStorage isGlobal)
  in
  newVar true, newVar false

let isConstant name =
  match string2integralValue name with
    | Some _ -> true
    | None -> false
  
let paramString signature =
  let argsWithTypes =
    List.map
      (fun (name, typ) ->
         if isConstant name then
           (llvmTypeName typ) ^ " " ^ name
         else
           (llvmTypeName typ) ^ " " ^ llvmName name
      )
      signature
  in
  combine ", " argsWithTypes

let stringMap f str =
  let newString = ref "" in
  let length = String.length str in
  for i = 0 to length - 1 do
    newString := !newString ^ f str.[i]
  done;
  !newString
  
let llvmEscapedString str = 
  Str.global_replace (Str.regexp "\\\\n") "\\\\0A" str

let lastUniqueId = ref 0
let newUniqueId() =
  incr lastUniqueId;
  !lastUniqueId
let newUniqueName() =
  "__temp_" ^ string_of_int (newUniqueId())

let rec sexpr2code = function
  | { id = "antiquote"; args = [{ id = id; args = args}] } ->
      begin
        { id = id; args = args }
      end
  | { id = id; args = [] } -> simpleExpr "simpleAst" ["\"" ^ id ^ "\""]
  | sexprWithArgs ->
      let tempVarName = newUniqueName() in
      let defVarExpr =
        { id = "var";
          args = [
            simpleExpr "ptr" ["ast"];
            idExpr tempVarName;
            simpleExpr "simpleAst" ["\"" ^ sexprWithArgs.id ^ "\""]]
        }
      in
      let returnExpr = idExpr tempVarName in
      let addChildExpr childExpr =
        { id = "addChild"; args = [
            idExpr tempVarName;
            childExpr;
          ] }
      in
      let argExprs = List.map sexpr2code sexprWithArgs.args in
      let argAddExprs = List.map addChildExpr argExprs in
      seqExpr( [defVarExpr] @ argAddExprs @ [returnExpr] )

let defaultBindings, externalFuncDecls, findIntrinsic =
  let callIntr intrName typ argVarNames =
    sprintf "%s %s %s\n" intrName (llvmTypeName typ) (combine ", " argVarNames)
  in
  let void argVarNames = "" in
  let compareIntrinsic typ name cond =
    let f argVarNames = 
      sprintf "icmp %s %s %s\n" cond (llvmTypeName typ) (combine ", " argVarNames)
    in
    (name, `Intrinsic f, `Bool, ["l", typ; "r", typ])
  in
  let twoArgIntrinsic name instruction (typ :composedType) =
    name, `Intrinsic (callIntr instruction typ), typ, ["l", typ; "r", typ]
  in

  let compareIntrinsics typ =
    let typeName = typeName typ in
    [
      compareIntrinsic typ (typeName ^ ".equal") "eq";
      compareIntrinsic typ (typeName ^ ".notEqual") "ne";
      compareIntrinsic typ (typeName ^ ".ugreater") "ugt";
      compareIntrinsic typ (typeName ^ ".ugreaterEqual") "uge";
      compareIntrinsic typ (typeName ^ ".uless") "ult";
      compareIntrinsic typ (typeName ^ ".ulessEqual") "ule";
      compareIntrinsic typ (typeName ^ ".sgreater") "sgt";
      compareIntrinsic typ (typeName ^ ".sgreaterEqual") "sge";
      compareIntrinsic typ (typeName ^ ".sless") "slt";
      compareIntrinsic typ (typeName ^ ".slessEqual") "sle";
    ]
  in
  let intrinsicFuncs =
    [
      twoArgIntrinsic "int.add" "add" `Int;
      twoArgIntrinsic "int.sub" "sub" `Int;
      twoArgIntrinsic "int.mul" "mul" `Int;
      twoArgIntrinsic "int.sdiv" "sdiv" `Int;
      twoArgIntrinsic "int.udiv" "udiv" `Int;
      twoArgIntrinsic "int.urem" "urem" `Int;
      twoArgIntrinsic "int.srem" "srem" `Int;
      
      (*     twoArgIntrinsic "int.shl" "shl" `Int; *)
      (*     twoArgIntrinsic "int.lshr" "lshr" `Int; *)
      (*     twoArgIntrinsic "int.ashr" "ashr" `Int; *)
      
      twoArgIntrinsic "int.and" "and" `Int;
      twoArgIntrinsic "int.or" "or" `Int;
      twoArgIntrinsic "int.xor" "xor" `Int;

      twoArgIntrinsic "float.add" "add" `Float;
      twoArgIntrinsic "float.sub" "sub" `Float;
      twoArgIntrinsic "float.mul" "mul" `Float;
      twoArgIntrinsic "float.fdiv" "fdiv" `Float;
      twoArgIntrinsic "float.frem" "frem" `Float;
      
      "printf", `ExternalFunc, `Void, ["test", `Pointer `Char];
      "void", `Intrinsic void, `Void, [];
    ]
    @ compareIntrinsics `Int
    @ compareIntrinsics `Char
  in
  let builtinMacros =
    let macro name f = (name, MacroSymbol { mname = name; mtransformFunc = f; }) in
    let delegateMacro macroName funcName =
      macro macroName (fun _ args -> { id = funcName; args = args; })
    in
    let templateMacro name params expr =
      macro name (fun _ args -> Ast2.replaceParams params args expr)
    in
    let quoteMacro =
      macro "quote"
        (fun _ args ->
           match args with
             | [quotedExpr] -> sexpr2code quotedExpr
             | [] -> simpleExpr "simpleAst" ["seq"]
             | args -> { id = "quote"; args = args }
        )
    in
    [
      delegateMacro "op+" "int.add";
      delegateMacro "op+_f" "float.add";

      templateMacro "echo" ["message"] ({ id = "seq"; args = [simpleExpr "printInt" ["message"]; idExpr "printNewline"] });

      quoteMacro;
(*       macro "asttest" Builtins.testRunMacro; *)
    ]
  in
  let defaultBindings =
    let toFunc (name, _, typ, args) =
      name, FuncSymbol (funcDecl name typ args)
    in
    (List.map toFunc intrinsicFuncs)
    @ builtinMacros
  in
  let externalFuncDecls =
    let rec defs = function
      | [] -> []
      | (name, `ExternalFunc, rettype, args) :: tail ->
          let decl = 
            sprintf "declare %s @%s(%s)"
              (llvmTypeName rettype)
              name
              (paramString args)
          in
          decl :: defs tail
      | _ :: tail -> defs tail
    in
    combine "\n" (defs intrinsicFuncs)
  in
  let findIntrinsic name =
    let rec find = function
      | [] -> None
      | (intrName, `Intrinsic gencodeF, _, _) :: _ when name = intrName -> Some gencodeF
      | _ :: tail -> find tail
    in
    find intrinsicFuncs
  in
  defaultBindings, externalFuncDecls, findIntrinsic
  
let gencodeSequence gencode exprs =
  let rec lastVarAndCode var code = function
    | [] -> var, code
    | expr :: tail ->
        let resultVar, exprCode = gencode expr in
        lastVarAndCode resultVar (code @ [exprCode]) tail
  in
  let resultVar, code = lastVarAndCode noVar [] exprs in
  resultVar, Common.indent (combine "\n\n" code)

let gencodeDefineVariable gencode var default =
  match var.vstorage with
    | MemoryStorage ->
        begin
          let typename = llvmTypeName var.typ
          and ptrname = (llvmName var.vname)
          in
          let comment = sprintf "; allocating var %s : %s/%s on stack\n\n"
            var.vname
            (Lang.typeName var.typ)
            typename
          in
          let initVar, initVarCode =
            match default with
              | Some expr -> gencode expr
              | None -> noVar, ""
          in
          let allocCode =
            match var.typ with
              | `Void -> initVarCode
              | _ ->
                  begin match default with
                    | Some expr ->
                        begin
                          initVarCode
                          ^ sprintf "%s = alloca %s\n\n" ptrname typename
                          ^ sprintf "store %s %s, %s* %s\n\n" typename initVar.rvname typename ptrname
                        end
                    | None ->
                        sprintf "%s = alloca %s\n\n" ptrname typename
                  end
          in
          (noVar, comment ^ allocCode)
        end
    | RegisterStorage ->
        begin
          let zeroElement = function
            | `Pointer _ -> Some "null"
            | `Record _ | `TypeRef _ -> None
            | #integralType as t -> Some (Lang.valueString (defaultValue t))
          in
          let initInstr = function
            | `Int | `Float | `Pointer _ -> "add"
            | `Bool -> "or"
            | _ as t -> raiseCodeGenError
                ~msg:(sprintf "no init instruction implemented for %s" (Lang.typeName t))
          in
          let name = llvmName var.vname
          and typ = llvmTypeName var.typ
          in
          let comment = sprintf "; defining var %s : %s\n\n" var.vname typ in
          let code =
            match var.typ with
              | `Pointer _ | `Record _ -> begin
                  raiseCodeGenError ~msg:"code gen for pointers and records with register storage not supported, yet"
                end
              | _ -> begin
                  match zeroElement var.typ, default with
                    | Some zeroElementStr, Some expr ->
                        let initVar, initCode = gencode expr in
                        initCode ^
                        sprintf "%s = %s %s %s, %s\n\n"
                          name
                          (initInstr var.typ)
                          typ
                          zeroElementStr
                          initVar.rvname
                    | _, _ ->
                        sprintf "%s = alloca %s\n\n" name typ
                end
          in
          (noVar, comment ^ code)
        end

let gencodeVariable v =
  let typeName = llvmTypeName v.typ in
  match v.vstorage with
    | RegisterStorage ->
        (resultVar v, "")
    | MemoryStorage ->
        let comment = sprintf "; accessing %s : %s\n\n" v.vname typeName in
        let localVar, localName =
          let v = newLocalTempVar v.typ in
          v, v.rvname
        in
        let code =
          sprintf "%s = load %s* %s\n\n" localName typeName (resultVar v).rvname
        in
        (localVar, comment ^ code)

let gencodeConstant c =
  {
    rvname = Lang.valueString c;
    rvtypename = llvmTypeName (Lang.typeOf c);
  },
  ""

let gencodeFuncCall gencode call =
  let rec varsAndCode vars code = function
    | [] -> vars, code
    | expr :: tail ->
        let resultVar, exprCode = gencode expr in
        varsAndCode (vars @ [resultVar.rvname]) (code ^ exprCode) tail
  in
  let vars, argevalCode = varsAndCode [] "" call.fcargs in
  let resultVar = newLocalTempVar call.fcrettype in
  let assignResultCode =
    if resultVar.rvtypename = "void" then ""
    else sprintf "%s = " resultVar.rvname
  in
  match findIntrinsic call.fcname with
    | None ->
        let comment, funccallCode =
          let signatureString =
            let argTypeNames = List.map paramTypeName call.fcparams in
            combine ", " argTypeNames
          in
(*           let argString = paramString (List.combine vars call.fcparams) in *)
          let argString =
            let toTypeAndArg name typ =
              llvmTypeName typ ^ " " ^ name
            in
            let typeAndArgs = List.map2 toTypeAndArg vars call.fcparams in
            combine ", " typeAndArgs
          in
          let comment =
            sprintf "; calling function %s(%s)\n\n" call.fcname argString
          in
          comment,
          (assignResultCode
           ^ (sprintf "call %s (%s)* @%s(%s)\n\n"
                (llvmTypeName call.fcrettype)
                signatureString
                call.fcname
                argString))
        in
        (resultVar, comment ^ argevalCode ^ funccallCode)
    | Some gencallCodeF ->
        let comment = sprintf "; calling intrinsic %s\n\n" call.fcname in
        let intrinsicCallCode =
          assignResultCode ^ (gencallCodeF vars)
        in
        (resultVar, comment ^ argevalCode ^ "\n\n" ^ intrinsicCallCode)

let offsetStringAndCode gencode countForm =
  match countForm with
    | `Constant IntVal count ->
        (string_of_int count), ""
    | `Variable var ->
        let valueVar, valueAccessCode = gencode (`Variable (var :> composedType variable)) in
        valueVar.rvname, valueAccessCode
    | _ -> raiseCodeGenError ~msg:"Invalid expression for count"

let checkType resultVar typ =
  if llvmTypeName typ <> resultVar.rvtypename then
    raiseCodeGenError ~msg:(sprintf "Internal error: expected %s to be of type %s instead of %s"
                              resultVar.rvname (llvmTypeName typ) resultVar.rvtypename)

let todoBindings = defaultBindings

let gencodeGenericIntr (gencode : Lang.form -> resultvar * string) = function
  | `NullptrIntrinsic targetTyp ->
      begin
        let ptrTypeLLVMName = llvmTypeName (`Pointer targetTyp) in
        let var = newLocalTempVar (`Pointer targetTyp) in
        let code = sprintf "%s = bitcast i8* null to %s\n\n" var.rvname ptrTypeLLVMName in
        (var, code)
      end
  | `MallocIntrinsic (typ, countForm) ->
      begin
        let countVar, preCode = gencode countForm in
        checkType countVar `Int;
        let var = newLocalTempVar (`Pointer typ) in
        let code = sprintf "%s = malloc %s, i32 %s" var.rvname (llvmTypeName typ) countVar.rvname in
        (var, preCode ^ code)
      end
  | `GetAddrIntrinsic var ->
      begin
        match var.vstorage with
          | MemoryStorage -> (resultVar var, "")
          | RegisterStorage -> raiseCodeGenError ~msg:"Getting address of register storage var not possible"
      end
  | `StoreIntrinsic (ptrForm, valueForm) ->
      begin
        let ptrVar, ptrAccessCode = gencode ptrForm in
        let valueVar, valueAccessCode = gencode valueForm in
        let code =
          sprintf "store %s %s, %s %s\n\n"
            valueVar.rvtypename valueVar.rvname ptrVar.rvtypename ptrVar.rvname
        in
        (noVar, ptrAccessCode ^ valueAccessCode ^ code)
      end
  | `LoadIntrinsic expr ->
      begin
        let targetType =
          match typeOfForm todoBindings expr with
            | `Pointer targetType -> targetType
            | nonPointerType ->
                raiseCodeGenError ~msg:("Expected pointer argument instead of "
                                        ^ (typeName nonPointerType))
        in
        let ptrVar, accessCode = gencode expr in
        let resultVar = newLocalTempVar targetType in
        let comment = sprintf "; loading %s\n\n" ptrVar.rvtypename in
        let code =
          sprintf "%s = load %s %s\n\n" resultVar.rvname ptrVar.rvtypename ptrVar.rvname
        in
        (resultVar, comment ^ accessCode ^ "\n\n" ^ code)
      end
  | `GetFieldPointerIntrinsic (recordForm, fieldName) ->
      begin
        let fieldType, fieldIndex =
          match typeOfForm todoBindings recordForm with
            | `Pointer `Record components ->
                let fieldType = match componentType components fieldName with
                  | Some fieldType -> fieldType
                  | None -> raiseCodeGenError ~msg:
                      (sprintf "Could not find field %s" fieldName)
                in
                let fieldIndex = componentNum components fieldName in
                fieldType, fieldIndex
            | _ as invalidType -> raiseCodeGenError ~msg:
                (sprintf "Expected pointer to record instead of %s" (typeName invalidType))
        in
        let ptrVar = newLocalTempVar (`Pointer fieldType) in
        let recordVar, recordAccessCode = gencode recordForm in
        let comment = sprintf "; obtaining address of %s.%s\n\n" recordVar.rvname fieldName in
        let code = sprintf "%s = getelementptr %s %s, i32 0, i32 %d\n\n"
          ptrVar.rvname
          recordVar.rvtypename
          recordVar.rvname
          fieldIndex
        in
        (ptrVar, comment ^ recordAccessCode ^ code)
      end
  | `PtrAddIntrinsic (ptrForm, offsetForm) ->
      begin
        let ptrType = typeOfForm todoBindings ptrForm in
        let resultVar = newLocalTempVar ptrType in
        let comment = sprintf "; ptr.add\n\n" in
        let ptrVar, ptrVarAccessCode = gencode ptrForm in
        let offsetVar, offsetAccessCode = gencode offsetForm in
        let code =
          sprintf "%s = getelementptr %s %s, i32 %s\n\n"
            resultVar.rvname ptrVar.rvtypename ptrVar.rvname offsetVar.rvname
        in
        (resultVar,
         comment ^
           ptrVarAccessCode ^
           offsetAccessCode ^
           code)
      end
  | `CastIntrinsic (targetType, valueForm) ->
      let valueVar, valueCode = gencode valueForm in
      let valueType = typeOfForm todoBindings valueForm in
      let resultVar = newLocalTempVar targetType in
      let comment = sprintf "; casting to %s\n\n" resultVar.rvtypename in
      let instructionName = 
        match valueType, targetType with
          | `Pointer _, `Int -> "ptrtoint"
          | `Int, `Pointer _ -> "inttoptr"
          | _, _ ->
              raiseCodeGenError ~msg:(sprintf "Cannot cast from %s to %s"
                                        (typeName valueType) (typeName targetType))
      in
      let code =
        sprintf "%s = %s %s %s to %s\n"
          resultVar.rvname
          instructionName
          valueVar.rvtypename
          valueVar.rvname
          (llvmTypeName targetType)
      in
      resultVar, comment ^ valueCode ^ "\n" ^ code
      
let gencodeAssignVar gencode var expr =
  let rvalVar, rvalCode = gencode expr in
  let name = (resultVar var).rvname in
  let typename = llvmTypeName var.typ in
  let comment = sprintf "; assigning new value to %s\n\n" name in
  let assignCode = sprintf "store %s %s, %s* %s\n\n" typename rvalVar.rvname typename name in
  (noVar, comment ^ rvalCode ^ "\n\n" ^ assignCode)

let gencodeReturn gencode expr =
  let exprVar, exprCode = gencode expr in
  let comment = sprintf "; return %s\n\n" exprVar.rvtypename in
  let isValueType name = String.length name > 0 && name <> "void" in
  let retCode = 
    if isValueType exprVar.rvtypename then
      sprintf "ret %s %s\n\n" exprVar.rvtypename exprVar.rvname
    else
      sprintf "ret void\n\n"
  in
  (noVar, comment ^ exprCode ^ "\n\n" ^ retCode)

let gencodeJump label =
  let code = sprintf "br label %%%s\n\n" label.lname in
  (noVar, code)
    
let gencodeLabel label =
  let _, dummyJumpCode = gencodeJump label in
  let code = sprintf "%s:\n\n" label.lname in
  (noVar, dummyJumpCode ^ code)

let gencodeBranch gencode branch =
  let condVar, preCode = gencode (`Variable (branch.bcondition :> Lang.typ Lang.variable)) in
  let code =
    sprintf "br i1 %s, label %%%s, label %%%s"
      condVar.rvname
      branch.trueLabel.lname
      branch.falseLabel.lname
  in
  (noVar, preCode ^ code)

let rec gencode : Lang.form -> resultvar * string = function
(*   | `ToplevelForm _ -> raiseCodeGenError ~msg:"toplevel form not allowed, here" *)
  | `Sequence exprs -> gencodeSequence gencode exprs
  | `DefineVariable (var, expr) -> gencodeDefineVariable gencode var expr
  | `Variable var -> gencodeVariable var
  | `Constant c -> gencodeConstant c
  | `FuncCall call -> gencodeFuncCall gencode call
  | `Return e -> gencodeReturn gencode e
  | `Label l -> gencodeLabel l
  | `Jump l -> gencodeJump l
  | `Branch b -> gencodeBranch gencode b
  | `AssignVar (var, expr) -> gencodeAssignVar gencode var expr
  | #genericIntrinsic as intr -> gencodeGenericIntr gencode intr
      
let countChar str c =
  let count = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = c then incr count
  done;
  !count
  
let llvmStringLength str =
  let length = String.length str in
  length - 2 * countChar str '\\'

let gencodeGlobalVar var =
  let varname = var.vname in
  match var.default with
    | StringLiteral value ->
        let contentVar = newGlobalTempVar (`Pointer `Char)
        and escapedValue = llvmEscapedString value
        in
        let length = llvmStringLength escapedValue + 1 in
        let stringStorageSrc =
          sprintf "%s = internal constant [%d x i8] c\"%s\\00\"\n\n"
            contentVar.rvname
            length
            escapedValue
        in
        let stringPointerSrc =
          sprintf "@%s = global i8* getelementptr ([%d x i8]* %s, i32 0, i32 0)\n\n"
            varname
            length
            contentVar.rvname
        in
        stringStorageSrc ^ stringPointerSrc
    | IntVal _ | BoolVal _ | FloatVal _ | CharVal _ ->
        sprintf "@%s = constant %s %s"
          varname
          (llvmTypeName var.typ)
          (Lang.valueString var.default)
    | VoidVal ->
        raiseCodeGenError ~msg:"global constant of type void not allowed"
    | PointerVal _ ->
        raiseCodeGenError ~msg:"global pointers not supported, yet"
    | RecordVal _ ->
        raiseCodeGenError ~msg:"global constant of record type not supported, yet"

let gencodeDefineFunc func =
  match func.impl with
    | None ->
        let paramTypeNames = List.map (fun (_, typ) -> paramTypeName typ) func.fargs in
        let paramString = combine ", " paramTypeNames in
        let decl = sprintf "%s @%s(%s) "
          (llvmTypeName func.rettype) func.fname paramString
        in
        "declare " ^ decl
    | Some impl ->
        let param2string (name, typ) = (paramTypeName typ) ^ " " ^ (llvmName name) in
        let paramString = combine ", " (List.map param2string func.fargs) in
        let decl = sprintf "%s @%s(%s) "
          (llvmTypeName func.rettype) func.fname paramString
        in
        let lastOrDefault list default = List.fold_left (fun _ r -> r) default list in
        let lastExpr = function
          | `Sequence exprs as seq -> lastOrDefault exprs seq
          | _ as expr -> expr
        in
        let resultVar, implCode = gencode impl in
        let impl = match lastExpr impl with
          | `Return _ ->
              sprintf "{\n\n%s\n\n}" implCode
          | _ ->
              let isTypeName name = String.length name > 0 && name <> "void" in
              (sprintf "{\n\n%s\n\n" implCode)
              ^ (if isTypeName resultVar.rvtypename then
                   (sprintf "  ret %s %s\n\n}"
                      resultVar.rvtypename
                      resultVar.rvname)
                 else "  ret void\n\n}")
        in
        "define " ^ decl ^ impl

let gencodeTypedef name typ =
  sprintf "%%%s = type %s\n\n" name (llvmTypeName typ)
  
let gencodeTL = function
  | `GlobalVar var -> gencodeGlobalVar var
  | `DefineFunc func -> gencodeDefineFunc func
  | `Typedef (name, typ) -> gencodeTypedef name typ

let genmodule (toplevelExprs :Lang.toplevelExpr list) :string =
  let rec seperateVarsAndFuncs = function
    | [] -> ([], [], [])
    | expr :: tail ->
        let typedefs, vars, funcs = seperateVarsAndFuncs tail in
        match expr with
          | `Typedef _ as typedef -> (typedef :: typedefs, vars, funcs)
          | `GlobalVar _ as var -> (typedefs, var :: vars, funcs)
          | `DefineFunc _ as func -> (typedefs, vars, func :: funcs)
  in
  let globalTypedefs, globalVars, globalFuncs = seperateVarsAndFuncs toplevelExprs in
  let headerCode = ""
  and typedefCode = List.map gencodeTL globalTypedefs
  and varCode = List.map gencodeTL globalVars
  and funcCode = List.map gencodeTL globalFuncs
  in
  "\n\n\n\n;;; header ;;;\n\n\n\n"
  ^ headerCode ^ "\n\n"
  ^ "\n\n\n\n;;; typedefs ;;;\n\n\n\n"
  ^ (combine "\n\n" typedefCode)
  ^ "\n\n\n\n;;; variables ;;;\n\n\n\n"
  ^ (combine "\n\n" varCode)
  ^ "\n\n\n\n;;; implementation ;;;\n\n\n\n"
  ^ (combine "\n\n" funcCode)
  ^ "\n\n" ^ externalFuncDecls
    
