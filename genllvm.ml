
open Ast2
open Lang
open Printf
open Bindings
open Common

let combine = Common.combine

exception CodeGenError of string
let raiseCodeGenError ~msg = raise (CodeGenError msg)

let typeOfForm = Semantic.typeOfForm
  ~onError:(fun ~msg ~found ~expected ->
              raiseCodeGenError ~msg:(sprintf "%s: expected %s but found %s"
                                   msg
                                   (Semantic.typeRequirementToString expected)
                                   (Typesystems.Zomp.typeName found) ) )

let escapeName name = "\"" ^ name ^ "\""

let llvmName name =
  let name = escapeName name in
  if name.[0] = '%' then name
  else "%" ^ name

let rec llvmTypeName : Lang.typ -> string = function
  | `Void -> "void"
  | `Int8 -> "i8"
  | `Int16 -> "i16"
  | `Int32 -> "i32"
  | `Int64 -> "i64"
  | `Bool -> "i1"
  | `Char -> "i8"
  | `Float -> "float"
  | `Double -> "double"
  | `TypeRef name -> "%" ^ name
  | `Pointer `Void -> "i8*"
  | `Pointer targetType -> (llvmTypeName targetType) ^ "*"
  | `Array (memberType, size) -> sprintf "[%d x %s]" size (llvmTypeName memberType)
  | `Record components ->
      let componentNames = List.map (fun (_, t) -> llvmTypeName t) components in
      "{ " ^ combine ", " componentNames ^ "}"
  | `Function ft ->
      sprintf "%s (%s)"
        (llvmTypeName ft.returnType)
        (Common.combine ", " (List.map llvmTypeName ft.argTypes))

let paramTypeName = function
  | `Char -> "i8 signext"
  | other -> llvmTypeName other

type resultvar = {
  rvname :string;
  rvtypename :string;
}

let resultVar var =
  let name = escapeName var.vname in
  {
    rvname = if var.vglobal then "@" ^ name else "%" ^ name;
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
  let strLength = String.length str in
  let rec nextChar pos acc =
    if pos < strLength then
      let chr = str.[pos] in
      if chr = '\\' then
        nextCharEscaped (pos+1) acc
      else if chr = '"' then
        nextChar (pos+1) ('2' :: '2' :: '\\' :: acc)
      else
        nextChar (pos+1) (chr::acc)
    else
      acc
  and nextCharEscaped pos acc =
    if pos < strLength then
      let chr = str.[pos] in
      let chars =
        match chr with
          | 'n' -> ['A'; '0'; '\\']
          | '0' -> ['0'; '0'; '\\']
          | '\\' -> ['C'; '5'; '\\']
          | invalid -> raiseCodeGenError ~msg:(sprintf "Cannot escape \\%c" invalid)
      in
      nextChar (pos+1) (chars @ acc)
    else
      raiseCodeGenError ~msg:"String may not end in \\"
  in
  let chars = nextChar 0 [] in
  let stringFromList chars =
    let len = List.length chars in
    let str = String.make len 'x' in
    listIteri (fun pos chr -> str.[len - pos - 1] <- chr) chars;
    str
  in
  stringFromList chars

(* let test_llvmEscapedString = *)
(*   let testCases = [ *)
(*     "\\n", "\\0A"; *)
(*     "\\\"", "\\22"; *)
(*     "\\0", "\\00"; *)
(*     "foo", "foo"; *)
(*   ] in *)
(*   print_newline(); *)
(*   List.iter *)
(*     (fun (input, expected) -> *)
(*        let found = llvmEscapedString input in *)
(*        if not (input = found) then *)
(*          printf "Error: llvmEscapedString '%s' => '%s' instead of '%s'\n" *)
(*            input found expected) *)
(*     testCases *)

let isValidLlvmString str =
  try
    let isHexDigit chr =
      let isInRange min max = chr >= min && chr <= max in
      isInRange '0' '9' || isInRange 'a' 'f' || isInRange 'A' 'F'
    in
    let strLength = String.length str in
    let rec checkFrom start =
      if start >= strLength then
        true
      else
        let backslashPos = String.index_from str start '\\' in
        if strLength > backslashPos + 2
          && isHexDigit str.[backslashPos+1]
          && isHexDigit str.[backslashPos+2] then
            checkFrom (backslashPos + 1)
        else
          false
    in
    checkFrom 0
  with Not_found ->
    true

let lastUniqueId = ref 0
let newUniqueId() =
  incr lastUniqueId;
  !lastUniqueId
let newUniqueName() =
  "__temp_" ^ string_of_int (newUniqueId())

let sexpr2codeNoAntiquotes recursion = function
  | { id = id; args = [] } ->
      if id = "'\\0'" then
        simpleExpr "ast:fromString" ["\"'!'\""]
      else
        simpleExpr "ast:fromString" ["\"" ^ id ^ "\""]
  | sexprWithArgs ->
      let tempVarName = newUniqueName() in
      let defVarExpr =
        { id = "var";
          args = [
            simpleExpr "ptr" ["ast"];
            idExpr tempVarName;
            simpleExpr "ast:fromString" ["\"" ^ sexprWithArgs.id ^ "\""]]
        }
      in
      let returnExpr = idExpr tempVarName in
      let addChildExpr childExpr =
        { id = "ast:addChild"; args = [
            idExpr tempVarName;
            childExpr;
          ] }
      in
      let argExprs = List.map recursion sexprWithArgs.args in
(*       let argExprs = List.map (sexpr2code ~antiquoteF) sexprWithArgs.args in *)
      let argAddExprs = List.map addChildExpr argExprs in
      seqExpr( [defVarExpr] @ argAddExprs @ [returnExpr] )

let rec sexpr2codeasis expr = sexpr2codeNoAntiquotes sexpr2codeasis expr

let rec sexpr2code ?(antiquoteF = (fun id args -> { id = id; args = args })) = function
  | { id = "antiquote"; args = [{ id = id; args = args}] } ->
      begin
        antiquoteF id args
      end
  | expr ->
      sexpr2codeNoAntiquotes (sexpr2code ~antiquoteF) expr

(** will turn #foo into (astFromInt foo) if foo evaluates to `Int etc. *)
let insertAstConstructors bindings =
  fun id args ->
    let default = { id = id; args = args } in
    match args with
      | [] ->
          begin match lookup bindings id with
            | VarSymbol { typ = `Int32 } ->
                { id = "ast:fromInt"; args = [idExpr id] }
            | VarSymbol { typ = `Pointer `Char } ->
                { id = "ast:fromString"; args = [idExpr id] }
            | _ -> default
          end
      | _ -> default


let defaultBindings, externalFuncDecls, findIntrinsic =
  let callIntr intrName typ argVarNames =
    sprintf "%s %s %s\n" intrName (llvmTypeName typ) (combine ", " argVarNames)
  in
  let void argVarNames = "" in
  let compareIntrinsic typ name cond =
    let instruction =
      match typ with
        | `Float | `Double -> "fcmp"
        | _ -> "icmp"
    in
    let f argVarNames =
      sprintf "%s %s %s %s\n" instruction cond (llvmTypeName typ) (combine ", " argVarNames)
    in
    (name, `Intrinsic f, `Bool, ["l", typ; "r", typ])
  in
  let twoArgIntrinsic name instruction (typ :composedType) =
    name, `Intrinsic (callIntr instruction typ), typ, ["l", typ; "r", typ]
  in
  let simpleTwoArgIntrinsincs typ namespace names =
    List.map (fun name -> twoArgIntrinsic (sprintf "%s:%s" namespace name) name typ) names
  in

  let compareIntrinsics typ typeName =
    let functionMapping =
      [
        "equal", "eq";
        "notEqual", "ne";
        "ugreater", "ugt";
        "ugreaterEqual", "uge";
        "uless", "ult";
        "ulessEqual", "ule";
        "sgreater", "sgt";
        "sgreaterEqual", "sge";
        "sless", "slt";
        "slessEqual", "sle";
      ]
    in
    List.map (fun (zompName, llvmName) ->
                compareIntrinsic typ (typeName ^ ":" ^ zompName) llvmName)
      functionMapping
  in
  let floatIntrinsics typ =
    let typeName = typeName typ in
    let functionMappings = [
      "equal", "eq";
      "notEqual", "ne";
      "greater", "gt";
      "greaterEqual", "ge";
      "less", "lt";
      "lessEqual", "le";
    ]
    in
    let makeIntrinsic prefix (zompName, llvmName) =
      compareIntrinsic typ (typeName ^ ":" ^ prefix ^ zompName) (prefix ^ llvmName)
    in
    List.map (makeIntrinsic "o") functionMappings
    @ simpleTwoArgIntrinsincs typ typeName ["add"; "sub"; "mul"; "fdiv"; "frem"];
  in
  let oneArgFunc name f = function
    | [arg] -> f arg
    | _ -> raiseCodeGenError ~msg:(sprintf "Only one argument expected by %s" name)
  in
  let convertIntr funcName intrName fromType toType =
    let convertIntrF intrName = oneArgFunc intrName
      (fun arg ->
         sprintf "%s %s %s to %s" intrName (llvmTypeName fromType) arg (llvmTypeName toType))
    in
    funcName, `Intrinsic (convertIntrF intrName), toType, ["v", fromType]
  in
  let truncIntIntr fromType toType =
    let name = sprintf "%s:to%s" (typeName fromType) (String.capitalize (typeName toType)) in
    let func = oneArgFunc name
      (fun arg -> sprintf "trunc %s %s to %s" (llvmTypeName fromType) arg (llvmTypeName toType)) in
    name, `Intrinsic func, toType, ["v", fromType]
  in
  let zextIntr fromType toType =
    let name = sprintf "%s:zextTo%s" (typeName fromType) (String.capitalize (typeName toType)) in
    let func = oneArgFunc name
      (fun arg -> sprintf "zext %s %s to %s" (llvmTypeName fromType) arg (llvmTypeName toType)) in
    name, `Intrinsic func, toType, ["v", fromType]
  in
  let intrinsicFuncs =
     [
      (*     twoArgIntrinsic "int:shl" "shl" `Int; *)
      (*     twoArgIntrinsic "int:lshr" "lshr" `Int; *)
      (*     twoArgIntrinsic "int:ashr" "ashr" `Int; *)
      "void", `Intrinsic void, `Void, [];

      convertIntr "float:toInt" "fptosi" `Float `Int32;
      convertIntr "int:toFloat" "sitofp" `Int32 `Float;
      convertIntr "int:toDouble" "sitofp" `Int32 `Double;
      convertIntr "double:toInt" "fptosi" `Double `Int32;
      convertIntr "float:toDouble" "fpext" `Float `Double;
      convertIntr "double:toFloat" "fptrunc" `Double `Float;

      truncIntIntr `Int64 `Int32;
      zextIntr `Int32 `Int64;
    ]
    @ simpleTwoArgIntrinsincs `Int32 "int" ["add"; "sub"; "mul"; "sdiv"; "udiv"; "urem"; "srem"; "and"; "or"; "xor"]
    @ simpleTwoArgIntrinsincs `Bool "bool" ["and"; "or"; "xor"]
    @ floatIntrinsics `Float
    @ floatIntrinsics `Double
    @ compareIntrinsics `Int32 "int"
    @ compareIntrinsics `Char (typeName `Char)
  in
  let builtinMacros =
    let macro name doc f = (name, MacroSymbol { mname = name; mdocstring = doc; mtransformFunc = f; }) in
    let quoteMacro =
      macro "quote"
        "ast..."
        (fun bindings args ->
           match args with
             | [quotedExpr] -> sexpr2code ~antiquoteF:(insertAstConstructors bindings) quotedExpr
             | [] -> simpleExpr "ast:fromString" ["seq"]
             | args -> { id = "quote"; args = args }
        )
    in
    let quoteasisMacro =
      macro "quoteasis" "ast..."
        (fun bindings args ->
           match args with
             | [quotedExpr] -> sexpr2codeasis quotedExpr
             | [] -> simpleExpr "ast:fromString" ["seq"]
             | args -> { id = "quote"; args = args }
        )
    in
    let bindingsIsNameUsed =
      macro "std:bindings:isNameUsed" "name"
        (fun bindings args ->
           match args with
             | [{ id = name; args = []}] ->
                 begin match Bindings.lookup bindings name with
                   | Bindings.UndefinedSymbol -> idExpr "false"
                   | _ -> idExpr "true"
                 end
             | _ ->
                 raiseCodeGenError ~msg:("std:bindings:isNameUsed expects exactly one argument")
        )
    in
    let bindingsLookupVar =
      let syntax = "name ('hasType' typeVar code ...) ('notFound' code ...)" in
      macro "std:bindings:matchVar" syntax
        (fun bindings args ->
           match args with
             | [ {id = name; args = []};
                 {id = "hasType"; args = {id = typeVar; args = []} :: onFound};
                 {id = "notFound"; args = onNotFound}] ->
                 begin match lookup bindings name with
                   | VarSymbol var ->
                       replaceParams [typeVar] [idExpr (Lang.typeName var.typ)]
                         (seqExpr onFound)
                   | _ ->
                       seqExpr onNotFound
                 end
             | _ ->
                 raiseCodeGenError ~msg:(
                   sprintf "std:bindings:matchVar expects syntax %s" syntax)
        )
    in
    let testMacro =
      let calls1i functionName arg =
        Zompvm.zompResetArgs();
        Zompvm.zompAddIntArg arg;
        Zompvm.zompRunFunctionStringWithArgs functionName
      in
      let calli1i functionName arg =
        Zompvm.zompResetArgs();
        Zompvm.zompAddIntArg arg;
        Zompvm.zompRunFunctionIntWithArgs functionName
      in
      macro "std:test" "()"
        (fun bindings args ->
           let sexprAddress = Zompvm.zompSimpleAst "foobar" in
           printf "ast has address %d\n" sexprAddress;
           flush stdout;
           if sexprAddress <> 0 then begin
             Zompvm.zompAddChild sexprAddress (Zompvm.zompSimpleAst "child");
             let name = calls1i "macroAstId" sexprAddress in
             let childCount = calli1i "macroAstChildCount" sexprAddress in
             Ast2.simpleExpr "was" [name; string_of_int childCount]
           end else begin
             Ast2.simpleExpr "returned 0" []
           end
        )
    in

    let opjuxMacro =
      macro "opjux" "opjux id args..."
        (fun bindings args -> Ast2.shiftLeft args)
    in
    let opcallMacro =
      macro "opcall" "opcall id args..."
        (fun bindings args -> Ast2.shiftLeft args)
    in
    let opseqMacro =
      macro "opseq" "opseq args..."
        (fun bindings args -> Ast2.seqExpr args)
    in
    [
      testMacro;
      quoteMacro;
      quoteasisMacro;
      bindingsIsNameUsed;
      bindingsLookupVar;

      (** macros to support indent expressions *)
      opjuxMacro;
      opcallMacro;
      opseqMacro;
    ]
  in
  let defaultBindings =
    let toFunc (name, _, typ, args) =
      name, FuncSymbol (funcDecl name typ args)
    in
    Bindings.fromSymbolList
      ((List.map toFunc intrinsicFuncs)
       @ builtinMacros)
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
            | `Pointer _ | `Function _ -> Some "null"
            | `Record _ | `TypeRef _ | `Array _ -> None
            | #integralType as t -> Some (Lang.valueString (defaultValue t))
          in
          let initInstr = function
            | `Int8 | `Int16 | `Int32 | `Int64 | `Float | `Pointer _ -> "add"
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
    rvname =
      (match c with
         | FloatVal f -> Machine.float2string f
         | _ -> Lang.valueString c);
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
          let calleeName, loadCode =
            match call.fcptr with
              | `NoFuncPtr -> "@" ^ escapeName call.fcname, ""
              | `FuncPtr ->
                  let typeOfCalledFunc fc =
                    `Function {
                      returnType = call.fcrettype;
                      argTypes = call.fcparams
                    }
                  in
                  let fptrVar = newLocalTempVar (`Pointer (typeOfCalledFunc call)) in
                  fptrVar.rvname,
                  sprintf "%s = load %s* %%%s\n" fptrVar.rvname fptrVar.rvtypename call.fcname
          in
          comment,
          (loadCode
           ^ assignResultCode
           ^ (sprintf "call %s (%s)* %s(%s)\n\n"
                (llvmTypeName call.fcrettype)
                signatureString
                calleeName
                argString))
        in
        (resultVar, comment ^ argevalCode ^ funccallCode)
    | Some gencallCodeF ->
        assert( call.fcptr = `NoFuncPtr );
        let comment = sprintf "; calling intrinsic %s\n\n" call.fcname in
        let intrinsicCallCode =
          assignResultCode ^ (gencallCodeF vars)
        in
        (resultVar, comment ^ argevalCode ^ "\n\n" ^ intrinsicCallCode)

let offsetStringAndCode gencode countForm =
  match countForm with
    | `Constant Int32Val count ->
        (Int32.to_string count), ""
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
        checkType countVar `Int32;
        let var = newLocalTempVar (`Pointer typ) in
        let code = sprintf "%s = malloc %s, i32 %s" var.rvname (llvmTypeName typ) countVar.rvname in
        (var, preCode ^ code)
      end
  | `GetAddrIntrinsic var ->
      begin
        match var.vstorage with
          | MemoryStorage -> (resultVar {var with typ = `Pointer var.typ},
                              sprintf "; addrOf %s\n" (typeName var.typ))
              (* | MemoryStorage -> (resultVar var, "") *)
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
        let comment = sprintf "; obtaining address of %s.%s (type = %s)\n\n"
          recordVar.rvname fieldName recordVar.rvtypename
        in
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
          | `Pointer _, `Int32 -> "ptrtoint"
          | `Int32, `Pointer _ -> "inttoptr"
          | `Pointer _, `Pointer _ -> "bitcast"
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
  let assignCode = sprintf "store %s %s, %s* %s\n\n"
    typename rvalVar.rvname typename name
  in
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
    sprintf "br %s %s, label %%%s, label %%%s"
      (llvmTypeName `Bool)
      condVar.rvname
      branch.trueLabel.lname
      branch.falseLabel.lname
  in
  (noVar, preCode ^ code)

let gencodeEmbeddedComment gencode comments =
  let commentLines = List.map (fun str -> "; " ^ str) comments in
  (noVar, Common.combine "\n" commentLines)

let rec gencode : Lang.form -> resultvar * string = function
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
  | `EmbeddedComment comments -> gencodeEmbeddedComment gencode comments

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
  let varname = "\"" ^ var.vname ^ "\"" in
  match var.vdefault with
    | StringLiteral value ->
        let contentVar = newGlobalTempVar (`Pointer `Char)
        and escapedValue = llvmEscapedString value
        in
        if not (isValidLlvmString escapedValue) then begin
          raiseCodeGenError ~msg:(sprintf "%s is not a valid llvm string" escapedValue);
        end;
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
    | Int8Val _ | Int16Val _ | Int32Val _ | Int64Val _ | BoolVal _ | CharVal _ ->
        sprintf "@%s = constant %s %s\n"
          varname
          (llvmTypeName var.typ)
          (Lang.valueString var.vdefault)
    | FloatVal f | DoubleVal f ->
        sprintf "@%s = constant %s %s\n"
          varname
          (llvmTypeName var.typ)
          (Machine.float2string f)
    | VoidVal ->
        raiseCodeGenError ~msg:"global constant of type void not allowed"
    | PointerVal _ | FunctionVal _ ->
        raiseCodeGenError ~msg:"global pointers not supported, yet"
    | RecordVal _ ->
        raiseCodeGenError ~msg:"global constant of record type not supported, yet"
    | ArrayVal _ ->
        raiseCodeGenError ~msg:"global constant of array type not supported, yet"

let gencodeDefineFunc func =
  match func.impl with
    | None ->
        let paramTypeNames = List.map (fun (_, typ) -> paramTypeName typ) func.fargs in
        let paramString = combine ", " paramTypeNames in
        let decl =
          sprintf "%s @%s(%s)\n"
            (llvmTypeName func.rettype)
            (escapeName func.fname)
            paramString
        in
        "declare " ^ decl
    | Some impl ->
        let param2string (name, typ) = (paramTypeName typ) ^ " " ^ (llvmName name) in
        let paramString = combine ", " (List.map param2string func.fargs) in
        let decl = sprintf "%s @%s(%s) "
          (llvmTypeName func.rettype) (escapeName func.fname) paramString
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
  let globalTypedefs, globalVars, globalFuncs =
    collectTimingInfo "seperating types, globals, funcs"
      (fun () -> seperateVarsAndFuncs toplevelExprs)
  in
  let headerCode = ""
  and typedefCode =
    collectTimingInfo "typedefs" (fun () -> List.map gencodeTL globalTypedefs )
  and varCode =
    collectTimingInfo "vars" (fun () -> List.map gencodeTL globalVars)
  and funcCode =
    collectTimingInfo "funcs" (fun () -> List.map gencodeTL globalFuncs)
  in
  collectTimingInfo "concatenating text"
    (fun () ->
       combine "" [
         "\n\n\n\n;;; header ;;;\n\n\n\n";
         headerCode; "\n\n";
         (combine "\n\n" typedefCode);
         "\n\n\n\n;;; variables ;;;\n\n\n\n";
         (combine "\n\n" varCode);
         "\n\n\n\n;;; implementation ;;;\n\n\n\n";
         (combine "\n\n" funcCode);
         "\n\n" ^ externalFuncDecls;
       ])

(*        "\n\n\n\n;;; header ;;;\n\n\n\n" *)
(*        ^ headerCode ^ "\n\n" *)
(*        ^ "\n\n\n\n;;; typedefs ;;;\n\n\n\n" *)
(*        ^ (combine "\n\n" typedefCode) *)
(*        ^ "\n\n\n\n;;; variables ;;;\n\n\n\n" *)
(*        ^ (combine "\n\n" varCode) *)
(*        ^ "\n\n\n\n;;; implementation ;;;\n\n\n\n" *)
(*        ^ (combine "\n\n" funcCode) *)
(*        ^ "\n\n" ^ externalFuncDecls *)
(*     ) *)

