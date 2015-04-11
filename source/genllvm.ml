
open Ast2
open Lang
open Types
open Printf
open Bindings
open Common

let combine = Common.combine

exception CodeGenError of string
let raiseCodeGenError ~msg = raise (CodeGenError msg)

type t = unit
let create () = ()

let typeOfForm = Semantic.typeOfForm
  ~onError:(fun ~msg ~found ~expected ->
              raiseCodeGenError ~msg:(sprintf "%s: expected %s but found %s"
                                   msg
                                   (Semantic.typeRequirementToString expected)
                                   (Types.typeName found) ) )

let locationComment loc =
  sprintf ";; %s\n" @@ Basics.locationToString loc

let locationCommentOpt locOpt =
  sprintf ";; %s\n" @@ Basics.locationOptToString locOpt

let countChar str c =
  let count = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = c then incr count
  done;
  !count

let llvmStringLength str =
  let length = String.length str in
  length - 2 * countChar str '\\'

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
  | `TypeRef name -> "%\"" ^ name ^ "\""
  | `Pointer `ParametricType _ ->
    "i8*"
  | `ParametricType t ->
    raiseCodeGenError ~msg:"cannot generate type representation for parametric type"
  | `TypeParam ->
    raiseCodeGenError
      ~msg:"cannot generate type representation for uninstantiated type parameter"
  | `Pointer `Void -> "i8*"
  | `Pointer `TypeParam -> "i8*"
  | `Pointer targetType -> (llvmTypeName targetType) ^ "*"
  | `Array (memberType, size) -> sprintf "[%d x %s]" size (llvmTypeName memberType)
  | `Record record ->
    let componentNames = List.map (fun (_, t) -> llvmTypeName t) record.fields in
    "{ " ^ combine ", " componentNames ^ "}"
  | `Function ft ->
      sprintf "%s (%s)"
        (llvmTypeName ft.returnType)
        (Common.combine ", " (List.map llvmTypeName ft.argTypes))
  | `ErrorType _ as t ->
    raiseCodeGenError
      ~msg:(sprintf "cannot generate type representation for %s"
              (typeName t))

let rec llvmTypeNameLong = function
  | `Record record ->
      let componentNames = List.map (fun (_, t) -> llvmTypeName t) record.fields in
      "{ " ^ combine ", " componentNames ^ "}"
  | _ as other ->
      llvmTypeName other

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

type gencodeResult = {
  gcrVar :resultvar;
  gcrCode :string;
  gcrFirstBBCode :string option;
}
let returnVarCode (var, code) = {
  gcrVar = var;
  gcrCode = code;
  gcrFirstBBCode = None
}
let return (var, code, firstBBCode) = {
  gcrVar = var;
  gcrCode = code;
  gcrFirstBBCode = Some firstBBCode;
}
let returnCombi (var, code, possibleFirstCodes) =
  match possibleFirstCodes with
    | [] -> { gcrVar = var; gcrCode = code; gcrFirstBBCode = None }
    | _ ->
        let firstCode =
          mapFilter (function None -> None | s -> s) possibleFirstCodes |> Common.combine "\n"
        in
        return (var, code, firstCode)

let lastTempVarNum = ref 0
let nextUID () = incr lastTempVarNum; !lastTempVarNum
let newGlobalTempVar, newLocalTempVar =
  let newVar isGlobal ?base (typ :Lang.typ) =
    let id = nextUID () in
    let name =
      match base with
        | Some str -> sprintf "temp_%s_%d" str id
        | None -> sprintf "temp%d" id
    in
    resultVar (variable name typ RegisterStorage isGlobal None)
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
        let num = match chr with
          | '\'' -> 39
          | '"' -> 34
          | '\\' -> 92
          | '0' -> 0
          | 'n' -> 10
          | 'r' -> 13
          | 't' -> 9

          | 'v' -> 11
          | 'a' -> 7
          | 'b' -> 8
          | 'f' -> 12
          | '?' -> 63

          | invalid -> raiseCodeGenError ~msg:(sprintf "cannot escape \\%c" invalid)
        in
        stringToRevCharList (sprintf "\\%02x" num)
      in
      nextChar (pos+1) (chars @ acc)
    else
      raiseCodeGenError ~msg:"string may not end in \\"
  in
  let chars = nextChar 0 [] in
  let stringFromList chars =
    let len = List.length chars in
    let str = String.make len 'x' in
    listIteri (fun pos chr -> Bytes.set str (len - pos - 1) chr) chars;
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
(*          printf "error: llvmEscapedString '%s' => '%s' instead of '%s'\n" *)
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

let sizeT =
  match Sys.word_size with
    | 32 -> `Int32
    | 64 -> `Int64
    | wordSize -> failwith (sprintf "invalid word size %d" wordSize)

let backendInfo = { Builtins.sizeT = sizeT }

let defaultBindings = Builtins.defaultBindings backendInfo

let findIntrinsic =
  let callIntr intrName typ argVarNames =
    sprintf "%s %s %s\n" intrName (llvmTypeName typ) (combine ", " argVarNames)
  in
  let compareIntrinsic typ name cond =
    let instruction =
      match typ with
        | `Float | `Double -> "fcmp"
        | _ -> "icmp"
    in
    let f argVarNames =
      sprintf "%s %s %s %s\n" instruction cond (llvmTypeName typ) (combine ", " argVarNames)
    in
    (name, `Intrinsic f, `Bool, [Lang.funcParam "l" typ; Lang.funcParam "r" typ])
  in

  let twoArgIntrinsic name instruction (typ :typ) =
    name, `Intrinsic (callIntr instruction typ), typ, [Lang.funcParam "l" typ; Lang.funcParam "r" typ]
  in
  let simpleTwoArgIntrinsincs typ namespace names =
    List.map (fun name -> twoArgIntrinsic (sprintf "%s:%s" namespace name) name typ) names
  in
  let mappedTwoArgIntrinsincs typ namespace namePairs =
    List.map (fun (name, llvmName) ->
      twoArgIntrinsic (sprintf "%s:%s" namespace name) llvmName typ) namePairs
  in

  let intCompareIntrinsics typ typeName =
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

        "greater", "ugt";
        "greaterEqual", "uge";
        "less", "ult";
        "lessEqual", "ule";
      ]
    in
    List.map (fun (zompName, llvmName) ->
      compareIntrinsic typ (typeName ^ ":" ^ zompName) llvmName)
      functionMapping
  in

  let intIntrinsics ?name typ =
    let intBinOps =
      ["add"; "sub"; "mul"; "sdiv"; "udiv"; "urem"; "srem"; "and"; "or"; "xor"]
    in
    let name =
      someOrDefault name $ typeName typ
    in

    simpleTwoArgIntrinsincs typ name intBinOps
    @ intCompareIntrinsics typ name
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
    @ mappedTwoArgIntrinsincs typ typeName
      ["add", "fadd"; "sub", "fsub"; "mul", "fmul"; "fdiv", "fdiv"; "frem", "frem"];
  (* TODO: rename fdiv, fmul, frem *)
  in

  let oneArgFunc name f = function
    | [arg] -> f arg
    | _ -> raiseCodeGenError ~msg:(sprintf "only one argument expected by %s" name)
  in

  let convertIntr funcName intrName fromType toType =
    let convertIntrF intrName = oneArgFunc intrName
      (fun arg ->
        sprintf "%s %s %s to %s" intrName (llvmTypeName fromType) arg (llvmTypeName toType))
    in
    funcName, `Intrinsic (convertIntrF intrName), toType, [Lang.funcParam "v" fromType]
  in

  let truncIntIntr fromType toType =
    let name = sprintf "%s:to%s" (typeName fromType) (String.capitalize (typeName toType)) in
    let func = oneArgFunc name
      (fun arg -> sprintf "trunc %s %s to %s" (llvmTypeName fromType) arg (llvmTypeName toType)) in
    name, `Intrinsic func, toType, [Lang.funcParam "v" fromType]
  in

  let zextIntr fromType toType =
    let name = sprintf "%s:zextTo%s"
      (typeName fromType)
      (String.capitalize (typeName toType))
    in
    let func = oneArgFunc name
      (fun arg -> sprintf "zext %s %s to %s" (llvmTypeName fromType) arg (llvmTypeName toType))
    in
    name, `Intrinsic func, toType, [Lang.funcParam "v" fromType]
  in

  let intrinsicFuncs =
    [
      twoArgIntrinsic "u32:shl" "shl" `Int32;
      twoArgIntrinsic "u32:lshr" "lshr" `Int32;
      twoArgIntrinsic "u32:ashr" "ashr" `Int32;

      (** deprecated *)
      convertIntr "float:toInt" "fptosi" `Float `Int32;
      convertIntr "int:toFloat" "sitofp" `Int32 `Float;
      convertIntr "int:toDouble" "sitofp" `Int32 `Double;
      convertIntr "double:toInt" "fptosi" `Double `Int32;
      convertIntr "float:toDouble" "fpext" `Float `Double;
      convertIntr "double:toFloat" "fptrunc" `Double `Float;

      (** deprecated *)
      truncIntIntr `Int32 `Char;
      zextIntr `Char `Int32;

      (** deprecated *)
      truncIntIntr `Int64 `Int32;
      zextIntr `Int32 `Int64;
    ]

    @ intIntrinsics `Int8
    @ intIntrinsics `Int16
    @ intIntrinsics `Int32
    @ intIntrinsics `Int64
    @ intIntrinsics sizeT ~name:Types.sizeTName

    @ simpleTwoArgIntrinsincs `Bool "bool" ["and"; "or"; "xor"]

    @ floatIntrinsics `Float
    @ floatIntrinsics `Double

    @ intCompareIntrinsics `Char (typeName `Char)
  in

  let lookupIntrinsic name =
    let rec find = function
      | [] -> None
      | (intrName, _, _, _) as intr :: _ when name = intrName -> Some intr
      | _ :: tail -> find tail
    in
    find intrinsicFuncs
  in

  let findIntrinsic name =
    match lookupIntrinsic name with
      | Some (_, `Intrinsic gencodeF, _, _) -> Some gencodeF
      | None -> None
  in

  (** Testing whether all built-in functions have intrinsics *)
  let checkIfAllBuiltinFunctionsAreSupported() =
    let checkForIntrinsic func =
      match lookupIntrinsic func.fname with
        | Some (_, _, returnType, args) ->
          begin
            let argsEqual { Lang.typ = funcType } { Lang.typ = intrType } =
              funcType = intrType
            in
            if returnType = func.rettype && List.for_all2 argsEqual func.fargs args then
              None
            else
              Some (sprintf "built-in function %s has non-matching types in LLVM backend." $ Lang.funcDeclToString func)
          end
        | None ->
          Some (sprintf "built-in function %s is not supported by LLVM backend" func.fname)
    in
    let errors = mapFilter checkForIntrinsic (Builtins.builtinIntrinsics backendInfo) in
    if errors <> [] then begin
      List.iter 
        (printf "error: %s\n")
        errors;

      printf "Supported intrinsics:\n";
      List.iter (fun (name, _, _, _) -> printf "  %s\n" name) intrinsicFuncs;

      failwith "internal error"
    end
  in
  checkIfAllBuiltinFunctionsAreSupported();

  findIntrinsic

let gencodeSequence gencode exprs =
  let rec lastVarAndCode var code firstBBCode = function
    | [] -> var, List.rev code, List.rev firstBBCode
    | expr :: tail ->
        let result = gencode expr in
        lastVarAndCode
          result.gcrVar
          (result.gcrCode :: code)
          (result.gcrFirstBBCode :: firstBBCode)
          tail
  in
  let resultVar, code, firstBBCode = lastVarAndCode noVar [] [] exprs in
  returnCombi (resultVar, Common.indent (combine "\n\n" code), firstBBCode)

let gencodeDefineVariable gencode var default =
  match var.vstorage with
    | MemoryStorage ->
        begin
          let typename = llvmTypeName var.typ
          and ptrname = (llvmName var.vname)
          in
          let comment = sprintf "; allocating var %s : %s/%s on stack\n\n"
            var.vname
            (typeName var.typ)
            typename
          in
          let init =
            match default with
              | Some expr -> gencode expr
              | None -> returnVarCode (noVar, "")
          in
          let initVar, initVarCode = init.gcrVar, init.gcrCode in
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
          returnCombi (noVar, comment ^ allocCode, [init.gcrFirstBBCode])
        end
    | RegisterStorage ->
        begin
          let zeroElement = function
            | `Pointer _ | `Function _ -> Some "null"
            | `Record _ | `TypeRef _ | `Array _ -> None
            | `TypeParam | `ParametricType _ -> None
            | `ErrorType _ -> None
            | #integralType as t -> Some (valueString (defaultValue t))
          in
          let initInstr = function
            | `Int8 | `Int16 | `Int32 | `Int64 | `Float | `Pointer _ -> "add"
            | `Bool -> "or"
            | _ as t -> raiseCodeGenError
                ~msg:(sprintf "no init instruction implemented for %s" (typeName t))
          in
          let name = llvmName var.vname
          and typ = llvmTypeName var.typ
          in
          let comment = sprintf "; defining var %s : %s\n\n" var.vname typ in
          let code, firstBBCode =
            match var.typ with
              | `Pointer _ | `Record _ -> begin
                  raiseCodeGenError ~msg:"code gen for pointers and records with register storage not supported, yet"
                end
              | _ -> begin
                  match zeroElement var.typ, default with
                    | Some zeroElementStr, Some expr ->
                        let init = gencode expr in
                        (init.gcrCode ^
                          sprintf "%s = %s %s %s, %s\n\n"
                          name
                          (initInstr var.typ)
                          typ
                          zeroElementStr
                          init.gcrVar.rvname,
                         init.gcrFirstBBCode)
                    | _, _ ->
                        (sprintf "%s = alloca %s\n\n" name typ, None)
                end
          in
          returnCombi (noVar, comment ^ code, [firstBBCode])
        end

let gencodeVariable v =
  let typeName = llvmTypeName v.typ in
  match v.vstorage with
    | RegisterStorage ->
        returnVarCode (resultVar v, "")
    | MemoryStorage ->
        let comment = sprintf "; accessing %s : %s\n\n" v.vname typeName in
        let localVar, localName =
          let v = newLocalTempVar v.typ in
          v, v.rvname
        in
        let code =
          sprintf "%s = load %s* %s\n\n" localName typeName (resultVar v).rvname
        in
        returnVarCode (localVar, comment ^ code)

let rec llvmValue c =
  match c with
    | Int8Val _ | Int16Val _ | Int32Val _ | Int64Val _ | BoolVal _ | CharVal _ ->
      valueString c
    | FloatVal f | DoubleVal f ->
      Machine.float2string f
    | RecordVal (name, fields) ->
      let fieldStrings = List.map (fun (_, fieldValue) ->
        let fieldType = typeOf fieldValue in
        llvmTypeName fieldType ^ " " ^ llvmValue fieldValue) fields
      in
      "{ " ^ Common.combine ", " fieldStrings ^ " }"
    | NullpointerVal _ ->
      "null"
    | VoidVal | StringLiteral _ | ArrayVal _ | ErrorVal _ ->
      raiseCodeGenError ~msg:(sprintf "constants of type %s not supported"
                                (typeName (typeOf c)))

let gencodeConstant c =
  returnVarCode (
    {
      rvname = llvmValue c;
      rvtypename = llvmTypeName (typeOf c);
    },
    "")

let gencodeFuncCall (gencode : Lang.form -> gencodeResult) call =
  let rec varsAndCode vars code firstBBCode = function
    | [] -> vars, code, firstBBCode
    | expr :: tail ->
        let result = gencode expr in
        varsAndCode
          (vars @ [result.gcrVar.rvname, result.gcrVar.rvtypename])
          (code ^ result.gcrCode)
          (firstBBCode @ [result.gcrFirstBBCode])
          tail
  in
  let vars, argevalCode, firstBBCode = varsAndCode [] "" [] call.fcargs in
  let resultVar = newLocalTempVar call.fcrettype in
  let recordTempVar = newLocalTempVar call.fcrettype in
  let assignResultCode, assignResultCodeFirstBB =
    match call.fcrettype with
      | `Record _ ->
          "",
          Some (sprintf "%s = alloca %s\n" recordTempVar.rvname recordTempVar.rvtypename)
      | _ ->
          (if resultVar.rvtypename = "void" then ""
          else sprintf "%s = " resultVar.rvname),
          None
  in
  match findIntrinsic call.fcname with
    | None ->
        let comment, funccallCode =
          let signatureString =
            let argTypeNames = List.map paramTypeName call.fcparams in
            let argTypeNamesWRet =
              match call.fcrettype with
                | `Record _ ->
                    sprintf "%s*" (llvmTypeName call.fcrettype) :: argTypeNames
                | _ -> argTypeNames
            in
            combine ", " argTypeNamesWRet
          in
          let argString =
            let toTypeAndArg (name, typename) =
              typename ^ " " ^ name
            in
            let typeAndArgs = List.map toTypeAndArg vars in
            match call.fcrettype with
              | `Record _ ->
                  combine ", " ((recordTempVar.rvtypename ^ "* " ^ recordTempVar.rvname) :: typeAndArgs)
              | _ ->
                  combine ", " typeAndArgs
          in
          let comment =
            sprintf "; calling function %s(%s%s)\n\n" call.fcname argString (if call.fcvarargs then ", ..." else "")
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
          let loadRecordCode =
            match call.fcrettype with
              | `Record _ ->
                  sprintf "%s = load %s* %s\n"
                    resultVar.rvname recordTempVar.rvtypename recordTempVar.rvname
              | _ ->
                  ""
          in
          comment,
          (loadCode
           ^ assignResultCode
           ^ (sprintf "call %s (%s%s)* %s(%s)\n\n"
                (match call.fcrettype with
                   | `Record _ -> "void"
                   | _ -> llvmTypeName call.fcrettype)
                signatureString
                (if call.fcvarargs then ", ..." else "")
                calleeName
                argString)
           ^ loadRecordCode)
        in
        returnCombi (resultVar,
                     comment ^ argevalCode ^ funccallCode,
                     firstBBCode @ [assignResultCodeFirstBB])
    | Some generateCodeForCall ->
        assert( call.fcptr = `NoFuncPtr );
        let comment = sprintf "; calling intrinsic %s\n\n" call.fcname in
        let intrinsicCallCode =
          assignResultCode ^ (generateCodeForCall (List.map fst vars))
        in
        returnCombi (
          resultVar,
          comment ^ argevalCode ^ "\n\n" ^ intrinsicCallCode,
          firstBBCode @ [assignResultCodeFirstBB])

let offsetStringAndCode gencode countForm =
  match countForm with
    | `Constant Int32Val count ->
        Int32.to_string count, ""
    | `Variable var ->
        let valueVar, valueAccessCode = gencode (`Variable (var :> typ variable)) in
        valueVar.rvname, valueAccessCode
    | _ -> raiseCodeGenError ~msg:"invalid expression for count"

let checkType resultVar typ =
  if llvmTypeName typ <> resultVar.rvtypename then
    raiseCodeGenError ~msg:(sprintf "internal error: expected %s to be of type %s instead of %s"
                              resultVar.rvname (llvmTypeName typ) resultVar.rvtypename)

let todoBindings = defaultBindings

let gencodeGenericIntr (gencode : Lang.form -> gencodeResult) = function
  | `MallocIntrinsic (_, typ, countForm) ->
      begin
        let count = gencode countForm in
        checkType count.gcrVar `Int32;
        let var = newLocalTempVar (`Pointer typ) in
        let code = sprintf "%s = malloc %s, i32 %s" var.rvname (llvmTypeName typ) count.gcrVar.rvname in
        returnVarCode (var, count.gcrCode ^ code)
      end
  | `SizeofIntrinsic (_, typ) ->
     begin
       let addr = newLocalTempVar (`Pointer typ) in
       let size = newLocalTempVar sizeT in
       let addrCode = sprintf "%s = getelementptr %s* null, %s 1\n" addr.rvname (llvmTypeName typ) (llvmTypeName sizeT) in
       let sizeCode = sprintf "%s = ptrtoint %s* %s to %s\n" size.rvname (llvmTypeName typ) addr.rvname (llvmTypeName sizeT) in
       returnVarCode (size, addrCode ^ sizeCode)
     end
  | `GetAddrIntrinsic (_, var) ->
      begin
        match var.vstorage with
          | MemoryStorage -> returnVarCode (
              resultVar (Lang.varWithType var (`Pointer var.typ)),
              sprintf "; addrOf %s\n" (typeName var.typ))
          | RegisterStorage ->
              raiseCodeGenError ~msg:"getting address of register storage var not possible"
      end
  | `StoreIntrinsic (_, ptrForm, valueForm) ->
      begin
        let ptr = gencode ptrForm in
        let value = gencode valueForm in
        let code =
          sprintf "store %s %s, %s %s\n\n"
            value.gcrVar.rvtypename value.gcrVar.rvname ptr.gcrVar.rvtypename ptr.gcrVar.rvname
        in
        returnCombi (noVar, ptr.gcrCode ^ value.gcrCode ^ code, [ptr.gcrFirstBBCode; value.gcrFirstBBCode])
      end
  | `LoadIntrinsic (_, expr) ->
      begin
        let targetType =
          match typeOfForm todoBindings expr with
            | `Pointer targetType -> targetType
            | nonPointerType ->
                raiseCodeGenError ~msg:("expected pointer argument instead of "
                                        ^ (typeName nonPointerType))
        in
        let ptr = gencode expr in
        let resultVar = newLocalTempVar targetType in
        let comment = sprintf "; loading %s\n\n" ptr.gcrVar.rvtypename in
        let code =
          sprintf "%s = load %s %s\n\n" resultVar.rvname ptr.gcrVar.rvtypename ptr.gcrVar.rvname
        in
        returnCombi (resultVar, comment ^ ptr.gcrCode ^ "\n\n" ^ code, [ptr.gcrFirstBBCode])
      end
  | `GetFieldPointerIntrinsic (_, recordForm, fieldName) ->
      begin
        let fieldType, fieldIndex =
          match typeOfForm todoBindings recordForm with
            | `Pointer `Record record ->
                let fieldType = match componentType record.fields fieldName with
                  | Some fieldType -> fieldType
                  | None -> raiseCodeGenError ~msg:
                      (sprintf "could not find field %s" fieldName)
                in
                let fieldIndex = componentNum record.fields fieldName in
                fieldType, fieldIndex
            | _ as invalidType -> raiseCodeGenError ~msg:
                (sprintf "expected pointer to record instead of %s" (typeName invalidType))
        in
        let ptrVar = newLocalTempVar (`Pointer fieldType) in
        let record = gencode recordForm in
        let comment = sprintf "; obtaining address of %s.%s (type = %s)\n\n"
          record.gcrVar.rvname fieldName record.gcrVar.rvtypename
        in
        let code = sprintf "%s = getelementptr %s %s, i32 0, i32 %d\n\n"
          ptrVar.rvname
          record.gcrVar.rvtypename
          record.gcrVar.rvname
          fieldIndex
        in
        returnCombi (ptrVar, comment ^ record.gcrCode ^ code, [record.gcrFirstBBCode])
      end
  | `PtrAddIntrinsic (_, ptrForm, offsetForm) ->
      begin
        let ptrType = typeOfForm todoBindings ptrForm in
        let resultVar = newLocalTempVar ptrType in
        let comment = sprintf "; ptr.add\n\n" in
        let ptr = gencode ptrForm in
        let offset = gencode offsetForm in
        let code =
          sprintf "%s = getelementptr %s %s, i32 %s\n\n"
            resultVar.rvname ptr.gcrVar.rvtypename ptr.gcrVar.rvname offset.gcrVar.rvname
        in
        returnCombi (
          resultVar,
          comment ^ ptr.gcrCode ^ offset.gcrCode ^ code,
          [ptr.gcrFirstBBCode; offset.gcrFirstBBCode])
      end
  | `PtrDiffIntrinsic (_, lhsForm, rhsForm) ->
    begin
      let llvmCodeRev = ref ([] :string list) in
      let emitCode str =
        llvmCodeRev := str :: !llvmCodeRev
      in
      let getCode() = List.rev !llvmCodeRev in

      let makePtrToInt base llvmPtrVar =
        let resultVar = newLocalTempVar ~base `Int32 in
        emitCode (sprintf "%s = ptrtoint %s %s to i32"
                    resultVar.rvname llvmPtrVar.rvtypename llvmPtrVar.rvname);
        resultVar.rvname;
      in

      let lhsPtrCode = gencode lhsForm in
      emitCode lhsPtrCode.gcrCode;
      let lhsIntVar = makePtrToInt "lhs_ptr_int" lhsPtrCode.gcrVar in

      let rhsPtrCode = gencode rhsForm in
      emitCode rhsPtrCode.gcrCode;
      let rhsIntVar = makePtrToInt "rhs_ptr_int" rhsPtrCode.gcrVar in
      
      let bytesDiffVar = newLocalTempVar ~base:"bytes_diff" `Int32 in
      emitCode (sprintf "%s = sub i32 %s, %s"
        bytesDiffVar.rvname lhsIntVar rhsIntVar);

      let ptrType = typeOfForm todoBindings lhsForm in
      let plusElementPtrVar = newLocalTempVar ~base:"plus_one_elm" ptrType in
      emitCode (sprintf "%s = getelementptr %s %s, i32 1"
        plusElementPtrVar.rvname plusElementPtrVar.rvtypename lhsPtrCode.gcrVar.rvname);

      let plusElementIntVar = makePtrToInt "plus_one_elm_int" plusElementPtrVar in

      let elementSizeVar = newLocalTempVar ~base:"elm_size" `Int32 in
      emitCode (sprintf "%s = sub i32 %s, %s"
        elementSizeVar.rvname plusElementIntVar lhsIntVar);

      let elementDiffVar = newLocalTempVar ~base:"elm_diff" `Int32 in
      emitCode (sprintf "%s = udiv i32 %s, %s"
        elementDiffVar.rvname bytesDiffVar.rvname elementSizeVar.rvname);

      returnCombi (
        elementDiffVar,
        Common.combine "\n" (getCode()),
        [lhsPtrCode.gcrFirstBBCode; rhsPtrCode.gcrFirstBBCode] )
    end
  | `CastIntrinsic (_, targetType, valueForm) ->
      let value = gencode valueForm in
      let valueType = typeOfForm todoBindings valueForm in
      let resultVar = newLocalTempVar targetType in
      let comment = sprintf "; casting to %s\n\n" resultVar.rvtypename in
      let instructionName =
        match valueType, targetType with
          | `Pointer _, `Int32 -> "ptrtoint"
          | `Int32, `Pointer _ -> "inttoptr"
          | `Pointer _, `Pointer _ -> "bitcast"

          | (#intType as source), (#intType as target) ->
            if bitcount target > bitcount source then
              "zext"
            else
              "trunc"

          | `Float, `Double ->
            "fpext"
          | `Double, `Float ->
            "fptrunc"

          | #intType, `Float
          | #intType, `Double ->
            "sitofp"

          | `Float, #intType
          | `Double, #intType ->
            "fptosi"

          | _, _ ->
              raiseCodeGenError ~msg:(sprintf "cannot cast from %s to %s"
                                        (typeName valueType) (typeName targetType))
      in
      let code =
        sprintf "%s = %s %s %s to %s\n"
          resultVar.rvname
          instructionName
          value.gcrVar.rvtypename
          value.gcrVar.rvname
          (llvmTypeName targetType)
      in
      returnCombi (resultVar, comment ^ value.gcrCode ^ "\n" ^ code, [value.gcrFirstBBCode])

let gencodeAssignVar gencode var expr =
  let rval = gencode expr in
  let name = (resultVar var).rvname in
  let typename = llvmTypeName var.typ in
  let comment = sprintf "; assigning new value to %s\n\n" name in
  let assignCode = sprintf "store %s %s, %s* %s\n\n"
    typename rval.gcrVar.rvname typename name
  in
  returnCombi (noVar, comment ^ rval.gcrCode ^ "\n\n" ^ assignCode, [rval.gcrFirstBBCode])

let gencodeReturn gencode (expr :Lang.form) =
  match expr with
    | `Constant (_, VoidVal) ->
      returnCombi (noVar, sprintf "ret void\n\n", [])
    | _ ->
      let expr = gencode expr in
      let comment = sprintf "; return %s\n\n" expr.gcrVar.rvtypename in
      let isValueType name = String.length name > 0 && name <> "void" in
      let retCode =
        if isValueType expr.gcrVar.rvtypename then
          sprintf "ret %s %s\n\n" expr.gcrVar.rvtypename expr.gcrVar.rvname
        else
          sprintf "ret void\n\n"
      in
      returnCombi (noVar,
                   comment ^ expr.gcrCode ^ "\n\n" ^ retCode,
                   [expr.gcrFirstBBCode])

let gencodeJump label =
  let code = sprintf "br label %%%s\n\n" label.lname in
  returnVarCode (noVar, code)

let gencodeLabel label =
  let jumpCode = gencodeJump label in
  let code = sprintf "%s:\n\n" label.lname in
  returnCombi (noVar, jumpCode.gcrCode ^ code, [jumpCode.gcrFirstBBCode])

let gencodeBranch gencode info branch =
  let cond = gencode (`Variable (info, (branch.bcondition :> Lang.typ Lang.variable))) in
  let code =
    sprintf "br %s %s, label %%%s, label %%%s"
      (llvmTypeName `Bool)
      cond.gcrVar.rvname
      branch.trueLabel.lname
      branch.falseLabel.lname
  in
  returnVarCode (noVar, cond.gcrCode ^ code)

let gencodeEmbeddedComment comments =
  let commentLines = List.map (fun str -> "; " ^ str) comments in
  returnVarCode (noVar, Common.combine "\n" commentLines)

let rec gencode : Lang.form -> gencodeResult = function
  | `Sequence (_, exprs) -> (gencodeSequence gencode exprs : gencodeResult)
  | `DefineVariable (_, var, expr) -> gencodeDefineVariable gencode var expr
  | `Variable (_, var) -> gencodeVariable var
  | `Constant (_, c) -> gencodeConstant c
  | `FuncCall (_, call) -> gencodeFuncCall gencode call
  | `Return (_, e) -> gencodeReturn gencode e
  | `Label (_, l) -> gencodeLabel l
  | `Jump (_, l) -> gencodeJump l
  | `Branch (info, b) -> gencodeBranch gencode info b
  | `AssignVar (_, var, expr) -> gencodeAssignVar gencode var expr
  | #genericIntrinsic as intr -> gencodeGenericIntr gencode intr
  | `EmbeddedComment (_, comments) -> gencodeEmbeddedComment comments

let gencodeGlobalVar (_ :t) gvar =
  let var = gvar.gvVar
  and initialValue = gvar.gvInitialValue
  in
  let varname = "\"" ^ var.vname ^ "\"" in
  match initialValue with
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
        sprintf "@%s = global %s %s\n"
          varname
          (llvmTypeName var.typ)
          (valueString initialValue)
    | FloatVal f | DoubleVal f ->
        sprintf "@%s = global %s %s\n"
          varname
          (llvmTypeName var.typ)
          (Machine.float2string f)
    | NullpointerVal _ ->
        sprintf "@%s = global %s null\n"
          varname
          (llvmTypeName var.typ)
    | VoidVal ->
        raiseCodeGenError ~msg:"global constant of type void not allowed"
    | RecordVal (_,[]) ->
        sprintf "@%s = global %s zeroinitializer\n" varname (llvmTypeName var.typ)
    | RecordVal (_, fields) ->
      let valueStrings = List.map
        (fun (_, value) ->
          llvmTypeName (Types.typeOf value)
          ^ " "
          ^ Types.valueString value)
        fields
      in
      sprintf "@%s = global %s { %s }\n" varname (llvmTypeName var.typ)
        (Common.combine "," valueStrings)
    | ArrayVal (memberType, values) ->
        let valueStr =
          if values = [] then
            "zeroinitializer"
          else
            let valueStrings = List.map Types.valueString values in
            "[" ^ Common.combine ", " valueStrings ^ "]"
        in
        sprintf "@%s = global %s %s\n" varname (llvmTypeName var.typ) valueStr
    | ErrorVal _ as value ->
      raiseCodeGenError ~msg:(sprintf "cannot generate global var of type %s"
                                (typeName (typeOf value)))

let gencodeDefineFunc (_ :t) func =
  let makeSignature retvalName paramString =
    match func.rettype with
      | `Record _ ->
          sprintf "void @%s(%s%s%s)\n"
            (escapeName func.fname)
            (sprintf "%s* sret \"%s\""
               (llvmTypeName func.rettype)
               retvalName ^ if String.length paramString > 0 then ", " else "")
            paramString
            (if func.cvarargs then ", ..." else "")
      | _ ->
          sprintf "%s @%s(%s%s)\n"
            (llvmTypeName func.rettype)
            (escapeName func.fname)
            paramString
            (if func.cvarargs then ", ..." else "")
  in
  match func.impl with
    | None ->
        let paramTypeNames = List.map (fun { Lang.typ } -> paramTypeName typ) func.fargs in
        let paramString = combine ", " paramTypeNames in
        let decl = makeSignature "" paramString in
        "declare " ^ decl
    | Some impl ->
        let argumentName name = name ^ "$arg" in
        let param2string { Lang.typ; Lang.vname = name } =
          (paramTypeName typ) ^ " " ^ (llvmName (argumentName name))
        in
        let paramString = combine ", " (List.map param2string func.fargs) in
        let retvalVar = Lang.variable
          ~name:"$retval"
          ~typ:(`Pointer func.rettype)
          ~storage:RegisterStorage
          ~global:false
          ~location:None
        in
        let decl = makeSignature retvalVar.vname paramString in
        let initStructCode =
          Common.combine "\n"
            (List.map
               (fun { Lang.typ; Lang.vname = name } ->
                 sprintf "  %%%s = alloca %s\n" name (llvmTypeName typ)
                 ^ sprintf "  store %s \"%s\", %s* \"%s\"\n"
                   (llvmTypeName typ) (argumentName name)
                   (llvmTypeName typ) name)
               func.fargs)
        in
        let lastOrDefault list default = List.fold_left (fun _ r -> r) default list in
        let lastExpr = function
          | `Sequence (_, exprs) as seq -> lastOrDefault exprs seq
          | _ as expr -> expr
        in
        let rec replaceReturns = function
          | `Sequence (_, forms) -> sequence (List.map replaceReturns forms)
          | `Return (info, form) ->
              sequence [
                `StoreIntrinsic (info, `Variable (info, retvalVar), form);
                `Return (info, `Constant (info, VoidVal))]
          | other -> other (** TODO: correctly transform everything *)
        in
        let impl =
          match func.rettype with
            | `Record _ -> replaceReturns impl
            | _ -> impl
        in
        let result = gencode impl in
        let resultVar, implCode = result.gcrVar, result.gcrCode in
        let impl = match lastExpr impl with
          | `Return _ ->
              sprintf "\n%s\n%s\n" initStructCode implCode
          | _ ->
              let returnsValue =
                match func.rettype with
                  | `Record _ -> false
                  | _ -> String.length resultVar.rvtypename > 0 && resultVar.rvtypename <> "void"
              in
              (sprintf "\n%s\n%s\n\n" initStructCode implCode)
              ^ (if returnsValue then
                   (sprintf "  ret %s %s\n"
                      resultVar.rvtypename
                      resultVar.rvname)
                 else "  ret void\n")
        in
        "define " ^ decl ^ " {\n" ^
          (match result.gcrFirstBBCode with
             | Some s -> ";; firstBBCode\n" ^ s
             | None -> "") ^ "\n" ^ impl ^ "\n}\n"

let gencodeTypedef (_:t) name = function
  | `ParametricType _ ->
      ""
  | typ ->
      sprintf "%%\"%s\" = type %s\n\n" name (llvmTypeNameLong typ)

let deleteBodiesAndCollectNames simpleforms =
  let isDefinedFunction func =
    match func.impl with
      | None -> false
      | Some _ ->
         Machine.zompRemoveFunctionBody func.fname
  in
  let redefinedFunctions = Common.mapFilter (function
    | `DefineFunc func when isDefinedFunction func -> Some func.fname
    | _ -> None) simpleforms
  in
  redefinedFunctions

let gencodeTL backend phase form =
  let llvmIr =
    match form with
      | `GlobalVar var -> gencodeGlobalVar backend var
      | `DefineFunc func -> gencodeDefineFunc backend func
      | `Typedef (name, typ) -> gencodeTypedef backend name typ
  in
  let llvmIrWithLoc =
    if String.length llvmIr = 0 then
      ""
    else
      locationComment (Lang.toplevelFormLocation form) ^ llvmIr
  in
  let code = Zompvm.codeFromLlvm llvmIrWithLoc in
  let functionNames = deleteBodiesAndCollectNames [form] in
  Zompvm.removeFunctionBodies functionNames;
  Zompvm.evalCode phase code;
  Zompvm.relinkFunctions functionNames

