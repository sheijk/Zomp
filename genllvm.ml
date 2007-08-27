
open Ast2
open Lang
open Printf
open Bindings

exception CodeGenError of string
let raiseCodeGenError ~msg = raise (CodeGenError msg)
  
let llvmName name =
  if name.[0] = '%' then name
  else "%" ^ name

let llvmGlobalName name = name
let llvmLocalName = llvmName
  
let isLocalVar name = String.length name <= 2 || name.[1] <> '$'
  
let rec llvmTypeName = function
  | `Void -> "void"
  | `String -> "i8*"
  | `Int -> "i32"
  | `Bool -> "i1"
  | `Pointer targetType -> (llvmTypeName targetType) ^ "*"
  | _ as t -> raiseCodeGenError
      ~msg:(sprintf "Do not know how to generate llvm typename for %s"
              (composedType2String t))
      
type resultvar = {
  rvname :string;
  rvtypename :string;
}

(* let var name typ = { rvname = llvmName name; rvtypename = typ } *)
(* let localVar name typ = { rvname = llvmLocalName name; rvtypename = typ } *)
(* let globalVar name typ = { rvname = llvmGlobalName name; rvtypename = typ } *)

let resultVar var = {
  rvname = if var.vglobal then "@" ^ var.vname else "%" ^ var.vname;
  rvtypename = llvmTypeName var.typ;
}
  
let noVar = { rvname = ""; rvtypename = "" }

let lastTempVarNum = ref 0
let nextUID () = incr lastTempVarNum; !lastTempVarNum
let newGlobalTempVar, newLocalTempVar =
  let newVar isGlobal typ =
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

let defaultBindings, externalFuncDecls, findIntrinsic =
  let callIntr intrName typ argVarNames =
    sprintf "%s %s %s" intrName (llvmTypeName typ) (combine ", " argVarNames)
  in
  let void argVarNames = "" in
  let compareIntrinsicI name cond =
    let f argVarNames = 
(*       sprintf "set%s i32 %s" cond (combine ", " argVarNames) *)
      sprintf "icmp %s i32 %s" cond (combine ", " argVarNames)
    in
    (name, `Intrinsic f, `Int, ["l", `Int; "r", `Int])
  in
  let twoArgIntrinsic name instruction (typ :composedType) =
    name, `Intrinsic (callIntr instruction typ), typ, ["l", typ]
  in

  let intrinsicFuncs = [
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
    
    "printf", `ExternalFunc, `Void, ["test", `String];
    "void", `Intrinsic void, `Void, [];

    compareIntrinsicI "int.equal" "eq";
    compareIntrinsicI "int.notEqual" "ne";
    compareIntrinsicI "int.ugreater" "ugt";
    compareIntrinsicI "int.ugreaterEqual" "uge";
    compareIntrinsicI "int.uless" "ult";
    compareIntrinsicI "int.ulessEqual" "ule";
    compareIntrinsicI "int.sgreater" "sgt";
    compareIntrinsicI "int.sgreaterEqual" "sge";
    compareIntrinsicI "int.sless" "slt";
    compareIntrinsicI "int.slessEqual" "sle";
  ]
  in
  let builtinMacros =
    let macro name f = (name, MacroSymbol { mname = name; mtransformFunc = f; }) in
    let delegateMacro macroName funcName =
      macro macroName (fun args -> { id = funcName; args = args; })
    in
    [
      delegateMacro "op+" "int.add";
      delegateMacro "op+_f" "float.add";
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
  
let indent string =
  let indentLine line =
    let len = String.length line in
    if len >= 1 && line.[len-1] = ':' then line
    else "  " ^ line
  in
  let lines = Str.split (Str.regexp "\n") string in
  let indentedLines = List.map indentLine lines in
  combine "\n" indentedLines

let gencodeSequence gencode exprs =
  let rec lastVarAndCode var code = function
    | [] -> var, code
    | expr :: tail ->
        let resultVar, exprCode = gencode expr in
        lastVarAndCode resultVar (code @ [exprCode]) tail
  in
  let resultVar, code = lastVarAndCode noVar [] exprs in
  resultVar, indent (combine "\n" code)

let gencodeDefineVariable gencode var expr =
  match var.vstorage with
    | RegisterStorage -> begin
        let zeroElement = function
          | `Pointer _ -> "null"
          | `Record _ -> raiseCodeGenError ~msg:"no vars of record type supported, yet"
          | #integralType as t -> integralValue2String (defaultValue t)
        in
        let initInstr = function
          | `Int | `Float | `Pointer _ -> "add"
          | `Bool -> "or"
          | _ as t -> raiseCodeGenError
              ~msg:(sprintf "no init instruction implemented for %s" (composedType2String t))
        in
        let initVar, initVarCode = gencode expr in
        let name = llvmName var.vname
        and typ = llvmTypeName var.typ
        in
        let comment = sprintf "; defining var %s : %s\n" var.vname typ in
        let code =
          match var.typ with
            | `Pointer _ -> begin
                raiseCodeGenError ~msg:"code gen for pointers not supported, yet"
              end
            | _ -> begin
                sprintf "%s = %s %s %s, %s"
                  name
                  (initInstr var.typ)
                  typ
                  (zeroElement var.typ)
                  initVar.rvname
              end
        in
        (noVar, comment ^ initVarCode ^ "\n" ^ code)
      end
    | MemoryStorage -> begin
        let comment = sprintf "; allocating var %s on stack" var.vname in
        let typename = llvmTypeName var.typ
        and ptrname = (llvmName var.vname)
        and initVar, initVarCode = gencode expr
        in
        let allocCode =
          (sprintf "%s = alloca %s\n" ptrname typename)
          ^ (sprintf "store %s %s, %s* %s" typename initVar.rvname typename ptrname)
        in
        (noVar, comment ^ initVarCode ^ "\n" ^ allocCode)
      end

let gencodeVariable v =
  let typeName = llvmTypeName v.typ in
  match v.vstorage with
    | RegisterStorage ->
        (resultVar v, "")
    | MemoryStorage ->
        let comment = sprintf "; accessing %s : %s\n" v.vname typeName in
        let localVar, localName =
          let v = newLocalTempVar v.typ in
          v, v.rvname
        in
        let code =
          sprintf "%s = load %s* %s\n" localName typeName (resultVar v).rvname
        in
        (localVar, comment ^ code)

let gencodeConstant c =
  {
    rvname = integralValue2String c;
    rvtypename = llvmTypeName (integralValue2Type c);
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
            let argTypeNames = List.map llvmTypeName call.fcparams in
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
            sprintf "; calling function %s(%s)\n" call.fcname argString
          in
          comment,
          (assignResultCode
           ^ (sprintf "call %s (%s)* @%s(%s)"
                (llvmTypeName call.fcrettype)
                signatureString
                call.fcname
                argString))
        in
        (resultVar, comment ^ argevalCode ^ funccallCode)
    | Some gencallCodeF ->
        let comment = sprintf "; calling intrinsic %s\n" call.fcname in
        let intrinsicCallCode =
          assignResultCode ^ (gencallCodeF vars)
        in
        (resultVar, comment ^ argevalCode ^ intrinsicCallCode)

let gencodeIfThenElse gencode ite =
  let condVar, condCode = gencode ite.cond
  and trueVar, trueCode = gencode ite.trueCode
  and falseVar, falseCode = gencode ite.falseCode
  in
  let comment = "; if/then/else" in
  let resultTypeName = trueVar.rvtypename in
  let id = nextUID () in
  let trueLabel = sprintf "true_cond%d" id
  and falseLabel = sprintf "false_cond%d" id
  and continueLabel = sprintf "continue%d" id
  and resultVarName = sprintf "%%iteResult%d" id
  in
  ( {rvname = llvmName resultVarName; rvtypename = resultTypeName},
   comment ^ "\n" ^
     condCode ^ "\n" ^
     resultVarName ^ "_ptr = alloca " ^ resultTypeName ^ "\n" ^
     "br i1 " ^ condVar.rvname ^ ", label %" ^ trueLabel ^ ", label %" ^ falseLabel ^ "\n" ^
     trueLabel ^ ":\n" ^ 
     trueCode ^ "\n" ^ 
     "store " ^ resultTypeName ^ " " ^ trueVar.rvname ^ ", " ^ resultTypeName ^ "* " ^ resultVarName ^ "_ptr\n" ^
     "br label %" ^ continueLabel ^ "\n" ^
     falseLabel ^ ":\n" ^
     falseCode ^ "\n" ^ 
     "store " ^ resultTypeName ^ " " ^ falseVar.rvname ^ ", " ^ resultTypeName ^ "* " ^ resultVarName ^ "_ptr\n" ^
     "br label %" ^ continueLabel ^ "\n" ^
     continueLabel ^ ":\n" ^
     resultVarName ^ " = load " ^ resultTypeName ^ "* " ^ resultVarName ^ "_ptr"
  )

    
let gencodeLoop gencode l =
  let preVar, preCode = gencode l.preCode
  and abortVar, abortCode = gencode l.abortTest
  and _, postCode = gencode l.postCode
  in
  let id = nextUID() in
  let beginLabel = sprintf "loop_begin%d" id
  and endLabel = sprintf "loop_end%d" id
  and centerLabel = sprintf "loop_center%d" id
  in
  let comment = sprintf "; loop %d\n" id in
  (preVar,
   comment
   ^ "br label %" ^ beginLabel ^ "\n"
   ^ beginLabel ^ ":\n"
   ^ preCode ^ "\n"
   ^ "br i1 " ^ abortVar.rvname ^ ", label %" ^ endLabel ^ ", label %" ^ centerLabel ^ "\n"
   ^ centerLabel ^ ":\n"
   ^ postCode ^ "\n"
   ^ "br label %" ^ beginLabel ^ "\n"
   ^ endLabel ^ ":\n"
  )

let gencodeAssignVar gencode var expr =
  let rvalVar, rvalCode = gencode expr in
  let name = (resultVar var).rvname in
  let typename = llvmTypeName var.typ in
  let comment = sprintf "; assigning new value to %s" name in
  let assignCode = sprintf "store %s %s, %s* %s" typename rvalVar.rvname typename name in
  (noVar, comment ^ rvalCode ^ "\n" ^ assignCode)
  
let rec gencode : Lang.expr -> resultvar * string = function
  | Sequence exprs -> gencodeSequence gencode exprs
  | DefineVariable (var, expr) -> gencodeDefineVariable gencode var expr
  | Variable var -> gencodeVariable var
  | Constant c -> gencodeConstant c
  | FuncCall call -> gencodeFuncCall gencode call
  | IfThenElse ite -> gencodeIfThenElse gencode ite
  | Loop l -> gencodeLoop gencode l
  | AssignVar (var, expr) -> gencodeAssignVar gencode var expr
      
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
    | StringVal value ->
        let contentVar = newGlobalTempVar `String
        and escapedValue = llvmEscapedString value
        in
        let length = llvmStringLength escapedValue + 1 in
        let stringStorageSrc =
          sprintf "%s = internal constant [%d x i8] c\"%s\\00\"\n"
            contentVar.rvname
            length
            escapedValue
        in
        let stringPointerSrc =
          sprintf "@%s = global i8* getelementptr ([%d x i8]* %s, i32 0, i32 0)\n"
            varname
            length
            contentVar.rvname
        in
        stringStorageSrc ^ stringPointerSrc
    | IntVal _ | BoolVal _ | FloatVal _ ->
        sprintf "@%s = constant %s %s"
          varname
          (llvmTypeName var.typ)
          (integralValue2String var.default)
    | VoidVal ->
        raiseCodeGenError ~msg:"global constant of type void not allowed"
    | PointerVal _ ->
        raiseCodeGenError ~msg:"global pointers not supported, yet"
    | RecordVal _ ->
        raiseCodeGenError ~msg:"global constant of record type not supported, yet"

let gencodeDefineFunc func =
  let param2string (name, typ) = (llvmTypeName typ) ^ " " ^ (llvmName name) in
  let paramString = combine ", " (List.map param2string func.fargs) in
  let decl = sprintf "%s @%s(%s) "
    (llvmTypeName func.rettype) func.fname paramString
  in
  match func.impl with
    | None -> "declare " ^ decl
    | Some impl ->
        let resultVar, implCode = gencode impl in
        let isTypeName name = String.length name > 0 && name <> "void" in
        let impl =
          (sprintf "{\n%s\n" implCode)
          ^ (if isTypeName resultVar.rvtypename then
               (sprintf "  ret %s %s\n}"
                  resultVar.rvtypename
                  resultVar.rvname)
             else "  ret void\n}")
        in
        "define " ^ decl ^ impl

let gencodeTL = function
  | GlobalVar var -> gencodeGlobalVar var
  | DefineFunc func -> gencodeDefineFunc func

let genmodule toplevelExprs =
  let rec sort = function
    | [] -> ([], [])
    | expr :: tail ->
        let vars, funcs = sort tail in
        match expr with
          | GlobalVar var -> (GlobalVar var :: vars, funcs)
          | DefineFunc func -> (vars, DefineFunc func :: funcs)
  in
  let globalVars, globalFuncs = sort toplevelExprs in
  let headerCode = ""
  and varCode = List.map gencodeTL globalVars
  and funcCode = List.map gencodeTL globalFuncs
  in
  headerCode ^ "\n"
  ^ (combine "\n" varCode)
  ^ "\n\n;;; implementation ;;;\n\n"
  ^ (combine "\n" funcCode)
  ^ "\n" ^ externalFuncDecls
    
