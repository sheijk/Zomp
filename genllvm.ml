
open Ast2
open Lang
open Printf
open Bindings

exception CodeGenError of string
let raiseCodeGenError ~msg = raise (CodeGenError msg)
  
let llvmName name =
  if name.[0] = '%' then name
  else "%" ^ name

let llvmGlobalVar = llvmName
let llvmLocalName = llvmName
  
let isLocalVar name = String.length name <= 2 || name.[1] <> '$'
  
let llvmTypeName = function
  | `String -> "sbyte*"
  | _ as t -> composedType2String t
      
type resultvar = {
  rvname :string;
  rvtypename :string;
}

let var name typ = { rvname = llvmName name; rvtypename = typ }
let localVar name typ = { rvname = llvmLocalName name; rvtypename = typ }
  
let noVar = { rvname = ""; rvtypename = "" }

let lastTempVarNum = ref 0
let newGlobalTempVar, newLocalTempVar =
  let newVar constructor typ = 
    incr lastTempVarNum;
    constructor (sprintf "temp%d" !lastTempVarNum) typ
  in
  newVar localVar, newVar var

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
    sprintf "%s %s %s" intrName (composedType2String typ) (combine ", " argVarNames)
  in
  let void argVarNames = "" in
  let twoArgIntr name instruction (typ :composedType) =
    name, `Intrinsic (callIntr instruction typ), typ, ["l", typ]
  in
  let intrinsicFuncs = [
    twoArgIntr "int.add" "add" `Int;
    twoArgIntr "int.sub" "sub" `Int;
    twoArgIntr "int.mul" "mul" `Int;
    twoArgIntr "int.sdiv" "sdiv" `Int;
    twoArgIntr "int.udiv" "udiv" `Int;
    twoArgIntr "int.urem" "urem" `Int;
    twoArgIntr "int.srem" "srem" `Int;
    
(*     twoArgIntr "int.shl" "shl" `Int; *)
(*     twoArgIntr "int.lshr" "lshr" `Int; *)
(*     twoArgIntr "int.ashr" "ashr" `Int; *)
    
    twoArgIntr "int.and" "and" `Int;
    twoArgIntr "int.or" "or" `Int;
    twoArgIntr "int.xor" "xor" `Int;

    twoArgIntr "float.add" "add" `Float;
    twoArgIntr "float.sub" "sub" `Float;
    twoArgIntr "float.mul" "mul" `Float;
    twoArgIntr "float.fdiv" "fdiv" `Float;
    twoArgIntr "float.frem" "frem" `Float;
    
    "printf", `ExternalFunc, `Void, ["test", `String];
    "void", `Intrinsic void, `Void, [];
  ]
  in
  let defaultBindings =
    let toFunc (name, _, typ, args) =
      name, FuncSymbol (funcDecl name typ args)
    in
    List.map toFunc intrinsicFuncs
  in
  let externalFuncDecls =
    let rec defs = function
      | [] -> []
      | (name, `ExternalFunc, rettype, args) :: tail ->
          let decl = 
            sprintf "declare %s %%%s(%s)"
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
  let lines = Str.split (Str.regexp "\n") string in
  let indentedLines = List.map (fun s -> "  " ^ s) lines in
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
  let zeroElement = function
    | `Pointer _ -> "nullptr"
    | #integralType as t -> integralValue2String (defaultValue t)
  in
  let initVar, initVarCode = gencode expr in
  let name = llvmName var.vname
  and typ = composedType2String var.typ
  in
  let comment = sprintf "; defining var %s : %s\n" var.vname typ in
  let code = sprintf "%s = add %s %s, %s"
    name typ
    (zeroElement var.typ)
    initVar.rvname
  in
  (noVar, comment ^ initVarCode ^ "\n" ^ code)

let isPointerTypeName typName =
  let len = String.length typName in
  len >= 1 && typName.[len-1] = '*'
  
let gencodeVariable v =
  let typeName = llvmTypeName v.typ in
  if v.vglobal = false then 
    (var v.vname typeName, "")
  else
    let comment = sprintf "; accessing %s : %s\n" v.vname typeName in
    let localName =
      let v = newLocalTempVar typeName in
      v.rvname
    in
    let code =
      sprintf "%s = load %s* %s\n" localName typeName (llvmName v.vname)
    in
    (localVar localName typeName, comment ^ code)

let gencodeConstant c =
  {
    rvname = integralValue2String c;
    rvtypename = integralType2String (integralValue2Type c);
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
  let resultVar = newLocalTempVar (composedType2String call.fcrettype) in
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
          let argString = paramString (List.combine vars call.fcparams) in
          let comment =
            sprintf "; calling function %s(%s)\n" call.fcname argString
          in
          comment,
          (assignResultCode
           ^ (sprintf "call %s (%s)* %%%s(%s)"
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
  noVar,
  (sprintf "; if/then/else not supported, yet\n")
  ^ (sprintf "%s\n%s\n%s\n if %s then %s else %s\n"
       condCode trueCode falseCode
       condVar.rvname trueVar.rvname falseVar.rvname)
    
let gencodeLoop gencode l =
  let _, preCode = gencode l.preCode
  and abortVar, abortCode = gencode l.abortTest
  and resultVar, postCode = gencode l.postCode
  in
  resultVar,
  ("; loop not supported, yet\n")
  ^ (sprintf "start:\n%s\n%s\nbreak if %s\n%s"
       preCode abortCode abortVar.rvname postCode)

(* let gencodeAssignVar gencode var expr = *)
(*   let exprResultVar, exprCode = gencode expr in *)
(*   let comment = sprintf "; assigning %s : %s\n" var.vname (composedType2String var.typ) in *)
(*   let assignCode = sprintf "%s = add %s %s, 0" *)
(*     (llvmName var.vname) *)
(*     (composedType2String var.typ) *)
(*     exprResultVar.rvname *)
(*   in *)
(*   (noVar, comment ^ exprCode ^ "\n" ^ assignCode) *)
  
let rec gencode : Lang.expr -> resultvar * string = function
  | Sequence exprs -> gencodeSequence gencode exprs
  | DefineVariable (var, expr) -> gencodeDefineVariable gencode var expr
  | Variable var -> gencodeVariable var
(*   | AssignVar (var, expr) -> gencodeAssignVar gencode var expr *)
  | Constant c -> gencodeConstant c
  | FuncCall call -> gencodeFuncCall gencode call
  | IfThenElse ite -> gencodeIfThenElse gencode ite
  | Loop l -> gencodeLoop gencode l
      
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
  let varname = llvmGlobalVar var.vname in
  match var.default with
    | StringVal value ->
        let contentVar = newGlobalTempVar "string"
        and escapedValue = llvmEscapedString value
        in
        let length = llvmStringLength escapedValue + 1 in
        let stringStorageSrc =
          sprintf "%s = internal constant [%d x sbyte] c\"%s\\00\"\n"
            contentVar.rvname
            length
            escapedValue
        in
        let stringPointerSrc =
          sprintf "%s = global sbyte* getelementptr ([%d x sbyte]* %s, int 0, int 0)\n"
            varname
            length
            contentVar.rvname
        in
        stringStorageSrc ^ stringPointerSrc
    | IntVal _ | BoolVal _ | FloatVal _ ->
        sprintf "%s = constant %s %s"
          varname
          (composedType2String var.typ)
          (integralValue2String var.default)
    | VoidVal ->
        raiseCodeGenError ~msg:"global constant of type void not allowed"

let gencodeDefineFunc func =
  let param2string (name, typ) = (llvmTypeName typ) ^ " " ^ (llvmName name) in
  let paramString = combine ", " (List.map param2string func.fargs) in
  let decl = sprintf "%s %%%s(%s) "
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
        decl ^ impl

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
  ^ "\n\nimplementation\n\n"
  ^ (combine "\n" funcCode)
  ^ "\n" ^ externalFuncDecls
    
