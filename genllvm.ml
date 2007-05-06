
open Ast2
open Lang
open Printf
open Bindings
  
let llvmName name =
  if name.[0] = '%' then name
  else "%" ^ name
    
let llvmLocalName name = "%$" ^ name
let isLocalVar name = String.length name > 2 && name.[1] = '$'
  
let llvmTypeName = function
  | String -> "sbyte*"
  | _ as t -> integralType2String t
      
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

let paramString signature =
  let argsWithTypes =
    List.map
      (fun (name, typ) -> (llvmTypeName typ) ^ " " ^ llvmName name)
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
    

let defaultBindings, externalFuncDecls, findIntrinsinc =
  let callAdd typ argVarNames =
    sprintf "add %s %s" (integralType2String typ) (combine ", " argVarNames)
  in
  let intrinsincFuncs = [
    "std_plusi", `Intrinsinc (callAdd Int), Int, ["l", Int; "r", Int];
    "printf", `ExternalFunc, Int, ["test", String];
  ]
  in
  let defaultBindings =
    let toFunc (name, _, typ, args) =
      name, FuncSymbol (func name typ args (Sequence []))
    in
    List.map toFunc intrinsincFuncs
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
    combine "\n" (defs intrinsincFuncs)
  in
  let findIntrinsinc name =
    let rec find = function
      | [] -> None
      | (intrName, `Intrinsinc gencodeF, _, _) :: _ when name = intrName -> Some gencodeF
      | _ :: tail -> find tail
    in
    find intrinsincFuncs
  in
  defaultBindings, externalFuncDecls, findIntrinsinc
  
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

let gencodeDefineVariable var =
  let name = llvmLocalName var.vname
  and typ = integralType2String var.typ
  and value = integralValue2String var.default
  in
  let code =
    (sprintf "%s = alloca %s\n" name typ) ^
      (sprintf "store %s %s, %s* %s" typ value typ name)
  in
  (noVar, code)
    
let gencodeVariable v =
  let typeName = llvmTypeName v.typ in
  if isLocalVar v.vname then 
    var v.vname typeName, ""
  else
    let localName = llvmLocalName v.vname in
    let code =
      sprintf "%s = load %s* %s\n" localName typeName (llvmName v.vname)
    in
    localVar v.vname typeName, code

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
  let resultVar = newLocalTempVar (integralType2String call.fcrettype) in
  match findIntrinsinc call.fcname with
    | None ->
        let funccallCode =
          let signatureString =
            let argTypeNames = List.map llvmTypeName call.fcparams in
            combine ", " argTypeNames
          in
          let argString = paramString (List.combine vars call.fcparams) in
          sprintf "%s = call %s (%s)* %%%s(%s)"
            resultVar.rvname
            (llvmTypeName call.fcrettype)
            signatureString
            call.fcname
            argString
        in
        (resultVar, argevalCode ^ funccallCode)
    | Some gencallCodeF ->
        let intrinsincCallCode =
          sprintf "%s = %s"
            resultVar.rvname
            (gencallCodeF vars)
        in
        (resultVar, argevalCode ^ intrinsincCallCode)

let gencodeIfThenElse gencode ite =
  let condVar, condCode = gencode ite.cond
  and trueVar, trueCode = gencode ite.trueCode
  and falseVar, falseCode = gencode ite.falseCode
  in
  noVar,
  sprintf "%s\n%s\n%s\n if %s then %s else %s\n"
    condCode trueCode falseCode
    condVar.rvname trueVar.rvname falseVar.rvname
    
let gencodeLoop gencode l =
  let _, preCode = gencode l.preCode
  and abortVar, abortCode = gencode l.abortTest
  and resultVar, postCode = gencode l.postCode
  in
  resultVar,
  sprintf "start:\n%s\n%s\nbreak if %s\n%s"
    preCode abortCode abortVar.rvname postCode
  
let rec gencode : Lang.expr -> resultvar * string = function
  | Sequence exprs -> gencodeSequence gencode exprs
  | DefineVariable var -> gencodeDefineVariable var
  | Variable var -> gencodeVariable var
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
  match var.default with
    | StringVal value ->
        let contentVar = newGlobalTempVar "string"
        and escapedValue = llvmEscapedString value
        in
        let length = llvmStringLength escapedValue + 1 in
        (sprintf "%s = internal constant [%d x sbyte] c\"%s\\00\"\n"
           contentVar.rvname
           length
           escapedValue
        ) ^ (sprintf "%%%s = global sbyte* getelementptr ([%d x sbyte]* %s, int 0, int 0)\n"
               var.vname
               length
               contentVar.rvname)
    | IntVal _ | BoolVal _ | FloatVal _ ->
        sprintf "%%%s = constant %s %s"
          var.vname
          (integralType2String var.typ)
          (integralValue2String var.default)

let gencodeDefineFunc func =
  let resultVar, implCode = gencode func.impl in
  let param2string (name, typ) = (integralType2String typ) ^ " " ^ (llvmName name) in
  let paramString = combine ", " (List.map param2string func.fargs) in
  let decl = sprintf "%s %%%s(%s) "
    (llvmTypeName func.rettype) func.fname paramString
  in
  (* let isTypeName name = try ignore(string2integralType name); true with _ -> false in *)
  let isTypeName name = String.length name > 0 in
  let impl =
    (sprintf "{\n%s\n" implCode)
    ^ (if isTypeName resultVar.rvtypename then
         (sprintf "  ret %s %s\n}"
            resultVar.rvtypename
            resultVar.rvname)
       else "ret void\n}")
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
    
