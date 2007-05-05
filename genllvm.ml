
open Ast2
open Lang
open Printf

let llvmName name = "%" ^ name

type resultvar = {
  rvname :string;
  rvtypename :string;
}

let var name typ = { rvname = llvmName name; rvtypename = typ }
  
let noVar = { rvname = ""; rvtypename = "" }

let lastTempVarNum = ref 0
let newTempVar typ =
  incr lastTempVarNum;
  var (sprintf "temp%d" !lastTempVarNum) typ
  
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
  let name = llvmName var.vname
  and typ = integralType2String var.typ
  and value = integralValue2String var.default
  in
  let code =
    (sprintf "%s = alloca %s\n" name typ) ^
      (sprintf "store %s %s, %s* %s" typ value typ name)
  in
  (noVar, code)

let gencodeVariable name =
  var name "unknownType", ""
(*   { *)
(*     rvname = llvmName name; *)
(*     rvtypename = "unknown" *)
(*   }, *)
(*   llvmName name *)

let gencodeConstant c =
  {
    rvname = integralValue2String c;
    rvtypename = integralType2String (integralValue2Type c);
  },
  ""

let gencodeFuncCall gencode call =
  noVar, "funcall!"
(*   let argStrings = List.map gencode call.fcargs in *)
(*   let paramString = combine " " argStrings in *)
(*   noVar, call.fcname ^ " " ^ paramString *)

let gencodeIfThenElse gencode ite =
  let condVar, condCode = gencode ite.cond
  and trueVar, trueCode = gencode ite.trueCode
  and falseVar, falseCode = gencode ite.falseCode
  in
  noVar,
  sprintf "%s\n%s\n%s\n if %s then %s else %s\n"
    condCode trueCode falseCode
    condVar.rvname trueVar.rvname falseVar.rvname
(*   let condCode = gencode ite.cond *)
(*   and trueCode = gencode ite.trueCode *)
(*   and falseCode = gencode ite.falseCode *)
(*   in *)
(*   noVar, sprintf "if %s then %s else %s" condCode trueCode falseCode *)
    
let gencodeLoop gencode l =
  let _, preCode = gencode l.preCode
  and abortVar, abortCode = gencode l.abortTest
  and resultVar, postCode = gencode l.postCode
  in
  resultVar,
  sprintf "start:\n%s\n%s\nbreak if %s\n%s"
    preCode abortCode abortVar.rvname postCode
(*   let preCode = indent (gencode l.preCode) *)
(*   and abortTest = gencode l.abortTest *)
(*   and postCode = indent (gencode l.postCode) *)
(*   in *)
(*   noVar, sprintf "loop {\n%s;\n  break if %s;\n%s;\n}" preCode abortTest postCode *)
  
let rec gencode : Lang.expr -> resultvar * string = function
  | Sequence exprs -> gencodeSequence gencode exprs
  | DefineVariable var -> gencodeDefineVariable var
  | Variable name -> gencodeVariable name
  | Constant c -> gencodeConstant c
  | FuncCall call -> gencodeFuncCall gencode call
  | IfThenElse ite -> gencodeIfThenElse gencode ite
  | Loop l -> gencodeLoop gencode l
      
      
let gencodeGlobaleVar var =
  sprintf "%%%s = constant %s %s"
    var.vname
    (integralType2String var.typ)
    (integralValue2String var.default)

let gencodeDefineFunc func =
  let resultVar, implCode = gencode func.impl in
  let param2string (name, typ) = (integralType2String typ) ^ " " ^ (llvmName name) in
  let paramString = combine ", " (List.map param2string func.args) in
  let decl = sprintf "%s %%%s(%s) "
    (integralType2String func.rettype) func.fname paramString
  in
  let impl = sprintf "{\n%s\n  ret %s %s;\n}"
    implCode
    resultVar.rvtypename
    resultVar.rvname
  in
  decl ^ impl
(*   sprintf "%s %%%s (%s) {\n%s;\n};\n" *)
(*     (integralType2String func.rettype) *)
(*     func.fname *)
(*     paramString *)
(*     implCode *)

let gencodeTL = function
  | GlobalVar var -> gencodeGlobaleVar var
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
  headerCode ^ "\n" ^
    (combine "\n" varCode) ^ "\n\nimplementation\n\n" ^
    (combine "\n" funcCode)
    
