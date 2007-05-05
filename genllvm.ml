
open Ast2
open Lang
open Printf

let indent string =
  let lines = Str.split (Str.regexp "\n") string in
  let indentedLines = List.map (fun s -> "  " ^ s) lines in
  combine "\n" indentedLines

let gencodeSequence gencode exprs =
      let codes = List.map gencode exprs in
      let append l r = l ^ r ^ ";\n" in
      let innerString = (List.fold_left append "" codes) in
      "{\n" ^ (indent innerString) ^ "\n}"

let gencodeDefineVariable var =
  sprintf "%s %s = %s"
    (integralType2String var.typ)
    var.vname
    (integralValue2String var.default)

let gencodeVariable name = name

let gencodeConstant c = integralValue2String c

let gencodeFuncCall gencode call =
  let argStrings = List.map gencode call.fcargs in
  let paramString = combine " " argStrings in
  call.fcname ^ " " ^ paramString

let gencodeIfThenElse gencode ite =
  let condCode = gencode ite.cond
  and trueCode = gencode ite.trueCode
  and falseCode = gencode ite.falseCode
  in
  sprintf "if %s then %s else %s" condCode trueCode falseCode
    
let gencodeLoop gencode l =
  let preCode = indent (gencode l.preCode)
  and abortTest = gencode l.abortTest
  and postCode = indent (gencode l.postCode)
  in
  sprintf "loop {\n%s;\n  break if %s;\n%s;\n}" preCode abortTest postCode
  
let rec gencode = function
  | Sequence exprs -> gencodeSequence gencode exprs
  | DefineVariable var -> gencodeDefineVariable var
  | Variable name -> gencodeVariable name
  | Constant c -> (integralValue2String c)
  | FuncCall call -> gencodeFuncCall gencode call
  | IfThenElse ite -> gencodeIfThenElse gencode ite
  | Loop l -> gencodeLoop gencode l


let gencodeGlobaleVar var =
  sprintf "%%%s = %s %s"
    var.vname
    (integralType2String var.typ)
    (integralValue2String var.default)

let gencodeDefineFunc func =
  let implCode = gencode func.impl in
  let param2string (name, typ) = (integralType2String typ) ^ " " ^ name in
  let paramString = combine ", " (List.map param2string func.args) in
  sprintf "%s %%%s (%s) %s;"
    (integralType2String func.rettype)
    func.fname
    paramString
    implCode

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
  let initCode = "" 
  and varCode = List.map gencodeTL globalVars
  and funcCode = List.map gencodeTL globalFuncs
  in
  initCode ^ "\n" ^
    (combine "\n" varCode) ^ "\nimplementation\n" ^
    (combine "\n" funcCode)
    
