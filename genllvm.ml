
open Ast2
open Lang
open Printf

let indent string =
  let lines = Str.split (Str.regexp "\n") string in
  let indentedLines = List.map (fun s -> "  " ^ s) lines in
  combine "\n" indentedLines
  
let rec gencode = function
  | Sequence exprs ->
      let codes = List.map gencode exprs in
      let append l r = l ^ r ^ ";\n" in
      let innerString = (List.fold_left append "" codes) in
      "{\n" ^ (indent innerString) ^ "\n}"
  | DefineVariable var ->
      sprintf "%s %s = %s"
        (integralType2String var.typ)
        var.vname
        (integralValue2String var.default)
  | Variable name -> name
  | Constant c -> (integralValue2String c)
  | FuncCall call ->
      let argStrings = List.map gencode call.fcargs in
      let paramString = combine " " argStrings in
      call.fcname ^ " " ^ paramString
  | IfThenElse ite ->
      let condCode = gencode ite.cond
      and trueCode = gencode ite.trueCode
      and falseCode = gencode ite.falseCode
      in
      sprintf "if %s then %s else %s" condCode trueCode falseCode
  | Loop l ->
      let preCode = indent (gencode l.preCode)
      and abortTest = gencode l.abortTest
      and postCode = indent (gencode l.postCode)
      in
      sprintf "loop {\n%s;\n  break if %s;\n%s;\n}" preCode abortTest postCode
      
let gencodeTL = function
  | GlobalVar var ->
      sprintf "%s %s = %s"
        (integralType2String var.typ)
        var.vname
        (integralValue2String var.default)
  | DefineFunc func ->
      let implCode = gencode func.impl in
      let param2string (name, typ) = (integralType2String typ) ^ " " ^ name in
      let paramString = combine ", " (List.map param2string func.args) in
      sprintf "%s %s (%s) %s;"
        (integralType2String func.rettype)
        func.fname
        paramString
        implCode

  
