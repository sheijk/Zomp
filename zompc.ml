open Ast2
open Printf
open Expander
open Genllvm

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let rec parse bindings code =
      try
        let expr = Parser2.main Lexer2.token lexbuf in
        let newBindings, simpleforms = Expander.translateTL bindings expr in
        parse newBindings (code @ simpleforms)
      with
          Lexer2.Eof -> code
    in
    let toplevelExprs :Lang.toplevelExpr list = parse Genllvm.defaultBindings [] in
    let llvmSource :string = genmodule toplevelExprs in
    printf "%s\n" llvmSource
  with
    | Parser2.Error ->
        let {
          Lexing.pos_fname = fileName;
          Lexing.pos_lnum = lineNum;
          Lexing.pos_bol = columNum;
          Lexing.pos_cnum = totalChars
        } = lexbuf.Lexing.lex_curr_p in
        eprintf
          "Parser error in [%s:%d:%d] (%d chars from beginning of file\n"
          fileName lineNum columNum totalChars
          (* TODO: update pos_lnum (and pos_fname) in lexer *)
    | _ as e -> 
        eprintf "Unknow error occured. Exiting\n";
        raise e
      

