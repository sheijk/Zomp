
open Ast2
open Printf
open Expander
open Genllvm

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let rec step bindings =
      printf "> ";
      flush stdout;
      let expr = Parser2.main Lexer2.token lexbuf in
      let asString = Ast2.expression2string expr in
      printf "=== Parsed ===\n%s\n" asString;
      let newBindings, simpleforms = Expander.translateTL bindings expr in
      let llvmCodes = List.map gencodeTL simpleforms in
      let llvmCode = combine "\n" llvmCodes in
      printf "=== LLVM ===\n%s\n\n" llvmCode;
      flush stdout;
      step newBindings
    in
    step Bindings.defaultBindings
  with
      Lexer2.Eof ->
        printf "EOF. Exiting\n"
    | Parser2.Error ->
        let {
          Lexing.pos_fname = fileName;
          Lexing.pos_lnum = lineNum;
          Lexing.pos_bol = columNum;
          Lexing.pos_cnum = totalChars
        } = lexbuf.Lexing.lex_curr_p in
        printf
          "Parser error in [%s:%d:%d] (%d chars from beginning of file\n"
          fileName lineNum columNum totalChars
          (* TODO: update pos_lnum (and pos_fname) in lexer *)
    | _ as e -> 
        printf "Unknow error occured. Exiting\n";
        raise e
      
