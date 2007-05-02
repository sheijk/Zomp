
open Ast2
open Printf
open Genllvm

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let rec step env =
      printf "> ";
      flush stdout;
      let expr = Parser2.main Lexer2.token lexbuf in
      let asString = Ast2.expression2string expr in
      let llvmCode = translateTL expr in
      printf "=== Parsed ===\n%s\n=== LLVM ===\n%s\n\n" asString llvmCode;
(*       printf "Evaluate expression:\n%s\n" (node2string expr); *)
(*       let result, newEnv = interprete env expr in *)
(*       printf "=>\n%s\n" (constant2string result); *)
      flush stdout;
      let newEnv = env in
      step newEnv
    in
    let defaultEnvironment () = () in
    step (defaultEnvironment())
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
      
