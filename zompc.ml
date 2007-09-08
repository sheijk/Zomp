open Ast2
open Printf
open Expander
open Genllvm
open Common

type llvmCode = string
    
type compilationResult =
  | CompilationSucceeded of llvmCode
  | CouldNotParse of string
  | CouldNotCompile of string

let parseChannel lexbuf parseF () : compilationResult =
  try
    let rec parse bindings codeAccum =
      try
        let expr = parseF lexbuf in
        let exprString = Ast2.expression2string expr in
        let exprComment = commentOut "; " exprString in
        printf "%s\n" exprComment;
        let newBindings, simpleforms = Expander.translateTL bindings expr in
        parse newBindings (codeAccum @ simpleforms)
      with
        | Lexer2.Eof | Sexprlexer.Eof -> codeAccum
    in
    let toplevelExprs :Lang.toplevelExpr list = parse Genllvm.defaultBindings [] in
    let llvmSource :string = genmodule toplevelExprs in
    CompilationSucceeded llvmSource
  with
    | Parser2.Error
    | Sexprparser.Error ->
        begin
          let {
            Lexing.pos_fname = fileName;
            Lexing.pos_lnum = lineNum;
            Lexing.pos_bol = columNum;
            Lexing.pos_cnum = totalChars
          } = lexbuf.Lexing.lex_curr_p
          in
          CouldNotParse (sprintf
                           "Parser error in [%s:%d:%d] %d chars from beginning of file\n"
                           fileName lineNum columNum totalChars)
            (* TODO: update pos_lnum (and pos_fname) in lexer *)
        end
    | Expander.IllegalExpression (expr, msg) ->
        begin
          CouldNotCompile (sprintf
                             "Error expanding to canonical simpleform in expression:\n%s\n\nMessage: %s\n"
                             (Ast2.expression2string expr)
                             msg)
        end
          
let rec readInput str =
  try readInput (str ^ read_line() ^ "\n")
  with End_of_file -> str

let ( |> ) l r =
  match l () with
    | CompilationSucceeded llvmCode ->
        printf "%s" llvmCode
    | CouldNotCompile error ->
        eprintf "%s" error
    | CouldNotParse error ->
        begin
          match r() with
            | CompilationSucceeded llvmCode ->
                printf "%s" llvmCode
            | CouldNotParse serror ->
                eprintf "Could not parse source\n";
                eprintf "cexpr: %s" error;
                eprintf "sexpr: %s" serror
            | CouldNotCompile error ->
                eprintf "%s" error;
        end

let () =
  let input = readInput "" in
  let parseAsCExpr =
    parseChannel (Lexing.from_string input) (fun lexbuf -> Parser2.main Lexer2.token lexbuf)
  and parseAsSExpr =
    parseChannel (Lexing.from_string input) (fun lexbuf -> Sexprparser.main Sexprlexer.token lexbuf)
  in
  parseAsCExpr |> parseAsSExpr

  
(* let () = *)
(*   let input = readInput "" in *)
(*   try *)
(*     parseChannel (Lexing.from_string input) (fun lexbuf -> Parser2.main Lexer2.token lexbuf) *)
(*   with *)
(*     | _ -> *)
(*         parseChannel (Lexing.from_string input) (fun lexbuf -> Sexprparser.main Sexprlexer.token lexbuf) *)

          
