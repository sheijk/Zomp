
open Ast2
open Printf

exception AbortExpr

let rec readExpr previousLines =
  let line = read_line() in
  let input = previousLines ^ line in
  if line = "!" then
    raise AbortExpr
  else if line = "!!" then
    begin
      printf "und tschuess\n";
      exit 0;
    end
  else
    let expr =
      try
        let lexbuf = Lexing.from_string (input ^ " !") in
        Parser2.main Lexer2.token lexbuf
      with _ ->
        begin
          try
            let lexbuf = Lexing.from_string (input ^ "!") in
            Sexprparser.main Sexprlexer.token lexbuf
          with _ ->
            printf "cont. inp.  # ";
            readExpr input
        end
    in
    expr

let () =
  printf "Welcome to the Zomp toplevel\n";
  printf "!! or ctrl-c - exit, ! - reset (ignore malious input from previous line\n";
  let rec step () =
    begin
      try
        printf "cexpr|sexpr # ";
        flush stdout;
        let expr = readExpr "" in
        let asString = Ast2.expression2string expr in
        printf " => %s\n" asString;
        if expr.id = "exit" then begin
        end
      with
        | Sexprparser.Error -> printf "parsing error (sexpr).\n"
        | Sexprlexer.UnknowChar c -> printf "lexer error: encountered unknown character %c.\n" c
        | Parser2.Error -> printf "parsing error (cexpr).\n"
        | AbortExpr -> printf "aborted expression, restarting with next line.\n"
    end;
    step ()
  in
  step ()

  
