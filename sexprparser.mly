%{
  exception ParseError of string

  let quoteId = function
    | "`" -> "quote"
    | "``" -> "quoteasis"
    | "#" -> "antiquote"
    | invalidString ->
        raise (ParseError
                 (Printf.sprintf
                    "Invalid quoting char: %s"
                    invalidString))

%}

%token PAREN_OPEN
%token PAREN_CLOSE
%token <string> QUOTE
%token <string> IDENTIFIER
(* %token END_SYMBOL *)
  
%start <Ast2.sexpr> main

%%

main:
| e = expr { e }
expr:
| q = QUOTE code = expr
    { { Ast2.id = quoteId q; args = [code] } }
| q = QUOTE id = IDENTIFIER
    { { Ast2.id = quoteId q; args = [{ Ast2.id = id; args = [] }] } }
| PAREN_OPEN PAREN_CLOSE
    { { Ast2.id = "seq"; args = []; } }
| PAREN_OPEN id = IDENTIFIER args = arg* PAREN_CLOSE
    { { Ast2.id = id; Ast2.args = args; } }
| PAREN_OPEN firstarg = expr args = arg* PAREN_CLOSE
    { { Ast2.id = "seq"; args = firstarg :: args; } }
arg:
| id = IDENTIFIER
    { { Ast2.id = id; Ast2.args = []; } }
| e = expr
    { e }
    
