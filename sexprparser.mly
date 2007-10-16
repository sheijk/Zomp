%{
  exception ParseError of string
%}

%token PAREN_OPEN
%token PAREN_CLOSE
%token ANTI_QUOTE
%token <string> IDENTIFIER
(* %token END_SYMBOL *)
  
%start <Ast2.expression> main

%%

main:
| e = expr { e }
expr:
| ANTI_QUOTE code = expr
    { { Ast2.id = "antiquote"; args = [code] } }
| ANTI_QUOTE id = IDENTIFIER
    { { Ast2.id = "antiquote"; args = [{ Ast2.id = id; args = [] }] } }
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
    
