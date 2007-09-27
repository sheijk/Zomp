%{
  exception ParseError of string
%}

%token PAREN_OPEN
%token PAREN_CLOSE
%token <string> IDENTIFIER
(* %token END_SYMBOL *)
  
%start <Ast2.expression> main

%%

main:
| e = expr { e }
expr:
| PAREN_OPEN PAREN_CLOSE
    { { Ast2.id = "seq"; args = []; } }
| PAREN_OPEN id = IDENTIFIER args = arg* PAREN_CLOSE
    { { Ast2.id = id; Ast2.args = args; } }
arg:
| id = IDENTIFIER
    { { Ast2.id = id; Ast2.args = []; } }
| e = expr
    { e }
    
