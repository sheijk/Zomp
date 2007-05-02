%{
  exception ParseError of string
%}

%token BLOCK_BEGIN BLOCK_END
%token <string> IDENTIFIER
%token SEPERATOR
  
%start <Ast2.expression> main

%%

main:
| e = expr SEPERATOR { e }
expr:
| id = IDENTIFIER params = param*
    { { Ast2.id = id; Ast2.args = params; } }
param:
| id = IDENTIFIER
    { { Ast2.id = id; Ast2.args = []; } }
| BLOCK_BEGIN expressions = main* BLOCK_END
    { { Ast2.id = "std:seq"; Ast2.args = expressions; } }
    
