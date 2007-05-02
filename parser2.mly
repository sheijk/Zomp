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
        { Ast2.Expr (id, params) }
param:
| id = IDENTIFIER
        { Ast2.Expr (id, []) }
| BLOCK_BEGIN expressions = main* BLOCK_END
        { Ast2.Expr ("std:seq", expressions) }
    
