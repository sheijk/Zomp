%{
  exception ParseError of string
    
  let opFuncName op = "op" ^ op
%}

%token BLOCK_BEGIN BLOCK_END
%token <string> IDENTIFIER
%token SEPERATOR
%token <string> OP_PLUS
  
%start <Ast2.expression> main

%%

main:
| e = expr SEPERATOR { e }
expr:
| id = IDENTIFIER params = param*
    { { Ast2.id = id; Ast2.args = params; } }
| l = expr op = OP_PLUS r = expr
    { { Ast2.id = opFuncName op; Ast2.args = [l; r] } }
param:
| id = IDENTIFIER
    { { Ast2.id = id; Ast2.args = []; } }
| BLOCK_BEGIN expressions = main* BLOCK_END
    { { Ast2.id = "seq"; Ast2.args = expressions; } }
    
