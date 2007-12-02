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

  let operatorName opSymbol = "op" ^ opSymbol
    
%}

%token PAREN_OPEN
%token PAREN_CLOSE
%token <string> QUOTE
%token <string> IDENTIFIER
%token <string> ADD_OP
%token <string> MULT_OP
  
%start <Ast2.sexpr> main

%%


main:
| PAREN_OPEN e = sexpr PAREN_CLOSE
    { e }
| PAREN_OPEN PAREN_CLOSE
    {{ Ast2.id = "seq"; args = [] }}
sexpr:
| id = IDENTIFIER args = sexprArg*
    {{ Ast2.id = id; args = args }}
| id = main args = sexprArg*
    {{ Ast2.id = "seq"; args = id :: args }}
sexprArg:
| id = IDENTIFIER
    {{ Ast2.id = id; args = [] }}
| e = main
    { e }
        
/*    
operatorExpr:
| l = main op = ADD_OP r = main
    {{ Ast2.id = operatorName op; args = [l; r] }}
| l = main op = MULT_OP r = main
    {{ Ast2.id = operatorName op; args = [l; r] }}
*/
    
/*
main:
| e = sexpr
    { e }
arg:
| id = IDENTIFIER
    { { Ast2.id = id; Ast2.args = []; } }
| e = sexpr
    { e }
sexpr:
| PAREN_OPEN PAREN_CLOSE
    { { Ast2.id = "seq"; args = []; } }
| PAREN_OPEN id = IDENTIFIER args = arg* PAREN_CLOSE
    { { Ast2.id = id; Ast2.args = args; } }
| PAREN_OPEN firstarg = sexpr args = arg* PAREN_CLOSE
    { { Ast2.id = "seq"; args = firstarg :: args; } }
| q = QUOTE id = IDENTIFIER
    { { Ast2.id = quoteId q; args = [{ Ast2.id = id; args = [] }] } }
| q = QUOTE code = sexpr
    { { Ast2.id = quoteId q; args = [code] } }
/**/
    
