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
%token <string> COMPARE_OP

%left COMPARE_OP
%left ADD_OP
%left MULT_OP
  
%start <Ast2.sexpr> main

%%

main:
| PAREN_OPEN e = sexpr PAREN_CLOSE
    { e }
| PAREN_OPEN e = operatorExpr PAREN_CLOSE
    { e }
| PAREN_OPEN PAREN_CLOSE
    {{ Ast2.id = "seq"; args = [] }}
| q = QUOTE e = main
    {{ Ast2.id = quoteId q; args = [e] }}
| q = QUOTE e = IDENTIFIER
    {{ Ast2.id = quoteId q; args = [{Ast2.id = e; args = []}] }}
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
operatorExpr:
| l = operatorArg op = ADD_OP r = operatorArg
    {{ Ast2.id = operatorName op; args = [l; r] }}
| l = operatorArg op = MULT_OP r = operatorArg
    {{ Ast2.id = operatorName op; args = [l; r] }}
| l = operatorArg op = COMPARE_OP r = operatorArg
    {{ Ast2.id = operatorName op; args = [l; r] }}
operatorArg:
| e = sexpr
    { e }
| e = operatorExpr
    { e }
        
