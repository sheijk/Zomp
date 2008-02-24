
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

(* %token PAREN_OPEN *)
(* %token PAREN_CLOSE *)
(* %token <string> QUOTE *)
(* %token <string> IDENTIFIER *)
(* %token <string> ADD_OP *)
(* %token <string> MULT_OP *)
(* %token <string> COMPARE_OP *)
(* %token <string> POST_OP *)
(* %token <string> PRE_OP *)
(* %token BRACKET_OPEN *)
(* %token BRACKET_CLOSE *)

(* %right COMPARE_OP *)
(* %right ADD_OP *)
(* %right MULT_OP *)
  
%token <string> IDENTIFIER
%token END
%token BEGIN_BLOCK
%token <string list> END_BLOCK
%token <int> WHITESPACE

%token OPEN_PAREN
%token CLOSE_PAREN
%token COMMA

%token <string> ADD_OP
%token <string> SUB_OP
%token <string> MULT_OP
%token <string> DIV_OP


%start <Ast2.sexpr> main

%%

main:
| expr = sexpr END
    { expr }
| expr = mexpr END
    { expr }
| expr = opexpr END
    { expr }

    
sexpr:
| WHITESPACE? id = IDENTIFIER WHITESPACE?
    { Ast2.idExpr id }
| WHITESPACE? id = IDENTIFIER args = sexprArg+ WHITESPACE?
    {{ Ast2.id = "opjux"; args = Ast2.idExpr id :: args }}

sexprArg:
| WHITESPACE? id = IDENTIFIER
    { Ast2.idExpr id }
| WHITESPACE? OPEN_PAREN expr = sexpr CLOSE_PAREN
    { expr }
| WHITESPACE? BEGIN_BLOCK exprs = main* END_BLOCK
    {{ Ast2.id = "opseq"; args = exprs }}

    
mexpr:
| WHITESPACE? id = IDENTIFIER OPEN_PAREN args = separated_list(COMMA, mexprArg) CLOSE_PAREN
    {{ Ast2.id = "opcall"; args = Ast2.idExpr id :: args }}

mexprArg:
| WHITESPACE? id = IDENTIFIER WHITESPACE?
    { Ast2.idExpr id }

    
opexpr:
| args = seplist2(COMMA, opexprArg)
    {{ Ast2.id = "op,"; args = args }}

| l = opexprArg op = ADD_OP r = opexprArg
    {{ Ast2.id = operatorName op; args = [l; r] }}
    
| l = opexprArg op = SUB_OP r = opexprArg
    {{ Ast2.id = operatorName op; args = [l; r] }}
    
| l = opexprArg op = MULT_OP r = opexprArg
    {{ Ast2.id = operatorName op; args = [l; r] }}
    
| l = opexprArg op = DIV_OP r = opexprArg
    {{ Ast2.id = operatorName op; args = [l; r] }}

    
opexprArg:
| WHITESPACE? id = IDENTIFIER WHITESPACE?
    { Ast2.idExpr id }

    
seplist2(SEP, X):
| l = X SEP r = X
    { [l; r] }
| x = X SEP rem = seplist2(SEP, X)
    { x :: rem }
        
/*
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
| id = IDENTIFIER op = POST_OP
    { Ast2.simpleExpr (operatorName op) [id] }
| op = PRE_OP id = IDENTIFIER
    { Ast2.simpleExpr (operatorName op) [id] }
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
| left = IDENTIFIER BRACKET_OPEN right = sexpr BRACKET_CLOSE
    {{ Ast2.id = "op[]"; Ast2.args = [Ast2.idExpr left; right] }}
operatorArg:
| e = sexpr
    { e }
| e = operatorExpr
    { e }
*/        

