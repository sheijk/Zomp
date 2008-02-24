
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

  let opName opSymbol = "op" ^ opSymbol
    
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

%left ADD_OP
%left MULT_OP
  
%start <Ast2.sexpr> main

%%

main:
| e = expr END
    { e }

expr:
| id = IDENTIFIER
    { Ast2.idExpr id }
| l = expr; op = ADD_OP; r = expr;
    {{ Ast2.id = opName op; args = [l; r] }}
| l = expr; op = MULT_OP; r = expr;
    {{ Ast2.id = opName op; args = [l; r] }}

