
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

%left WHITESPACE
%left ADD_OP SUB_OP
%left MULT_OP DIV_OP
(* %left CALL *)
  
%start <Ast2.sexpr> main

%%

main:
| WHITESPACE? e = expr WHITESPACE? END
    { e }

expr:
| id = IDENTIFIER;
  { Ast2.idExpr id }

| l = expr; WHITESPACE; r = expr;
  { match l with
      | {Ast2.id = "opjux"; args = largs } -> {Ast2.id = "opjux"; args = largs @ [r] }
      | _ -> { Ast2.id = "opjux"; args = [l; r] }
  }

| l = expr; op = opsymbol; r = expr;
  {{ Ast2.id = opName op; args = [l; r] }}

| id = IDENTIFIER; OPEN_PAREN; args = separated_list(COMMA, expr); CLOSE_PAREN;
  {{ Ast2.id = "opcall"; args = Ast2.idExpr id :: args }}

%inline opsymbol:
| o = ADD_OP
| o = SUB_OP
| o = MULT_OP
| o = DIV_OP
    { o }
    
