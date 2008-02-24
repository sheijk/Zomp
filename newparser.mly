
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

  let mergeJux l r =
    match l, r with
      | {Ast2.id = "opjux"; args = largs }, _ ->
          {Ast2.id = "opjux"; args = largs @ [r]}
      | _, {Ast2.id = "opjux"; args = rargs } ->
          {Ast2.id = "opjux"; args = l :: rargs}
      | _ ->
          {Ast2.id = "opjux"; args = [l;r]}

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
%token <string> ASSIGN_OP
%token <string> COMPARE_OP

%left WHITESPACE BEGIN_BLOCK
%nonassoc COMPARE_OP
%nonassoc ASSIGN_OP
%left ADD_OP SUB_OP
%left MULT_OP DIV_OP
  
%start <Ast2.sexpr> main

%%

main:
| WHITESPACE? e = expr WHITESPACE? END
    { e }
    
expr:
| id = IDENTIFIER;
  { Ast2.idExpr id }

| l = expr; op = opsymbol; r = expr;
  {{ Ast2.id = opName op; args = [l; r] }}

| id = IDENTIFIER; OPEN_PAREN; args = separated_list(COMMA, expr); CLOSE_PAREN;
  {{ Ast2.id = "opcall"; args = Ast2.idExpr id :: args }}

| l = expr; WHITESPACE; r = expr;
  { mergeJux l r }
    
| l = expr; BEGIN_BLOCK; args = main*; END_BLOCK;
  { mergeJux l { Ast2.id = "opseq"; args = args }}
    
%inline opsymbol:
| o = ADD_OP
| o = SUB_OP
| o = MULT_OP
| o = DIV_OP
| o = ASSIGN_OP
| o = COMPARE_OP
    { o }
    
