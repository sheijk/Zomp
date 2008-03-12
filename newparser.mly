
%{
  exception ParseError of string
  let raiseParseError str = raise (ParseError str)

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
(*       | _, {Ast2.id = "opjuxNOMERGE"; args = [r]} -> *)
(*           {Ast2.id = "opjux"; args = [l; r] } *)
      | {Ast2.id = "opjux"; args = largs }, _ ->
          {Ast2.id = "opjux"; args = largs @ [r]}
      | _, {Ast2.id = "opjux"; args = rargs } ->
          {Ast2.id = "opjux"; args = l :: rargs}
      | _ ->
          {Ast2.id = "opjux"; args = [l;r]}

  let checkTerminators expr terminators =
    let rec checkAll = function
      | _, [] ->
          ()
      | { Ast2.id = exprId; args = [] } :: remExprs, firstTerm :: remTerms when exprId = firstTerm ->
          checkAll (remExprs, remTerms)
      | _, _ ->
          raiseParseError (Printf.sprintf "Invalid terminators: %s for %s"
                             (Common.combine " " terminators)
                             (Ast2.toString expr) )
    in
    checkAll (Ast2.idExpr expr.Ast2.id :: expr.Ast2.args, terminators)
      
%}

%token <string> IDENTIFIER
%token END
%token BEGIN_BLOCK
%token <string list> END_BLOCK
%token <int> WHITESPACE

%token OPEN_PAREN
%token CLOSE_PAREN
%token COMMA
%token OPEN_CURLY
%token CLOSE_CURLY
  
%token <string> ADD_OP
%token <string> MULT_OP
%token <string> ASSIGN_OP
%token <string> COMPARE_OP
%token <string> LAZY_BOOL_OP
%token <string> STRICT_BOOL_OP
%token DOT
%token <string> PREFIX_OP
%token <string> POSTFIX_OP
%token <string> QUOTE

%left WHITESPACE BEGIN_BLOCK
%nonassoc COMPARE_OP
%nonassoc ASSIGN_OP
%left ADD_OP
%left MULT_OP
%nonassoc DOT
%left POSTFIX_OP
%right PREFIX_OP
  
%start <Ast2.sexpr> main

%%

  main:
| WHITESPACE?; e = expr; WHITESPACE?; END;
  { e }
    
    expr:
| id = IDENTIFIER;
  { Ast2.idExpr id }

| id = IDENTIFIER; OPEN_PAREN; args = separated_list(COMMA, expr); CLOSE_PAREN;
  {{ Ast2.id = "opcall"; args = Ast2.idExpr id :: args }}

(* | OPEN_CURLY; e = expr; CLOSE_CURLY; *)
(*   {{ Ast2.id = "op{}"; args = [e] }} *)

| q = QUOTE; OPEN_CURLY; e = expr; END?; CLOSE_CURLY;
  {{ Ast2.id = q; args = [e] }}
    
| l = expr; op = opsymbol; r = expr;
  {{ Ast2.id = opName op; args = [l; r] }}

| op = PREFIX_OP; r = expr;
  {{ Ast2.id = "pre" ^ opName op; args = [r] }}

| l = expr; op = POSTFIX_OP;
  {{ Ast2.id = "post" ^ opName op; args = [l] }}
    
| l = expr; WHITESPACE; r = expr;
  { mergeJux l r }
    
| l = expr; BEGIN_BLOCK; args = main*; terminators = END_BLOCK;
  { checkTerminators l terminators;
    mergeJux l { Ast2.id = "opseq"; args = args }}

| l = expr; DOT; r = expr;
  {{ Ast2.id = "op."; args = [l; r] }}
    
%inline opsymbol:
| o = ADD_OP
| o = MULT_OP
| o = ASSIGN_OP
| o = COMPARE_OP
| o = LAZY_BOOL_OP
| o = STRICT_BOOL_OP
    { o }
    
