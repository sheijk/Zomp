
%{
  exception ParseError of string
  let raiseParseError str = raise (ParseError str)

  let quoteId = function
    | "`" -> "quote"
    | "$" -> "quote"
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
      | { Ast2.id = exprId; args = [] } :: remExprs, firstTerm :: remTerms
          when exprId = firstTerm ->
          checkAll (remExprs, remTerms)
      | _, _ ->
          raiseParseError (Printf.sprintf "Invalid terminators: %s for %s"
                             (Common.combine " " terminators)
                             (Ast2.toString expr) )
    in
    checkAll (Ast2.idExpr expr.Ast2.id :: expr.Ast2.args, terminators)

  let exprOfNoTerm ((expr :Ast2.sexpr), (terminators :string list)) =
    match terminators with
      | [] -> expr
      | invalidTerminators -> raiseParseError
          (Printf.sprintf "Expected no terminators but found %s in %s"
             (Common.combine " " invalidTerminators)
             (Ast2.toString expr))

  let rec extractExprsAndCheckTerminators headExpr exprAndTerminators =
    let rec worker acc = function
      | [] -> List.rev acc
      | [expr, terminators] ->
          checkTerminators headExpr terminators;
          worker (expr::acc) []
      | (expr, []) :: rem ->
          worker (expr::acc) rem
      | (expr, invalidTerminators) :: rem ->
          raiseParseError (Printf.sprintf "Expected no terminators but found %s in %s"
                             (Common.combine " " invalidTerminators)
                             (Ast2.toString expr))
    in
    worker [] exprAndTerminators

  let juxExpr hd args = { Ast2.id = "opjux"; args = hd :: args }
  let callExpr hd args = { Ast2.id = "opcall"; args = hd :: args }
  let idExpr = Ast2.idExpr
  let seqExpr args = { Ast2.id = "opseq"; args = args }
  let expr id args = { Ast2.id = id; args = args }
%}

%token <string> IDENTIFIER
%token END
%token BEGIN_BLOCK
%token <string list> END_BLOCK

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

%nonassoc COMPARE_OP
%nonassoc ASSIGN_OP
%nonassoc STRICT_BOOL_OP LAZY_BOOL_OP
%left ADD_OP
%left MULT_OP
%nonassoc DOT
%left POSTFIX_OP
%right PREFIX_OP

%start <Ast2.sexpr> main

%%

  main:
| e = expr; END;
  { e }

  expr:
| exprAndTerminator = exprArg;
  { let expr, terminators = exprAndTerminator in
    (* checkTerminators expr terminators; *)
    expr }
| firstAndTerms = exprArg; argsAndTerminators = exprArg+;
  {
    let first = exprOfNoTerm firstAndTerms in
    let args = extractExprsAndCheckTerminators first argsAndTerminators in
    { Ast2.id = "opjux"; args = first :: args }}
| e = opExpr;
  { e }

exprArg:
| id = IDENTIFIER;
  { idExpr id, [] }
| e = exprArg; s = POSTFIX_OP;
  { expr ("post" ^ opName s) [fst e], [] }
| s = PREFIX_OP; e = exprArg;
  { expr ("pre" ^ opName s) [fst e], [] }
| OPEN_PAREN; e = expr; CLOSE_PAREN;
  { e, [] }
| BEGIN_BLOCK; exprs = main+; terminators = END_BLOCK;
  { seqExpr exprs, terminators }
| l = exprArg; DOT; r = exprArg;
  { expr "op." [exprOfNoTerm l; exprOfNoTerm r], [] }

| q = QUOTE; id = IDENTIFIER;
  { expr (quoteId q) [idExpr id], [] }
| q = QUOTE; OPEN_CURLY; CLOSE_CURLY;
  { expr (quoteId q) [{Ast2.id = "seq"; args = []}], [] }
| q = QUOTE; OPEN_CURLY; e = expr; CLOSE_CURLY;
  { expr (quoteId q) [e], [] }

opExpr:
| l = expr; o = opSymbol; r = expr;
  { expr (opName o) [l; r] }

%inline opSymbol:
| o = ADD_OP
| o = MULT_OP
| o = ASSIGN_OP
| o = COMPARE_OP
| o = LAZY_BOOL_OP
| o = STRICT_BOOL_OP
{ o }

