
%{
  exception ParseError of string
  open Printf
  open Ast2
  let raiseParseError str = raise (ParseError str)

  let reportParsedExpression e =
    let locationToString l =
      sprintf "%s:%d" l.fileName l.line
    in
    let str = toString e in
    begin match e.location with
       | Some l -> printf "Parsed '%s..'@%s\n" (locationToString l) str
       | None -> printf "Parsed '%s..' w/o location\n" str
    end;
    flush stdout

  let combineLocations exprs =
    match exprs with
      | [] -> None
      | first :: _ -> first.location

  let getLocation loc =
    { fileName = loc.Lexing.pos_fname;
      line = loc.Lexing.pos_lnum }

  let withLoc expr lbloc = { expr with location = Some (getLocation lbloc) }

  let juxExpr exprs =
    let jux = juxExpr exprs in
    { jux with location = combineLocations exprs }
  let callExpr exprs =
    let e = callExpr exprs in
    { e with location = combineLocations exprs }
  let seqExpr exprs =
    let e = opseqExpr exprs in
    { e with location = combineLocations exprs }

  let expr name exprs =
    let e = expr name exprs in
    { e with location = combineLocations exprs }

  let idExprLoc id loc =
    let l = getLocation loc in
    let e = idExpr id in
    { e with location = Some l }

  let quoteId = function
    | "`" -> "quote"
    | "$" -> "quote"
    | "$$" -> "quoteasis"
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
      | {id = "opjux"; args = largs }, _ ->
          expr "opjux" (largs @ [r])
      | _, {id = "opjux"; args = rargs } ->
          expr "opjux" (l :: rargs)
      | _ ->
          expr "opjux" [l;r]

  let checkTerminators expr terminators =
    let rec checkAll = function
      | _, [] ->
          ()
      | { id = exprId; args = [] } :: remExprs, firstTerm :: remTerms
          when exprId = firstTerm ->
          checkAll (remExprs, remTerms)
      | _, _ ->
          raiseParseError (Printf.sprintf "Invalid terminators: %s for %s"
                             (Common.combine " " terminators)
                             (toString expr) )
    in
    checkAll (idExpr expr.id :: expr.args, terminators)

  let expectNoTerminators = function
    | [] -> ()
    | invalidTerminators ->
        raiseParseError (Printf.sprintf "Expected no terminators but found: %s"
                           (Common.combine " " invalidTerminators))

  let exprOfNoTerm ((expr :sexpr), (terminators :string list)) =
    match terminators with
      | [] -> expr
      | invalidTerminators -> raiseParseError
          (Printf.sprintf "Expected no terminators but found %s in %s"
             (Common.combine " " invalidTerminators)
             (toString expr))

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
                             (toString expr))
    in
    worker [] exprAndTerminators

  let rec extractKeywordsAndExprsAndCheckTerminators = function
    | [] -> []
    | [(keyword, (expr, terminators))] ->
        checkTerminators (idExpr keyword) terminators;
        [(keyword, expr)]
    | (keyword, (expr, [])) :: rem ->
        (keyword, expr) :: extractKeywordsAndExprsAndCheckTerminators rem
    | (keyword, (expr, terminators)) :: _ ->
        raiseParseError (Printf.sprintf "Found invalid terminators '%s' after block in expr %s"
                           (Common.combine ", " terminators)
                           (toString expr))

  let keywordAndExprToList (keyword, expr) = [idExpr keyword; expr]
%}

%token <string> IDENTIFIER
%token END
%token BEGIN_BLOCK
%token <string list> END_BLOCK

%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_ARGLIST
%token COMMA
%token OPEN_CURLY
%token CLOSE_CURLY

%token OPEN_BRACKET
%token OPEN_BRACKET_POSTFIX
%token CLOSE_BRACKET

(* the order of the tokens does not define precedence! see below *)
%token <string> MULT_OP
%token <string> ADD_OP
%token <string> ASSIGN_OP
%token <string> COMPARE_OP
%token <string> LAZY_BOOL_OP
%token <string> STRICT_BOOL_OP
%token <string> MOD_OP
%token DOT
%token <string> PREFIX_OP
%token <string> POSTFIX_OP
%token <string> QUOTE
%token <string> SEMICOLON
%token <string> EXCLAMATION_OP

%left SEMICOLON
%nonassoc ASSIGN_OP
%left LAZY_BOOL_OP
%nonassoc COMPARE_OP
%left ADD_OP
%left MULT_OP MOD_OP
%left STRICT_BOOL_OP

%right PREFIX_OP
%right OPEN_ARGLIST
%right OPEN_BRACKET_POSTFIX
%left POSTFIX_OP
%right EXCLAMATION_OP
%left DOT
%right QUOTE

%start <sexpr> main

%%

main:
| e = terminatedExpr;
  { e }

terminatedExpr:
| e = expr END;
  { e }

expr:
| e = infixExpr;
  { e }

infixExpr:
| l = infixExpr; op = opSymbol; r = infixExpr;
  { expr (opName op) [l; r] }
| c = juxExpr;
  { c }

juxExpr:
| hd = prepostExpr; exprs = prepostExpr+;
  { juxExpr (hd :: exprs) }
| e = prepostExpr;
  { e }

prepostExpr:
| e = closedExpr;
  { e }

closedExpr:
| id = IDENTIFIER;
  { idExprLoc id $startpos }

| q = QUOTE; OPEN_CURLY; CLOSE_CURLY;
  { let e = expr (quoteId q) [withLoc (seqExpr []) $startpos] in
    { e with location = Some (getLocation $startpos) } }
| q = QUOTE; OPEN_CURLY; e = expr; CLOSE_CURLY;
  { expr (quoteId q) [e] }
| q = QUOTE; e = closedExpr;
  { expr (quoteId q) [e] }

| op = PREFIX_OP; e = closedExpr;
  { expr ("pre" ^ opName op) [e] }
| e = closedExpr; op = POSTFIX_OP;
  { expr ("post" ^ opName op) [e] }

| l = closedExpr; DOT; r = closedExpr;
  { expr "op." [l; r] }
| l = closedExpr; op = EXCLAMATION_OP; r = closedExpr;
  { expr (opName op) [l; r] }

| OPEN_PAREN; e = expr; CLOSE_PAREN;
  { e }

| OPEN_CURLY; CLOSE_CURLY;
  { withLoc (expr "op{}" []) $startpos }
| OPEN_CURLY; e = expr; CLOSE_CURLY;
  { expr "op{}" [e] }

| OPEN_BRACKET; CLOSE_BRACKET;
  { withLoc (expr "op[]" []) $startpos }
| OPEN_BRACKET; e = expr; CLOSE_BRACKET;
  { expr "op[]" [e] }

| head = closedExpr; OPEN_BRACKET_POSTFIX; arg = expr; CLOSE_BRACKET;
  { expr "postop[]" [head; arg] }

| head = closedExpr; OPEN_ARGLIST; args = separated_list(COMMA, expr); CLOSE_PAREN;
  { callExpr (head :: args) }

| BEGIN_BLOCK; exprs = terminatedExpr*; END_BLOCK;
  { seqExpr exprs }
    

%inline opSymbol:
| o = ADD_OP
| o = MULT_OP
| o = MOD_OP
| o = ASSIGN_OP
| o = COMPARE_OP
| o = LAZY_BOOL_OP
| o = STRICT_BOOL_OP
| o = SEMICOLON
{ o }

