
%{
  exception ParseError of string
  open Printf
  let raiseParseError str = raise (ParseError str)

  let reportParsedExpression e =
    let locationToString l =
      sprintf "%s:%d" l.Ast2.fileName l.Ast2.line
    in
    let str = Ast2.toString e in
    begin match e.Ast2.location with
       | Some l -> printf "Parsed '%s..'@%s\n" (locationToString l) str
       | None -> printf "Parsed '%s..' w/o location\n" str
    end;
    flush stdout

  let combineLocations exprs =
    match exprs with
    | [] -> None
    | first :: _ -> first.Ast2.location

  let getLocation loc =
    { Ast2.fileName = loc.Lexing.pos_fname;
      line = loc.Lexing.pos_lnum }

  let withLoc expr lbloc = { expr with Ast2.location = Some (getLocation lbloc) }


  let juxExpr exprs =
    let jux = Ast2.juxExpr exprs in
    { jux with Ast2.location = combineLocations exprs }
      
  let callExpr exprs =
    let e = Ast2.callExpr exprs in
    { e with Ast2.location = combineLocations exprs }
  let idExpr = Ast2.idExpr
  let seqExpr exprs =
    let e = Ast2.opseqExpr exprs in
    { e with Ast2.location = combineLocations exprs }

  let expr name exprs =
    let e = Ast2.expr name exprs in
    { e with Ast2.location = combineLocations exprs }

  let idExprLoc id loc =
    let l = getLocation loc in
    let e = idExpr id in
    { e with Ast2.location = Some l }

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
      | {Ast2.id = "opjux"; args = largs }, _ ->
          expr "opjux" (largs @ [r])
      | _, {Ast2.id = "opjux"; args = rargs } ->
          expr "opjux" (l :: rargs)
      | _ ->
          expr "opjux" [l;r]

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
    checkAll (idExpr expr.Ast2.id :: expr.Ast2.args, terminators)

  let expectNoTerminators = function
    | [] -> ()
    | invalidTerminators ->
        raiseParseError (Printf.sprintf "Expected no terminators but found: %s"
                           (Common.combine " " invalidTerminators))

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

  let rec extractKeywordsAndExprsAndCheckTerminators = function
    | [] -> []
    | [(keyword, (expr, terminators))] ->
        checkTerminators (Ast2.idExpr keyword) terminators;
        [(keyword, expr)]
    | (keyword, (expr, [])) :: rem ->
        (keyword, expr) :: extractKeywordsAndExprsAndCheckTerminators rem
    | (keyword, (expr, terminators)) :: _ ->
        raiseParseError (Printf.sprintf "Found invalid terminators '%s' after block in expr %s"
                           (Common.combine ", " terminators)
                           (Ast2.toString expr))

  let keywordAndExprToList (keyword, expr) = [Ast2.idExpr keyword; expr]
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
%left DOT
%right EXCLAMATION_OP

%start <Ast2.sexpr> main

%%

main:
| e = main2;
  {
    (* reportParsedExpression e; *)
    e
  }

%inline main2:
| e = kwexpr; END;
  { e }

kwexpr:
| e = expr;
  { e }

expr:
| first = exprArg; argsAndT = exprArgList;
  { let args, terminators = argsAndT in
    checkTerminators first terminators;
    juxExpr (first :: args) }

| e = juxExpr;
  { e }


juxExpr:
| e = exprArg;
  { e }

| first = exprArg; args = exprArg+;
  { juxExpr (first :: args) }

| e = opExpr;
  { e }

| e = opExpr; blockAndTerm = block;
  { let block, terminators = blockAndTerm in
    checkTerminators e terminators;
    juxExpr [e; block] }

exprArgList:
| blockAndT = block;
  { let block, terminators = blockAndT in
    [block], terminators }
| argsAndT = alternateExprArgsAndBlock;
  { argsAndT }
| blockAndT = block; argsAndT = alternateExprArgsAndBlock;
  { let block = exprOfNoTerm blockAndT in
    let args, terminators = argsAndT in
    block :: args, terminators }

alternateExprArgsAndBlock:
| args = exprArg+; blockAndT = block;
  { let block, terminators = blockAndT in
    args @ [block], terminators }

| args = exprArg+; blockAndT = block; remAndT = alternateExprArgsAndBlock;
  { let block = exprOfNoTerm blockAndT in
    let rem, terminators = remAndT in
    args @ [block] @ rem, terminators }

exprArgInner:
| id = IDENTIFIER;
  { idExprLoc id $startpos }

| OPEN_PAREN; e = kwexpr; CLOSE_PAREN;
  { e }

| OPEN_CURLY; CLOSE_CURLY;
  { withLoc (expr "op{}" []) $startpos }
| OPEN_CURLY; e = kwexpr; CLOSE_CURLY;
  { expr "op{}" [e] }

| OPEN_BRACKET; CLOSE_BRACKET;
  { withLoc (expr "op[]" []) $startpos }
| OPEN_BRACKET; e = kwexpr; CLOSE_BRACKET;
  { expr "op[]" [e] }

| q = QUOTE; id = IDENTIFIER;
  { expr (quoteId q) [idExprLoc id $startpos] }
| q = QUOTE; OPEN_CURLY; CLOSE_CURLY;
  { let e = expr (quoteId q) [withLoc (seqExpr []) $startpos] in
    { e with Ast2.location = Some (getLocation $startpos) } }
| q = QUOTE; OPEN_CURLY; e = expr; CLOSE_CURLY;
  { expr (quoteId q) [e] }
| q = QUOTE; OPEN_CURLY; blockAndT = block; CLOSE_CURLY;
  { let block, terminators = blockAndT in
    expectNoTerminators terminators;
    expr (quoteId q) [block] }
| q = QUOTE; blockAndT = block;
  { let block, terminators = blockAndT in
    expectNoTerminators terminators;
    expr (quoteId q) [block] }
| head = exprArgInner; OPEN_BRACKET_POSTFIX; arg = expr; CLOSE_BRACKET;
  { expr "postop[]" [head; arg] }
| head = exprArgInner; OPEN_ARGLIST; args = separated_list(COMMA, expr); CLOSE_PAREN;
  { callExpr (head :: args) }
| call = exprArgInner; op = POSTFIX_OP;
  { expr ("post" ^ opName op) [call] }

exprArg:
| s = PREFIX_OP; e = exprArg;
  { expr ("pre" ^ opName s) [e] }
| e = dotExpr;
  { e }

dotExpr:
| l = dotExpr; DOT; r = dotExpr;
  { expr "op." [l; r] }
| l = dotExpr; op = EXCLAMATION_OP; r = dotExpr;
  { expr (opName op) [l; r] }
| e = exprArgInner;
  { e }

%inline block:
| BEGIN_BLOCK; exprs = main2*; terminators = END_BLOCK;
  { seqExpr exprs, terminators }

opExpr:
| l = juxExpr; o = opSymbol; r = juxExpr;
  {expr (opName o) [l; r]}
| l = juxExpr; o = opSymbol; rAndT = block;
  { let r, terminators = rAndT in
    expectNoTerminators terminators;
    Ast2.expr (opName o) [l; r] }

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

