
%{
  open Printf
  open Ast2
  open Basics

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

  let getLocation loc =
    { fileName = loc.Lexing.pos_fname;
      line = loc.Lexing.pos_lnum;
      column = Some loc.Lexing.pos_cnum; }

  let withLoc expr lbloc = Ast2.withLoc (getLocation lbloc) expr

  let raiseParseError loc str = raise (ParseError (loc, str))
  let raiseParseErrorLex lexloc str = raiseParseError (getLocation lexloc) str
  let raiseParseErrorNoLoc str = raiseParseError fakeLocation str

  let raiseFoundButExpected lexloc found expected =
    raiseParseErrorLex
      lexloc
      (sprintf "expected '%s' but found '%s'" expected found)

  let raiseParseErrorMissingClosing errorPos openPosAndSymbol =
    let openPos, expectedSymbol = openPosAndSymbol in
    raiseParseErrorLex errorPos
      (sprintf "missing closing %s, opened at line %d:%d"
         expectedSymbol
         openPos.Lexing.pos_lnum
         openPos.Lexing.pos_cnum)

  (** Will cause value to be evaluated even if it is not needed. This only
      happens infrequently in the error case so it's fine. *)
  let expectClosing expectedSymbol (lexloc, foundSymbol) value =
    if expectedSymbol = foundSymbol then
      value
    else
      raiseFoundButExpected lexloc foundSymbol expectedSymbol

  let idExprLoc id loc =
    let l = getLocation loc in
    Ast2.idExprLoc l id

  let quoteId = function
    | "`" -> "quote"
    | "$" -> "quote"
    | "$$" -> "quoteasis"
    | "``" -> "quoteasis"
    | "#" -> "antiquote"
    | invalidString ->
        raise (ParseError
                 (fakeLocation,
                  Printf.sprintf
                    "invalid quoting char: %s"
                    invalidString))

  let opName opSymbol = "op" ^ opSymbol

  let mergeJux l r =
    match l, r with
      | {id = "opjux"; args = largs }, _ ->
          exprInferLoc "opjux" (largs @ [r])
      | _, {id = "opjux"; args = rargs } ->
          exprInferLoc "opjux" (l :: rargs)
      | _ ->
          exprInferLoc "opjux" [l;r]

  let checkTerminators expr terminators =
    let rec checkAll = function
      | _, [] ->
          ()
      | { id = exprId; args = [] } :: remExprs, firstTerm :: remTerms
          when exprId = firstTerm ->
          checkAll (remExprs, remTerms)
      | _, _ ->
          raiseParseErrorNoLoc
            (Printf.sprintf "invalid terminators: %s for %s"
               (Common.combine " " terminators)
               (toString expr) )
    in
    checkAll (Ast2.idExprLoc Basics.fakeLocation expr.id :: expr.args, terminators)

  let expectNoTerminators = function
    | [] -> ()
    | invalidTerminators ->
        raiseParseErrorNoLoc (Printf.sprintf "expected no terminators but found: %s"
                                (Common.combine " " invalidTerminators))

  let exprOfNoTerm ((expr :sexpr), (terminators :string list)) =
    match terminators with
      | [] -> expr
      | invalidTerminators -> raiseParseErrorNoLoc
          (Printf.sprintf "expected no terminators but found %s in %s"
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
          raiseParseErrorNoLoc (Printf.sprintf "expected no terminators but found %s in %s"
                             (Common.combine " " invalidTerminators)
                             (toString expr))
    in
    worker [] exprAndTerminators

  let rec extractKeywordsAndExprsAndCheckTerminators = function
    | [] -> []
    | [(keyword, (expr, terminators))] ->
        checkTerminators (Ast2.idExprLoc fakeLocation keyword) terminators;
        [(keyword, expr)]
    | (keyword, (expr, [])) :: rem ->
        (keyword, expr) :: extractKeywordsAndExprsAndCheckTerminators rem
    | (keyword, (expr, terminators)) :: _ ->
        raiseParseErrorNoLoc (Printf.sprintf "found invalid terminators '%s' after block in expr %s"
                           (Common.combine ", " terminators)
                           (toString expr))

  let keywordAndExprToList (keyword, expr) = [Ast2.idExprLoc fakeLocation keyword; expr]
%}

%token <string> IDENTIFIER
%token END
%token EOF
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

%start <Ast2.sexpr list> main

%%

main:
| e = terminatedExpr* EOF;
  { e }

terminatedExpr:
| e = expr END;
  { e }

expr:
| e = infixExpr;
  { e }

infixExpr:
| l = infixExpr; op = opSymbol; r = infixExpr;
  { exprInferLoc (opName op) [l; r] }
| c = juxExpr;
  { c }

juxExpr:
| hd = prepostExpr; exprs = prepostExpr+;
  { juxExprInferLoc (hd :: exprs) }
| e = prepostExpr;
  { e }

prepostExpr:
| e = closedExpr;
  { e }

closedExpr:
| id = IDENTIFIER;
  { idExprLoc id $startpos }

| q = QUOTE; OPEN_CURLY; CLOSE_CURLY;
  { let e = exprInferLoc (quoteId q) [withLoc (seqExprInferLoc []) $startpos] in
    Ast2.withLoc (getLocation $startpos) e }
| q = QUOTE; OPEN_CURLY; e = expr; CLOSE_CURLY;
  { exprInferLoc (quoteId q) [e] }
| q = QUOTE; e = closedExpr;
  { exprInferLoc (quoteId q) [e] }

| op = PREFIX_OP; e = closedExpr;
  { exprInferLoc ("pre" ^ opName op) [e] }
| e = closedExpr; op = POSTFIX_OP;
  { exprInferLoc ("post" ^ opName op) [e] }

| l = closedExpr; DOT; r = closedExpr;
  { exprInferLoc "op." [l; r] }
| l = closedExpr; op = EXCLAMATION_OP; r = closedExpr;
  { exprInferLoc (opName op) [l; r] }

| OPEN_PAREN; e = expr; posAndSymbol = closeBrackets;
  { expectClosing ")" posAndSymbol e }

| OPEN_CURLY; posAndSymbol = closeBrackets;
  { expectClosing "}" posAndSymbol (withLoc (exprInferLoc "op{}" []) $startpos) }
| OPEN_CURLY; e = expr; posAndSymbol = closeBrackets;
  { expectClosing "}" posAndSymbol (exprInferLoc "op{}" [e]) }

| OPEN_BRACKET; posAndSymbol = closeBrackets;
  { expectClosing "]" posAndSymbol (withLoc (exprInferLoc "op[]" []) $startpos) }
| OPEN_BRACKET; e = expr; posAndSymbol = closeBrackets;
  { expectClosing "]" posAndSymbol (exprInferLoc "op[]" [e]) }

| posAndSymbol = openBrackets; expr; e = error;
  { ignore e;
    raiseParseErrorMissingClosing $startpos(e) posAndSymbol }


| head = closedExpr; OPEN_ARGLIST; args = separated_list(COMMA, expr); posAndSymbol = closeBrackets;
  { expectClosing ")" posAndSymbol (callExprInferLoc (head :: args)) }
| closedExpr; o = OPEN_ARGLIST; separated_list(COMMA, expr); e = error;
  { ignore e;
    ignore o;
    raiseParseErrorMissingClosing $startpos(e) ($startpos(o), ")") }

| head = closedExpr; OPEN_BRACKET_POSTFIX; arg = expr; posAndSymbol = closeBrackets;
  { expectClosing "]" posAndSymbol (exprInferLoc "postop[]" [head; arg]) }
| closedExpr; o = OPEN_BRACKET_POSTFIX; e = error;
  { ignore e;
    ignore o;
    raiseParseErrorMissingClosing $startpos(e) ($startpos(o), "]") }

| BEGIN_BLOCK; exprs = terminatedExpr*; END_BLOCK;
  { seqExprInferLoc exprs }

%inline closeBrackets:
| c = CLOSE_PAREN;
  { ignore c; $startpos(c), ")" }
| c = CLOSE_BRACKET;
  { ignore c; $startpos(c), "]" }
| c = CLOSE_CURLY;
  { ignore c; $startpos(c), "}" }

%inline openBrackets:
| c = OPEN_PAREN;
  { ignore c;
    $startpos(c), ")" }
| c = OPEN_CURLY;
  { ignore c;
    $startpos(c), "}" }
| c = OPEN_BRACKET;
  { ignore c;
    $startpos(c), "]" }

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

