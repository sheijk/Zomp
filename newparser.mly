
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
      | { Ast2.id = exprId; args = [] } :: remExprs, firstTerm :: remTerms when exprId = firstTerm ->
          checkAll (remExprs, remTerms)
      | _, _ ->
          raiseParseError (Printf.sprintf "Invalid terminators: %s for %s"
                             (Common.combine " " terminators)
                             (Ast2.toString expr) )
    in
    checkAll (Ast2.idExpr expr.Ast2.id :: expr.Ast2.args, terminators)


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
(* %nonassoc DOT *)
%left POSTFIX_OP
%right PREFIX_OP

%start <Ast2.sexpr> main

%%

  main:
| id = IDENTIFIER; END;
  { Ast2.idExpr id }
| e = sexpr; END;
  { e }
| e = mexpr; END;
  { e }
| e = opexpr; END;
  { e }
| e = quoteexpr; END;
  { e }
| e = dotexpr; END;
  { e }

  sexpr:
| head = sexprArg; argsAndTerminators = sexprArg+;
  {
    let rec checkArgTerms = function
      | [] -> ()
      | [_, terminators] -> checkTerminators (fst head) terminators
      | (_, []) :: rem -> checkArgTerms rem
      | (_, invalidTerms) :: _ ->
          raiseParseError (Printf.sprintf "Expected no terminators but found %s"
                             (Common.combine " " invalidTerms))
    in
    checkArgTerms argsAndTerminators;
    let args = List.map fst argsAndTerminators in
    { Ast2.id = "opjux"; args = (fst head) :: args }}

%inline sexprArg:
| id = IDENTIFIER;
  { Ast2.idExpr id, [] }
| OPEN_PAREN; e = sexpr; CLOSE_PAREN;
  { e, [] }
| e = mexpr;
| e = opexpr;
| e = dotexpr;
  { e, [] }
| BEGIN_BLOCK; exprs = main+; terminators = END_BLOCK;
  { seqExpr exprs, terminators }
| e = quoteexpr;
  { e, [] }

mexpr:
| hd = mexprHead; OPEN_PAREN; CLOSE_PAREN;
  {{ Ast2.id = "opcall"; args = [hd] }}

| hd = mexprHead; OPEN_PAREN; arg = singleMexprArg; CLOSE_PAREN;
  {{ Ast2.id = "opcall"; args = [hd; arg] }}

| hd = mexprHead; OPEN_PAREN; arg1 = mexprArg; COMMA; arg2 = mexprArg; remArgs = mexprArgSep*; CLOSE_PAREN;
  { callExpr hd (arg1 :: arg2 :: remArgs) }

%inline mexprHead:
| id = IDENTIFIER;
  { idExpr id }
(* | OPEN_PAREN; q = QUOTE; id = IDENTIFIER; CLOSE_PAREN; *)
(*   { expr (quoteId q) [(idExpr id)] } *)

%inline mexprArgSep:
| COMMA; arg = mexprArg;
  { arg }

%inline mexprArg:
| arg = singleMexprArg;
  { arg }
| arg = sexpr;
  { arg }

%inline singleMexprArg:
| id = IDENTIFIER;
  { Ast2.idExpr id }
| e = opexpr;
  { e }
| e = mexpr;
  { e }
| e = quoteexpr;
  { e }

  opexpr:
| l = opexprArg; s = opsymbol; r = opexprArg;
  { expr (opName s) [l; r] }
| e = opexprArg; s = POSTFIX_OP;
  { expr ("post" ^ opName s) [e] }
| s = PREFIX_OP; e = opexprArg;
  { expr ("pre" ^ opName s) [e] }

  opexprArg:
| e = opexpr;
  { e }
| e = mexpr;
  { e }
| id = IDENTIFIER; (* causes shift/reduce conflict *)
  { idExpr id }
| e = quoteexpr;
  { e }
| e = dotexpr;
  { e }

%inline opsymbol:
| o = ADD_OP
| o = MULT_OP
| o = ASSIGN_OP
| o = COMPARE_OP
| o = LAZY_BOOL_OP
| o = STRICT_BOOL_OP
  { o }

quoteexpr:
| q = QUOTE; OPEN_CURLY; e = quotable; CLOSE_CURLY;
  { expr (quoteId q) [e] }
| q = QUOTE; id = IDENTIFIER;
  { expr (quoteId q) [idExpr id] }
| q = QUOTE; OPEN_CURLY; CLOSE_CURLY;
  {{ Ast2.id = quoteId q; args = [{Ast2.id = "seq"; args = []}] }}

%inline quotable:
| e = sexpr;
| e = mexpr;
| e = opexpr;
| e = dotexpr;
  { e }
| id = IDENTIFIER;
  { idExpr id }
| BEGIN_BLOCK; exprs = main*; terminators = END_BLOCK;
  (* causes reduce/reduce conflict *)
  { assert( List.length terminators = 0 );
    seqExpr exprs }


%inline dotexpr:
| l = dotexprFront; DOT; r = IDENTIFIER;
  { expr "op." [l; idExpr r] }
| l = dotexprFront; DOT; r = mexpr;
| l = dotexprFront; DOT; r = quoteexpr;
  { expr "op." [l; r] }

%inline dotexprFront:
| id = IDENTIFIER;
  { idExpr id }
| q = QUOTE; id = IDENTIFIER;
  (* causes conflict *)
  { expr (quoteId q) [(idExpr id)] }



