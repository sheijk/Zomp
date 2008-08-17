
open Indentlexer
open Testing
open Printf
open Newparser

module IndentLexerTestCase : CASE_STRUCT =
struct
  type input = string
  type output = token list

  let printInput str = printf "%s" str
    
  let printOutput tokens = printTokens tokens 

  let outputEqual l r =
    if List.length l = List.length r then
      List.for_all2 (=) l r
    else
      false
    
  type result = [ `Return of output | `Exception of string ]

  let testedFunc str =
    if str.[0] = '\n' then
      lexString str
    else
      lexString str

  let testCases : (input * result) list =
    let id x = IDENTIFIER x in
    let ids stringList =
      let idTokens = List.map (fun str -> (IDENTIFIER str :> token)) stringList in
      idTokens
    in
    let parsedAsId id =
      id, `Return [IDENTIFIER id; END]
    in
    let infixOp op optoken =
      ["l " ^ op ^ " r", `Return [id "l"; optoken; id "r"; END];
       parsedAsId ("op" ^ op)]
    in
    let isValidId id = id, `Return [IDENTIFIER id; END] in
    ignore( ids [] );
    [
      isValidId "single";
      isValidId "123";
      isValidId "foo:bar";
      isValidId "_";
      isValidId "_1";
      isValidId "foo_";

      "foo(3) + 1", `Return [id "foo"; OPEN_ARGLIST; id "3"; CLOSE_PAREN;
                             ADD_OP "+"; id "1"; END];

      "foo (3) + 1", `Return [id "foo"; OPEN_PAREN; id "3"; CLOSE_PAREN;
                             ADD_OP "+"; id "1"; END];

      "foo bar", `Return [id "foo"; id "bar"; END];

      "a+b", `Return [id "a"; ADD_OP "+"; id "b"; END];
      "a   + b", `Return [id "a"; ADD_OP "+"; id "b"; END];

      "a,b", `Return [id "a"; COMMA; id "b"; END];
      "x, y", `Return [id "x"; COMMA; id "y"; END];
      "foo , bar", `Return [id "foo"; COMMA; id "bar"; END];

      "a +_f b", `Return [id "a"; ADD_OP "+_f"; id "b"; END];

      "(a + b)*c", `Return [
        OPEN_PAREN; id "a"; ADD_OP "+"; id "b"; CLOSE_PAREN;
        MULT_OP "*"; id "c"; END];

      "space... ", `Return [id "space"; POSTFIX_OP "..."; END];
      "lineend...", `Return [id "lineend"; POSTFIX_OP "..."; END];

      "&blah", `Return [PREFIX_OP "&"; id "blah"; END];
      "*deref", `Return [PREFIX_OP "*"; id "deref"; END];
      "foo *ptr", `Return [id "foo"; PREFIX_OP "*"; id "ptr"; END];
      "float*", `Return [id "float"; POSTFIX_OP "*"; END];
      "float* var", `Return [id "float"; POSTFIX_OP "*"; id "var"; END];

      "opAtEndOfLine = ", `Return [id "opAtEndOfLine"; ASSIGN_OP "="; END];
      "opAtEndOfLine =", `Return [id "opAtEndOfLine"; ASSIGN_OP "="; END];

      (* strings and numbers *)
      "1337", `Return [id "1337"; END];
      "10.3", `Return [id "10.3"; END];
      "100.", `Return [id "100."; END];
      ".01", `Return [id ".01"; END];
      "\"foobar\"", `Return [id "\"foobar\""; END];
      "\"windows\\\\path\"", `Return [id "\"windows\\\\path\""; END];
      "'x'", `Return [id "'x'"; END];
      "'\\n'", `Return [id "'\\n'"; END];
      "'\\0'", `Return [id "'\\0'"; END];
      "'\\\\'", `Return [id "'\\\\'"; END];

      (* quotes *)
      "$", `Return [QUOTE "$"; END];
      "#", `Return [QUOTE "#"; END];

      "${foo}", `Return [QUOTE "$"; OPEN_CURLY; id "foo"; CLOSE_CURLY; END];
      "${class\n  child1\nend}",
      `Return [QUOTE "$";
               OPEN_CURLY;
               id "class"; BEGIN_BLOCK; id "child1"; END; END_BLOCK [];
               CLOSE_CURLY; END];

      (* simple one-line expressions *)
      "var int y\n", `Return( ids ["var"; "int"; "y"] @ [END] );
      "var int x", `Return( ids ["var"; "int"; "x"] @ [END] );

      "first line\nsecond line\n",
      `Return( [id "first"; id "line"; END;
                id "second"; id "line"; END] );

      (* simple multi-line expressions *)
      "if a then\n\
      \  foobar\n\
      end",
      `Return( ids ["if"; "a"; "then"] @ [BEGIN_BLOCK]
               @ [id "foobar"; END]
               @ [END_BLOCK []; END] );

      (* block end with tokens *)
      "foreach num IN primes\n\
      \  printLine num\n\
      end foreach num",
      `Return( ids ["foreach"; "num"; "IN"; "primes"] @ [BEGIN_BLOCK]
               @ ids ["printLine"; "num"] @ [END]
               @ [END_BLOCK ["foreach"; "num"]; END] );

      (* multi-part multi-line expressions *)
      "if cond then\n\
      \  print 1\n\
      else\n\
      \  print 2\n\
      end",
      `Return( ids ["if"; "cond"; "then"] @ [BEGIN_BLOCK]
               @ ids ["print"; "1"] @ [END]
               @ [END_BLOCK []; id "else"; BEGIN_BLOCK]
               @ ids ["print"; "2"] @ [END]
               @ [END_BLOCK []; END] );

      (* leading whitespace/newlines *)
      "   a b c", `Return (ids ["a"; "b"; "c"] @ [END]);
      "first\n\n\nsecond", `Return [id "first"; END; id "second"; END];
      "\n\n\n\nfirst\n\n\n", `Return [id "first"; END];

      (* fail if indent level is reduced too much *)
      "main\n\
      \  begin foo\n\
      \    body\n\
      next\n",
      `Exception "Should fail because indent level is reduced too much";

(*       "main blah\n\ *)
(*       \  nested\n\ *)
(*       \    body\n\ *)
(*       \  nested2\n\ *)
(*       end main", *)
(*       `Exception "Should fail because \"nested\" has no end terminator"; *)

      parsedAsId "op&&";
      parsedAsId "op||";
      parsedAsId "op++";
      parsedAsId "op*";
      (* TODO: alle operatoren testen *)

      "foo: bar", `Return [KEYWORD_ARG "foo"; id "bar"; END];
      "if: foo then:", `Return [KEYWORD_ARG "if"; id "foo"; KEYWORD_ARG "then"; END];
      (* "this is a line:", `Return [id "this"; id "is"; id "a"; id "line"; *)
      (*                             EXTENDED_INDENT; END]; *)
    ]
    @ infixOp "+" (ADD_OP "+")
    @ infixOp "-" (ADD_OP "-")
    @ infixOp "&&" (LAZY_BOOL_OP "&&")
    @ infixOp "||" (LAZY_BOOL_OP "||")
end

let () =
  let module M = Tester(IndentLexerTestCase) in
  M.runTestsAndReport "indentlexer"
  
let () =
  let testCases = [
    "iff", "end", true;
    "iff", "end iff", true;
    "iff", "end wrong", false;
    "iff", "end iff  ", true;
    "iff", "end iff blah", false;
    "iff", "end   iff", true;
  ] in
  
  let blockEndRE name = sprintf "end\\( +%s\\)? *$" name in

  printf "\n";
  let boolToString b = if b then "true" else "false" in
  let errorOccured = ref false in
  let testF (blockName, line, shallEnd) =
    if shallEnd != (line =~ blockEndRE blockName) then begin
      errorOccured := true;
      printf "Failure in blockEndRE:\n  Input = %s\n  Expected = %s\n  Found = %s\n"
        (sprintf "%s, '%s'" blockName line) (boolToString shallEnd) (boolToString (not shallEnd))
    end
  in
  List.iter testF testCases

let () =
  let l = lexbufFromString "d.zomp" "abcde" in
  let expectChar chr = assert( chr = l.readChar() ) in
  expectChar 'a';
  expectChar 'b';
  l.backTrack 2;
(*   l.putbackString "x"; *)
(*   l.putbackString "y"; *)
  expectChar 'a';
  expectChar 'b'
  

