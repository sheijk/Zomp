
open Indentlexer
open Testing
open Printf
open Newparser
open Common

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
      isValidId "0x123";
      isValidId "0x1f";
      isValidId "0b1011";

      (* floating point numbers *)
      isValidId "0.0";
      isValidId "20.031";
      isValidId "1.";
      isValidId "856.";
      isValidId ".3";
      isValidId "1.0d";
      isValidId "7.0f";
      isValidId ".3d";
      isValidId "10.3";
      isValidId ".01";

      (* strings and numbers *)
      isValidId "1337";
      isValidId "-20";
      isValidId "\"foobar\"";
      isValidId "\"windows\\\\path\"";
      isValidId "'x'";
      isValidId "'\\n'";
      isValidId "'\\0'";
      isValidId "'\\\\'";

      (* operators *)
      "foo(3) + 1", `Return [id "foo"; OPEN_ARGLIST; id "3"; CLOSE_PAREN;
                             ADD_OP "+"; id "1"; END];

      "foo (3) + 1", `Return [id "foo"; OPEN_PAREN; id "3"; CLOSE_PAREN;
                             ADD_OP "+"; id "1"; END];

      "foo bar", `Return [id "foo"; id "bar"; END];

      "a+b", `Return [id "a"; ADD_OP "+"; id "b"; END];
      "a   + b", `Return [id "a"; ADD_OP "+"; id "b"; END];
      "2+3", `Return [id "2"; ADD_OP "+"; id "3"; END];
      "1.0 + 2.0", `Return [id "1.0"; ADD_OP "+"; id "2.0"; END];
      "1. + 2.", `Return [id "1."; ADD_OP "+"; id "2."; END];
      "1.+2.", `Return [id "1."; ADD_OP "+"; id "2."; END];
      "FOO*bar", `Return [id "FOO"; MULT_OP "*"; id "bar"; END];
      "x[1]-2", `Return [id "x"; OPEN_BRACKET_POSTFIX; id "1"; CLOSE_BRACKET;
                         ADD_OP "-"; id "2"; END];
      "x[1]/3", `Return [id "x"; OPEN_BRACKET_POSTFIX; id "1"; CLOSE_BRACKET;
                         MULT_OP "/"; id "3"; END];

      "a,b", `Return [id "a"; COMMA; id "b"; END];
      "x, y", `Return [id "x"; COMMA; id "y"; END];
      "foo , bar", `Return [id "foo"; COMMA; id "bar"; END];

      "a +_f b", `Return [id "a"; ADD_OP "+_f"; id "b"; END];

      "(a + b)*c", `Return [
        OPEN_PAREN; id "a"; ADD_OP "+"; id "b"; CLOSE_PAREN;
        MULT_OP "*"; id "c"; END];
      "10*(a+b)", `Return [
        id "10"; MULT_OP "*";
        OPEN_PAREN;
        id "a"; ADD_OP "+"; id "b";
        CLOSE_PAREN; END];

      "10+*ptr", `Return [id "10"; ADD_OP "+"; PREFIX_OP "*"; id "ptr"];

      "space... ", `Return [id "space"; POSTFIX_OP "..."; END];
      "lineend...", `Return [id "lineend"; POSTFIX_OP "..."; END];

      "foo.bar", `Return [id "foo"; DOT; id "bar"; END];
      "foo.bar.baz", `Return [id "foo"; DOT; id "bar"; DOT; id "baz"; END];
      "x.10", `Return [id "x"; DOT; id "10"; END];
      "3.toInt", `Return [id "3"; DOT; id "toInt"; END];

      "&blah", `Return [PREFIX_OP "&"; id "blah"; END];
      "*&blah", `Return [PREFIX_OP "*"; PREFIX_OP "&"; id "blah"; END];
      "ppInt**++", `Return [id "ppInt"; POSTFIX_OP "*"; POSTFIX_OP "*";
                            POSTFIX_OP "++"; END];
      "foo(&x)", `Return [id "foo";
                          OPEN_ARGLIST; PREFIX_OP "&"; id "x"; CLOSE_PAREN; END];
      "foo( &x )", `Return [id "foo";
                            OPEN_ARGLIST; PREFIX_OP "&"; id "x"; CLOSE_PAREN; END];
      "blah(*x)", `Return [id "blah"; OPEN_ARGLIST;
                           PREFIX_OP "*"; id "x"; CLOSE_PAREN; END];
      "blup(x*)", `Return [id "blup"; OPEN_ARGLIST; id "x";
                           POSTFIX_OP "*"; CLOSE_PAREN; END];
      "blup(x*++)", `Return [id "blup"; OPEN_ARGLIST; id "x";
                            POSTFIX_OP "*"; POSTFIX_OP "++";
                           CLOSE_PAREN; END];
      "*deref", `Return [PREFIX_OP "*"; id "deref"; END];
      "foo *ptr", `Return [id "foo"; PREFIX_OP "*"; id "ptr"; END];
      "float*", `Return [id "float"; POSTFIX_OP "*"; END];
      "float* var", `Return [id "float"; POSTFIX_OP "*"; id "var"; END];

      "char**", `Return [id "char"; POSTFIX_OP "*"; POSTFIX_OP "*"; END];
      "(float*)*", `Return [OPEN_PAREN; id "float"; POSTFIX_OP "*";
                          CLOSE_PAREN; POSTFIX_OP "*"; END];
      "(foo baz*)", `Return
        [OPEN_PAREN; id "foo"; id "baz"; POSTFIX_OP "*"; CLOSE_PAREN; END];

      "opAtEndOfLine = ", `Return [id "opAtEndOfLine"; ASSIGN_OP "="; END];
      "opAtEndOfLine =", `Return [id "opAtEndOfLine"; ASSIGN_OP "="; END];

      "[10]", `Return [OPEN_BRACKET; id "10"; CLOSE_BRACKET; END];
      "array[3]", `Return [id "array";
                           OPEN_BRACKET_POSTFIX; id "3"; CLOSE_BRACKET; END];
      "_[_]", `Return [id "_";
                       OPEN_BRACKET_POSTFIX; id "_"; CLOSE_BRACKET; END];

      "int*[10]", `Return [id "int"; POSTFIX_OP "*";
                           OPEN_BRACKET_POSTFIX; id "10"; CLOSE_BRACKET; END];

      (let semi = LAZY_BOOL_OP ";" in
      "a;b;c",  `Return [id "a"; semi; id "b"; semi; id "c"; END]);

      "l +. r", `Return [id "l"; ADD_OP "+."; id "r"; END];

      "a op+ b", `Return [id "a"; id "op+"; id "b"; END];
      "a op* b", `Return [id "a"; id "op*"; id "b"; END];
      "a op/ b", `Return [id "a"; id "op/"; id "b"; END];
      "a op+_foo b", `Return [id "a"; id "op+_foo"; id "b"; END];
      "x op+_u32_u32 y", `Return [id "x"; id "op+_u32_u32"; id "y"; END];
      "a op; b", `Return [id "a"; id "op;"; id "b"; END];
      "a op, b", `Return [id "a"; id "op,"; id "b"; END];
      "foo op:= bar", `Return [id "foo"; id "op:="; id "bar"; END];
      "l op+. r", `Return [id "l"; id "op+."; id "r"; END];

      (* quotes *)
      "$", `Return [QUOTE "$"; END];
      "#", `Return [QUOTE "#"; END];
      "$$", `Return [QUOTE "$$"; END];

      "${foo}", `Return [QUOTE "$"; OPEN_CURLY; id "foo"; CLOSE_CURLY; END];
      "${class\n  child1\nend}",
      `Return [QUOTE "$";
               OPEN_CURLY;
               id "class"; BEGIN_BLOCK; id "child1"; END; END_BLOCK [];
               CLOSE_CURLY; END];

      "foo()", `Return [id "foo"; OPEN_ARGLIST; CLOSE_PAREN; END];
      "\"strfun\"(a)", `Return [id "\"strfun\"";
                                OPEN_ARGLIST; id "a"; CLOSE_PAREN; END];

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

      parsedAsId "op&&";
      parsedAsId "op||";
      parsedAsId "op++";
      parsedAsId "op*";
      parsedAsId "op==_str";
      (* TODO: alle operatoren testen *)

      "foo: bar", `Return [KEYWORD_ARG "foo"; id "bar"; END];
      "if: foo then:", `Return [KEYWORD_ARG "if"; id "foo";
                                KEYWORD_ARG "then"; END];
      (* "this is a line:", `Return [id "this"; id "is"; id "a"; id "line"; *)
      (*                             EXTENDED_INDENT; END]; *)

      (** reproduce various bugs *)
      "(num--)", `Return [OPEN_PAREN; id "num"; POSTFIX_OP "--";
                          CLOSE_PAREN; END];
      "${foo*}", `Return [QUOTE "$"; OPEN_CURLY; id "foo";
                          POSTFIX_OP "*"; CLOSE_CURLY; END];
      "[i++]", `Return [OPEN_BRACKET; id "i"; POSTFIX_OP "++";
                        CLOSE_BRACKET; END];
      "structptr*.member", `Return [id "structptr"; POSTFIX_OP "*";
                                    DOT; id "member"; END];

      "foo 1.\nbar 2", `Return [id "foo"; id "1."; END; id "bar"; id "2"; END];
      "foo(1.,0.)", `Return [id "foo"; OPEN_ARGLIST;
                             id "1."; COMMA; id "0."; CLOSE_PAREN; END];
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

let () = Indentlexer.runInternalTests()


