
open Indentlexer
open Testing
open Printf
open Newparser
open Common

module IndentLexerTestCase : sig
  include CASE_STRUCT
  val validateTestCases : unit -> string option
end = struct
  type input = string
  type output = token list

  let inputToString str = sprintf "%s" str
  let outputToString tokens = tokensToString tokens

  let outputEqual l r =
    if List.length l = List.length r then
      List.for_all2 (=) l r
    else
      false

  type result = [ `Return of output | `Exception of string ]

  let testedFunc = lexString

  let testCases : (input * result) list =
    let id x = IDENTIFIER x in
    let ids stringList =
      let idTokens = List.map (fun str -> (IDENTIFIER str :> token)) stringList in
      idTokens
    in
    let isValidId id = id, `Return [IDENTIFIER id; END] in
    let isValidBinOp tokenF op =
      ["l" ^ op ^ "r", `Return [id "l"; tokenF op; id "r"; END];
       isValidId ("op" ^ op)]
    in
    let areValidInfixOps ops tokenF = List.flatten (List.map (isValidBinOp tokenF) ops) in
    let isValidPrefixOp op = (op ^ "id"), `Return [PREFIX_OP op; id "id"; END] in
    let isValidPostfixOp op = ("id" ^ op), `Return [id "id"; POSTFIX_OP op; END] in
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
      isValidId "-10.0";
      isValidId "-9.";
      isValidId "-.3";
      isValidId "-6.234f";
      isValidId "0x12";
      isValidId "0xff";
      isValidId "0xFF";
      isValidId "0xf0";
      isValidId "0xF0";
      isValidId "0x1c";
      isValidId "0x1C";
      isValidId "0b1001";
      isValidId "1_000";
      isValidId "20_090";
      isValidId "451_034";
      (** lexer does not validate int constants *)
      isValidId "1_23_000";

      (* strings and numbers *)
      isValidId "1337";
      isValidId "-20";
      isValidId "\"foobar\"";
      isValidId "\"windows\\\\path\"";
      isValidId "\"with escaped\\\" string quote\"";
      isValidId "\"\"";
      isValidId "'x'";
      isValidId "'\\n'";
      isValidId "'\\0'";
      isValidId "'\\\\'"; (* that's two backslashes in zomp syntax.. *)
      isValidId "'\\''";

      (* comments *)
      "a // b c d", `Return [id "a"; END];
      "a /* xxx */ b", `Return [id "a"; id "b"; END];
      "\" //foo\"", `Return [id "\" //foo\""; END];
      "\"//foo\"", `Return [id "\"//foo\""; END];
      (let str = "\" /*no comment*/\"" in
       sprintf "stringlit %s" str, `Return [id "stringlit"; id str; END]);
      (let str = "\"/*no comment*/\"" in
       sprintf "stringlit %s" str, `Return [id "stringlit"; id str; END]);
      "'\"' /*'*/ x",
      `Return [id "'\"'"; id "x"; END];
      (let str = "\"with escaped\\\" /*string*/ quote\"" in
       sprintf "stringlit %s" str, `Return [id "stringlit"; id str; END]);

      (* operators *)
      "foo(3) + 1", `Return [id "foo"; OPEN_ARGLIST; id "3"; CLOSE_PAREN;
                             ADD_OP "+"; id "1"; END];
      "(foo*)(a, b)",
      `Return [OPEN_PAREN; id "foo"; POSTFIX_OP "*"; CLOSE_PAREN;
               OPEN_ARGLIST; id "a"; COMMA; id "b"; CLOSE_PAREN; END];

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

      "ptr2array*[7]", `Return [id "ptr2array"; POSTFIX_OP "*";
                                OPEN_BRACKET_POSTFIX; id "7"; CLOSE_BRACKET; END];

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

      "10 + *ptr", `Return [id "10"; ADD_OP "+"; PREFIX_OP "*"; id "ptr"; END];

      "op*(10)", `Return [id "op*"; OPEN_ARGLIST; id "10"; CLOSE_PAREN; END];
      "3*(10)", `Return [id "3"; MULT_OP "*"; OPEN_PAREN; id "10"; CLOSE_PAREN; END];

      "space... ", `Return [id "space"; POSTFIX_OP "..."; END];
      "lineend...", `Return [id "lineend"; POSTFIX_OP "..."; END];

      "1.0++", `Return [id "1.0"; POSTFIX_OP "++"; END];
      " ++x:y", `Return [PREFIX_OP "++"; id "x:y"; END];

      "a = b", `Return [id "a"; ASSIGN_OP "="; id "b"; END];
      "x += 10", `Return [id "x"; ASSIGN_OP "+="; id "10"; END];
      "x -= 10", `Return [id "x"; ASSIGN_OP "-="; id "10"; END];
      "x *= 10", `Return [id "x"; ASSIGN_OP "*="; id "10"; END];
      "x /= 10", `Return [id "x"; ASSIGN_OP "/="; id "10"; END];
      "x &= 10", `Return [id "x"; ASSIGN_OP "&="; id "10"; END];
      "x |= 10", `Return [id "x"; ASSIGN_OP "|="; id "10"; END];
      "x %= 10", `Return [id "x"; ASSIGN_OP "%="; id "10"; END];
      "x ++= 10", `Return [id "x"; ASSIGN_OP "++="; id "10"; END];

      "-10", `Return [id "-10"; END];
      "-x", `Return [PREFIX_OP "-"; id "x"; END];
      "!true", `Return [PREFIX_OP "!"; id "true"; END];

      "foo.bar", `Return [id "foo"; DOT; id "bar"; END];
      "foo.bar.baz", `Return [id "foo"; DOT; id "bar"; DOT; id "baz"; END];
      "x.10", `Return [id "x"; DOT; id "10"; END];
      "3.toInt", `Return [id "3"; DOT; id "toInt"; END];

      (** prefix and postfix operators *)
      isValidPrefixOp "*";
      isValidPrefixOp "&";
      isValidPrefixOp "++";
      isValidPrefixOp "--";
      isValidPrefixOp "!";
      isValidPrefixOp "?";
      isValidPrefixOp "-";
      isValidPrefixOp "+";
      isValidPrefixOp "~";

      isValidPostfixOp "++";
      isValidPostfixOp "--";
      isValidPostfixOp "...";
      isValidPostfixOp "*";
      isValidPostfixOp "&";
      isValidPostfixOp "?";
      isValidPostfixOp "!";
      isValidPostfixOp "+";

      "&blah", `Return [PREFIX_OP "&"; id "blah"; END];
      "blubber&", `Return [id "blubber"; POSTFIX_OP "&"; END];
      "--b", `Return [PREFIX_OP "--"; id "b"; END];
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
      "foo()++", `Return [id "foo"; OPEN_ARGLIST; CLOSE_PAREN; POSTFIX_OP "++"; END];
      "*deref", `Return [PREFIX_OP "*"; id "deref"; END];
      "foo *ptr", `Return [id "foo"; PREFIX_OP "*"; id "ptr"; END];
      "float*", `Return [id "float"; POSTFIX_OP "*"; END];
      "float* var", `Return [id "float"; POSTFIX_OP "*"; id "var"; END];
      "&#blah", `Return [PREFIX_OP "&"; QUOTE "#"; id "blah"; END];

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

      "[]", `Return [OPEN_BRACKET; CLOSE_BRACKET; END];
      "{}", `Return [OPEN_CURLY; CLOSE_CURLY; END];
      "[1 2]", `Return [OPEN_BRACKET; id "1"; id "2"; CLOSE_BRACKET; END];
      "{x y}", `Return [OPEN_CURLY; id "x"; id "y"; CLOSE_CURLY; END];

      (let semi = SEMICOLON ";" in
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
      "${class:\n  child1\nend}",
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

      (* simple multi-line expressions *)
      "first line\nsecond line\n",
      `Return( [id "first"; id "line"; END;
                id "second"; id "line"; END] );

      "this is a very long line\n" ^
        "  continued on the next one",
      `Return([id "this"; id "is"; id "a"; id "very"; id "long"; id "line";
               id "continued"; id "on"; id "the"; id "next"; id "one"; END]);

      "the continued\n" ^
        "  line here\n" ^
        "next line",
      `Return([id "the"; id "continued"; id "line"; id "here"; END;
               id "next"; id "line"; END]);

      "indent block:\n" ^
        "  content of block\n" ^
        "end",
      `Return [id "indent"; id "block"; BEGIN_BLOCK;
               id "content"; id "of"; id "block"; END;
               END_BLOCK[]; END];

      "foo (bar:\n\
      \  line0\n\
      end)",
      `Return [id "foo"; OPEN_PAREN; id "bar"; BEGIN_BLOCK;
               id "line0"; END;
               END_BLOCK []; CLOSE_PAREN; END];

      "foo (:\n\
      \  line1\n\
      end) bar",
      `Return [id "foo"; OPEN_PAREN; BEGIN_BLOCK;
               id "line1"; END;
               END_BLOCK []; CLOSE_PAREN; id "bar"; END];

      "foo (blk:\n\
      \  line1\n\
      end blk) bar",
      `Return [id "foo"; OPEN_PAREN; id "blk"; BEGIN_BLOCK;
               id "line1"; END;
               END_BLOCK ["blk"]; CLOSE_PAREN; id "bar"; END];

      "multi block:\n" ^
        "  first line\n" ^
        "  second line\n" ^
        "else:\n" ^
        "  third line\n" ^
        "end",
      `Return [id "multi"; id "block"; BEGIN_BLOCK;
               id "first"; id "line"; END;
               id "second"; id "line"; END;
               END_BLOCK []; id "else"; BEGIN_BLOCK;
               id "third"; id "line"; END;
               END_BLOCK []; END];

      "cont begin\n" ^
        "  of block:\n" ^
        "  line 1\n" ^
        "  line 2\n" ^
        "end",
      `Return [id "cont"; id "begin"; id "of"; id "block"; BEGIN_BLOCK;
               id "line"; id "1"; END;
               id "line"; id "2"; END;
               END_BLOCK []; END];

      "if a then:\n\
      \  foobar\n\
      end",
      `Return( ids ["if"; "a"; "then"] @ [BEGIN_BLOCK]
               @ [id "foobar"; END]
               @ [END_BLOCK []; END] );

      (* block end with tokens *)
      "foreach num IN primes:\n\
      \  printLine num\n\
      end foreach num",
      `Return( ids ["foreach"; "num"; "IN"; "primes"] @ [BEGIN_BLOCK]
               @ ids ["printLine"; "num"] @ [END]
               @ [END_BLOCK ["foreach"; "num"]; END] );

      "block:\n\
      \  foobar\n\
      end block",
      `Return [IDENTIFIER "block"; BEGIN_BLOCK;
               IDENTIFIER "foobar"; END;
               END_BLOCK ["block"]; END];

      "block:\n\" ^
      \  foobar()\n\
      end wrong terminator",
      `Exception "Tokens following end must appear line starting block";

      (* multi-part multi-line expressions *)
      "if cond then:\n\
      \  print 1\n\
      else:\n\
      \  print 2\n\
      end",
      `Return( ids ["if"; "cond"; "then"] @ [BEGIN_BLOCK]
               @ ids ["print"; "1"] @ [END]
               @ [END_BLOCK []; id "else"; BEGIN_BLOCK]
               @ ids ["print"; "2"] @ [END]
               @ [END_BLOCK []; END] );

      "empty block:\nend", `Return [IDENTIFIER "empty"; IDENTIFIER "block";
                                    BEGIN_BLOCK; END_BLOCK []; END];

      "switch expr:\n\
      case 1:\n\
      \  print one\n\
      default:\n\
      \  stuff\n\
      end",
      `Return [IDENTIFIER "switch"; IDENTIFIER "expr"; BEGIN_BLOCK; END_BLOCK [];
               IDENTIFIER "case"; IDENTIFIER "1"; BEGIN_BLOCK;
               IDENTIFIER "print"; IDENTIFIER "one"; END;
               END_BLOCK []; IDENTIFIER "default"; BEGIN_BLOCK;
               IDENTIFIER "stuff"; END;
               END_BLOCK []; END];

      "class:\n\
       public:\n\
       private:\n\
       end",
      `Return [IDENTIFIER "class"; BEGIN_BLOCK; END_BLOCK[];
               IDENTIFIER "public"; BEGIN_BLOCK; END_BLOCK[];
               IDENTIFIER "private"; BEGIN_BLOCK; END_BLOCK[]; END];

      "empty ${seq:\nend}",
      `Return [IDENTIFIER "empty"; QUOTE "$"; OPEN_CURLY; IDENTIFIER "seq"; BEGIN_BLOCK;
               END_BLOCK []; CLOSE_CURLY; END];

      (* leading whitespace/newlines *)
      "   a b c", `Return (ids ["a"; "b"; "c"] @ [END]);
      "first\n\n\nsecond", `Return [id "first"; END; id "second"; END];
      "\n\n\n\nfirst\n\n\n", `Return [id "first"; END];

      (* fail if indent level is reduced too much *)
      "main:\n\
      \  begin foo:\n\
      \    body\n\
      next\n",
      `Exception "Should fail because indent level is reduced too much";

      "if foo:\n\
      \  ontrue\n\
      else:\n\
      \  onfalse\n",
      `Exception "Missing terminator in last line";

      isValidId "op&&";
      isValidId "op||";
      isValidId "op++";
      isValidId "op*";
      isValidId "op==_str";
      isValidId "op{}";
      isValidId "op;";
      (* TODO: alle operatoren testen *)

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

      "test:\n\
      \  nest1:\n\
      \    void\n\
      \  end\n\
      \  --x\n\
      end\n",
      `Return [id "test"; BEGIN_BLOCK;
               id "nest1"; BEGIN_BLOCK;
               id "void"; END;
               END_BLOCK []; END;
               PREFIX_OP "--"; id "x"; END;
               END_BLOCK []; END];

      "10 == -10", `Return [id "10"; COMPARE_OP "=="; id "-10"; END];
    ]
    @ areValidInfixOps ["*"; "/"; "**"] (fun n -> MULT_OP n)
    @ areValidInfixOps ["+"; "-"; "+."; "++"] (fun n -> ADD_OP n)
    @ areValidInfixOps [
      "=";
      "*="; "/="; "**="; "+="; "-="; "+.="; "++=";
      "&&="; "||=";
      "&="; "|="; "^="; "<<="; ">>=";
      "%=";]
      (fun n -> ASSIGN_OP n)
    @ areValidInfixOps [">"; ">="; "<"; "<="; "=="; "!="] (fun n -> COMPARE_OP n)
    @ areValidInfixOps ["&&"; "||"] (fun n -> LAZY_BOOL_OP n)
    @ areValidInfixOps ["&"; "|"; "^"; "<<"; ">>"] (fun n -> STRICT_BOOL_OP n)
    @ areValidInfixOps ["%"] (fun n -> MOD_OP n)
    @ areValidInfixOps ["!"] (fun n -> EXCLAMATION_OP n)

  let validateTestCases() =
    let errors =
      mapFilter
        (fun (i, o) ->
           match o with
             | `Return tokens ->
                 if lastElement tokens <> Some END then
                   Some (sprintf "Test case lacks END:\n%s\n%s\n\n"
                           i (tokensToString tokens))
                 else
                   None
             | _ -> None)
        testCases
    in
    match errors with
      | [] -> None
      | _ -> Some (Common.combine "\n" errors)
end

let () =
  let module M = Tester(IndentLexerTestCase) in
  match IndentLexerTestCase.validateTestCases() with
    | None ->
        M.runTestsAndReport "indentlexer"
    | Some errors ->
        printf "Errors in indentlexer test cases:\n%s\n" errors;
        at_exit (fun () -> printf "Invalid indentlexer test cases, no results\n")

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


