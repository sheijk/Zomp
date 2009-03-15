(*
 * Simple program parsing code from stdin using the indentation based
 * lexer and parser
 *)

open Printf
open Common
open Newparser

let readBlock channel =
  let rec readLine lineAcc =
    flush stdout;
    let line = input_line channel in
    if line = "" then begin
      let line2 = input_line channel in
      if line2 = "" then
        line2 :: lineAcc
      else
        readLine (line :: line2 :: lineAcc)
    end else begin
      readLine (line :: lineAcc)
    end
  in
  Common.combine "\n" (List.rev (readLine []))

let parseSExpr source =
  let lexbuf = Lexing.from_string source in
  let lexstate = Indentlexer.lexbufFromString "dummy.zomp" source in
  let lexFunc _ = Indentlexer.token lexstate in
  let rec read acc =
    try
      let expr = Newparser.main lexFunc lexbuf in
      read (expr :: acc)
    with
      | Indentlexer.Eof -> acc
  in
  let revExprs = read [] in
  try
    let invalidChar = Indentlexer.readChar lexstate in
    failwith (sprintf "Not at end of input, read %c" invalidChar)
  with Indentlexer.Eof ->
    List.rev revExprs

let printEachOnLine printF list =
  List.iter (fun x -> printF x; print_newline()) list

exception ParsingFailure of exn * string

let rec ast2SimpleString ast =
  "(" ^ ast.Ast2.id ^ " " ^ Common.combine " " (List.map ast2SimpleString ast.Ast2.args) ^ ")"

module IndentParserTestCase : Testing.CASE_STRUCT =
struct
  type input = string
  type output = Ast2.sexpr list

  let printInput = print_string
  let printOutput = printEachOnLine (printf "%s" ++ Ast2.toString)

  let outputEqual l r =
    List.length l = List.length r
    && List.for_all2 Ast2.equals l r

  type result = [ `Return of output | `Exception of string ]

  let testedFunc str =
    try
      parseSExpr str
    with error ->
      let tokens = Indentlexer.lexString str in
      let str = Indentlexer.tokensToString tokens in
      raise (ParsingFailure(error, "Tokens: " ^ str ^ "; caused error: " ^ Printexc.to_string error))

  let testCases : (input * result) list =
    let intVar name = Ast2.simpleExpr "opjux" ["var"; "int"; name] in
    let se = Ast2.simpleExpr
    and jux args = { Ast2.id = "opjux"; args = List.map Ast2.idExpr args }
    and call args = { Ast2.id = "opcall"; args = List.map Ast2.idExpr args }
    and seqExpr args = { Ast2.id = "opseq"; args = args }
    and se2 f l r = Ast2.simpleExpr f [l; r]
    and se1 f arg = Ast2.simpleExpr f [arg]
    and id = Ast2.idExpr
    in

    ignore(se1 "" "");
    ignore(se2 "" "" "");
    ignore(call []);
    ignore(seqExpr []);
    ignore(intVar "");
    ignore(se "" []);
    ignore(jux []);

    let expr id args = {Ast2.id = id; args =  args} in
    let juxExpr = expr "opjux" in
    let callExpr = expr "opcall" in
    ignore(juxExpr []);
    ignore(callExpr []);
    let expectValidId name = name, `Return [id name] in
    [
      (** simple expression *)
      expectValidId "blargh";
      expectValidId "1.0";
      expectValidId "1.0f";
      expectValidId "200.33435d";
      expectValidId "-9";
      expectValidId "-20.3";
      expectValidId "-30.2d";
      expectValidId "_";

      (** juxtaposition *)
      "var int x", `Return [ se "opjux" ["var"; "int"; "x"] ];
      "var int x\nvar int y", `Return [intVar "x"; intVar "y"];
      "foo", `Return [id "foo"];
      "foo bar", `Return [jux ["foo"; "bar"]];

      (** s-expressions *)
      "foo (nested bar)",
      `Return [ {Ast2.id = "opjux"; args = [
                   id "foo";
                   jux ["nested"; "bar"];
                 ]} ];
      "(ptr float) x",
      `Return [juxExpr [jux ["ptr"; "float"]; id "x"]];

      (** basic operators *)
      "x + y", `Return [se "op+" ["x"; "y"]];
      "x+y", `Return [se "op+" ["x"; "y"]];
      "a - b", `Return [se "op-" ["a"; "b"]];
      (* "-x", `Return [se1 "preop-" "x"]; *)
      "foo * bar", `Return [se "op*" ["foo"; "bar"]];
      "p/q", `Return [se2 "op/" "p" "q"];
      (* "a, b", `Return [se2 "op," "a" "b"]; *)
      (* "a, b, c", `Return [se "op," ["a"; "b"; "c"]]; *)
      "x = 1", `Return [se2 "op=" "x" "1"];
      "foo := 20", `Return [se2 "op:=" "foo" "20"];
      "0 == blah", `Return [se2 "op==" "0" "blah"];
      "1 != 2", `Return [se2 "op!=" "1" "2"];
      "a < b", `Return [se2 "op<" "a" "b"];
      "foo <= bar", `Return [se2 "op<=" "foo" "bar"];
      "c > d", `Return [se2 "op>" "c" "d"];
      "x >= y", `Return [se2 "op>=" "x" "y"];
      "base ** exp", `Return [se2 "op**" "base" "exp"];

      "true & false", `Return [se2 "op&" "true" "false"];
      "a|b", `Return [se2 "op|" "a" "b"];
      "1 | 2 | 3", `Return [expr "op|" [se2 "op|" "1" "2"; id "3"]];
      "a&&b", `Return [se2 "op&&" "a" "b"];
      "c || d", `Return [se2 "op||" "c" "d"];
      "a^b", `Return [se2 "op^" "a" "b"];

      (** pre/postfix operators *)
      "foo... ", `Return [se1 "postop..." "foo"];
      "bar...", `Return [se1 "postop..." "bar"];
      "main...\n\
      \  child\n\
      end",
      `Return [juxExpr [
                 se1 "postop..." "main";
                 seqExpr [
                   id "child";
                 ];
               ]];
      "int*", `Return [se1 "postop*" "int"];
      "int**", `Return [expr "postop*" [expr "postop*" [id "int"]]];
      "(int*)*", `Return [expr "postop*" [expr "postop*" [id "int"]]];
      "*ptr", `Return [se1 "preop*" "ptr"];
      "**p", `Return [expr "preop*" [expr "preop*" [id "p"]]];
      "handlePtr &var", `Return [juxExpr [id "handlePtr"; se "preop&" ["var"]]];
      "(foo bar*)", `Return [juxExpr [id "foo"; se "postop*" ["bar"]]];
      (* todo post/prefix ops which work without additional whitespace *)

      "++i", `Return [se1 "preop++" "i"];
      "x++", `Return [se1 "postop++" "x"];
      "--foo", `Return [se1 "preop--" "foo"];
      "blah--", `Return [se1 "postop--" "blah"];

      "foo ++ bar", `Return [se2 "op++" "foo" "bar"];
      "line1; line2", `Return [se2 "op;" "line1" "line2"];
      "line1;line2", `Return [se2 "op;" "line1" "line2"];
      "line1 ; line2", `Return [se2 "op;" "line1" "line2"];
      "macro op; l r", `Return [jux ["macro"; "op;"; "l"; "r"]];

      (** indexed operators *)
      "x +_f y", `Return [se2 "op+_f" "x" "y"];
      "op== a b", `Return [jux ["op=="; "a"; "b"]];
      "op==_str lstr rstr", `Return [jux ["op==_str"; "lstr"; "rstr"]];
      (* todo *)

      (** operator precedence *)
      "x + y * 10", `Return [expr "op+" [id "x"; se2 "op*" "y" "10"]];
      "x+y*10", `Return [expr "op+" [id "x"; se2 "op*" "y" "10"]];
      "x * y + 10", `Return [expr "op+" [se2 "op*" "x" "y"; id "10"]];
      "x*y+10", `Return [expr "op+" [se2 "op*" "x" "y"; id "10"]];
      "a / b / c", `Return [expr "op/" [se2 "op/" "a" "b"; id "c"]];

      "a + 1 > 11", `Return [expr "op>" [se2 "op+" "a" "1"; id "11"]];
      "a + 1 >= 12", `Return [expr "op>=" [se2 "op+" "a" "1"; id "12"]];
      "a + 1 < 13", `Return [expr "op<" [se2 "op+" "a" "1"; id "13"]];
      "a + 1 <= 14", `Return [expr "op<=" [se2 "op+" "a" "1"; id "14"]];
      (* todo *)

      "sin x + cos y", `Return [expr "op+" [jux ["sin"; "x"]; jux ["cos"; "y"]]];

      "a +5", `Exception "Unbalanced whitespace";
      "a- b", `Exception "Unbalanced whitespace";

      (** operator identifiers *)
      "op> a b", `Return [jux ["op>"; "a"; "b"]];
      "postop* l r", `Return [jux ["postop*"; "l"; "r"]];
      "macro preop& val", `Return [jux ["macro"; "preop&"; "val"]];

      (** invalid cases *)
      "§", `Exception "Invalid char";

      (** m-expressions *)
      "print()", `Return [se1 "opcall" "print"];
      "func(arg)", `Return [se2 "opcall" "func" "arg"];
      "plus(3, 5)", `Return [se "opcall" ["plus"; "3"; "5"]];
      "func(a, b, c)", `Return [se "opcall" ["func"; "a"; "b"; "c"]];

      "func int get5()", `Return [juxExpr [id "func"; id "int"; call ["get5"]]];

      "mainloop render()", `Return [juxExpr [id "mainloop"; call ["render"]]];
      "while empty(list)", `Return [juxExpr [id "while";
                                             call ["empty"; "list"]]];

      "while equal(a, b)", `Return [juxExpr [
                                      id "while";
                                      call ["equal"; "a"; "b"]]];

      "sqrt( if pos then x else 0 )",
      `Return [callExpr [id "sqrt";
                         jux ["if"; "pos"; "then"; "x"; "else"; "0"]]];

      "sqrt(2 + 3)", `Return [callExpr [id "sqrt"; se2 "op+" "2" "3"]];

      "foo [123]", `Return [juxExpr [id "foo"; se1 "op[]" "123"]];
      "array[0]", `Return [se2 "postop[]" "array" "0"];
      "op[] arg", `Return [jux ["op[]"; "arg"]];
      "postop[] l r", `Return [jux ["postop[]"; "l"; "r"]];

      (** s-expressions (currently not supported) *)
      "quote (foo bar)", `Return [juxExpr [id "quote"; jux ["foo"; "bar"]]];
      "printAst(foo bar)", `Return [callExpr [id "printAst"; jux ["foo"; "bar"]]];

      "print (foo bar baz)", `Return [juxExpr [id "print";
                                               jux ["foo"; "bar"; "baz"]]];
      "print sin(10)", `Return [juxExpr [id "print"; call ["sin"; "10"]]];
      "while equal(l, r)", `Return [juxExpr [id "while";
                                             call ["equal"; "l"; "r"]]];

      "func int foo(int x, int y)",
      `Return [
        juxExpr [
          id "func";
          id "int";
          callExpr [id "foo"; jux ["int"; "x"]; jux ["int"; "y"]];
        ]];

      "sqrt(sin(x))",
      `Return [callExpr [id "sqrt"; callExpr [id "sin"; id "x"]]];

      (** indenting *)
      "type point\n\
      \  int x\n\
      \  int y\n\
      end",
      `Return [
        { Ast2.id = "opjux";
          args = [
            id "type";
            id "point";
            seqExpr [jux ["int"; "x"]; jux ["int"; "y"]];
          ]}];

      "simple\n\
      \  single\n\
      end",
      `Return [juxExpr [id "simple"; seqExpr [id "single"]]];

      "simple\n\
      \  single sexpr child\n\
      end",
      `Return [
        juxExpr [id "simple"; seqExpr [jux ["single"; "sexpr"; "child"]]]
      ];

      "simple2\n\
      \  single(mexpr, child)\n\
      end",
      `Return [
        juxExpr [id "simple2"; seqExpr [callExpr [id "single"; id "mexpr"; id "child"]]]
      ];

      "forEachLine(line, file)\n\
      \  print line\n\
      end",
      `Return [
        juxExpr [
          call ["forEachLine"; "line"; "file"];
          seqExpr [
            jux ["print"; "line"];
          ]]];

      "let x + y\n\
      \  plus(x, y)\n\
      end",
      `Return [
        juxExpr [
          expr "op+" [jux ["let"; "x"]; id "y"];
          seqExpr [call ["plus"; "x"; "y"]]
        ]];

      "let print() =\n\
      \  print(\"Hello, World!\")\n\
      end",
      `Return [
        expr "op=" [
          juxExpr [id "let"; call ["print"]];
          seqExpr [call ["print"; "\"Hello, World!\""]]
        ]];

      (* "log(\n\ *)
      (* \  'foo'\n\ *)
      (* \  'bar'\n\ *)
      (* end)", *)
      (* `Return [ *)
      (*   callExpr [id "log"; se1 "foo" "bar"]; *)
      (* ]; *)

      "stuff\n\
      \  child0\n\
      more\n\
      \  child1\n\
      end",
      `Return [
        juxExpr [
          id "stuff";
          seqExpr [id "child0"];
          id "more";
          seqExpr [id "child1"];
        ]
      ];

      "if (10 > 20) then\n\
      \  print 1\n\
      else\n\
      \  print 2\n\
      end\n",
      `Return [
        juxExpr [
          id "if";
          se2 "op>" "10" "20";
          id "then";
          seqExpr [jux ["print"; "1"]];
          id "else";
          seqExpr [jux ["print"; "2"]];
        ]
      ];

      "if foo\n" ^
        "  ontrue\n" ^
        "else\n" ^
        "  onfalse\n",
      `Return [juxExpr [id "if"; id "foo";
                        seqExpr[id "ontrue";];
                        id "else";
                        seqExpr[id "onfalse"]]];

      "main blah\n" ^
        "  nested\n" ^
        "    body\n" ^
        "  nested2\n" ^
        "end main\n",
      `Exception "Should fail because 'nested' has no 'end' terminator";

      "main foo\n" ^
        "  nested\n" ^
        "end wrong",
      `Exception "Should fail because 'wrong' would need to be 'main' or be omitted";

      (* "empty block\n" ^ *)
      (*   "end", *)
      (* `Return [juxExpr [id "empty"; id "block"; seqExpr[]]]; *)

      (* (\*       "for: p in primes\n\ *\) *)
      (* (\*       \  print p\n\ *\) *)
      (* (\*       \  log p", *\) *)
      (* (\*       `Return [juxExpr [ *\) *)
      (* (\*                  id "for"; id "p"; id "in"; id "primes"; *\) *)
      (* (\*                  seqExpr [ *\) *)
      (* (\*                    jux ["print"; "p"]; *\) *)
      (* (\*                    jux ["log"; "p"]; *\) *)
      (* (\*                  ]]]; *\) *)

      "main blah\n\
      \  nested\n\
      \    body\n\
      \  nested2\n\
      end main",
      `Exception "Should fail because \"nested\" has no end terminator";

      (** dot notation *)
      "foo.bar", `Return [se2 "op." "foo" "bar"];
      "a.b.c", `Return [expr "op." [se2 "op." "a" "b"; id "c"]];
      "x.y.z.w", `Return [expr "op." [expr "op." [se2 "op." "x" "y"; id "z"]; id "w"]];
      "c.img = 1.0", `Return [expr "op=" [se2 "op." "c" "img"; id "1.0"]];

      "foo.print(1, 2)",
      `Return [expr "op." [id "foo"; call ["print"; "1"; "2"]]];

      "while cond.true do",
      `Return [juxExpr [id "while"; expr "op." [id "cond"; id "true"]; id "do"]];

      "item.print()", `Return [expr "op." [id "item"; call ["print"]]];
      "blah.add(10)", `Return [expr "op." [id "blah"; call ["add"; "10"]]];
      "x.add(2 * 3)",
      `Return [expr "op." [id "x"; callExpr [id "add"; se2 "op*" "2" "3"]]];

      "*foo.print()",
      `Return [expr "preop*" [expr "op." [id "foo"; call ["print"]]]];

      (** juxtaposition has highest priority *)
      "print 10 + 20",
      `Return [expr "op+" [jux ["print"; "10"]; id "20"]];

      "let x + y = 20",
      `Return [expr "op=" [expr "op+" [jux ["let"; "x"]; id "y"]; id "20"]];

      (** precedence for s/m-expressions *)
      "x + add(1, 2)",
      `Return[ expr "op+" [id "x"; call ["add"; "1"; "2"]] ];

      "sqrt(1) * 10",
      `Return[ expr "op*" [call ["sqrt"; "1"]; id "10"] ];

      "add(1, 2) + add(3, 4)",
      `Return [expr "op+" [call ["add"; "1"; "2"]; call ["add"; "3"; "4"]]];

      (** quotations *)
      "$foo", `Return [se1 "quote" "foo"];
      "$$blah", `Return [se1 "quoteasis" "blah"];
      "${}", `Return [se1 "quote" "seq"];
      "${foo}", `Return [se1 "quote" "foo"];
      "${sexpr arg0 arg1}", `Return [expr "quote" [jux ["sexpr"; "arg0"; "arg1"]]];
      (* "${call(foo)}", `Return [expr "quote" [call  ["call"; "foo"]]]; *)
      "${class\n\
      \  child1\n\
      end}",
      `Return [expr "quote" [juxExpr [id "class"; seqExpr [id "child1"]]]];
      "${10 + 20}", `Return [expr "quote" [se2 "op+" "10" "20"]];
      (* "${try foo.bar()}", *)
      (* `Return [ *)
      (*   expr "quote" [ *)
      (*     juxExpr [ *)
      (*       id "try"; *)
      (*       expr "op." [id "foo"; call ["bar"]]; *)
      (*     ]]]; *)

      "#foo.bar", `Return [expr "op." [se1 "antiquote" "foo"; id "bar"]];
      "abc.#def", `Return [expr "op." [id "abc"; se1 "antiquote" "def"]];

      (* "#foo(1,2)", `Return [expr "antiquote" [ *)
      (*                         expr "opcall" [ *)
      (*                           id "foo"; *)
      (*                           id "1"; *)
      (*                           id "2"]]]; *)

      "ret ${foo bar}", `Return [juxExpr [
                                   id "ret";
                                   expr "quote" [jux ["foo"; "bar"]]]];
      "ast:print ${\n\
      \  foo bar\n\
      end}",
      `Return [juxExpr [
                 id "ast:print";
                 expr "quote" [
                   seqExpr [jux ["foo"; "bar"]]
                 ]]];

      (* "callFunc(#insert)", `Return [callExpr [id "callFunc"; se1 "antiquote" "insert"]]; *)

      "left + $right", `Return [expr "op+" [id "left"; se1 "quote" "right"]];

      "#funcName(10)", `Return [expr "opcall" [se1 "antiquote" "funcName"; id "10"]];

      (** keyword arguments **)
      "assert: a != b",
      `Return [expr "opkeyword" [id "assert"; se2 "op!=" "a" "b"]];

      "if: a then: b",
      `Return [expr "opkeyword" [id "if"; id "a";
                                 id "then"; id "b"]];

      "if: a > b then: --b",
      `Return [expr "opkeyword" [id "if"; se2 "op>" "a" "b";
                                 id "then"; se1 "preop--" "b"]];

      "print errormsg unless: allIsFine()",
      `Return [expr "opkeyword" [id "default"; jux ["print"; "errormsg"];
                                 id "unless"; call ["allIsFine"]]];

      "if: some cond then: (print x unless: moreCond())",
      `Return [expr "opkeyword" [id "if";
                                 jux ["some"; "cond"];
                                 id "then";
                                 expr "opkeyword"
                                   [id "default"; jux ["print"; "x"];
                                    id "unless"; call ["moreCond"]]]];

      "for: 1 to 2 do:\n\
      \  print x\n\
      end",
      `Return [expr "opkeyword" [id "for"; jux ["1"; "to"; "2"];
                                 id "do"; seqExpr [
                                   jux ["print"; "x"];
                                 ]]];

      "foo bar in:\n\
      \  some code\n\
      \  more code\n\
      end",
      `Return [expr "opkeyword" [
                 id "default"; jux ["foo"; "bar"];
                 id "in"; seqExpr [
                   jux ["some"; "code"];
                   jux ["more"; "code"];
                 ]]];

      (** test whitespace tolerance *)
      "int x ", `Return [jux ["int"; "x"]];
      "a   b  c", `Return [jux ["a"; "b"; "c"]];
      "  foo bar", `Return [jux ["foo"; "bar"]];
      "\n\nabc def\n\n", `Return [jux ["abc"; "def"]];

      "first line\n" ^
        "\n" ^
        "second line",
      `Return [jux ["first"; "line"]; jux ["second"; "line"]];

      "first line\n" ^
        "   \n" ^
        "after spaces line",
      `Return [jux ["first"; "line"]; jux ["after"; "spaces"; "line"]];

      (** misc *)
      "", `Return [];
      "foo\n  bar\nend wrong", `Exception "'end wrong' should be 'end foo' or 'end'";

      (** string and char parsing *)
      expectValidId "\"foo\"";
      expectValidId "'x'";
      expectValidId "'\\n'";
      expectValidId "'\\0'";

      expectValidId "\"line0\nline1\nline2\"";

      (** comments *)
      "// single line comment\n" ^
        "var int x",
      `Return [jux ["var"; "int"; "x"]];

      "// comment only", `Return [];

      (* "whitespace(atEOF)\n ", `Return [call ["whitespace"; "atEOF"]]; *)
      "nonempty whitespace lines\n   \ninbetween",
      `Return [jux ["nonempty"; "whitespace"; "lines"]; id "inbetween"];

      (* "foo(bar)\n" ^ *)
      (*   "// comment at end of file", *)
      (* `Return [call ["foo"; "bar"]]; *)

      "first line\n" ^
        "// comment inbetween\n" ^
        "second line", `Return [jux ["first"; "line"]; jux ["second"; "line"]];

      "first line\n" ^
        "    // comment inbetween indented\n" ^
        "second line", `Return [jux ["first"; "line"]; jux ["second"; "line"]];

      "var int x // the x variable\n",
      `Return [jux ["var"; "int"; "x"]];

      "/* multiline comment in a single line */", `Return [];
      ("/* multi" ^
         "line" ^
         "comment */"),
      `Return [];

      "var int /* blah */ y", `Return [jux ["var"; "int"; "y"]];

      "/* comment*/var int x", `Return [jux ["var"; "int"; "x"]];

      "line one\n" ^
        "/* comment line */\n" ^
        "line two",
      `Return [jux ["line"; "one"]; jux ["line"; "two"]];

      "line one\n" ^
        "  /*indented comment only */\n" ^
        "last line",
      `Return [jux ["line"; "one"]; jux ["last"; "line"]];

      "first\n" ^
        "/* comment followed by whitespace */    \n" ^
        "second",
      `Return [id "first"; id "second"];

      "/*012*/odd length", `Return [jux ["odd"; "length"]];
      "/*0123*/even length", `Return [jux ["even"; "length"]];
    ]
end

let () =
  let module M = Testing.Tester(IndentParserTestCase) in
  M.runTestsAndReport "indentparser"




