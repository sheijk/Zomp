
open Indentlexer
open Testing
open Printf
  
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
    let id x = `Identifier x in
    let ids stringList =
      let idTokens = List.map (fun str -> (`Identifier str :> token)) stringList in
      idTokens
    in
    let parsedAsId id =
      id, `Return [`Identifier id; `End]
    in
    let infixOp op optoken =
      ["l " ^ op ^ " r", `Return [id "l"; optoken; id "r"; `End];
       parsedAsId ("op" ^ op)]
    in
    ignore( ids [] );
    [
      "single", `Return [id "single"; `End];

      "foo(3) + 1", `Return [id "foo"; `OpenParen; id "3"; `CloseParen; `Add "+"; id "1"; `End];
      
      "foo bar", `Return [id "foo"; id "bar"; `End];

      "a+b", `Return [id "a"; `Add "+"; id "b"; `End];
      "a   + b", `Return [id "a"; `Add "+"; id "b"; `End];

      "a,b", `Return [id "a"; `Comma; id "b"; `End];
      "x, y", `Return [id "x"; `Comma; id "y"; `End];
      "foo , bar", `Return [id "foo"; `Comma; id "bar"; `End];

      "a +_f b", `Return [id "a"; `Add "+_f"; id "b"; `End];

      "(a + b)*c", `Return [`OpenParen; id "a"; `Add "+"; id "b"; `CloseParen; `Mult "*"; id "c"; `End];

      "space... ", `Return [id "space"; `Postfix "..."; `End];
      "lineend...", `Return [id "lineend"; `Postfix "..."; `End];

      "&blah", `Return [`Prefix "&"; id "blah"; `End];
      "*deref", `Return [`Prefix "*"; id "deref"; `End];
      "foo *ptr", `Return [id "foo"; `Prefix "*"; id "ptr"; `End];
      "float*", `Return [id "float"; `Postfix "*"; `End];
      "float* var", `Return [id "float"; `Postfix "*"; id "var"; `End];

      (* strings and numbers *)
      "1337", `Return [id "1337"; `End];
      "10.3", `Return [id "10.3"; `End];
      "100.", `Return [id "100."; `End];
      ".01", `Return [id ".01"; `End];
      "\"foobar\"", `Return [id "\"foobar\""; `End];
      "\"windows\\\\path\"", `Return [id "\"windows\\\\path\""; `End];
      "'x'", `Return [id "'x'"; `End];
      "'\\n'", `Return [id "'\\n'"; `End];
      "'\\0'", `Return [id "'\\0'"; `End];
      "'\\\\'", `Return [id "'\\\\'"; `End];

      (* quotes *)
      "$", `Return [`Quote "$"; `End];
      "#", `Return [`Quote "#"; `End];

      "${foo}", `Return [`Quote "$"; `OpenCurlyBrackets; id "foo"; `CloseCurlyBrackets; `End];
      "${class\n  child1\nend}",
      `Return [`Quote "$";
               `OpenCurlyBrackets;
               id "class"; `BeginBlock; id "child1"; `End; `EndBlock [];
               `CloseCurlyBrackets; `End];

      (* simple one-line expressions *)
      "var int y\n", `Return( ids ["var"; "int"; "y"] @ [`End] );
      "var int x", `Return( ids ["var"; "int"; "x"] @ [`End] );

      "first line\nsecond line\n",
      `Return( [id "first"; id "line"; `End;
                id "second"; id "line"; `End] );

      (* simple multi-line expressions *)
      "if a then\n\
      \  foobar\n\
      end",
      `Return( ids ["if"; "a"; "then"] @ [`BeginBlock]
               @ [id "foobar"; `End]
               @ [`EndBlock []; `End] );
      
      (* block end with tokens *)
      "foreach num IN primes\n\
      \  printLine num\n\
      end foreach num",
      `Return( ids ["foreach"; "num"; "IN"; "primes"] @ [`BeginBlock]
               @ ids ["printLine"; "num"] @ [`End]
               @ [`EndBlock ["foreach"; "num"]; `End] );

      (* multi-part multi-line expressions *)
      "if cond then\n\
      \  print 1\n\
      else\n\
      \  print 2\n\
      end",
      `Return( ids ["if"; "cond"; "then"] @ [`BeginBlock]
               @ ids ["print"; "1"] @ [`End]
               @ [`EndBlock []; id "else"; `BeginBlock]
               @ ids ["print"; "2"] @ [`End]
               @ [`EndBlock []; `End] );

      (* leading whitespace/newlines *)
      "   a b c", `Return (ids ["a"; "b"; "c"] @ [`End]);
      "first\n\n\nsecond", `Return [id "first"; `End; id "second"; `End];
      "\n\n\n\nfirst\n\n\n", `Return [id "first"; `End];
  
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
    ]
    @ infixOp "+" (`Add "+")
    @ infixOp "-" (`Add "-")
    @ infixOp "&&" (`LazyBoolOp "&&")
    @ infixOp "||" (`LazyBoolOp "||")
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
  

