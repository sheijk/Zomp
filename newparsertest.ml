(*
 * Simple program parsing code from stdin using the indentation based
 * lexer and parser
 *)

open Printf
open Common

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

let tokenToString =
  let os symbol arg =
    if String.length arg > 0 then
      symbol
    else
      symbol ^ "_" ^ arg
  in
  function
    | Newparser.END -> "`nl"
    | Newparser.IDENTIFIER id -> id
    | Newparser.BEGIN_BLOCK -> "{"
    | Newparser.END_BLOCK [] -> "}"
    | Newparser.END_BLOCK params -> sprintf "}(%s)" (Common.combine ", " params)
    | Newparser.WHITESPACE count -> String.make count '_'
    | Newparser.OPEN_PAREN -> "("
    | Newparser.CLOSE_PAREN -> ")"
    | Newparser.COMMA -> ","
    | Newparser.ADD_OP arg -> os "+" arg
    | Newparser.MULT_OP arg -> os "*" arg
    | Newparser.ASSIGN_OP arg -> os "=" arg
    | Newparser.COMPARE_OP arg -> os "==" arg
    | Newparser.DOT -> "."
    | Newparser.POSTFIX_OP arg -> os "post" arg
    | Newparser.PREFIX_OP arg -> os "pre" arg
    | Newparser.LAZY_BOOL_OP arg -> os "lb" arg
    | Newparser.STRICT_BOOL_OP arg -> os "sb" arg
      
let lexFunc lexstate (_ :Lexing.lexbuf) =
  let iexprToken = Iexprtest.token lexstate in
  match iexprToken with
    | `End -> Newparser.END
    | `BeginBlock -> Newparser.BEGIN_BLOCK
    | `EndBlock args -> Newparser.END_BLOCK args
    | `Whitespace count -> Newparser.WHITESPACE count
    | `Identifier name -> Newparser.IDENTIFIER name
    | `OpenParen -> Newparser.OPEN_PAREN
    | `CloseParen -> Newparser.CLOSE_PAREN
    | `Comma -> Newparser.COMMA
    | `Add arg -> Newparser.ADD_OP arg
    | `Mult arg -> Newparser.MULT_OP arg
    | `Assign arg -> Newparser.ASSIGN_OP arg
    | `Compare arg -> Newparser.COMPARE_OP arg
    | `Dot -> Newparser.DOT
    | `Postfix arg -> Newparser.POSTFIX_OP arg
    | `Prefix arg -> Newparser.PREFIX_OP arg
    | `LazyBoolOp arg -> Newparser.LAZY_BOOL_OP arg
    | `StrictBoolOp arg -> Newparser.STRICT_BOOL_OP arg

let parseSExpr source =
  let lexbuf = Lexing.from_string source in
  let lexstate = Iexprtest.lexbufFromString "dummy.zomp" source in
  let lexFunc = lexFunc lexstate in
  let rec read acc =
    try
      let expr = Newparser.main lexFunc lexbuf in
      read (expr :: acc)
    with
      | End_of_file -> acc
  in
  List.rev (read [])
  
(* let () = *)
(*   if Array.length Sys.argv > 1 && Sys.argv.(1) = "-i" then *)
(*     let rec parse() = *)
(*       flush stdout; *)
(*       let block = readBlock stdin in *)
(*       begin try *)
(*         let exprs = parseSExpr block in *)
(*         List.iter (fun expr -> printf "=>\n%s\n---\n" (Ast2.toString expr)) exprs *)
(*       with *)
(*         | Newparser.Error -> printf "Parser error\n" *)
(*       end; *)
(*       parse() *)
(*     in *)
(*     parse() *)
(*   else () *)

let printEachOnLine printF list =
  List.iter (fun x -> printF x; print_newline()) list
    
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
      let tokens = Iexprtest.lexString str in
      Iexprtest.printTokens tokens;
      raise error
    
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
    ignore(call []);
    ignore(seqExpr []);
    let expr id args = {Ast2.id = id; args =  args} in
    let juxExpr = expr "opjux" in
    let callExpr = expr "opcall" in
    ignore(juxExpr []);
    ignore(callExpr []);
    [
      (** juxtaposition *)
      "var int x", `Return [ se "opjux" ["var"; "int"; "x"] ];
      "var int x\nvar int y", `Return [intVar "x"; intVar "y"];
      "foo", `Return [id "foo"];
      "foo bar", `Return [jux ["foo"; "bar"]];

      (** s-expressions todo: support this at all? *)
(*       "foo (nested bar)", *)
(*       `Return [ {Ast2.id = "opjux"; args = [ *)
(*                    id "foo"; *)
(*                    jux ["nested"; "bar"]; *)
(*                  ]} ]; *)

      (** basic operators *)
      "x + y", `Return [se "op+" ["x"; "y"]];
      "x+y", `Return [se "op+" ["x"; "y"]];
      "a - b", `Return [se "op-" ["a"; "b"]];
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
      "a&&b", `Return [se2 "op&&" "a" "b"];
      "c || d", `Return [se2 "op||" "c" "d"];

      (** pre/postfix operators *)
      "foo... ", `Return [se1 "postop..." "foo"];
      "bar...", `Return [se1 "postop..." "bar"];
      "main...\n  child\nend",
      `Return [juxExpr [
                 se1 "postop..." "main";
                 seqExpr [
                   id "child";
                 ];
               ]];
      "int*", `Return [se1 "postop*" "int"];
      "*ptr", `Return [se1 "preop*" "ptr"];
      (* todo post/prefix ops which work without additional whitespace *)

      (** indexed operators *)
      "x +_f y", `Return [se2 "op+_f" "x" "y"];
      (* todo *)

      (** operator precedence *)
      "x + y * 10", `Return [expr "op+" [id "x"; se2 "op*" "y" "10"]];
      "x+y*10", `Return [expr "op+" [id "x"; se2 "op*" "y" "10"]];
      "x * y + 10", `Return [expr "op+" [se2 "op*" "x" "y"; id "10"]];
      "x*y+10", `Return [expr "op+" [se2 "op*" "x" "y"; id "10"]];
      "a / b / c", `Return [expr "op/" [se2 "op/" "a" "b"; id "c"]];

      "a + 1 > 10", `Return [expr "op>" [se2 "op+" "a" "1"; id "10"]];
      "a + 1 < 10", `Return [expr "op<" [se2 "op+" "a" "1"; id "10"]];
      "a + 1 >= 10", `Return [expr "op>=" [se2 "op+" "a" "1"; id "10"]];
      "a + 1 <= 10", `Return [expr "op<=" [se2 "op+" "a" "1"; id "10"]];
      (* todo *)
      
      (** invalid cases *)
      "§", `Exception "Invalid char";

      (** m-expressions *)
      "print()", `Return [se1 "opcall" "print"];
      "func(arg)", `Return [se2 "opcall" "func" "arg"];
      "func(a, b, c)", `Return [se "opcall" ["func"; "a"; "b"; "c"]];

      "mainloop render()", `Return [juxExpr [id "mainloop"; call ["render"]]];
      "while empty(list)", `Return [juxExpr [id "while"; call ["empty"; "list"]]];
      
      "while equal(a, b)", `Return [juxExpr [id "while"; call ["equal"; "a"; "b"]]];

      "sqrt(2 + 3)",
      `Return [callExpr [id "sqrt"; se2 "op+" "2" "3"]];

      (** indenting *)
      "type point\n  int x\n  int y\nend",
      `Return [
        { Ast2.id = "opjux";
          args = [
            id "type";
            id "point";
            seqExpr [jux ["int"; "x"]; jux ["int"; "y"]];
          ]}];

      "let x + y\n  plus(x, y)\nend",
      `Return [
        juxExpr [
          id "let";
          se2 "op+" "x" "y";
          seqExpr [call ["plus"; "x"; "y"]]
        ]];

      (** dot notation *)
      "foo.print(1, 2)",
      `Return [expr "op." [id "foo"; call ["print"; "1"; "2"]]];
      
      "foo.print()", `Return [expr "op." [id "foo"; call ["print"]]];
      "blah.add(10)", `Return [expr "op." [id "blah"; call ["add"; "10"]]];
      "x.add(2 * 3)",
      `Return [expr "op." [id "x"; callExpr [id "add"; se2 "op*" "2" "3"]]];

      (* "(\*foo).print()", *)
      (* `Return [expr "op." [se1 "op*" "foo"; call ["print"]]]; *)

      (** special handling for first token *)
      "print 10 + 20",
      `Return [juxExpr [id "print"; expr "op+" [id "10"; id "20"]]];
      
      "let x + y = 20",
      `Return [juxExpr [id "let"; expr "op=" [se2 "op+" "x" "y"; id "20"]]];

      (** precedence for s/m-expressions *)
      "x + add(1, 2)",
      `Return[ expr "op+" [id "x"; call ["add"; "1"; "2"]] ];

      "sqrt(1) * 10",
      `Return[ expr "op*" [call ["sqrt"; "1"]; id "10"] ];

      "add(1, 2) + add(3, 4)",
      `Return [expr "op+" [call ["add"; "1"; "2"]; call ["add"; "3"; "4"]]];

      "if 2 > 3 then",
      `Return [juxExpr [id "if"; se2 "op>" "2" "3"; id "then"]];

      (* "for i = 0 .. 100", *)
      (* `Return [juxExpr [id "for"; expr "op=" [id "i"; se2 "op.." "0" "100"]]]; *)

      (** test whitespace tolerance *)
      "int x ", `Return [jux ["int"; "x"]];
      "a   b  c", `Return [jux ["a"; "b"; "c"]];
      "  foo bar", `Return [jux ["foo"; "bar"]];
    ]
end
  
let () =
  let module M = Testing.Tester(IndentParserTestCase) in
  M.runTestsAndPrintErrors()



  
