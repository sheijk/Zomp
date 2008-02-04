
open Iexpr
open Printf


type location = {
  line :int;
  fileName :string;
}
    
type 'token lexbuf = {
  readChar : unit -> char;
  putbackString : string -> unit;
  mutable location : location;
  mutable prevIndent : int;
  mutable pushedTokens : 'token list;
}

let returnMultipleTokens lexbuf first remaining =
  lexbuf.pushedTokens <- remaining;
  first
    
let isNewline chr = chr = '\n'
let isWhitespace chr = chr = ' '

let isBlockEndLine line =
  Str.string_match (Str.regexp "^end.*$") line 0
    
exception UnknowToken of location * string
let raiseUnknownToken loc str = raise (UnknowToken (loc, str))

type tokens = [ `Begin | `End ]

let appendChar string char = string ^ String.make 1 char
  
let readUntil abortOnChar lexbuf =
  let acc = ref "" in
  let rec worker () =
    let nextChar = lexbuf.readChar() in
    if abortOnChar nextChar then begin
      ()
    end else begin
      acc := appendChar !acc nextChar;
      worker()
    end
  in
  begin try
    worker()
  with End_of_file -> () end;
  !acc


type indentToken = [
| `BeginBlock
| `EndBlock of string list
| `End
| `Whitespace of int
]

type userToken = [
| `Identifier of string
]

type token = [ indentToken | userToken ]
    
let whitespaceRE = Str.regexp " +"

let rules =
  let re = Str.regexp in
  [
    re "[a-zA-Z0-9_.:]+", (fun str -> `Identifier str);
    whitespaceRE, (fun str -> `Whitespace (String.length str));
  ]
  
let token (lexbuf : token lexbuf) : token =
  let ruleMatches string (regexp, _) =
    Str.string_match regexp string 0 &&
      String.length (Str.matched_group 0 string) = String.length string
  in
  
  let rec worker stringAcc =
    let currentChar = lexbuf.readChar() in
    if currentChar = '!' then (** hack to allow to abort within file *)
      raise End_of_file;

    if isNewline currentChar then begin
      let rec consumeWhitespace indent =
        let eof, nextChar =
          try false, lexbuf.readChar()
          with End_of_file -> true, '\n'
        in
        if eof then
          indent
        else if isWhitespace nextChar then
          consumeWhitespace (indent+1)
        else if isNewline nextChar then
          consumeWhitespace indent
        else begin
          lexbuf.putbackString (String.make 1 nextChar);
          indent
        end
      in
      let indent = consumeWhitespace 0 in
      let prevIndent = lexbuf.prevIndent in
      lexbuf.prevIndent <- indent;
      if indent = prevIndent then begin
        `End
      end else if indent > prevIndent then begin
        `BeginBlock
      end else (* indent < prevIndent *) begin
        let line = readUntil isNewline lexbuf in
        if isBlockEndLine line then begin
          lexbuf.putbackString "\n";
          let endArgs line =
            match Str.split whitespaceRE line with
              | "end" :: args -> args
              | _ -> failwith "splitting block end line failed"
          in
          returnMultipleTokens lexbuf `End [`EndBlock (endArgs line)]
        end else begin
          lexbuf.putbackString (line ^ "\n");
          returnMultipleTokens lexbuf `End [`EndBlock[]; worker stringAcc]
        end
      end

    end else begin (** no whitespace *)
      let input = stringAcc ^ (String.make 1 currentChar) in
      let matchingRules = List.filter (ruleMatches input) rules in
      match matchingRules with
        | [regexp, makeToken as rul] ->
            let input = ref input in
            begin try
              while true do
                let nextChar = lexbuf.readChar() in
                let nextInput = !input ^ String.make 1 nextChar in
                if ruleMatches nextInput rul then
                  input := nextInput
                else begin
                  lexbuf.putbackString (String.make 1 nextChar);
                  raise End_of_file
                end
              done;
            with End_of_file -> (); end;
            makeToken !input
        | [] ->
            raiseUnknownToken lexbuf.location input
        | _ ->
            worker input

    end
  in
  match lexbuf.pushedTokens with
    | [] -> worker ""
    | firstToken :: remainingTokens ->
        lexbuf.pushedTokens <- remainingTokens;
        firstToken

let printToken (lineIndent, indentNext) token =
  let printIndent indent =
    if indentNext = `Indent then
      printf "%s" (String.make (4 * indent) ' ')
  in
  match token with
    | `EndBlock args ->
        printIndent (lineIndent - 1);
        begin
          match args with
            | [] -> printf "}"
            | _ -> printf "}(%s)" (Common.combine ", " args)
        end;
        lineIndent - 1, `Indent
    | _ as t ->
        printIndent lineIndent;
        match t with
          | `BeginBlock ->
              printf "{\n";
              lineIndent + 1, `Indent
          | `End -> printf "`End\n"; lineIndent, `Indent
          | `Identifier str -> printf "%s" str; lineIndent, `DontIndent
          | `Whitespace length -> printf "_"; lineIndent, `DontIndent
          | _ -> failwith "match failure"
      
let makeLexbuf fileName readCharFunc =
  let buffer = ref "" in
  let eof = ref false in
  let rec lexbuf = 
    let readCharExtraNewline () =
      if !eof then
        raise End_of_file
      else
        try
          readCharFunc()
        with End_of_file ->
          eof := true;
          '\n'
    in
    let readCharWithBuffer () =
      let chr = 
        if String.length !buffer > 0 then begin
          let chr = !buffer.[0] in
          buffer := String.sub !buffer 1 (max (String.length !buffer -1) 0);
          chr
        end else
          readCharExtraNewline()
      in
      if isNewline chr then
        lexbuf.location <- { lexbuf.location with line = lexbuf.location.line + 1 };
      chr
    in
    {
      readChar = readCharWithBuffer;
      putbackString = (fun string -> buffer := string ^ !buffer);
      location = { line = 0; fileName = fileName };
      prevIndent = 0;
      pushedTokens = [];
    }
  in
  lexbuf

let lexbufFromChannel fileName channel = makeLexbuf fileName (fun () -> input_char channel)

let lexbufFromString fileName string =
  let stringLength = String.length string in
  let position = ref 0 in
  let readCharFunc() =
    if !position < stringLength then begin
      let chr = string.[!position] in
      incr position;
      chr
    end else
      raise End_of_file
  in
  makeLexbuf fileName readCharFunc
    
(* Main --------------------------------------------------------------------- *)

let tokenizeString str =
  let lexbuf = lexbufFromString "dummy.zomp" str in
  let rec worker acc =
    let maybeToken = 
      try Some (token lexbuf)
      with End_of_file -> None
    in
    match maybeToken with
      | Some t -> worker (t::acc)
      | None -> List.rev acc
  in
  worker []

let printTokens tokens =
  let rec worker context = function
    | [] -> ()
    | t :: remTokens ->
        let newContext = printToken context t in
        printf " ";
        worker newContext remTokens
  in
  worker (0, `DontIndent) tokens
  
(*
let () =
  let sourceFile = "indent.zomp" in
  let lexbuf = lexbufFromChannel sourceFile (open_in sourceFile) in
  let rec worker indent =
    let t = token lexbuf in
    let newIndent = printToken indent t in
    printf " ";
    worker newIndent
  in
  try
    printf " ";
    worker (0, `DontIndent)
  with End_of_file ->
    printf "\n--- done ---\n"
*)

(* Tests -------------------------------------------------------------------- *)

module type CASE_STRUCT = sig
  type input
  type output

  val printInput : input -> unit
  val printOutput : output -> unit

  val outputEqual : output -> output -> bool

  type result = [ `Return of output | `Exception of string ]

  val testedFunc : input -> output
  val testCases : (input * result) list
end

module Tester(Cases :CASE_STRUCT) = struct
  include Cases
    
  type error = {
    input :input;
    found :result;
    expected :result;
  }

  let printResult = function
    | `Return output -> printOutput output
    | `Exception msg -> printf "Exception '%s'" msg

  let error ~input ~found ~expected =
    `Error {
      input = input;
      found = found;
      expected = expected
    }

  let runTestCase (input, expected) =
    let found = 
      try
        `Return (testedFunc input)
      with _ as exc ->
        `Exception (Printexc.to_string exc)
    in
    match expected, found with
      | `Exception _, `Exception _ ->
          `Ok found
      | `Return expectedVal, `Return foundVal ->
          if (outputEqual expectedVal foundVal) then
            `Ok found
          else
            error ~input ~found ~expected
      | _, _ ->
          error ~input ~found ~expected

  let runTests () : error list =
    let results = List.map runTestCase testCases in
    let errors = Common.mapFilter (function `Error e -> Some e | _ -> None) results in
    errors

  let runTestsAndPrintErrors() =
    let printError error =
      printf "\n--- UnitTest Failure ---\n";
      printf "Input: '"; printInput error.input; printf "'\n";
      printf "Expected: '"; printResult error.expected; printf "'\n";
      printf "Found: '"; printResult error.found; printf "'\n";
    in
    List.iter printError (runTests())
      
  exception UnitTestFailure of error list
end

let tokenEqual l r =
  match l, r with
    | `Whitespace _, `Whitespace _ -> true
    | _, _ -> l = r
  
module IndentLexerTestCase : CASE_STRUCT =
struct
  type input = string
  type output = token list

  let printInput str = printf "%s" str
  let printOutput tokens = printTokens tokens 
  let outputEqual l r =
    List.length l = List.length r
    &&
    List.for_all2 tokenEqual l r
    
  type result = [ `Return of output | `Exception of string ]

  let testedFunc = tokenizeString

  let testCases : (input * result) list =
    let ids stringList =
      let idTokens = List.map (fun str -> (`Identifier str :> token)) stringList in
      Common.combineList (`Whitespace 1) idTokens
    in
    [
      "var int y\n", `Return( ids ["var"; "int"; "y"] @ [`End] );
      "var int x", `Return( ids ["var"; "int"; "x"] @ [`End] );
      
    ]
end

module IndentLexerTester = Tester(IndentLexerTestCase)

let () = IndentLexerTester.runTestsAndPrintErrors()
  
let () =
  let testCases = [
    "iff", "end", true;
    "iff", "end iff", true;
    "iff", "end wrong", false;
    "iff", "end iff  ", true;
    "iff", "end iff blah", false;
    "iff", "end   iff", true;
  ] in
  
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

    

