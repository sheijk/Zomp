(*
 * Indentation based lexer and tests
 *)

(* open Iexpr *)
open Printf
open Common

type location = {
  line :int;
  fileName :string;
}

type indentToken = [
| `BeginBlock
| `EndBlock of string list
| `End
| `Whitespace of int
]

type userToken = [
| `Identifier of string
| `OpenParen | `CloseParen
| `Comma
| `Add of string
| `Sub of string
| `Mult of string
| `Div of string
]

let lastMatchedString = ref ""
let (=~) string regexp =
  lastMatchedString := string;
  Str.string_match (Str.regexp regexp) string  0
let nthmatch n =
  Str.matched_group n !lastMatchedString
    
let whitespaceRE = Str.regexp " +"


let trim str =
  let rec findFirstNonWS str index succ =
    if index < String.length str && index >= 0 && str.[index] = ' ' then
      findFirstNonWS str (succ index) succ
    else
      index
  in
  let frontSpaces = findFirstNonWS str 0 ((+) 1) in
  let frontTrimmed = String.sub str frontSpaces (String.length str - frontSpaces)
  in
  let frontTrimmedLength = String.length frontTrimmed in
  let backSpaces = frontTrimmedLength - 1 - findFirstNonWS frontTrimmed (frontTrimmedLength-1) (fun x -> x - 1) in
  String.sub frontTrimmed 0 (frontTrimmedLength - backSpaces)
    
let rules =
  let re = Str.regexp in
  let stringRule str token = 
    re str,
    (fun matchedStr -> 
       token)
  in
  let opre symbol = (sprintf "%s\\(_[a-zA-Z]+\\)?" symbol) in
  let opRule symbol tokenF =
    re (opre symbol),
    tokenF
  in
  let opRuleWS symbol tokenF =
    re (sprintf " +%s +" (opre symbol)),
    (fun str -> tokenF (trim str))
  in
  let opRules symbol tokenF =
    [opRule symbol tokenF;
     opRuleWS symbol tokenF]
  in
  [
    re "[a-zA-Z0-9]+", (fun str -> `Identifier str);
    whitespaceRE, (fun str -> `Whitespace (String.length str));
    stringRule " *( *" `OpenParen;
    stringRule " *) *" `CloseParen;
    stringRule " *, *" `Comma;
  ]
  @ opRules "\\+" (fun s -> `Add s)
  @ opRules "-" (fun s -> `Sub s)
  @ opRules "\\*" (fun s -> `Mult s)
  @ opRules "/" (fun s -> `Div s)


type token = [ indentToken | userToken ]

type 'token lexerstate = {
  readChar : unit -> char;
  putbackString : string -> unit;
  mutable location : location;
  mutable prevIndent : int;
  mutable pushedTokens : 'token list;
}

let returnMultipleTokens state first remaining =
  state.pushedTokens <- remaining;
  first
    
let isNewline chr = chr = '\n'
let isWhitespace chr = chr = ' '

let isBlockEndLine line =
  Str.string_match (Str.regexp "^end.*$") line 0
    
exception UnknowToken of location * string
let raiseUnknownToken loc str = raise (UnknowToken (loc, str))

exception IndentError of location * string
let raiseIndentError loc str = raise (IndentError (loc, str))

let appendChar string char = string ^ String.make 1 char
  
let readUntil abortOnChar state =
  let acc = ref "" in
  let rec worker () =
    let nextChar = state.readChar() in
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


let token (lexbuf : token lexerstate) : token =
  let ruleBeginMatches string (regexp, _) =
    Str.string_partial_match regexp string 0 &&
      String.length (Str.matched_group 0 string) = String.length string
  and ruleMatches string (regexp, _) =
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
      end else if indent = prevIndent - 2 then begin
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
          returnMultipleTokens lexbuf `End [`EndBlock []]
        end
      end else begin (* indent is more than one level less than in previous line *)
        raiseIndentError lexbuf.location
          "Indentation may only be reduced by two spaces at a time"
      end

    end else begin (** no newline *)
      let rec findToken prevInput prevFullMatches =
        let input = prevInput ^ String.make 1 (lexbuf.readChar()) in
        let beginMatches = List.filter (ruleBeginMatches input) rules in
        let fullMatches =
          match List.filter (ruleMatches input) rules with
            | [] -> prevFullMatches
            | fullMatches ->
                let inputLength = String.length input in
                List.map (fun m -> inputLength, m) fullMatches
        in
        match beginMatches with
          | [] ->
              begin match fullMatches with
                | [] -> raiseUnknownToken lexbuf.location input
                | [length, (_, makeToken)] ->
                    let spareInput = Str.string_after input (length) in
                    let consumedInput = Str.string_before input (length) in
                    lexbuf.putbackString spareInput;
                    makeToken consumedInput
                | multi -> raiseUnknownToken lexbuf.location input
              end
          | multi ->
              findToken input fullMatches
      in
      lexbuf.putbackString (String.make 1 currentChar);
      findToken stringAcc []
(*       let input = ref( stringAcc ^ String.make 1 currentChar ) in *)
(*       let rec findToken () =  *)
(*         match List.filter (ruleBeginMatches !input) rules with *)
(*           | [] -> *)
(*               raiseUnknownToken lexbuf.location !input *)
(*           | [_, makeToken as rul] -> *)
(*               begin try *)
(*                 while true do *)
(*                   let nextChar = lexbuf.readChar() in *)
(*                   let nextInput = !input ^ String.make 1 nextChar in *)
(*                   if ruleBeginMatches nextInput rul then *)
(*                     input := nextInput *)
(*                   else begin *)
(*                     lexbuf.putbackString (String.make 1 nextChar); *)
(*                     raise End_of_file *)
(*                   end *)
(*                 done; *)
(*               with End_of_file -> (); end; *)
(*               if ruleMatches !input rul then  *)
(*                 makeToken !input *)
(*               else ( *)
(*                 raiseUnknownToken lexbuf.location !input ) *)
(*           | matchingRules -> *)
(*               input := !input ^ String.make 1 (lexbuf.readChar()); *)
(*               findToken() *)
(*       in *)
(*       findToken () *)
    end
  in
  match lexbuf.pushedTokens with
    | [] -> worker ""
    | firstToken :: remainingTokens ->
        lexbuf.pushedTokens <- remainingTokens;
        firstToken

let printToken (lineIndent, indentNext) (token :token) =
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
        let noind = lineIndent, `DontIndent
        and ind = lineIndent, `Indent
        in
        printIndent lineIndent;
        match t with
          | `BeginBlock ->
              printf "{\n";
              lineIndent + 1, `Indent
          | `EndBlock _ -> failwith "match failure"
          | `End -> printf "`End\n"; ind
          | `Identifier str -> printf "%s" str; noind
          | `Whitespace length -> printf "_"; noind
          | `Comma -> printf ","; noind
          | `Add arg
          | `Sub arg
          | `Mult arg
          | `Div arg
            -> printf "%s" arg; noind
          | `OpenParen -> printf "("; noind
          | `CloseParen -> printf ")"; noind
      
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

let dummymllexbuf = 
  {
    Lexing.refill_buff = (fun _ -> ());
    lex_buffer = "";
    lex_buffer_len = 0;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_eof_reached = false;
    lex_mem = [| |];
    lex_start_p = Lexing.dummy_pos;
    lex_curr_p = Lexing.dummy_pos;
  }
  
let lexString str =
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
  
(* Tests -------------------------------------------------------------------- *)
    
let tokenEqual l r =
  match l, r with
    | `Whitespace _, `Whitespace _ -> true
    | _, _ -> l = r

open Testing
  
module IndentLexerTestCase : CASE_STRUCT =
struct
  type input = string
  type output = token list

  let printInput str = printf "%s" str
    
  let printOutput tokens = printTokens tokens 

  let outputEqual l r =
    if List.length l = List.length r then
      List.for_all2 tokenEqual l r
    else
      false
    
  type result = [ `Return of output | `Exception of string ]

  let testedFunc = lexString

  let testCases : (input * result) list =
    let id x = `Identifier x in
    let ids stringList =
      let idTokens = List.map (fun str -> (`Identifier str :> token)) stringList in
      Common.combineList (`Whitespace 1) idTokens
    in
    [
      "single", `Return [id "single"; `End];

      "foo bar", `Return [id "foo"; `Whitespace 1; id "bar"; `End];

      "a+b", `Return [id "a"; `Add "+"; id "b"; `End];
      "a   + b", `Return [id "a"; `Add "+"; id "b"; `End];

      "a,b", `Return [id "a"; `Comma; id "b"; `End];
      "x, y", `Return [id "x"; `Comma; id "y"; `End];
      "foo , bar", `Return [id "foo"; `Comma; id "bar"; `End];

      "a +_f b", `Return [id "a"; `Add "+_f"; id "b"; `End];
      
      (* simple one-line expressions *)
      "var int y\n", `Return( ids ["var"; "int"; "y"] @ [`End] );
      "var int x", `Return( ids ["var"; "int"; "x"] @ [`End] );

      "first line\nsecond line\n",
      `Return( [id "first"; `Whitespace 1; id "line"; `End;
                id "second"; `Whitespace 1; id "line"; `End] );
      
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
    ]
end

let () =
  let module M = Tester(IndentLexerTestCase) in
  M.runTestsAndPrintErrors()

let blockEndRE name =
  sprintf "end\\( +%s\\)? *$" name

  
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

    

