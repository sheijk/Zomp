(*
 * Indentation based lexer and tests
 *)

(* open Iexpr *)
open Printf


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

let rules =
  let re = Str.regexp in
  let stringRule str token = 
    re str,
    (fun matchedStr -> 
       assert( str = matchedStr );
       token)
  in
  let opRule symbol tokenF =
    re (sprintf "%s\\(_[a-zA-Z]+\\)?" symbol),
    tokenF
  in
  [
    re "[a-zA-Z0-9]+", (fun str -> `Identifier str);
    whitespaceRE, (fun str -> `Whitespace (String.length str));
    stringRule "(" `OpenParen;
    stringRule ")" `CloseParen;
    stringRule "," `Comma;
    opRule "+" (fun s -> `Add s);
    opRule "-" (fun s -> `Sub s);
    opRule "*" (fun s -> `Mult s);
    opRule "/" (fun s -> `Div s);
  ]

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

    end else begin (** no whitespace *)
      let input = ref( stringAcc ^ String.make 1 currentChar ) in
      let rec findToken() = 
        match List.filter (ruleBeginMatches !input) rules with
          | [] -> 
              raiseUnknownToken lexbuf.location !input
          | [_, makeToken as rul] ->
              begin try
                while true do
                  let nextChar = lexbuf.readChar() in
                  let nextInput = !input ^ String.make 1 nextChar in
                  if ruleBeginMatches nextInput rul then
                    input := nextInput
                  else begin
                    lexbuf.putbackString (String.make 1 nextChar);
                    raise End_of_file
                  end
                done;
              with End_of_file -> (); end;
              if ruleMatches !input rul then 
                makeToken !input
              else
                raiseUnknownToken lexbuf.location !input
          | matchingRules ->
              printf "multiple rules\n"; flush stdout;
              input := !input ^ String.make 1 (lexbuf.readChar());
              findToken()
      in
      findToken()
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
    let ids stringList =
      let idTokens = List.map (fun str -> (`Identifier str :> token)) stringList in
      Common.combineList (`Whitespace 1) idTokens
    in
    [
      (* simple one-line expressions *)
      "var int y\n", `Return( ids ["var"; "int"; "y"] @ [`End] );
      "var int x", `Return( ids ["var"; "int"; "x"] @ [`End] );

      "first line\nsecond line\n",
      `Return( [`Identifier "first"; `Whitespace 1; `Identifier "line"; `End;
                `Identifier "second"; `Whitespace 1; `Identifier "line"; `End] );
      
      (* simple multi-line expressions *)
      "if a then\n\
      \  foobar\n\
      end",
      `Return( ids ["if"; "a"; "then"] @ [`BeginBlock]
               @ [`Identifier "foobar"; `End]
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
               @ [`EndBlock []; `Identifier "else"; `BeginBlock]
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
  
module IndentLexerTester = Tester(IndentLexerTestCase)

let () = IndentLexerTester.runTestsAndPrintErrors()

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

    

