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
(* | `Whitespace of int *)
]

type userToken = [
| `Identifier of string
| `OpenParen | `CloseParen
| `OpenCurlyBrackets | `CloseCurlyBrackets
| `Comma
| `Add of string
| `Mult of string
| `StrictBoolOp of string
| `LazyBoolOp of string
| `Assign of string
| `Compare of string
| `Dot
| `Prefix of string
| `Postfix of string
| `Quote of string
]

let lastMatchedString = ref ""
let (=~) string regexp =
  lastMatchedString := string;
  Str.string_match (Str.regexp regexp) string  0
let nthmatch n =
  Str.matched_group n !lastMatchedString
    
let whitespaceRE = Str.regexp " +"

type token = [ indentToken | userToken ]

let rules : ((Str.regexp * Str.regexp) * (string -> [< token | `Ignore | `PutBack of token * string])) list =
  (*   let re = Str.regexp in *)
  let re regexpString = Str.regexp "\\(.\\|\n\\)", Str.regexp regexpString in
  let idFunc s = `Identifier s in
  let regexpRule str token = 
    re str,
    (fun matchedStr ->
       token)
  in
  let opre symbol = (sprintf "%s\\(_[a-zA-Z]+\\)?" (Str.quote symbol)) in
  let validIdentifierChars = "a-zA-Z0-9:_" in
  let validIdentifierFirstChar = "a-zA-Z" in
  let opRule symbol tokenF =
    (Str.regexp "[a-z)]",
     Str.regexp (opre symbol)),
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
  let opRulesMultiSym symbols tokenF =
    let rules = List.map (fun symbol -> opRules symbol tokenF) symbols in
    List.flatten rules
  in
  let postfixRule symbol =
    re (sprintf "\\(%s +\\|%s\n\\)" (Str.quote symbol) (Str.quote symbol)),
    (fun s ->
       if Str.last_chars s 1 = "\n" then
         `PutBack (`Postfix (trim (Str.first_chars s (String.length s - 1))), "\n")
       else
         let trimmed = trim s in
         let removedWhitespace = String.length s - String.length trimmed in
         `PutBack( `Postfix (trim s), String.make removedWhitespace ' ' ) )
  in
  let prefixRule symbol =
    (Str.regexp "\\(\n\\| +\\)", Str.regexp (sprintf "%s" (Str.quote symbol))),
    (fun s -> `Prefix (trim s))
  in
  let stringRule = re "\"[^\"]*\"", idFunc in
  let charRule = re "'[^']+'", idFunc in
  let intRule = re "[0-9]+", idFunc in
  let floatRule = re "\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)", idFunc in
  let identifierRule =
    re (sprintf "[%s][%s]*" validIdentifierFirstChar validIdentifierChars),
    (fun str -> `Identifier str);
  in
  let quoteRule str =
    re (Str.quote str),
    (fun foundStr ->
       assert( str = foundStr );
       `Quote str)
  in
  [
    identifierRule;
    (Str.regexp ".*", whitespaceRE), (fun _ -> `Ignore);
(*     (Str.regexp ".*", whitespaceRE), (fun str -> `Whitespace (String.length str)); *)
    regexpRule "(" `OpenParen;
    regexpRule ")" `CloseParen;
    regexpRule "{" `OpenCurlyBrackets;
    regexpRule "}" `CloseCurlyBrackets;
    regexpRule " *, *" `Comma;
    regexpRule "\\." `Dot;
    quoteRule "$";
    quoteRule "#";
    postfixRule "...";
    postfixRule "*";
    prefixRule "*";
    prefixRule "&";
    stringRule;
    charRule;
    intRule;
    floatRule;
  ]
  @ opRules "+" (fun s -> `Add s)
  @ opRules "-" (fun s -> `Add s)
  @ opRules "*" (fun s -> `Mult s)
  @ opRules "/" (fun s -> `Mult s)
  @ opRules "**" (fun s -> `Mult s)
  @ opRulesMultiSym ["="; ":="] (fun s -> `Assign s)
  @ opRulesMultiSym ["=="; "!="; ">"; ">="; "<"; "<=";] (fun s -> `Compare s)
  @ opRulesMultiSym ["&"; "|"; "^"] (fun s -> `StrictBoolOp s)
  @ opRulesMultiSym ["&&"; "||"] (fun s -> `LazyBoolOp s)
    
type 'token lexerstate = {
  readChar : unit -> char;
  backTrack : int -> unit;
(*   putbackString : string -> unit; *)
  mutable location : location;
  mutable prevIndent : int;
  mutable pushedTokens : 'token list;
  mutable readTokenBefore : bool;
  mutable lastReadChars :string;
  mutable endOfLastToken :char;
}

let returnMultipleTokens state first remaining =
  state.pushedTokens <- remaining;
  first
    
let isNewline chr = chr = '\n'
let isWhitespace chr = chr = ' '

let isBlockEndLine line =
  Str.string_match (Str.regexp "^end.*$") line 0
    
exception UnknowToken of location * string * string
let raiseUnknownToken loc str reason = raise (UnknowToken (loc, str, reason))

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
  let putback lexbuf string =
    let len = String.length string in
    if Str.last_chars lexbuf.lastReadChars len <> string then begin
      printf "Assertion failure";
      assert false;
    end;
    lexbuf.backTrack len
  in
  
  let ruleBeginMatches prevChar string ((prevCharRE, regexp), _) =
    Str.string_match prevCharRE prevChar 0 &&
      Str.string_partial_match regexp string 0 &&
      String.length (Str.matched_group 0 string) = String.length string
  and ruleMatches prevChar string ((prevCharRE, regexp), _) =
    Str.string_match prevCharRE prevChar 0 &&
      Str.string_match regexp string 0 &&
      String.length (Str.matched_group 0 string) = String.length string
  in
  
  let rec worker () =
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
          lexbuf.backTrack 1;
          indent
        end
      in
      let indent = consumeWhitespace 0 in
      let prevIndent = lexbuf.prevIndent in
      lexbuf.prevIndent <- indent;
      if lexbuf.readTokenBefore = false then begin
        if indent = prevIndent then
          worker ()
        else
          raiseIndentError lexbuf.location
            "First line needs to be indented at column 0"
      end else begin
        if indent = prevIndent then begin
          `End
        end else if indent > prevIndent then begin
          `BeginBlock
        end else if indent = prevIndent - 2 then begin
          let line = readUntil isNewline lexbuf in
          if isBlockEndLine line then begin
            putback lexbuf "\n";
            if line =~ "^end\\(.*\\)$" then
              match List.rev (Str.split whitespaceRE (nthmatch 1)) with
                | "}" :: args ->
                    returnMultipleTokens lexbuf `End [`EndBlock (List.rev args); `CloseCurlyBrackets]
                | args ->
                    returnMultipleTokens lexbuf `End [`EndBlock (List.rev args)]
            else
              failwith "splitting block end line failed"
          end else begin
            putback lexbuf (line ^ "\n");
            returnMultipleTokens lexbuf `End [`EndBlock []]
          end
        end else begin (* indent is more than one level less than in previous line *)
          raiseIndentError lexbuf.location
            "Indentation may only be reduced by two spaces at a time"
        end
      end

    end else begin (** no newline *)
      let rec findToken prevInput prevFullMatches =
        let actualChar = lexbuf.readChar() in
        let input = prevInput ^ String.make 1 actualChar in
        let prevChar = String.make 1 lexbuf.endOfLastToken in
        assert( String.length prevChar = 1);
        let beginMatches = List.filter (ruleBeginMatches prevChar input) rules in
        let fullMatches =
          match List.filter (ruleMatches prevChar input) rules with
            | [] -> prevFullMatches
            | fullMatches -> 
                let inputLength = String.length input in
                List.map (fun m -> inputLength, m) fullMatches
        in
        let tokenFromRule length makeToken =
          let spareInput = Str.string_after input length in
          let consumedInput = Str.string_before input length in
          putback lexbuf spareInput;
          let token =
            match makeToken consumedInput with
              | `PutBack(token, prefetched) ->
                  putback lexbuf prefetched;
                  token
              | `Ignore ->
                  worker ()
              | #token as token -> token
          in
          token
        in
        match beginMatches with
          | [] ->
              begin match fullMatches with
                | [] -> raiseUnknownToken lexbuf.location input
                    (sprintf "no matching rule (after '%s')" prevChar)
                | [length, (_, makeToken)] ->
                    tokenFromRule length makeToken
                | multi ->
                    begin
                      match List.filter (ruleMatches prevChar (prevInput ^ " ")) rules with
                        | [_, makeToken] ->
                            tokenFromRule (String.length input + 1) makeToken
                        | [] ->
                            raiseUnknownToken lexbuf.location input
                              (sprintf "no matching rules at end of line (after '%s')" prevChar)
                        | multi ->
                            raiseUnknownToken lexbuf.location input
                              (sprintf "multiple rules matching (after '%s')" prevChar)
                    end
              end
          | oneOrMoreBeginMatches ->
              findToken input fullMatches
      in
      lexbuf.backTrack 1;
      findToken "" []
    end
  in
  let token =
    match lexbuf.pushedTokens with
      | [] -> worker ()
      | firstToken :: remainingTokens ->
          lexbuf.pushedTokens <- remainingTokens;
          firstToken
  in
  lexbuf.readTokenBefore <- true;
  lexbuf.endOfLastToken <- (Str.last_chars lexbuf.lastReadChars 1).[0];
  lexbuf.lastReadChars <- Str.last_chars lexbuf.lastReadChars 1;
  token

let tokenToString (lineIndent, indentNext) (token :token) =
  let indentString indent =
    if indentNext = `Indent then
      sprintf "%s" (String.make (4 * indent) ' ')
    else
      ""
  in
  match token with
    | `EndBlock args ->
        indentString (lineIndent - 1) ^
          begin
            match args with
              | [] -> "`EndBlock"
              | _ -> sprintf "`EndBlock(%s)" (Common.combine ", " args)
          end,
        (lineIndent - 1, `Indent)
    | _ as t ->
        let noind = lineIndent, `DontIndent
        and ind = lineIndent, `Indent
        in
        let str, (indent, doindent) =
        match t with
          | `BeginBlock ->
              "`BeginBlock\n", (lineIndent + 1, `Indent)
          | `EndBlock _ -> failwith "match failure"
          | `End -> "`End\n", ind
          | `Identifier str -> str, noind
(*           | `Whitespace length -> "_", noind *)
          | `Comma -> ",", noind
          | `Add arg
          | `Mult arg
          | `Assign arg
          | `Compare arg
          | `LazyBoolOp arg
          | `StrictBoolOp arg
            -> "op" ^ arg, noind
          | `Postfix arg -> "post" ^ arg, noind
          | `Prefix arg -> "pre" ^ arg, noind
          | `OpenParen -> "(", noind
          | `CloseParen -> ")", noind
          | `OpenCurlyBrackets -> "{", noind
          | `CloseCurlyBrackets -> "}", noind
          | `Dot -> ".", noind
          | `Quote str -> "$" ^ str, noind
        in
        indentString lineIndent ^ str, (indent, doindent)
      
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
      lexbuf.lastReadChars <- lexbuf.lastReadChars ^ String.make 1 chr;
      chr
    in
    let backTrack count =
      buffer := Str.last_chars lexbuf.lastReadChars count ^ !buffer;
      lexbuf.lastReadChars <- Str.first_chars lexbuf.lastReadChars
        (String.length lexbuf.lastReadChars - count);
    in
    {
      readChar = readCharWithBuffer;
      backTrack = backTrack;
(*       putbackString = (fun string -> buffer := string ^ !buffer); *)
      location = { line = 0; fileName = fileName };
      prevIndent = 0;
      pushedTokens = [];
      readTokenBefore = false;
      lastReadChars = "\n";
      endOfLastToken = '\n';
    }
  in
  lexbuf

let lexbufFromChannel fileName channel = makeLexbuf fileName (fun () -> input_char channel)

let lexbufFromString fileName string =
  let endlineString = string ^ "\n" in
  let stringLength = String.length endlineString in
  let position = ref 0 in
  let readCharFunc() =
    if !position < stringLength then begin
      let chr = endlineString.[!position] in
      incr position;
      chr
    end else
      raise End_of_file
  in
  makeLexbuf fileName readCharFunc

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

let tokensToString tokens =
  let rec worker context acc = function
    | [] -> acc
    | t :: remTokens ->
        let str, newContext = tokenToString context t in
        worker newContext (str::acc) remTokens
  in
  let revLines = worker (0, `DontIndent) [] tokens in
  Common.combine " " (List.rev revLines)

let printTokens tokens =
  let rec worker context = function
    | [] -> ()
    | t :: remTokens ->
        let str, newContext = tokenToString context t in
        printf "%s " str;
        worker newContext remTokens
  in
  worker (0, `DontIndent) tokens

(* Tests -------------------------------------------------------------------- *)
    
let tokenEqual l r =
  match l, r with
(*     | `Whitespace _, `Whitespace _ -> true *)
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
(*       Common.combineList (`Whitespace 1) idTokens *)
    in
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

    

