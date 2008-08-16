(*
 * Indentation based lexer and tests
 *)

(* open Iexpr *)
open Printf
open Common

exception Eof

let lastMatchedString = ref ""
let (=~) string regexp =
  lastMatchedString := string;
  Str.string_match (Str.regexp regexp) string  0
let nthmatch n =
  Str.matched_group n !lastMatchedString

let whitespaceRE = Str.regexp " +"

type location = {
  line :int;
  fileName :string;
}

type token = Newparser.token
open Newparser

(* type tokenBuilder = string -> [token | `Ignore | `PutBack of token * string] *)
type tokenOrAction = [
  | `Token of token
  | `Ignore
  | `PutBack of token * string
]

type tokenBuilder = string -> tokenOrAction

let trimLinefeed str =
  let noLineFeeds = mapString (function '\n' -> ' ' | c -> c) str in
  trim noLineFeeds

let lastChar str = str.[String.length str - 1]

(**
 * each rule consists of two regexps (one for the string allowed to precede the expression
 * and one which matches the token) and a function turning the matched string into a token
 *)
let rules : ((Str.regexp * Str.regexp) * tokenBuilder) list =
  let re regexpString = Str.regexp "\\(.\\|\n\\)", Str.regexp regexpString in
  let idFunc s = `Token (IDENTIFIER s) in
  let regexpRule str token =
    re str,
    (fun matchedStr -> `Token token)
  in
  let opre symbol = (sprintf "%s\\(_[a-zA-Z]+\\)?" (Str.quote symbol)) in
  let validIdentifierChars = "a-zA-Z0-9:_" in
  let validIdentifierFirstChar = "a-zA-Z_" in
  let opRule symbol (tokenF :string -> token) =
    (Str.regexp "[a-z)]",
     Str.regexp (opre symbol ^ "[^ \n]")),
    (fun str ->
       let lastChar = str.[String.length str - 1] in
       let str2 = Str.string_before str (String.length str - 1) in
       `PutBack( tokenF (trim str2), String.make 1 lastChar ) )
  in
  let opRuleWS symbol (tokenF :string -> token) =
    re (sprintf " +%s\\( +\\|\n\\)" (opre symbol)),
    (fun str ->
       let token = (tokenF (trimLinefeed str)) in
       if lastChar str = '\n' then
         `PutBack(token, "\n")
       else
         `Token token)
  in
  let opRules symbol (tokenF :string -> token) =
    [opRule symbol tokenF;
     opRuleWS symbol tokenF]
  in
  let opRulesMultiSym symbols tokenF =
    let rules = List.map (fun symbol -> opRules symbol tokenF) symbols in
    List.flatten rules
  in
  let contextInsensitiveOp symbol f =
    (Str.regexp "", Str.regexp symbol), (fun t -> `Token (f t))
  in
  let postfixRule symbol =
    re (sprintf "\\(%s +\\|%s\n\\)" (Str.quote symbol) (Str.quote symbol)),
    (fun s ->
       if String.length s >= 1 && Str.last_chars s 1 = "\n" then
         `PutBack (POSTFIX_OP (trim (Str.first_chars s (String.length s - 1))), "\n")
       else
         let trimmed = trim s in
         let removedWhitespace = String.length s - String.length trimmed in
         `PutBack( POSTFIX_OP (trim s), String.make removedWhitespace ' ' ) )
  in
  let prefixRule symbol =
    (Str.regexp "\\(\n\\| +\\)", Str.regexp (sprintf "%s" (Str.quote symbol))),
    (fun s -> `Token (PREFIX_OP (trim s)))
  in
  let stringRule = re "\"[^\"]*\"", idFunc in
  let charRule = re "'[^']+'", idFunc in
  let intRule = re "-?[0-9]+[a-zA-Z]*", idFunc in
  let floatRule = re "-?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)[a-zA-Z]*", idFunc in
  let identifierRule =
    re (sprintf "[%s][%s]*" validIdentifierFirstChar validIdentifierChars),
    (fun str -> `Token (IDENTIFIER str));
  in
  let quoteRule str =
    re (Str.quote str),
    (fun foundStr ->
       assert( str = foundStr );
       `Token (QUOTE str))
  in
  let whitespaceRule = (Str.regexp ".*", whitespaceRE), (fun _ -> `Ignore) in
  let opfuncRule prefix =
    re ((Str.quote prefix) ^ "[+-\\*/&.><=!;|]+ *"),
    (fun (str:string) -> `Token (IDENTIFIER (trim str)))
  in
  [
    (Str.regexp "[^a-zA-Z0-9]", Str.regexp_string "("), (fun s -> `Token OPEN_PAREN);
    (Str.regexp "[a-zA-Z0-9]", Str.regexp_string "("), (fun s -> `Token OPEN_ARGLIST);
    identifierRule;
    whitespaceRule;
    opfuncRule "op";
    opfuncRule "preop";
    opfuncRule "postop";
    (* regexpRule "(" OPEN_PAREN; *)
    regexpRule ")" CLOSE_PAREN;
    regexpRule "{" OPEN_CURLY;
    regexpRule "}" CLOSE_CURLY;
    regexpRule " *, *" COMMA;
    regexpRule "\\." DOT;
    quoteRule "$";
    quoteRule "#";
    postfixRule "...";
    postfixRule "*";
    prefixRule "*";
    prefixRule "&";
    postfixRule "++";
    postfixRule "--";
    prefixRule "++";
    prefixRule "--";
    stringRule;
    charRule;
    intRule;
    floatRule;
    contextInsensitiveOp ";" (fun s -> LAZY_BOOL_OP s);
  ]
  @ opRules "+" (fun s -> ADD_OP s)
  @ opRules "-" (fun s -> ADD_OP s)
  @ opRules "*" (fun s -> MULT_OP s)
  @ opRules "/" (fun s -> MULT_OP s)
  @ opRules "**" (fun s -> MULT_OP s)
  @ opRules "++" (fun s -> ADD_OP s)
  @ opRulesMultiSym ["="; ":="] (fun s -> ASSIGN_OP s)
  @ opRulesMultiSym ["=="; "!="; ">"; ">="; "<"; "<=";] (fun s -> COMPARE_OP s)
  @ opRulesMultiSym ["&&"; "||"] (fun s -> LAZY_BOOL_OP s)
  @ opRulesMultiSym ["&"; "|"; "^"] (fun s -> STRICT_BOOL_OP s)
    (** Attention: all characters used as operators need to be listed in the regexp in opfuncRule *)

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
  with Eof -> () end;
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
      raise Eof;

    if isNewline currentChar then begin
      let rec consumeWhitespaceAndReturnIndent indent =
        let eof, nextChar =
          try false, lexbuf.readChar()
          with Eof -> true, '\n'
        in
        if eof then
          indent
        else if isWhitespace nextChar then
          consumeWhitespaceAndReturnIndent (indent+1)
        else if isNewline nextChar then
          consumeWhitespaceAndReturnIndent 0
        else begin
          lexbuf.backTrack 1;
          indent
        end
      in
      let indent = consumeWhitespaceAndReturnIndent 0 in
      let prevIndent = lexbuf.prevIndent in
      lexbuf.prevIndent <- indent;

      if lexbuf.readTokenBefore = false then begin
        if indent = prevIndent then worker ()
        else raiseIndentError lexbuf.location "First line needs to be indented at column 0"

      end else begin
        (* lookup forward for `Ignore and possibly reset indent? *)
        if indent = prevIndent then begin
          `Token END
        end else if indent = prevIndent + 2 then begin
          `Token BEGIN_BLOCK
        end else if indent > prevIndent then begin
          raiseIndentError lexbuf.location
            (sprintf "Indentation was increased by %d spaces but only 2 are legal" (indent - prevIndent))
        end else if indent = prevIndent - 2 then begin
          let line = readUntil isNewline lexbuf in
          if isBlockEndLine line then begin
            putback lexbuf "\n";
            if line =~ "^end\\(.*\\)$" then
              match List.rev (Str.split whitespaceRE (nthmatch 1)) with
                | "}" :: args ->
                    returnMultipleTokens lexbuf (`Token END) [END_BLOCK (List.rev args); CLOSE_CURLY]
                | args ->
                    returnMultipleTokens lexbuf (`Token END) [END_BLOCK (List.rev args)]
            else
              failwith "splitting block end line failed"
          end else begin
            putback lexbuf (line ^ "\n");
            returnMultipleTokens lexbuf (`Token END) [END_BLOCK []]
          end
        end else begin (* indent is more than one level less than in previous line *)
          raiseIndentError lexbuf.location
            (sprintf "Indentation was reduced by %d spaces but only steps of 2 are legal" (prevIndent - indent))
        end
      end

    (** no newline *)
    end else begin
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
          let token : [`Ignore | `Token of token] =
            match (makeToken consumedInput : tokenOrAction) with
              | `PutBack(token, prefetched) ->
                  putback lexbuf prefetched;
                  (`Token token :> [`Ignore|`Token of token])
              | `Ignore -> `Ignore
              | `Token token -> ((`Token token) : [`Ignore | `Token of token])
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
  let rec getNextToken() =
    match worker() with
      | `Ignore ->
          let lastReadChar =
            let len = String.length lexbuf.lastReadChars in
            if len >= 1 then lexbuf.lastReadChars.[len-1]
            else '\n'
          in
          lexbuf.endOfLastToken <- lastReadChar;
          getNextToken()
      | `Token token -> token
  in
  let token =
    match lexbuf.pushedTokens with
      | [] -> getNextToken()
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
    | END_BLOCK args ->
        indentString (lineIndent - 1) ^
          begin
            match args with
              | [] -> "END_BLOCK"
              | _ -> sprintf "END_BLOCK(%s)" (Common.combine ", " args)
          end,
        (lineIndent - 1, `Indent)
    | _ as t ->
        let noind = lineIndent, `DontIndent
        and ind = lineIndent, `Indent
        in
        let str, (indent, doindent) =
        match t with
          | BEGIN_BLOCK ->
              "BEGIN_BLOCK\n", (lineIndent + 1, `Indent)
          | END_BLOCK _ -> failwith "match failure"
          | END -> "END\n", ind
          | IDENTIFIER str -> str, noind
(*           | Whitespace length -> "_", noind *)
          | COMMA -> ",", noind
          | ADD_OP arg
          | MULT_OP arg
          | ASSIGN_OP arg
          | COMPARE_OP arg
          | LAZY_BOOL_OP arg
          | STRICT_BOOL_OP arg
            -> "op" ^ arg, noind
          | POSTFIX_OP arg -> "post" ^ arg, noind
          | PREFIX_OP arg -> "pre" ^ arg, noind
          | OPEN_PAREN -> "(", noind
          | OPEN_ARGLIST -> "(`", noind
          | CLOSE_PAREN -> ")", noind
          | OPEN_CURLY -> "{", noind
          | CLOSE_CURLY -> "}", noind
          | DOT -> ".", noind
          | QUOTE str -> "$" ^ str, noind
        in
        indentString lineIndent ^ str, (indent, doindent)

type preprocessorState =
  | Source
  | OneLineComment
  | MultiLineComment

let makeLexbuf fileName readCharFunc =
  let buffer = ref "" in
  let eof = ref false in

  let rec lexbuf =
    let readCharExtraNewline () =
      if !eof then
        raise Eof
      else
        try
          readCharFunc()
        with Eof ->
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
    let preprocessedRead () =
      let rec readSource() =
        (* let readNextTwoChars() = readCharWithBuffer(), (try Some(readCharWithBuffer()) with Eof -> None) in *)
        match readCharWithBuffer(), (try Some(readCharWithBuffer()) with Eof -> None) with
        (* match readNextTwoChars() with *)
          | '/', Some '/' -> readSingleLineComment()
          | '/', Some '*' -> readMultiLineComment()
          | lastCharInFile, None -> lastCharInFile
          | char, Some _ ->
              backTrack 1;
              char
      and readSingleLineComment() =
        match readCharWithBuffer() with
          | '\n' -> '\n'
          | _ -> readSingleLineComment()
      and readMultiLineComment() =
        match  readCharWithBuffer(), (try Some(readCharWithBuffer()) with Eof -> None) with
          | '*', Some '/' -> readSource()
          | lastCharInFile, None -> lastCharInFile
          | _, Some '*' -> backTrack 1; readMultiLineComment();
          | _, Some _ -> readMultiLineComment()
      in
      readSource()
    in
    {
      readChar = preprocessedRead;
      backTrack = backTrack;
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
      raise Eof
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
      with Eof -> None
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

