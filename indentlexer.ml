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
  | `MultiTokens of token list
]

type tokenBuilder = string -> tokenOrAction

let trimLinefeed str =
  let noLineFeeds = mapString (function '\n' -> ' ' | c -> c) str in
  trim noLineFeeds

module Rules : sig
  type rule

  val rules : (rule * tokenBuilder) list

  val ruleBeginMatches : string -> string -> rule * tokenBuilder -> bool
  val ruleMatches : string -> string -> rule * tokenBuilder -> bool
end = struct

  type charre =
    | Any
    | NoWSOrOp
    | Whitespace
    | OpenParen
    | Identifier
    | Not of charre
    | Operator
    | Or of (charre * charre)

  type rule = (charre * Str.regexp)

  let opSymbols = "-+\\*/&.><=!|:;,"

  let rec charreMatch cre charStr =
    let isIdentifier char =
      (char >= 'a' && char <= 'z') ||
        (char >= 'A' && char <= 'Z') ||
        (char >= '0' && char <= '9') ||
        (char = '_')
    in
    match cre with
      | Any -> true
      | NoWSOrOp ->
          (String.length charStr = 1) &&
            (let char = charStr.[0] in
             isIdentifier char ||
               (char = ')') ||
               (char = ']') ||
               (char = '}'))
      | Whitespace ->
          (String.length charStr = 1 &&
              let char = charStr.[0] in
              (char = ' ') || (char = '\n'))
      | Not icre ->
          not (charreMatch icre charStr)
      | Or (l, r) ->
          charreMatch l charStr || charreMatch r charStr
      | Identifier ->
          (String.length charStr = 1) &&
            isIdentifier charStr.[0]
      | OpenParen ->
          (String.length charStr = 1 &&
              let char = charStr.[0] in
              (char = '(' || char = '[' || char = '{'))
      | Operator ->
          (String.length charStr = 1 &&
              let char = charStr.[0] in
              String.contains opSymbols char)

  (**
     * each rule consists of two regexps (one for the string allowed to precede the expression
     * and one which matches the token) and a function turning the matched string into a token
  *)
  let rules : ((charre * Str.regexp) * tokenBuilder) list =
    let re regexpString =
      Any, Str.regexp regexpString
    in
    let idFunc s = `Token (IDENTIFIER s) in
    let regexpRule str token =
      re str,
      (fun matchedStr -> `Token token)
    in
    let opre symbol = (sprintf "%s\\(_[a-zA-Z]+\\)?" (Str.quote symbol)) in
    let validIdentifierChars = "a-zA-Z0-9:_" in
    let validIdentifierFirstChar = "a-zA-Z_" in
    let validIdentifierLastChar = "a-zA-Z0-9_" in
    let identifierRE =
      sprintf "\\([%s][%s]*[%s]" validIdentifierFirstChar validIdentifierChars validIdentifierLastChar
      ^ sprintf "\\|[%s]\\)" validIdentifierFirstChar
    in
    let identifierRule =
      re identifierRE, (fun str -> `Token (IDENTIFIER str));
    in
    let opRule symbol (tokenF :string -> token) =
      (NoWSOrOp,
       Str.regexp (opre symbol ^ (sprintf "[%s0-9(]" validIdentifierFirstChar))),
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
      (Any, Str.regexp symbol), (fun t -> `Token (f t))
    in
    let stringRule = re "\"[^\"]*\"", idFunc in
    let charRule = re "'[^']+'", idFunc in
    let quoteRule str =
      re (Str.quote str),
      (fun foundStr ->
         assert( str = foundStr );
         `Token (QUOTE str))
    in
    let whitespaceRule = (Any, whitespaceRE), (fun _ -> `Ignore) in
    let opfuncRule prefix =
      re ((Str.quote prefix) ^ sprintf "[%s]+\\(_[a-zA-Z0-9_]+\\)? *" opSymbols),
      (fun (str:string) -> `Token (IDENTIFIER (trim str)))
    in
    let keywordRule =
      re (identifierRE ^ ":\\( \\| *\n\\)"),
      (fun id ->
         let keyword, colon = Common.splitAt id (FromBack 2) in
         let token = KEYWORD_ARG keyword in
         if lastChar colon = '\n' then
           `PutBack( token, "\n" )
         else
           `Token token)
    in
    let opbracketRules =
      [re "op\\[\\]", (fun s -> `Token (IDENTIFIER s));
       re "postop\\[\\]", (fun s -> `Token (IDENTIFIER s))]
    in
    let whitespaceDependentToken chr afterWhitepace afterId =
      [
        (Not Identifier, Str.regexp_string chr),
        (fun _ -> `Token afterWhitepace);
        (Identifier, Str.regexp_string chr),
        (fun s -> `Token afterId);
      ]
    in
    whitespaceDependentToken "(" OPEN_PAREN OPEN_ARGLIST
    @ (
      let bracketRule =
        (Or (Whitespace, OpenParen), Str.regexp_string "["), fun _ -> `Token OPEN_BRACKET
      in
      let postfixBracketRule =
        (Or (Identifier, Operator), Str.regexp_string "["), fun _ -> `Token OPEN_BRACKET_POSTFIX
      in
      [bracketRule; postfixBracketRule])
    @ opbracketRules
    @ (
      let floatRule =
        let trimIdFunc s =
          let trimNewlines str =
            let isLegalNumChar chr =
              let between min max = chr >= min && chr <= max in
              let is c = chr = c in
              between '0' '9' ||
                between 'a' 'z' ||
                between 'A' 'Z' ||
                is '.' ||
                is ' ' ||
                is '\t'
            in
            let strLength = String.length str in
            if strLength > 0 && not (isLegalNumChar str.[strLength-1]) then
              let number, last = Common.splitLastChar str in
              number, String.make 1 last
            else
              Common.trim str, ""
          in
          let trimmedS, putback = trimNewlines s in
          let token = (IDENTIFIER trimmedS) in
          if String.length putback = 0 then
            `Token token
          else
            `PutBack (token, putback)
        in
        let dotnumRE = "[0-9]*\\.[0-9]+[a-zA-Z]*" in
        let numdotRE = "[0-9]+\\.\\([^a-zA-Z]\\| *\n\\)" in
        (Not Identifier, Str.regexp (sprintf "-?\\(%s\\|%s\\)" dotnumRE numdotRE)),
        trimIdFunc
      in
      [floatRule])
    @ (
      let numberRE = "\\(0x\\|0b\\)?[0-9]+[a-zA-Z]*" in
      let intRule = re numberRE, idFunc in
      let negIntRule =
        (Or (Whitespace, OpenParen), Str.regexp ("-" ^ numberRE)), idFunc
      in
      [intRule; negIntRule])
    @ (
      let postfixOps = ["++"; "--"; "..."; "*"] in
      let prefixOps = ["*"; "&"; "++"; "--"] in
      let buildRE oplist =
        "\\(" ^ Common.combine "\\|" (List.map Str.quote oplist) ^ "\\)+"
      in
      let postfixRule =
        (NoWSOrOp,
         Str.regexp (buildRE postfixOps)),
        (fun s ->
           if String.length s >= 1 && (
             let last = Str.last_chars s 1 in
             String.contains "\n()[]{}" last.[0])
           then
             failwith (sprintf "Internal error at token \"%s\"" s)
           else
             match splitup postfixOps s with
               | Some tokens -> `MultiTokens (List.map (fun n -> POSTFIX_OP n) tokens)
               | None ->
                   failwith (sprintf "Internal error in parser at token \"%s\"" s))
      in
      let prefixRule =
        (Or (Whitespace, Or (OpenParen, Operator)),
         Str.regexp (buildRE prefixOps)),
        fun s ->
          match splitup prefixOps s with
            | Some tokens -> `MultiTokens (List.map (fun n -> PREFIX_OP n) tokens)
            | None -> `Ignore
      in
      [postfixRule; prefixRule])
    @ [
      identifierRule;
      whitespaceRule;
      keywordRule;
      opfuncRule "op";
      opfuncRule "preop";
      opfuncRule "postop";
      regexpRule ")" CLOSE_PAREN;
      regexpRule "{" OPEN_CURLY;
      regexpRule "}" CLOSE_CURLY;
      regexpRule "]" CLOSE_BRACKET;
      regexpRule " *, *" COMMA;
      regexpRule "\\." DOT;
      quoteRule "$";
      quoteRule "$$";
      quoteRule "#";
      stringRule;
      charRule;
      contextInsensitiveOp ";" (fun s -> LAZY_BOOL_OP s);
    ]
    @ opRules "+" (fun s -> ADD_OP s)
    @ opRules "+." (fun s -> ADD_OP s)
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

  let ruleBeginMatches prevChar string ((prevCharRE, regexp), _) =
    charreMatch prevCharRE prevChar &&
      Str.string_partial_match regexp string 0 &&
      String.length (Str.matched_group 0 string) = String.length string

  let ruleMatches prevChar string ((prevCharRE, regexp), _) =
    charreMatch prevCharRE prevChar &&
      Str.string_match regexp string 0 &&
      String.length (Str.matched_group 0 string) = String.length string
end

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

type stats = {
  ruleCount :int;
  foundTokens :int ref;
  beginmatchCalcCount :int ref;
  beginmatchMatchNums :int ref;
  fullmatchCalcCount :int ref;
  fullmatchMatchNums :int ref;
}

let stats = {
  ruleCount = List.length Rules.rules;
  foundTokens = ref 0;
  beginmatchCalcCount = ref 0;
  beginmatchMatchNums = ref 0;
  fullmatchCalcCount = ref 0;
  fullmatchMatchNums = ref 0;
}

let printStats() =
  if !(stats.beginmatchCalcCount) > 0 then begin
    printf "----- lexer stats:\n";
    printf "  %d rules\n" stats.ruleCount;
    printf "  %d tokens found\n" !(stats.foundTokens);
    printf "  %d times made begin match set\n" !(stats.beginmatchCalcCount);
    printf "  %d total begin matches\n" !(stats.beginmatchMatchNums);
    printf "  %d total matches calculated\n" !(stats.fullmatchMatchNums);
    printf "  %d total full matches\n" !(stats.fullmatchMatchNums);
    printf "  %f%% begin matches average\n"
      (float !(stats.beginmatchMatchNums) /. float !(stats.beginmatchCalcCount));
    printf "  %f%% full matches average\n"
      (float !(stats.fullmatchMatchNums) /. float !(stats.fullmatchCalcCount));
    printf "-----\n"
  end

let token (lexbuf : token lexerstate) : token =
  let putback lexbuf string =
    let len = String.length string in
    let lastChars = Str.last_chars lexbuf.lastReadChars len in
    if lastChars <> string then begin
      printf
        "Assertion failure: expected \"%s\" but found \"%s\"" lastChars string;
      assert false;
    end;
    lexbuf.backTrack len
  in

  let rec worker () =
    let currentChar = lexbuf.readChar() in
    if currentChar = '!' then (** hack to allow to abort within file *)
      raise Eof;

    if isNewline currentChar then begin
      collectTimingInfo "newline"
        (fun () ->
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
             let onSameIndent() =
               `Token END
             and onMoreIndent() =
               `Token BEGIN_BLOCK
             and onLessIndent() =
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
             in
             (** lookup forward for `Ignore and possibly reset indent? *)
             if indent = prevIndent then begin
               onSameIndent()
             end else if indent = prevIndent + 2 then begin
               onMoreIndent()
             end else if indent > prevIndent then begin
               raiseIndentError lexbuf.location
                 (sprintf "Indentation was increased by %d spaces but only 2 are legal"
                    (indent - prevIndent))
             end else if indent = prevIndent - 2 then begin
               onLessIndent()
             (** indent is more than one level less than in previous line *)                 
             end else begin
               raiseIndentError lexbuf.location
                 (sprintf
                    "Indentation was reduced by %d spaces but only steps of 2 are legal"
                    (prevIndent - indent))
             end
           end)

    (** no newline *)
    end else begin
      let incrn r n =
        r := !r + n
      in
      collectTimingInfo "no newline"
        (fun () ->
           let rec findToken prevInput prevFullMatches rules =
             let actualChar = lexbuf.readChar() in
             let input = prevInput ^ String.make 1 actualChar in
             let prevChar = String.make 1 lexbuf.endOfLastToken in
             assert( String.length prevChar = 1);
             let beginMatches =
               collectTimingInfo "beginMatches"
                 (fun () ->
                    List.filter (Rules.ruleBeginMatches prevChar input) rules)
             in
             incr stats.beginmatchCalcCount;
             incrn stats.beginmatchMatchNums (List.length beginMatches);
             let fullMatches =
               collectTimingInfo "fullMatches"
                 (fun () ->
                    match List.filter (Rules.ruleMatches prevChar input) beginMatches with
                      | [] -> prevFullMatches
                      | fullMatches ->
                          let inputLength = String.length input in
                          List.map (fun m -> inputLength, m) fullMatches)
             in
             incr stats.fullmatchCalcCount;
             incrn stats.fullmatchMatchNums (List.length fullMatches);
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
                   | `MultiTokens (firstToken::remTokens) ->
                       returnMultipleTokens lexbuf (`Token firstToken) remTokens
                   | `MultiTokens [] -> `Ignore
               in
               token
             in
             let tokenFromRule = sampleFunc2 "tokenFromRule" tokenFromRule in
             match beginMatches with
               | [] ->
                   begin match fullMatches with
                     | [] -> raiseUnknownToken lexbuf.location input
                         (sprintf "no matching rule (after '%s')" prevChar)
                     | [length, (_, makeToken)] ->
                         tokenFromRule length makeToken
                     | multi ->
                         begin
                           match List.filter (Rules.ruleMatches prevChar (prevInput ^ " ")) rules with
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
                   findToken input fullMatches oneOrMoreBeginMatches
           in
           lexbuf.backTrack 1;
           incr stats.foundTokens;
           findToken "" [] Rules.rules)
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
          | KEYWORD_ARG str -> "`kwd(" ^ str ^ ")", noind
          | OPEN_BRACKET -> "[", noind
          | OPEN_BRACKET_POSTFIX -> "[_postfix", noind
          | CLOSE_BRACKET -> "]", noind
          (* | EXTENDED_INDENT -> "`extind", noind *)
        in
        indentString lineIndent ^ str, (indent, doindent)

type preprocessorState =
  | Source
  | OneLineComment
  | MultiLineComment

let fakeLocation = { line = 0; fileName = "fake" }

let stripComments source =
  let sourceLength = String.length source in
  let readPos = ref 0 in
  let strippedSource = String.make (sourceLength+1) '\n' in
  let writePos = ref 0 in
  let writeChar chr =
    strippedSource.[!writePos] <- chr;
    incr writePos
  in
  let unexpectedEofInComment() =
    raiseIndentError fakeLocation "Unexpected Eof while parsing comment"
  in
  let rec copySource() =
    if !readPos <= sourceLength - 2 then begin
      readPos := !readPos + 2;
      match source.[!readPos-2], source.[!readPos-1] with
        | '/', '/' ->
            skipSingleLineComment()
        | '/', '*' ->
            skipMultiLineComment()
        | chr1, '/' ->
            writeChar chr1;
            readPos := !readPos - 1;
            copySource();
        | chr1, chr2 ->
            writeChar chr1;
            writeChar chr2;
            copySource()
    end else begin
      if !readPos = sourceLength - 1 then
        writeChar source.[sourceLength - 1]
    end
  and skipSingleLineComment() =
    if !readPos <= sourceLength - 1 then
      let chr = source.[!readPos] in
      incr readPos;
      if chr = '\n' then begin
        writeChar '\n';
        copySource()
      end else
        skipSingleLineComment()
    else
      unexpectedEofInComment()
  and skipMultiLineComment() =
    if !readPos <= sourceLength - 2 then begin
      readPos := !readPos + 2;
      match source.[!readPos-2], source.[!readPos-1] with
        | '*', '/' -> copySource()
        | chr, '*' ->
            decr readPos;
            skipMultiLineComment()
        | _, _ ->
            skipMultiLineComment()
    end else
      unexpectedEofInComment()
  in
  copySource();
  String.sub strippedSource 0 (!writePos + 1)
  
let makeLexbuf fileName source =
  let buffer = ref (stripComments source) in

  (* hide source param to avoid accidental usage *)
  let source = 10 in
  ignore source;

  let rec lexbuf =
    let sourceLength = String.length !buffer in
    let position = ref 0 in
    let readCharFunc() =
      if !position < sourceLength then begin
        let chr = !buffer.[!position] in
        incr position;
        lexbuf.lastReadChars <- lexbuf.lastReadChars ^ String.make 1 chr;
        chr
      end else
        raise Eof
    in

    let backTrack n =
      position := !position - n;
      lexbuf.lastReadChars <- Str.first_chars lexbuf.lastReadChars
        (String.length lexbuf.lastReadChars - n);
    in
    {
      readChar = readCharFunc;
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

let lexbufFromString fileName string =
  let sourceWithEOL = string ^ "\n" in
  makeLexbuf fileName sourceWithEOL

let lexbufFromChannel fileName channel =
  let source = Common.readChannel channel in
  lexbufFromString fileName source


let readChar lexstate = lexstate.readChar()

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
        printf "<%s> " str;
        worker newContext remTokens
  in
  worker (0, `DontIndent) tokens

let runInternalTests() =
  let l = lexbufFromString "d.zomp" "abcde" in
  let expectChar chr = assert( chr = readChar l ) in
  expectChar 'a';
  expectChar 'b';
  l.backTrack 2;
  expectChar 'a';
  expectChar 'b'

