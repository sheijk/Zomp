(*
 * Indentation based lexer and tests
 *)

(* open Iexpr *)
open Printf
open Common

exception Eof
type location = {
  line :int;
  fileName :string;
}

exception UnknowToken of location * string * string
let raiseUnknownToken loc str reason = raise (UnknowToken (loc, str, reason))

exception IndentError of location * string
let raiseIndentError loc str = raise (IndentError (loc, str))

type token = Newparser.token
open Newparser

type tokenOrAction = [
  | `Token of token
  | `Ignore
  | `PutBack of token * string
  | `MultiTokens of token list
]

type tokenBuilder = string -> tokenOrAction

module Util =
struct
  let lastMatchedString = ref ""
  let (=~) string regexp =
    lastMatchedString := string;
    Str.string_match (Str.regexp regexp) string  0
  let nthmatch n =
    Str.matched_group n !lastMatchedString

  let whitespaceRE = Str.regexp " +"

  let trimLinefeed str =
    let noLineFeeds = mapString (function '\n' -> ' ' | c -> c) str in
    trim noLineFeeds

  let isNewline chr = chr = '\n'
  let isWhitespace chr = chr = ' '

  let isBlockEndLine line =
    Str.string_match (Str.regexp "^end.*$") line 0

  let appendChar string char = string ^ String.make 1 char

end

open Util

let stripComments source =
  let fakeLocation = { line = 0; fileName = "fake" } in
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
            | COMMA -> ",", noind
            | ADD_OP arg
            | MULT_OP arg
            | MOD_OP arg
            | ASSIGN_OP arg
            | COMPARE_OP arg
            | LAZY_BOOL_OP arg
            | STRICT_BOOL_OP arg
              -> "op" ^ arg, noind
            | POSTFIX_OP arg -> "post" ^ arg, noind
            | PREFIX_OP arg -> "pre" ^ arg, noind
            | OPEN_PAREN -> "(", noind
            | OPEN_ARGLIST -> "post(", noind
            | CLOSE_PAREN -> ")", noind
            | OPEN_CURLY -> "{", noind
            | CLOSE_CURLY -> "}", noind
            | DOT -> ".", noind
            | QUOTE str -> "$" ^ str, noind
            | KEYWORD_ARG str -> "`kwd(" ^ str ^ ")", noind
            | OPEN_BRACKET -> "[", noind
            | OPEN_BRACKET_POSTFIX -> "post[", noind
            | CLOSE_BRACKET -> "]", noind
        in
        indentString lineIndent ^ str, (indent, doindent)

let token2str token =
  fst (tokenToString (0, `DontIndent) token)

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

module Rules : sig
  type rule

  val rules : (rule * tokenBuilder) list

  val ruleMatchesAt : prevToken:[`Whitespace | `Token of token] -> source:string -> pos:int -> rule * tokenBuilder ->
    int option
end = struct

  type charre =
    | Any
    | NoWSOrOp
    | Whitespace
    | OpenParen
    | ClosingParen
    | Identifier
    | Not of charre
    | Operator
    | PostfixOperator
    | Or of (charre * charre)

  type rule = (charre * Str.regexp)

  let opSymbols = "-+\\*/&.><=!|:;,%"

  let rec charreMatch cre token =
    let isOperator = function
      | ADD_OP _ | MULT_OP _ | ASSIGN_OP _ | COMPARE_OP _ | LAZY_BOOL_OP _ | STRICT_BOOL_OP _ | MOD_OP _
      | PREFIX_OP _ | POSTFIX_OP _ | QUOTE _
      | COMMA
          -> true
      | IDENTIFIER _ | END | BEGIN_BLOCK | END_BLOCK _
      | OPEN_PAREN | CLOSE_PAREN | OPEN_ARGLIST | OPEN_CURLY | CLOSE_CURLY
      | OPEN_BRACKET | OPEN_BRACKET_POSTFIX | CLOSE_BRACKET
      | DOT
      | KEYWORD_ARG _
        -> false
    in
    let isOpenParen = function
      | OPEN_PAREN | OPEN_ARGLIST | OPEN_BRACKET | OPEN_BRACKET_POSTFIX | OPEN_CURLY -> true
      | _ -> false
    in
    let isClosingParen = function
      | CLOSE_PAREN | CLOSE_BRACKET | CLOSE_CURLY -> true
      | _ -> false
    in
    match cre, token with
      | Any, _ -> true
      | NoWSOrOp, _ ->
          (match token with
             | `Whitespace -> false
             | `Token t -> not (isOperator t) && not (isOpenParen t))
      | Whitespace, `Whitespace -> true
      | Whitespace, `Token BEGIN_BLOCK -> true
      | Whitespace, `Token END -> true
      | Whitespace, _ -> false
      | Not icre, _ ->
          not (charreMatch icre token)
      | Or (l, r), _ ->
          charreMatch l token || charreMatch r token
      | Identifier, `Token IDENTIFIER _ -> true
      | Identifier, _ -> false
      | OpenParen, `Token t -> isOpenParen t
      | OpenParen, `Whitespace -> false
      | ClosingParen, `Token t -> isClosingParen t
      | ClosingParen, `Whitespace -> false
      | Operator, `Whitespace -> false
      | Operator, `Token t -> isOperator t
      | PostfixOperator, `Token POSTFIX_OP _ -> true
      | PostfixOperator, _ -> false

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
      (Not (Or (Whitespace, Or (Operator, OpenParen))),
       Str.regexp (Str.quote symbol ^ (sprintf "[%s0-9(]" validIdentifierFirstChar))),
      (fun str ->
         let strLength = String.length str in
         let lastChar = str.[strLength - 1] in
         let withoutLastChar = Str.string_before str (strLength - 1) in
         `PutBack( tokenF (trim withoutLastChar), String.make 1 lastChar ) )
    in
    let opRuleWS symbol (tokenF :string -> token) =
      (Whitespace, Str.regexp (sprintf " *%s\\( +\\|\n\\)" (opre symbol))),
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
         else if lastChar colon = ' ' then
           `PutBack( token, " " )
         else
           `Token token)
    in
    (let preArglist = Or (Identifier, ClosingParen) in
     let re = Str.regexp_string "(" in
     [
       (preArglist, re), (fun _ -> `Token OPEN_ARGLIST);
       (Not preArglist, re), (fun _ -> `Token OPEN_PAREN)
     ])
    @ (
      let bracketRule =
        (Or (Whitespace, OpenParen), Str.regexp_string "["), fun _ -> `Token OPEN_BRACKET
      in
      let postfixBracketRule =
        (Or (Identifier, Operator), Str.regexp_string "["),
        fun _ -> `Token OPEN_BRACKET_POSTFIX
      in
      [bracketRule; postfixBracketRule])
    @
      (let opbracketRules =
         [re "op\\[\\]", (fun s -> `Token (IDENTIFIER s));
          re "postop\\[\\]", (fun s -> `Token (IDENTIFIER s))]
       in
       opbracketRules)
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
                is '.'
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
        (Not (Or (Identifier, ClosingParen)), Str.regexp ("-" ^ numberRE)), idFunc
      in
      [intRule; negIntRule])
    @ (
      let postfixOps = ["++"; "--"; "..."; "*"] in
      let prefixOps = ["*"; "&"; "++"; "--"] in
      let buildRE oplist =
        "\\(" ^ Common.combine "\\|" (List.map Str.quote oplist) ^ "\\)+"
      in
      let postfixRule =
        (Or (Identifier, ClosingParen),
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
    @ opRules "%" (fun s -> MOD_OP s)
    @ opRules "**" (fun s -> MULT_OP s)
    @ opRules "++" (fun s -> ADD_OP s)
    @ opRulesMultiSym ["="; ":="] (fun s -> ASSIGN_OP s)
    @ opRulesMultiSym ["=="; "!="; ">"; ">="; "<"; "<=";] (fun s -> COMPARE_OP s)
    @ opRulesMultiSym ["&&"; "||"] (fun s -> LAZY_BOOL_OP s)
    @ opRulesMultiSym ["&"; "|"; "^"] (fun s -> STRICT_BOOL_OP s)
      (** Attention: all characters used as operators need to be listed in the regexp in opfuncRule *)

  let ruleMatchesAt ~prevToken ~source ~pos ((prevCharRE, regexp), _) =
    if (charreMatch prevCharRE prevToken && Str.string_match regexp source pos) then
      Some (Str.match_end() - Str.match_beginning())
    else
      None
end

type 'token lexerstate = {
  content : string;
  contentLength : int;
  mutable position : int;
  mutable location : location;
  mutable prevIndent : int;
  mutable pushedTokens : 'token list;
  mutable readTokenBefore : bool;
  mutable lastReadChars :string;
  mutable previousToken :[`Whitespace | `Token of token];
}

let locationOfLexstate ls = ls.location

let moveToNextLine ls =
  ls.location <- { ls.location with line = ls.location.line + 1 }

let readChars lexbuf n =
  if lexbuf.position + n < lexbuf.contentLength then begin
    let str = String.sub lexbuf.content lexbuf.position n in
    lexbuf.position <- lexbuf.position + n;
    lexbuf.lastReadChars <- lexbuf.lastReadChars ^ str;
    String.iter
      (fun chr ->
         if chr = '\n' then
           moveToNextLine lexbuf)
      str;
    str
  end else
    raise Eof

let readChar lexbuf =
  if lexbuf.position < lexbuf.contentLength then begin
    let chr = lexbuf.content.[lexbuf.position] in
    lexbuf.position <- lexbuf.position + 1;
    lexbuf.lastReadChars <- lexbuf.lastReadChars ^ String.make 1 chr;
    if chr = '\n' then
      moveToNextLine lexbuf;
    chr
  end else
    raise Eof

let backTrack lexbuf n =
  lexbuf.position <- lexbuf.position - n;
  lexbuf.lastReadChars <- Str.first_chars lexbuf.lastReadChars
    (String.length lexbuf.lastReadChars - n)

let returnMultipleTokens state first remaining =
  state.pushedTokens <- remaining;
  first

let readUntil abortOnChar state =
  let acc = ref "" in
  let rec worker () =
    let nextChar = readChar state in
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

module Stats =
struct
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
end
open Stats

let printStats = Stats.printStats

let token (lexbuf : token lexerstate) : token =
  let putback lexbuf string =
    let len = String.length string in
    let lastChars = Str.last_chars lexbuf.lastReadChars len in
    if lastChars <> string then begin
      printf
        "Assertion failure: expected \"%s\" but found \"%s\"" lastChars string;
      assert false;
    end;
    backTrack lexbuf len
  in

  let rec worker () =
    let currentChar = readChar lexbuf in

    if isNewline currentChar then begin
      lexbuf.previousToken <- `Whitespace;
      collectTimingInfo "newline"
        (fun () ->
           let rec consumeWhitespaceAndReturnIndent indent =
             let eof, nextChar =
               try false, readChar lexbuf
               with Eof -> true, '\n'
             in
             if eof then
               indent
             else if isWhitespace nextChar then
               consumeWhitespaceAndReturnIndent (indent+1)
             else if isNewline nextChar then
               consumeWhitespaceAndReturnIndent 0
             else begin
               backTrack lexbuf 1;
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
      collectTimingInfo "no newline"
        (fun () ->
           let rec findToken() =
             let matchingRules =
               let unsorted =
                 mapFilter
                   (fun rule ->
                      match
                        Rules.ruleMatchesAt
                          ~prevToken:lexbuf.previousToken
                          ~source:lexbuf.content
                          ~pos:lexbuf.position
                          rule
                      with
                        | Some len -> Some (len, rule)
                        | None -> None)
                   Rules.rules
               in
               List.sort (fun (len1, _) (len2, _) -> 1 - compare len1 len2) unsorted
             in
             let input = String.sub lexbuf.content lexbuf.position 1 in
             let ptokenToString = function
               | `Whitespace -> "< >"
               | `Token t -> token2str t
             in
             let length, (rule, makeToken) =
               match matchingRules with
                 | [] ->
                     raiseUnknownToken lexbuf.location input
                       (sprintf "no matching rules (after '%s')" (ptokenToString lexbuf.previousToken))
                 | [single] -> single
                 | first :: second :: _ ->
                     if (fst first > fst second) then
                       first
                     else
                       raiseUnknownToken lexbuf.location input
                         (sprintf "multiple rules matching (after '%s')" (ptokenToString lexbuf.previousToken))
             in
             ignore rule;
             let tokenString = readChars lexbuf length in

             let token : [`Ignore | `Token of token] =
               match (makeToken tokenString : tokenOrAction) with
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
           backTrack lexbuf 1;
           incr stats.foundTokens;
           findToken())
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
          if lastReadChar = ' ' then
            lexbuf.previousToken <- `Whitespace;
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
  lexbuf.previousToken <- `Token token;
  lexbuf.lastReadChars <- Str.last_chars lexbuf.lastReadChars 1;
  token

let makeLexbuf fileName source =
  let buffer = stripComments source in
  let rec lexbuf =
    {
      content = buffer;
      contentLength = String.length buffer;
      position = 0;
      location = { line = 0; fileName = fileName };
      prevIndent = 0;
      pushedTokens = [];
      readTokenBefore = false;
      lastReadChars = "\n";
      previousToken = `Whitespace;
    }
  in
  lexbuf

let lexbufFromString fileName string =
  let sourceWithEOL = string ^ "\n" in
  makeLexbuf fileName sourceWithEOL

let lexbufFromChannel fileName channel =
  let source = Common.readChannel channel in
  lexbufFromString fileName source

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

let runInternalTests() =
  let l = lexbufFromString "d.zomp" "abcde" in
  let expectChar chr = assert( chr = readChar l ) in
  expectChar 'a';
  expectChar 'b';
  backTrack l 2;
  expectChar 'a';
  expectChar 'b'

