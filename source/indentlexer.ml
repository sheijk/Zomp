(*
 * Indentation based lexer and tests
 *)

(* open Iexpr *)
open Printf
open Common
open Basics

exception Eof

exception UnknowToken of location * string * string
let raiseUnknownToken loc str reason =
  raise (UnknowToken (loc, str, reason))

let unknownTokenToErrorMsg (loc, token, reason) =
  let msgNoLocation = sprintf "unknown token '%s' (%s)" token reason in
  match loc with
    | Some location ->
      sprintf "%s: %s" (locationToString location) msgNoLocation
    | None -> msgNoLocation

exception IndentError of location * string
let raiseIndentError loc str =
  raise (IndentError (loc, str))

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

  let trimLinefeed str =
    let noLineFeeds = mapString (function '\n' -> ' ' | c -> c) str in
    trim noLineFeeds

  let isNewline chr = chr = '\n'

  let whitespaceRE = Str.regexp " +"
  let isWhitespace chr = chr = ' '

  let validIdentifierChars = "a-zA-Z0-9:_"
  let validIdentifierFirstChar = "a-zA-Z_"
  let validIdentifierLastChar = "a-zA-Z0-9_"

  let isValidIdentifierChar chr =
    ( chr >= 'a' && chr <= 'z' ) ||
      ( chr >= 'A' && chr <= 'Z' ) ||
      ( chr >= '0' && chr <= '9' ) ||
      ( chr = '_' ) ||
      ( chr = ':' )

  let isNotValidIdentifierOrWhitespace chr =
    not (isValidIdentifierChar chr || chr == ' ')

  let isBlockEndLine line =
    Str.string_match (Str.regexp "^end.*$") line 0

  let appendChar string char = string ^ String.make 1 char

end

open Util

(** Characters that are valid as escape sequences *)
let validEscapeChars = ['\''; '"'; '\\'; '0'; 'n'; 'r'; 't'; 'v'; 'a'; 'b'; 'f'; '?']

let stripComments fileName source =
  let sourceLength = String.length source in

  let getReadPos, readTwoChars, readOneChar, moveBackReadPos, getLine =
    let readPos = ref 0 in
    let line = ref 0 in

    let getReadPos() = !readPos in
    let getLine() = !line in
    let readOneChar() =
      let chr = source.[!readPos] in
      if chr = '\n' then
        line := !line + 1;
      readPos := !readPos + 1;
      chr
    in
    let readTwoChars() =
      (* weird errors when omitting the let bindings here *)
      let a = readOneChar() in
      let b = readOneChar() in
      a, b
    in
    let moveBackReadPos() =
      readPos := !readPos - 1
    in
    getReadPos, readTwoChars, readOneChar, moveBackReadPos, getLine
  in

  let strippedSource = String.make (sourceLength+1) '\n' in
  let writePos = ref 0 in
  let writeChar chr =
    strippedSource.[!writePos] <- chr;
    incr writePos
  in
  let unexpectedEof src =
    raiseIndentError { line = getLine(); fileName = fileName } (sprintf "unexpected end of file while parsing %s" src)
  in
  let copyChar signalError =
    if getReadPos() <= sourceLength - 1 then begin
      let chr = readOneChar() in
      writeChar chr;
      chr
    end else
      signalError()
  in

  let unexpectedEofInComment() = unexpectedEof "comment"
  and unexpectedEofInStringLiteral() = unexpectedEof "string literal"
  and unexpectedEofInCharLiteral() = unexpectedEof "char literal"
  in

  let copyEscapeChar signalError =
    let chr = copyChar signalError in
    if not (List.mem chr validEscapeChars) then
      raiseIndentError { line = getLine() + 1; fileName = fileName }
        (sprintf "invalid escape sequence (char %c)" chr);
    chr
  in

  let rec copySource() =
    if getReadPos() <= sourceLength - 1 then begin
      match readOneChar() with
        | '/' ->
            if getReadPos() <= sourceLength - 1 then begin
              match readOneChar() with
                | '/' ->
                    writeChar ' '; writeChar ' ';
                    skipSingleLineComment();
                | '*' ->
                    writeChar ' '; writeChar ' ';
                    skipMultiLineComment()
                | '"' ->
                    writeChar '/'; writeChar '"';
                    copyStringLiteral()
                | chr ->
                    writeChar '/'; writeChar chr;
                    copySource()
            end
        | '"' ->
            writeChar '"';
            copyStringLiteral()
        | '\'' ->
            writeChar '\'';
            copyCharLiteral();
        | chr ->
            writeChar chr;
            copySource()
    end
  and skipSingleLineComment() =
    if getReadPos() <= sourceLength - 1 then
      let chr = readOneChar() in
      if chr = '\n' then begin
        writeChar '\n';
        copySource()
      end else
        skipSingleLineComment()
    else
      unexpectedEofInComment()
  and skipMultiLineComment() =
    if getReadPos() <= sourceLength - 2 then begin
      match readTwoChars() with
        | '*', '/' -> copySource()
        | chr, '*' ->
            if chr = '\n' then
              writeChar '\n';
            moveBackReadPos();
            skipMultiLineComment()
        | chr1, chr2 ->
            if chr1 = '\n' then
              writeChar '\n';
            if chr2 = '\n' then
              writeChar '\n';
            skipMultiLineComment()
    end else
      unexpectedEofInComment()
  and copyStringLiteral() =
    let copyChar() = copyChar unexpectedEofInStringLiteral in
    let copyEscapeChar() = copyEscapeChar unexpectedEofInStringLiteral in
    match copyChar() with
      | '"' ->
          copySource()
      | '\\' ->
          let _ = copyEscapeChar() in
          copyStringLiteral()
      | chr ->
          copyStringLiteral()
  and copyCharLiteral() =
    let copyChar() = copyChar unexpectedEofInCharLiteral in
    let copyEscapeChar() = copyEscapeChar unexpectedEofInCharLiteral in
    let invalidCharLiteral msg =
      raiseIndentError
        { line = getLine() + 1; fileName = fileName }
        msg
    in
    match copyChar() with
      | '\\' ->
          begin match copyEscapeChar(), copyChar() with
            | _, '\'' ->
                copySource()
            | c1, c2 ->
                invalidCharLiteral (sprintf "'\\%c%c is no a valid char literal" c1 c2)
          end
      | '\'' ->
          invalidCharLiteral
            "'' is not a valid char literal, did you mean '\\''?"
      | c1 ->
          begin match copyChar() with
            | '\'' ->
                copySource()
            | c2 ->
                invalidCharLiteral (sprintf "'%c%c is no a valid char literal" c1 c2)
          end
  in
  copySource();
  String.sub strippedSource 0 (!writePos + 1)

let beginIndentBlockChar = '\r'

let markIndentBlocks source =
  let sourceLength = String.length source in
  let pos = ref 0 in
  let indentBlockRE = Str.regexp ":[ \t]*\n" in
  begin try
    while !pos < sourceLength do
      pos := Str.search_forward indentBlockRE source !pos;
      source.[!pos] <- ' ';
      while not (isNewline source.[!pos]) do
        incr pos
      done;
      source.[!pos] <- beginIndentBlockChar;
    done
  with Not_found -> () end

let tokenToString (lineIndent, indentNext) (token :token) =
  let indentString indent =
    if indentNext = `Indent then
      if indent >= 0 then
        sprintf "%s" (String.make (4 * indent) ' ')
      else
        sprintf "(illegal indent %d)" indent
    else
      ""
  in
  let withArg str arg =
    if String.length arg = 0 then
      str
    else
      str ^ "(" ^ arg ^ ")"
  in
  match token with
    | END_BLOCK args ->
      indentString (lineIndent - 1) ^
        begin
          match args with
            | [] -> "END_BLOCK"
            | _ -> sprintf "END_BLOCK(%s)"
              (Common.combine ", " (List.map (sprintf "'%s'") args))
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
          | EOF -> "EOF\n", noind
          | IDENTIFIER str -> "IDENTIFIER(" ^ str ^ ")", noind
          | COMMA -> "COMMA", noind
          | ADD_OP arg -> withArg "ADD_OP" arg, noind
          | MULT_OP arg -> withArg "MULT_OP" arg, noind
          | MOD_OP arg -> withArg "MOD_OP" arg, noind
          | ASSIGN_OP arg -> withArg "ASSIGN_OP" arg, noind
          | COMPARE_OP arg -> withArg "COMPARE_OP" arg, noind
          | LAZY_BOOL_OP arg -> withArg "LAZY_BOOL_OP" arg, noind
          | STRICT_BOOL_OP arg -> withArg "STRICT_BOOL_OP" arg, noind
          | EXCLAMATION_OP arg -> withArg "EXCLAMATION_OP" arg, noind
          | POSTFIX_OP arg -> withArg "POSTFIX_OP" arg, noind
          | PREFIX_OP arg -> withArg "PREFIX_OP" arg, noind
          | OPEN_PAREN -> "OPEN_PAREN", noind
          | OPEN_ARGLIST -> "OPEN_ARGLIST", noind
          | CLOSE_PAREN -> "CLOSE_PAREN", noind
          | OPEN_CURLY -> "OPEN_CURLY", noind
          | CLOSE_CURLY -> "CLOSE_CURLY", noind
          | DOT -> "DOT", noind
          | QUOTE str -> withArg "QUOTE" str, noind
          | OPEN_BRACKET -> "OPEN_BRACKET", noind
          | OPEN_BRACKET_POSTFIX -> "OPEN_BRACKET_POSTFIX", noind
          | CLOSE_BRACKET -> "CLOSE_BRACKET", noind
          | SEMICOLON arg -> withArg "SEMICOLON" arg, noind
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

  val ruleMatchesAt : prevToken:[`Whitespace | `Token of token]
    -> source:string
    -> pos:int
    -> rule * tokenBuilder
    -> int option

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

  let opSymbols = "-+\\*/&.><=!|:;,%^{}~"

  let rec charreMatch cre token =
    let isOperator = function
      | ADD_OP _ | MULT_OP _ | ASSIGN_OP _ | COMPARE_OP _ | LAZY_BOOL_OP _
      | STRICT_BOOL_OP _ | MOD_OP _ | EXCLAMATION_OP _
      | PREFIX_OP _ | POSTFIX_OP _ | QUOTE _
      | COMMA | SEMICOLON _
          -> true
      | IDENTIFIER _ | END | EOF | BEGIN_BLOCK | END_BLOCK _
      | OPEN_PAREN | CLOSE_PAREN | OPEN_ARGLIST | OPEN_CURLY | CLOSE_CURLY
      | OPEN_BRACKET | OPEN_BRACKET_POSTFIX | CLOSE_BRACKET
      | DOT
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
    let identifierRE =
      sprintf "\\([%s][%s]*[%s]" validIdentifierFirstChar validIdentifierChars validIdentifierLastChar
      ^ sprintf "\\|[%s]\\)" validIdentifierFirstChar
    in
    let identifierRule =
      re identifierRE, (fun str -> `Token (IDENTIFIER str))
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
    let stringRule = re "\"\\([^\"\\]\\|\\\\.\\)*\"", idFunc in
    let charRule = re "'[^']'", idFunc in
    let escapedCharRule = re "'\\\\.'", idFunc in
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
      let numberRE = "\\(0\\|[1-9][0-9_]*\\)" in
      let negIntRule =
        (Not (Or (Identifier, ClosingParen)), Str.regexp ("-" ^ numberRE)), idFunc
      in
      let hexnumberRE = "0x[0-9a-fA-F]+" in
      let binnumberRE = "0b[01]+" in
      [re numberRE, idFunc;
       negIntRule;
       re hexnumberRE, idFunc;
       re binnumberRE, idFunc;
      ])
    @ (
      let postfixOps = ["++"; "--"; "..."; "*"; "&"; "?"; "!"; "+"] in
      let prefixOps = ["*"; "&"; "++"; "--"; "!"; "?"; "-"; "+"; "~"] in
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
             failwith (sprintf "internal error at token \"%s\"" s)
           else
             match splitup postfixOps s with
               | Some tokens -> `MultiTokens (List.map (fun n -> POSTFIX_OP n) tokens)
               | None ->
                   failwith (sprintf "internal error in parser at token \"%s\"" s))
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
      escapedCharRule;
      contextInsensitiveOp ";" (fun s -> SEMICOLON s);
    ]
    @ opRules "+" (fun s -> ADD_OP s)
    @ opRules "+." (fun s -> ADD_OP s)
    @ opRules "-" (fun s -> ADD_OP s)
    @ opRules "*" (fun s -> MULT_OP s)
    @ opRules "/" (fun s -> MULT_OP s)
    @ opRules "%" (fun s -> MOD_OP s)
    @ opRules "**" (fun s -> MULT_OP s)
    @ opRules "++" (fun s -> ADD_OP s)
    @ opRules "!" (fun s -> EXCLAMATION_OP s)
    @ opRulesMultiSym
      ["="; ":="; "+="; "-="; "*="; "/="; "&="; "|="; "%="; "++="; "**="; "+.=";
       "&&="; "||="; "^="; "<<="; ">>="]
      (fun s -> ASSIGN_OP s)
    @ opRulesMultiSym ["=="; "!="; ">"; ">="; "<"; "<=";] (fun s -> COMPARE_OP s)
    @ opRulesMultiSym ["&&"; "||"] (fun s -> LAZY_BOOL_OP s)
    @ opRulesMultiSym ["&"; "|"; "^"; "<<"; ">>"] (fun s -> STRICT_BOOL_OP s)
      (** Attention: all characters used as operators need to be listed in the regexp in opfuncRule *)

  let ruleMatchesAt ~prevToken ~source ~pos ((prevCharRE, regexp), _) =
    if (charreMatch prevCharRE prevToken && Str.string_match regexp source pos) then
      Some (Str.match_end() - Str.match_beginning())
    else
      None
end

type source =
  | Unprocessed of string
  | NoCommentsAndIndent of string * int

type 'token tokenStreamState =
(** Pushed [] is the normal state. No seperate tag to avoid having an invalid state *)
| Pushed of 'token list
| EndOfInput

type 'token lexerstate = {
  mutable content : source;
  mutable position : int;
  mutable location : location;
  mutable prevIndent : int;
  mutable state : 'token tokenStreamState;
  mutable readTokenBefore : bool;
  mutable lastReadChars :string;
  mutable previousToken :[`Whitespace | `Token of token];
}

let locationOfLexstate ls = ls.location

let moveByLines ls lines =
  ls.location <- { ls.location with line = ls.location.line + lines }

let sourceAndSize lexbuf =
  match lexbuf.content with
    | NoCommentsAndIndent (source, length) -> source, length
    | Unprocessed unprocessed ->
      let buffer = stripComments lexbuf.location.fileName unprocessed in
      markIndentBlocks buffer;
      let source, length = buffer, String.length buffer in
      lexbuf.content <- NoCommentsAndIndent (source, length);
      source, length

let readChars lexbuf (source, sourceLength) n =
  if lexbuf.position + n < sourceLength then begin
    let str = String.sub source lexbuf.position n in
    lexbuf.position <- lexbuf.position + n;
    lexbuf.lastReadChars <- lexbuf.lastReadChars ^ str;
    String.iter
      (fun chr ->
        if chr = '\n' || chr = beginIndentBlockChar then
          moveByLines lexbuf 1)
      str;
    str
  end else
    raise Eof

let readOneChar lexbuf (source, sourceLength) =
  if lexbuf.position < sourceLength then begin
    let chr = source.[lexbuf.position] in
    lexbuf.position <- lexbuf.position + 1;
    lexbuf.lastReadChars <- lexbuf.lastReadChars ^ String.make 1 chr;
    if chr = '\n' || chr = beginIndentBlockChar then
      moveByLines lexbuf 1;
    chr
  end else
    raise Eof

let readChar lexbuf =
  let source, sourceLength = sourceAndSize lexbuf in
  readOneChar lexbuf (source, sourceLength)

let backTrack lexbuf n =
  lexbuf.position <- lexbuf.position - n;
  lexbuf.lastReadChars <- Str.first_chars lexbuf.lastReadChars
    (String.length lexbuf.lastReadChars - n)

let returnMultipleTokens state first remaining =
  assert (state.state = Pushed []);
  state.state <- Pushed remaining;
  first

let putback lexbuf string =
  let uncountNewline c =
    if c = '\n' || c = beginIndentBlockChar then
      moveByLines lexbuf (-1)
  in
  String.iter uncountNewline string;

  let len = String.length string in
  let lastChars = Str.last_chars lexbuf.lastReadChars len in
  if lastChars <> string then begin
    printf
      "assertion failure: expected \"%s\" but found \"%s\"" lastChars string;
    assert false;
  end;
  backTrack lexbuf len
    
let readUntil abortOnChar state srcAndLength =
  let acc = ref "" in
  let rec worker () =
    let nextChar = readOneChar state srcAndLength in
    if abortOnChar nextChar then begin
      putback state (String.make 1 nextChar)
    end else begin
      acc := appendChar !acc nextChar;
      worker()
    end
  in
  begin try
          worker()
    with Eof -> ()
  end;
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
  let source, sourceLength = sourceAndSize lexbuf in

  let rec consumeWhitespaceAndReturnIndent indent =
    let eof, nextChar =
      try false, readOneChar lexbuf (source, sourceLength)
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

  let findToken() =
    let matchingRules =
      let unsorted =
        mapFilter
          (fun rule ->
             match
               Rules.ruleMatchesAt
                 ~prevToken:lexbuf.previousToken
                 ~source
                 ~pos:lexbuf.position
                 rule
             with
               | Some len -> Some (len, rule)
               | None -> None)
          Rules.rules
      in
      List.sort (fun (len1, _) (len2, _) -> 1 - compare len1 len2) unsorted
    in
    let input = String.sub source lexbuf.position 1 in
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
    let tokenString = readChars lexbuf (source, sourceLength) length in

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

  let handleEndLineOrPutTokens firstToken secondToken =
    let line = readUntil isNotValidIdentifierOrWhitespace lexbuf (source, sourceLength) in
    if isBlockEndLine line then begin
      if line =~ "^end\\(.*\\)$" then
        match List.rev (Str.split whitespaceRE (nthmatch 1)) with
          | "}" :: args ->
              Some (END_BLOCK (List.rev args), [CLOSE_CURLY])
          | args ->
              Some (END_BLOCK (List.rev args), [])
      else
        failwith "splitting block end line failed"
    end else begin
      putback lexbuf line;
      None
    end
  in

  let interpreteNewline currentChar continueLexing =
    lexbuf.previousToken <- `Whitespace;
    let indent = consumeWhitespaceAndReturnIndent 0 in
    let prevIndent = lexbuf.prevIndent in

    if lexbuf.readTokenBefore = false then begin
      if indent = prevIndent then continueLexing ()
      else raiseIndentError lexbuf.location "first line needs to be indented at column 0"
    end else begin
      let onSameIndent() =
        `Token END
      and onMoreIndent() =
        lexbuf.prevIndent <- indent;
        if indent = prevIndent then
          match handleEndLineOrPutTokens BEGIN_BLOCK (END_BLOCK []) with
            | Some (hd, rem) -> returnMultipleTokens lexbuf (`Token BEGIN_BLOCK) (hd::rem)
            | None -> returnMultipleTokens lexbuf (`Token BEGIN_BLOCK) [END_BLOCK[]]
        else
          `Token BEGIN_BLOCK
      and continueLine() =
        `Ignore
      and onLessIndent() =
        lexbuf.prevIndent <- indent;
        match handleEndLineOrPutTokens END (END_BLOCK []) with
          | Some (hd, remTokens) -> returnMultipleTokens lexbuf (`Token END) (hd :: remTokens)
          | None -> returnMultipleTokens lexbuf (`Token END) [END_BLOCK[]]
      in
      (** lookup forward for `Ignore and possibly reset indent? *)
      if currentChar = beginIndentBlockChar then
        onMoreIndent()
      else if indent = prevIndent then begin
        onSameIndent()
      (* end else if indent = prevIndent + 4 then begin *)
      end else if indent > prevIndent then begin
        continueLine()
        (* raiseIndentError lexbuf.location *)
        (*   (sprintf "Indentation was increased by %d spaces but only 2 are legal" *)
        (*      (indent - prevIndent)) *)
      end else if indent = prevIndent - 2 then begin
        onLessIndent()
      (** indent is more than one level less than in previous line *)
      end else begin
        raiseIndentError lexbuf.location
          (sprintf
             "indentation was reduced by %d spaces but only steps of 2 are legal"
             (prevIndent - indent))
      end
    end
  in

  let rec worker () =
    let currentChar = readOneChar lexbuf (source, sourceLength) in

    if currentChar = beginIndentBlockChar or isNewline currentChar then begin
      collectTimingInfo "newline"
        (fun () ->
           interpreteNewline currentChar worker)
    (** no newline *)
    end else begin
      collectTimingInfo "no newline"
        (fun () ->
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
  try
    let token =
      match lexbuf.state with
        | Pushed [] -> getNextToken()
        | Pushed (firstToken :: remainingTokens) ->
          lexbuf.state <- Pushed remainingTokens;
          firstToken
        | EndOfInput ->
          raise Eof
    in

    lexbuf.readTokenBefore <- true;
    lexbuf.previousToken <- `Token token;
    lexbuf.lastReadChars <- Str.last_chars lexbuf.lastReadChars 1;
    token
  with Eof as exn ->
    if lexbuf.state = EndOfInput then
      raise exn
    else begin
      lexbuf.state <- EndOfInput;
      EOF
    end

let makeLexbuf fileName source =
  let rec lexbuf =
    {
      content = Unprocessed source;
      position = 0;
      location = { line = 1; fileName = fileName };
      prevIndent = 0;
      state = Pushed [];
      readTokenBefore = false;
      lastReadChars = "\n";
      previousToken = `Whitespace;
    }
  in
  lexbuf

let lexbufFromString ~fileName string =
  let sourceWithEOL = string ^ "\n" in
  makeLexbuf fileName sourceWithEOL

let lexbufFromChannel ~fileName channel =
  let source = Common.readChannel channel in
  lexbufFromString ~fileName source

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

let lexString ~fileName str =
  let lexbuf = lexbufFromString ~fileName str in
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
  let l = lexbufFromString ~fileName:"indentlexer.ml-runInternalTests" "abcde" in
  let source, sourceLength = sourceAndSize l in
  let expectChar chr = assert( chr = readOneChar l (source, sourceLength) ) in
  expectChar 'a';
  expectChar 'b';
  backTrack l 2;
  expectChar 'a';
  expectChar 'b'

