(**
 * Some very basic types which are used by most modules.
 *)

open Printf
open Common

type location = {
  fileName :string;
  line :int;
  column :int option;
}

let location fileName line column = { fileName; line; column }

(** A fake location used in places where location info is not available due to
legacy code. Always use this value so it get's easier to search for places that
need to be fixed. This should go away once all code in the compiler handles
locations properly. *)
let fakeLocation = { fileName = "???.zomp"; line = 1; column = None }

(** The location used for compiler provided types, macros, etc. *)
let builtinLocation = location "builtin" 0 None

let locationToString loc =
  match loc.column with
    | Some column ->
      sprintf "%s:%d:%d" loc.fileName loc.line column
    | None ->
      sprintf "%s:%d" loc.fileName loc.line

let locationOptToString locOpt =
  locationToString (someOrDefault locOpt fakeLocation)

let locationEqual lhs rhs =
  (lhs.line = rhs.line) &&
    ((String.compare lhs.fileName rhs.fileName) = 0)

(** Catch some common kinds of invalid locations. *)
let locationValid = function
  | { fileName = ""; line = 0; column = (Some 0 | None) } ->
    false
  | _ ->
    true

module DiagnosticKind =
struct
  type t =
  | Error
  | Warning
  | Info
  | Other of string

  let toString = function
    | Error -> "error"
    | Warning -> "warning"
    | Info -> "info"
    | Other str -> sprintf "other('%s')" str

  let parse = function
    | "error" -> Error
    | "warning" -> Warning
    | "info" -> Info
    | otherString -> Other otherString
end

let formatDiagnostics kind location message =
  sprintf "%s: %s: %s" (locationToString location) (DiagnosticKind.toString kind) message

let formatError = formatDiagnostics DiagnosticKind.Error
let formatWarning = formatDiagnostics DiagnosticKind.Warning
let formatInfo = formatDiagnostics DiagnosticKind.Info

let diagnosticRe =
  let fileRe = "\\([a-zA-Z_0-9/\\.-]+\\)" in
  let lineRe = "\\([0-9]+\\)" in
  let columnColonRe = "\\([0-9]+:\\)" in
  let re =
    sprintf "^%s:%s:%s? *\\(error\\|warning\\|info\\): \\(.*\\)"
      fileRe lineRe columnColonRe
  in
  Str.regexp re
  
let parseDiagnostics line =
  if (Str.string_match diagnosticRe line 0) then
    let fileName = Str.matched_group 1 line in
    let lineNum = int_of_string (Str.matched_group 2 line) in
    let column =
      try
        let columnStr, _ = splitLastChar (Str.matched_group 3 line) in
        Some (int_of_string columnStr)
      with Not_found ->
        None
    in
    let kind = DiagnosticKind.parse (Str.matched_group 4 line) in
    let message = Str.matched_group 5 line in
    Some (location fileName lineNum column, kind, message)
  else
    None

exception ParseError of location * string

type fragmentType = String | Comment | Source
let fragmentTypeToString = function
  | String -> "string"
  | Comment -> "comment"
  | Source -> "source"

(** Characters that are valid as escape sequences *)
let validEscapeChars = ['\''; '"'; '\\'; '0'; 'n'; 'r'; 't'; 'v'; 'a'; 'b'; 'f'; '?']

exception CommentError of location * string
let raiseCommentError ~file ~line ~column ~msg =
  raise (CommentError (location file line (Some column), msg))

let parseCommentsAndStrings
    (write : fragmentType -> string -> unit)
    fileName source =
  let sourceLength = String.length source in

  let getReadPos, readTwoChars, readOneChar, moveBackReadPos, getLine, getColumn =
    let readPos = ref 0 in
    let line = ref 0 in
    let currentLineStart = ref 0 in

    let getReadPos() = !readPos in
    let getLine() = !line in
    let readOneChar() =
      let chr = source.[!readPos] in
      if chr = '\n' then begin
        line := !line + 1;
        currentLineStart := !readPos;
      end;
      readPos := !readPos + 1;
      chr
    in
    let getColumn() =
      !readPos - !currentLineStart
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
    getReadPos, readTwoChars, readOneChar, moveBackReadPos, getLine, getColumn
  in

  let buffer = String.make (sourceLength) ' ' in
  let writePos = ref 0 in
  let totalWrittenChars = ref 0 in
  let fragmentType = ref Source in
  let startNewFragment nextFragmentType =
    if !writePos > 0 then begin
      write !fragmentType (String.sub buffer 0 !writePos);
    end;
    writePos := 0;
    fragmentType := nextFragmentType;
  in
  let writeChar chr =
    buffer.[!writePos] <- chr;
    incr writePos;
    incr totalWrittenChars;
  in
  let unexpectedEof src =
    let file, line, column = fileName, getLine(), getColumn() in
    let msg = sprintf "unexpected end of file while parsing %s" src in
    raiseCommentError ~file ~line ~column ~msg
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
    if not (List.mem chr validEscapeChars) then begin
      let line, file, column = getLine(), fileName, getColumn() in
      let msg = sprintf "invalid escape sequence (char %c)" chr in
      raiseCommentError ~file ~line ~column ~msg;
    end;
    chr
  in

  let assertReading expectedFragmentType =
    if !fragmentType <> expectedFragmentType then begin
      let file, line, column = fileName, getLine(), getColumn() in
      let msg = sprintf "Expected to be reading %s but was reading %s"
        (fragmentTypeToString expectedFragmentType)
        (fragmentTypeToString !fragmentType)
      in
      raiseCommentError ~file ~line ~column ~msg
    end
  in

  let rec copySource() =
    assertReading Source;
    if getReadPos() <= sourceLength - 1 then begin
      match readOneChar() with
        | '/' ->
          if getReadPos() <= sourceLength - 1 then begin
            match readOneChar() with
              | '/' ->
                startNewFragment Comment;
                writeChar '/'; writeChar '/';
                skipSingleLineComment();
              | '*' ->
                startNewFragment Comment;
                writeChar '/'; writeChar '*';
                skipMultiLineComment()
              | '"' ->
                writeChar '/';
                startNewFragment String;
                writeChar '"';
                copyStringLiteral()
              | chr ->
                writeChar '/'; writeChar chr;
                copySource()
          end
        | '"' ->
          startNewFragment String;
          writeChar '"';
          copyStringLiteral()
        | '\'' ->
          startNewFragment String;
          writeChar '\'';
          copyCharLiteral();
        | chr ->
          writeChar chr;
          copySource()
    end
  and skipSingleLineComment() =
    assertReading Comment;
    if getReadPos() <= sourceLength - 1 then
      let chr = readOneChar() in
      writeChar chr;
      if chr = '\n' then begin
        startNewFragment Source;
        copySource()
      end else
        skipSingleLineComment()
    else
      unexpectedEofInComment()
  and skipMultiLineComment() =
    assertReading Comment;
    if getReadPos() <= sourceLength - 2 then begin
      match readTwoChars() with
        | '*', '/' ->
          writeChar '*';
          writeChar '/';
          startNewFragment Source;
          copySource()
        | chr, '*' ->
          writeChar chr;
          moveBackReadPos();
          skipMultiLineComment()
        | chr1, chr2 ->
          writeChar chr1;
          writeChar chr2;
          skipMultiLineComment()
    end else
      unexpectedEofInComment()
  and copyStringLiteral() =
    assertReading String;
    let copyChar() = copyChar unexpectedEofInStringLiteral in
    let copyEscapeChar() = copyEscapeChar unexpectedEofInStringLiteral in
    match copyChar() with
      | '"' ->
        startNewFragment Source;
        copySource()
      | '\\' ->
        let _ = copyEscapeChar() in
        copyStringLiteral()
      | chr ->
        copyStringLiteral()
  and copyCharLiteral() =
    assertReading String;
    let copyChar() = copyChar unexpectedEofInCharLiteral in
    let copyEscapeChar() = copyEscapeChar unexpectedEofInCharLiteral in
    let invalidCharLiteral msg =
      let file, line, column = fileName, getLine(), getColumn() in
      raiseCommentError ~file ~line ~column ~msg
    in
    match copyChar() with
      | '\\' ->
        begin match copyEscapeChar(), copyChar() with
          | _, '\'' ->
            startNewFragment Source;
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
            startNewFragment Source;
            copySource()
          | c2 ->
            invalidCharLiteral (sprintf "'%c%c is no a valid char literal" c1 c2)
        end
  in
  copySource();
  assert (!totalWrittenChars = sourceLength);
  startNewFragment Source

