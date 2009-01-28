exception Eof
type location = { line : int; fileName : string; }
type token = Newparser.token
(* type tokenOrAction = *)
(*     [ `Ignore *)
(*     | `PutBack of Newparser.token * string *)
(*     | `Token of Newparser.token ] *)
(* type tokenBuilder = string -> tokenOrAction *)
(* val trimLinefeed : string -> string *)
(* val rules : ((Str.regexp * Str.regexp) * tokenBuilder) list *)
type 'a lexerstate
val readChar : 'a lexerstate -> char
(* type 'a lexerstate = { *)
(*   readChar : unit -> char; *)
(*   backTrack : int -> unit; *)
(*   mutable location : location; *)
(*   mutable prevIndent : int; *)
(*   mutable pushedTokens : 'a list; *)
(*   mutable readTokenBefore : bool; *)
(*   mutable lastReadChars : string; *)
(*   mutable endOfLastToken : char; *)
(* } *)
(* val returnMultipleTokens : 'a lexerstate -> 'b -> 'a list -> 'b *)
(* val isNewline : char -> bool *)
(* val isWhitespace : char -> bool *)
(* val isBlockEndLine : string -> bool *)
exception UnknowToken of location * string * string
(* val raiseUnknownToken : location -> string -> string -> 'a *)
exception IndentError of location * string
(* val raiseIndentError : location -> string -> 'a *)
(* val appendChar : string -> char -> string *)
(* val readUntil : (char -> bool) -> 'a lexerstate -> string *)
val token : Newparser.token lexerstate -> Newparser.token
(* val tokenToString : *)
(*   int * [> `Indent ] -> *)
(*   Newparser.token -> string * (int * [> `DontIndent | `Indent ]) *)
(* type preprocessorState = Source | OneLineComment | MultiLineComment *)
(* val makeLexbuf : string -> (unit -> char) -> 'a lexerstate *)
val lexbufFromChannel : string -> in_channel -> 'a lexerstate
val lexbufFromString : string -> string -> 'a lexerstate
(* val dummymllexbuf : Lexing.lexbuf *)
val lexString : string -> Newparser.token list
val tokensToString : Newparser.token list -> string
val printTokens : Newparser.token list -> unit
val runInternalTests : unit -> unit

val printStats : unit -> unit

