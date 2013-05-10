exception Eof
type token = Newparser.token
type 'a lexerstate

(** Found a character sequence that is not a valid token. *)
exception UnknowToken of Basics.location * string * string
val unknownTokenToErrorMsg : Basics.location option * string * string -> string

(** Found invalid indentation *)
exception IndentError of Basics.location * string

(** Read the next token *)
val token : Newparser.token lexerstate -> Newparser.token

val lexbufFromChannel : string -> in_channel -> 'a lexerstate
val lexbufFromString : string -> string -> 'a lexerstate
val lexString : string -> Newparser.token list

val tokensToString : Newparser.token list -> string
val printTokens : Newparser.token list -> unit

(** For testing, only. Returns None if at end of input. *)
val readChar : 'a lexerstate -> char

val runInternalTests : unit -> unit
val printStats : unit -> unit

val locationOfLexstate : 'a lexerstate -> Basics.location

