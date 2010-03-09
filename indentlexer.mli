exception Eof
type location = { line : int; fileName : string; }
val locationToString : location -> string
type token = Newparser.token
type 'a lexerstate
val readChar : 'a lexerstate -> char
exception UnknowToken of location * string * string
exception IndentError of location * string
val token : Newparser.token lexerstate -> Newparser.token

val lexbufFromChannel : string -> in_channel -> 'a lexerstate
val lexbufFromString : string -> string -> 'a lexerstate
val lexString : string -> Newparser.token list

val tokensToString : Newparser.token list -> string
val printTokens : Newparser.token list -> unit

val runInternalTests : unit -> unit
val printStats : unit -> unit

val locationOfLexstate : 'a lexerstate -> location

