exception Eof
type token = Newparser.token
type 'a lexerstate
val readChar : 'a lexerstate -> char
exception UnknowToken of Basics.location * string * string
val unknownTokenToErrorMsg : Basics.location option * string * string -> string
exception IndentError of Basics.location * string
val token : Newparser.token lexerstate -> Newparser.token

val lexbufFromChannel : string -> in_channel -> 'a lexerstate
val lexbufFromString : string -> string -> 'a lexerstate
val lexString : string -> Newparser.token list

val tokensToString : Newparser.token list -> string
val printTokens : Newparser.token list -> unit

val runInternalTests : unit -> unit
val printStats : unit -> unit

val locationOfLexstate : 'a lexerstate -> Basics.location

