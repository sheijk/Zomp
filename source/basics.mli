(** Various very simple basic types. **)

type location = {
  fileName : string;
  line : int;
  column : int option; 
}

val location : string -> int -> int option -> location
val fakeLocation : location
val builtinLocation : location

val locationToString : location -> string
val locationOptToString : location option -> string

val locationEqual : location -> location -> bool
val locationValid : location -> bool
val filterValidLocation : location option -> location option

module DiagnosticKind :
sig
  type t = 
      Error
    | Warning
    | Info
    | Other of string

  val toString : t -> string
  val parse : string -> t
end

val formatDiagnostics : DiagnosticKind.t -> location -> string -> string
val formatError : location -> string -> string
val formatWarning : location -> string -> string
val formatInfo : location -> string -> string

val parseDiagnostics :
  string -> (location * DiagnosticKind.t * string) option

(** Parsing error. Moved here to reduce dependency on Newparser module. *)
exception ParseError of location * string

(** Splitting source into comments, source, and strings. *)
type fragmentType = String | Comment | Source
val fragmentTypeToString : fragmentType -> string

val validEscapeChars : char list

exception CommentError of location * string
val raiseCommentError :
  file:string -> line:int -> column:int -> msg:string -> 'a

val parseCommentsAndStrings :
  (fragmentType -> string -> unit) -> string -> string -> unit

(**
   Will return two closures, write and getLines. Use write to add preprocessed
   source code which consists of segments of either comments, string literals,
   or raw source code.
   getLines() will return a list of strings. Each string corresponds to one line
   of code that is valid and escaped HTML. It's source/string/comment segments
   are wrapped inside <span class="source/source-comment/source-string>". All
   span tags are closed at the end of each line.
 *)
val makeHtmlSourceWriter : unit -> (fragmentType -> string -> unit) * (unit -> string list)

