exception FailedToEvaluateLLVMCode of string * string
module CommonCombinators :
  sig
    val map2nd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
    val map1st : ('a -> 'b) -> 'a * 'c -> 'b * 'c
    val ( <<= ) : ('a -> 'b) -> 'a -> 'b
    val ( >>= ) : 'a -> ('a -> 'b) -> 'b
    val ( ++ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val ( $ ) : ('a -> 'b) -> 'a -> 'b
    val ( |> ) : 'a -> ('a -> 'b) -> 'b
    val ( =~ ) : string -> string -> bool
    val makeGuardedFunction :
      ('a -> 'b) -> ('b -> 'c) -> 'a -> ('b -> 'd) -> 'd
    type ('a, 'b) staged = Staged of ('a -> 'b)
  end
val map2nd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
val map1st : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val ( <<= ) : ('a -> 'b) -> 'a -> 'b
val ( >>= ) : 'a -> ('a -> 'b) -> 'b
val ( ++ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( $ ) : ('a -> 'b) -> 'a -> 'b
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( =~ ) : string -> string -> bool
val makeGuardedFunction : ('a -> 'b) -> ('b -> 'c) -> 'a -> ('b -> 'd) -> 'd
type ('a, 'b) staged =
  ('a, 'b) CommonCombinators.staged =
    Staged of ('a -> 'b)
module CommonNum :
  sig
    val fromTo : int -> int -> int list
    val safeParseInt : string -> int
    val restrictToSingleprecision : float -> float
  end
val fromTo : int -> int -> int list
val safeParseInt : string -> int
val restrictToSingleprecision : float -> float
module CommonString :
  sig
    val combine : string -> string list -> string
    val commentOut : string -> ?stopDelim:string -> string -> string
    val lineCount : string -> int
    val indent : string -> string
    val removeQuotes : string -> string
    val dequoteString :
      char -> string -> [> `NotQuoted of string | `Quoted of string ]
    val trim : string -> string
    val mapString : (char -> char) -> string -> string
    type sequenceAnchor = FromFront of int | FromBack of int
    val splitAt : string -> sequenceAnchor -> string * string
    val lastChar : string -> char
    val splitLastChar : string -> string * char
    val endsWith : string -> string -> bool
    val splitup : string list -> string -> string list option
    val foldString : string -> ('a -> char -> 'a) -> 'a -> 'a
    val stringToRevCharList : string -> char list
    val stringToCharList : string -> char list
    val restrictLength : int -> string -> string
  end
val combine : string -> string list -> string
val commentOut : string -> ?stopDelim:string -> string -> string
val lineCount : string -> int
val indent : string -> string
val removeQuotes : string -> string
val dequoteString :
  char -> string -> [> `NotQuoted of string | `Quoted of string ]
val trim : string -> string
val mapString : (char -> char) -> string -> string
type sequenceAnchor =
  CommonString.sequenceAnchor =
    FromFront of int
  | FromBack of int
val splitAt : string -> sequenceAnchor -> string * string
val lastChar : string -> char
val splitLastChar : string -> string * char
val endsWith : string -> string -> bool
val splitup : string list -> string -> string list option
val foldString : string -> ('a -> char -> 'a) -> 'a -> 'a
val stringToRevCharList : string -> char list
val stringToCharList : string -> char list
val restrictLength : int -> string -> string
module CommonFile :
  sig
    val readLinesRev : in_channel -> string list
    val readLines : in_channel -> string list
    val readChannel : in_channel -> string
    val withFileForReading : string -> (in_channel -> 'a) -> 'a
    val withFileForWriting : string -> (out_channel -> 'a) -> 'a
    val withFileForAppending : string -> (out_channel -> 'a) -> 'a
    val absolutePath : string -> string
    val canonicalFileName : string -> string
    val findFileIn : string -> string list -> string option
    val readFileLines : string -> string list
    val readFile : ?paths:string list -> string -> string
  end
val readLinesRev : in_channel -> string list
val readLines : in_channel -> string list
val readChannel : in_channel -> string
val withFileForReading : string -> (in_channel -> 'a) -> 'a
val withFileForWriting : string -> (out_channel -> 'a) -> 'a
val withFileForAppending : string -> (out_channel -> 'a) -> 'a
val absolutePath : string -> string
val canonicalFileName : string -> string
val findFileIn : string -> string list -> string option
val readFileLines : string -> string list
val readFile : ?paths:string list -> string -> string
module CommonOption :
  sig
    val applyIfSome : ('a -> 'b option) -> 'a option -> 'b option
    val someOrDefault : 'a option -> 'a -> 'a
    val applyOptOrDefault : 'a option -> ('a -> 'b) -> 'b -> 'b
    val some : 'a -> 'a option
    val id : 'a -> 'a
  end
val applyIfSome : ('a -> 'b option) -> 'a option -> 'b option
val someOrDefault : 'a option -> 'a -> 'a
val applyOptOrDefault : 'a option -> ('a -> 'b) -> 'b -> 'b
val some : 'a -> 'a option
val id : 'a -> 'a
module CommonList :
  sig
    val lastElement : 'a list -> 'a option
    val splitAfter : int -> 'a list -> 'a list * 'a list
    val partitionList : ('a -> 'b) -> 'a list -> 'a list list
    val listIteri : (int -> 'a -> unit) -> 'a list -> unit
    val listMapi : (int -> 'a -> 'b) -> 'a list -> 'b list
    val listMap2i : (int -> 'a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val listFold2i :
      (int -> 'a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
    val listCreate : int -> 'a -> 'a list
    val listCreateF : int -> (int -> 'a) -> 'a list
    val mapFilter : ('a -> 'b option) -> 'a list -> 'b list
    val multiMap : ('a -> 'b list) -> 'a list -> 'b list
    val mapfold : ('a -> 'b -> 'c list * 'a) -> 'a -> 'b list -> 'c list * 'a
    val combineList : 'a -> 'a list -> 'a list
    val listContains : 'a -> 'a list -> bool
    val indexOf : 'a -> 'a list -> int
    val addToList : 'a list ref -> 'a -> [< `Back | `Front ] -> unit
    val tryAll :
      onSuccess:('a -> 'b) -> ifAllFailed:'b Lazy.t -> 'a Lazy.t list -> 'b
    val tryAllCollectingErrors :
      onSuccess:('a -> 'b) ->
      ifAllFailed:(exn list -> 'b) -> 'a Lazy.t list -> 'b
  end
val lastElement : 'a list -> 'a option
val splitAfter : int -> 'a list -> 'a list * 'a list
val partitionList : ('a -> 'b) -> 'a list -> 'a list list
val listIteri : (int -> 'a -> unit) -> 'a list -> unit
val listMapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val listMap2i : (int -> 'a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val listFold2i :
  (int -> 'a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val listCreate : int -> 'a -> 'a list
val listCreateF : int -> (int -> 'a) -> 'a list
val mapFilter : ('a -> 'b option) -> 'a list -> 'b list
val multiMap : ('a -> 'b list) -> 'a list -> 'b list
val mapfold : ('a -> 'b -> 'c list * 'a) -> 'a -> 'b list -> 'c list * 'a
val combineList : 'a -> 'a list -> 'a list
val listContains : 'a -> 'a list -> bool
val indexOf : 'a -> 'a list -> int
val addToList : 'a list ref -> 'a -> [< `Back | `Front ] -> unit
val tryAll :
  onSuccess:('a -> 'b) -> ifAllFailed:'b Lazy.t -> 'a Lazy.t list -> 'b
val tryAllCollectingErrors :
  onSuccess:('a -> 'b) ->
  ifAllFailed:(exn list -> 'b) -> 'a Lazy.t list -> 'b
module Ref :
  sig
    val get : 'a ref -> 'a
    val getter : 'a ref -> unit -> 'a
    val set : 'a ref -> 'a -> unit
  end
module Vector :
  sig
    type 'a t
    val make : unit -> 'a t
    val append : 'a t -> 'a -> int
    val size : 'a t -> int
    val get : 'a t -> int -> 'a
  end
module Statistics :
  sig
    type section
    type counter
    val createSection : string -> section
    val createCounter : section -> string -> int -> (unit -> int) -> counter
    type sectionRegisterFunc = sectionName:string -> unit
    type counterRegisterFunc =
        sectionName:string ->
        name:string -> fractionalDigits:int -> id:int -> unit
    val setImplementation :
      sectionRegisterFunc -> counterRegisterFunc -> unit
  end
module Profiling :
  sig
    type timingInfo = {
      name : string;
      mutable totalTime : float;
      mutable childs : timingInfo list;
    }
    val makeTimingInfo : string -> timingInfo
    val findOrCreate : timingInfo -> string -> timingInfo
    val toplevelTimingInfos : timingInfo
    val toplevelStartTime : float
    val printFlatTimings : unit -> unit
    val totalTime : timingInfo -> float
    val printTimings : unit -> unit
    val timingStack : timingInfo list ref
    val parentTimingInfo : unit -> timingInfo
    val guarded : (unit -> 'a) -> finally:(unit -> unit) -> 'a
    val pushTimingContext : string -> timingInfo * float
    val popTimingContext : timingInfo * float -> unit
    val collectTimingInfo : string -> (unit -> 'a) -> 'a
  end
val collectTimingInfo : string -> (unit -> 'a) -> 'a
val sampleFunc1 : string -> ('a -> 'b) -> 'a -> 'b
val sampleFunc2 : string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val sampleFunc3 : string -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val sampleFunc4 :
  string -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
