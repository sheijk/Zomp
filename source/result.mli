type flag = Success | Fail
type 'a t = private {
  flag :flag;
  diagnostics :Serror.t list;
  results :'a list;
}

val make : flag -> results:'a list -> diagnostics:Serror.t list -> 'a t
val fail : Serror.t list -> 'a t
val success : 'a list -> 'a t

val flag : 'a t -> flag
val succeeded : 'a t -> bool
val failed : 'a t -> bool
val results : 'a t -> 'a list
val diagnostics : 'a t -> Serror.t list
val replaceResults : 'a t -> ('a list -> 'b list) -> 'b t

