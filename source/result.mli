type flag = Success | Fail
type 'a t = private {
  flag :flag;
  diagnostics :Serror.t list;
  results :'a list;
}

val make : flag -> results:'a list -> diagnostics:Serror.t list -> 'a t

val flag : 'a t -> flag
val results : 'a t -> 'a list
val diagnostics : 'a t -> Serror.t list
