(** Interface for parser. **)

type parsingResult = Exprs of Ast2.sexpr list | Error of Serror.t
val parseIExprs : fileName:string -> string -> parsingResult

val fixFileName : string -> Ast2.sexpr -> Ast2.sexpr

(* deprecated *)
val parseIExprsOpt : fileName:string -> string -> Ast2.t list option
val parseIExpr : fileName:string -> string -> Ast2.t option

