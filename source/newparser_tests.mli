val parseSExpr : string -> Ast2.sexpr list
val printEachOnLine : ('a -> 'b) -> 'a list -> unit
exception ParsingFailure of exn * string
val ast2SimpleString : Ast2.sexpr -> string
module IndentParserTestCase : Testing.CASE_STRUCT
val runTests : unit -> Testing.Summary.t
