module IndentLexerTestCase :
  sig
    type input
    type output
    val inputToString : input -> string
    val outputToString : output -> string
    val outputEqual : output -> output -> bool
    type result = [ `Exception of string | `Return of output ]
    val testedFunc : input -> output
    val testCases : (input * result) list
    val validateTestCases : unit -> string option
  end
val runTests : unit -> Testing.Summary.t
val runTerminatorTests : unit -> Testing.Summary.t
