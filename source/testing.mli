(** Unit tester. **)

module Summary :
sig
 type t = {
   name : string; 
   succeeded : bool; 
   summary : string;
 }
end

(** Format a summary message. *)
val testSummary : string -> int -> int -> string

module type CASE_STRUCT =
sig
  type input
  type output
  val inputToString : input -> string
  val outputToString : output -> string
  val outputEqual : output -> output -> bool
  type result = [ `Exception of string | `Return of output ]
  val testedFunc : input -> output
  val testCases : (input * result) list
end

module Tester : functor (Cases : CASE_STRUCT) ->
sig
  type input = Cases.input
  type output = Cases.output
  val inputToString : input -> string
  val outputToString : output -> string
  val outputEqual : output -> output -> bool
  type result = [ `Exception of string | `Return of output ]
  val testedFunc : input -> output
  val testCases : (input * result) list
  type error = { input : input; found : result; expected : result; }
  val resultToString :
    [< `Exception of string | `Return of output ] -> string
  val printResult : [< `Exception of string | `Return of output ] -> unit
  val error :
    input:input ->
    found:result -> expected:result -> [> `Error of error ]
  val errorToString : string -> error -> string
  val printError : string -> error -> unit
  val runTestCase :
    input * result ->
    [> `Error of error
    | `Ok of [> `Exception of string | `Return of output ] ]
  val runTests : unit -> int * int * error list
  val runTestsAndReport : string -> Summary.t
  exception UnitTestFailure of error list
end

