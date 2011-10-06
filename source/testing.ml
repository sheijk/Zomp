(*
 * Provides a simple unit testing framework
 *)

open Printf

module type CASE_STRUCT = sig
  type input
  type output

  val inputToString : input -> string
  val outputToString : output -> string

  val outputEqual : output -> output -> bool

  type result = [ `Return of output | `Exception of string ]

  val testedFunc : input -> output
  val testCases : (input * result) list
end

module Tester(Cases :CASE_STRUCT) = struct
  include Cases

  type error = {
    input :input;
    found :result;
    expected :result;
  }

  let resultToString = function
    | `Return output -> outputToString output
    | `Exception msg -> sprintf "Exception '%s'" msg

  let printResult r = print_string (resultToString r)

  let error ~input ~found ~expected =
    `Error {
      input = input;
      found = found;
      expected = expected
    }

  let errorToString suiteName error =
    Common.combine "" [
      sprintf "\n--- %s UnitTest Failure ---\n" suiteName;
      sprintf "Input: '\n"; inputToString error.input; sprintf "'\n";
      sprintf "Expected: '\n"; resultToString error.expected; sprintf "'\n";
      sprintf "Found: '\n"; resultToString error.found; sprintf "'\n" ]

  let printError suiteName e = print_string (errorToString suiteName e)

  let runTestCase (input, expected) =
    let found =
      try
        `Return (testedFunc input)
      with _ as exc ->
        `Exception (Printexc.to_string exc)
    in
    match expected, found with
      | `Exception _, `Exception _ ->
          `Ok found
      | `Return expectedVal, `Return foundVal ->
          if (outputEqual expectedVal foundVal) then
            `Ok found
          else
            error ~input ~found ~expected
      | _, _ ->
          error ~input ~found ~expected

  (** test case count, error count, error list *)
  let runTests () : int * int * error list =
    let results = List.map runTestCase testCases in
    let errors = Common.mapFilter (function `Error e -> Some e | _ -> None) results in
    let errorCount = List.length errors in
    let testCount = List.length testCases in
    testCount, errorCount, errors

  let printTestSummary name (testCount, errorCount, errors) =
    if errorCount > 0 then
      printf "%d/%d %s tests failed\n" errorCount testCount name
    else
      printf "All %d %s tests succeeded\n" testCount name

  let runTestsAndReport name =
    let testCount, errorCount, errors = runTests() in
    List.iter (printError name) errors;
    at_exit (fun () ->
               printTestSummary name (testCount, errorCount, errors);
               flush stdout)

  exception UnitTestFailure of error list
end

