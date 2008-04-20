(*
 * Provides a simple unit testing framework
 *)

open Printf
  
module type CASE_STRUCT = sig
  type input
  type output

  val printInput : input -> unit
  val printOutput : output -> unit

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

  let printResult = function
    | `Return output -> printOutput output
    | `Exception msg -> printf "Exception '%s'" msg

  let error ~input ~found ~expected =
    `Error {
      input = input;
      found = found;
      expected = expected
    }

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

  let runTests () : error list =
    let results = List.map runTestCase testCases in
    let errors = Common.mapFilter (function `Error e -> Some e | _ -> None) results in
    errors

  let runTestsAndPrintErrors() =
    let printError error =
      printf "\n--- UnitTest Failure ---\n";
      printf "Input: '\n"; printInput error.input; printf "'\n";
      printf "Expected: '\n"; printResult error.expected; printf "'\n";
      printf "Found: '\n"; printResult error.found; printf "'\n";
    in
    let errors = runTests() in
    List.iter printError errors;
    let errorCount = List.length errors in
    let testCount = List.length testCases in
    if errorCount > 0 then      
      printf "%d/%d tests failed\n" errorCount testCount;
      
  exception UnitTestFailure of error list
end

  
