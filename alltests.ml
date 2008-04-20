
let run() =
  let module IParserTests = Testing.Tester(Newparsertest.IndentParserTestCase) in
  IParserTests.runTestsAndPrintErrors();
  
(*   let module ExpanderTests = Testing.Tester(Expandertests.Expandertests) in *)
(*   ExpanderTests.runTestsAndPrintErrors() *)
     
