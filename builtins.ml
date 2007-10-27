open Printf
open Ast2

let testRunMacro args =
  let constructMacroFunction() = () in
  let constructCallerFunction args = () in
  let callMacro() =
    Zompvm.zompRunFunctionInt "macroExec"
  in
  let calli1i functionName arg =
    Zompvm.zompResetArgs();
    Zompvm.zompAddIntArg arg;
    Zompvm.zompRunFunctionIntWithArgs functionName
  in
  let extractSExprFromNativeAst astAddress =
    let childCount = calli1i "macroAstChildCount" astAddress in
(*     let astName = Zompvm.zompRunFunctionString "macroAstId" in *)
    { id = sprintf "native%d" childCount; args = args }
  in
  constructMacroFunction();
  constructCallerFunction args;
  let astAddress = callMacro() in
  printf "ast = %d\n" astAddress; flush stdout;
  extractSExprFromNativeAst astAddress

