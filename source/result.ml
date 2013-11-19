type flag = Success | Fail

type 'a t = {
  flag :flag;
  diagnostics :Serror.t list;
  results :'a list;
}

let make flag ~results ~diagnostics = { flag; results; diagnostics }
let flag result = result.flag
let diagnostics result = result.diagnostics
let results result = result.results
