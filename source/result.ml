type flag = Success | Fail

type 'a t = {
  flag :flag;
  diagnostics :Serror.t list;
  results :'a list;
}

let make flag ~results ~diagnostics = { flag; results; diagnostics }
let fail ~results ~diagnostics = make Fail ~results ~diagnostics
let success ~results ~diagnostics = make Success ~results ~diagnostics

let flag result = result.flag
let succeeded result = result.flag = Success
let failed result = result.flag = Fail
let diagnostics result = result.diagnostics
let results result = result.results
let replaceResults result mapResults = { result with results = mapResults result.results }

