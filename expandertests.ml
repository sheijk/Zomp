
(* open Common *)
(* open Printf *)
  
(* module Expandertests : Testing.CASE_STRUCT = *)
(* struct *)
(*   type requirement = [ *)
(*     | `Expressions of Lang.toplevelExpr list *)
(*   ] *)

(*   let reqToString = function *)
(*     | `Expressions exprs -> begin *)
(*         "Expressions:\n" ^ Common.combine "\n  " (List.map Lang.toplevelFormToString exprs) *)
(*       end *)
      
(*   type input = string *)
(*   type output = requirement list *)
(*   type result = [ `Return of output | `Exception of string ] *)
(*   let printInput = print_string *)
(*   let printOutput = List.iter (printf "%s" ++ reqToString) *)

(*   let testedFunc (source :input) : output = [] *)


(*   let testCases : (input * result) list = [ *)
(*     "'x'", `Return [] *)
(*   ] *)
(* end *)


