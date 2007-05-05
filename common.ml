
let rec translatelst translateF bindings = function
  | [] -> bindings, []
  | expr :: tail ->
      let newBindings, sf = translateF bindings expr in
      let resultingBindings, sfuncs = translatelst translateF newBindings tail in
      resultingBindings, (sf @ sfuncs)
        
let rec translate errorF translators bindings expr =
  let rec t = function
    | [] -> errorF expr "Expression can not be translated"
    | f :: remf -> begin
        match f (translate errorF translators) bindings expr with
          | Some (newBindings, result) -> (newBindings, result)
          | None -> t remf
      end
  in
  t translators

