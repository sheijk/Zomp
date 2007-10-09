

let rec combine seperator = function
    [] -> ""
  | [str] -> str
  | hd :: tl -> hd ^ seperator ^ (combine seperator tl)

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

let commentOut startDelim ?(stopDelim = "") multiLineSource =
  let rec combine seperator = function
      [] -> ""
    | [str] -> str
    | hd :: tl -> hd ^ seperator ^ (combine seperator tl)
  in
  let newlineRE = Str.regexp "\n" in
  let lines = Str.split newlineRE multiLineSource in
  let commentedLines = List.map (fun line -> startDelim ^ line ^ stopDelim) lines in
  combine "\n" commentedLines
  

let readFile filename =
  let file = open_in filename in
  let rec read() =
	try
	  let line = input_line file in
	  line ^ "\n" ^ read()
	with
		End_of_file -> ""
  in
  let content = read() in
  close_in file;
  content

    
let some x = Some x
let id x = x
  
let rec tryAll ~onSuccess ~ifAllFailed = function
  | [] -> Lazy.force ifAllFailed
  | f :: rem ->
      try
        onSuccess (Lazy.force f)
      with _ ->
        tryAll ~ifAllFailed ~onSuccess rem

let tryAllCollectingErrors ~onSuccess ~ifAllFailed funcs =
  let rec worker exceptions = function
    | [] -> ifAllFailed exceptions
    | f :: rem ->
        try
          onSuccess (Lazy.force f)
        with e ->
          worker (e :: exceptions) rem
  in
  worker [] funcs
          
