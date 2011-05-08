
let () =
  let rec read str =
    try read (str ^ read_line() ^ "\n")
    with End_of_file -> str
  in
  let text = read "" in
  Printf.printf "--------------------\n%s--------------------\n" text
    
