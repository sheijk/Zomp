open Printf

let writingToFile fileName ~process =
  let stream = open_out fileName in
  try
    process stream;
    close_out stream;
  with error ->
    close_out stream;
    raise error

let writeFunc stream num =
  fprintf stream
    "(func void test%d () (\n  (printlnString \"Called function %d\")\n  %s ))\n\n"
    num num
    (if num > 0 then sprintf "(test%d)" (num-1) else "")

    
let () =
  let funcCount = int_of_string Sys.argv.(1) in
  let fileName = Sys.argv.(2) in
  printf "Generating %d functions in %s\n" funcCount fileName;
  writingToFile
    fileName
    ~process:(fun stream ->
                for i = 0 to funcCount do
                  writeFunc stream i
                done;
                fprintf stream
                  "(func int main () (\n  (printlnString \"Calling test functions\")\n  (test%d)\n  (ret 0) ))"
                  (funcCount - 1)
             )

