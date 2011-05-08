
open Printf

let source = String.make 10000000 'x'

let countX1 source =
  let count = ref 0 in
  let sourceLength = String.length source in
  for pos = 0 to sourceLength - 1 do
    if source.[pos] = 'x' then incr count;
  done;
  !count

let countX2 source =
  let readF =
    let pos = ref 0 in
    let sourceLength = String.length source in
    fun () ->
      if !pos < sourceLength then begin
        incr pos;
        Some source.[!pos - 1]
      end else
        None
  in
  let rec worker count =
    match readF() with
      | Some chr ->
          worker (if chr = 'x' then count+1 else count)
      | None ->
          count
  in
  worker 0

let () =
  printf "Measuring performance of different lexing approaches\n";
  for i = 0 to 100 do
    let count = countX1 source in
    printf "%d times 'x'\n" count;
  done

