(**
   Will generate a summary HTML report of all previous test runs.
*)
open Printf

let succeededRe = Str.regexp ".*=succeeded$"

module Stats = struct
  type results = {
    mutable total : int;
    mutable succeeded :int;
  }

  type t = {
    results : results array;
    mutable unknownLines : int;
  }

  let toString categories stats =
    let resultStrs =
      List.map2 (fun name results -> sprintf "%s %d/%d" name results.succeeded results.total)
        categories
        (Array.to_list stats.results)
    in
    Common.combine ", " resultStrs
end

let collectStats categories lines =
  let open Stats in
  let stats = {
    results = Array.init (List.length categories) (fun _ -> { total  = 0; succeeded = 0 });
    unknownLines = 0
  } in
  let rec worker = function
    | [] -> ()
    | line :: remLines ->
      let succeeded = Str.string_match succeededRe line 0 in
      let firstWord =
        try
          let firstSlashPos = String.index line '/' in
          String.sub line 0 firstSlashPos
        with Failure _ ->
          ""
      in
      begin try
        let wordIndex = Common.indexOf firstWord categories in
        stats.results.(wordIndex).total <- stats.results.(wordIndex).total + 1;
        if succeeded then
          stats.results.(wordIndex).succeeded <- stats.results.(wordIndex).succeeded + 1;
      with Failure _ ->
        stats.unknownLines <- stats.unknownLines + 1
      end;
      worker remLines
  in
  worker lines;
  stats

let () =
  let dir = match Sys.argv with
    | [| _; dir |] -> dir
    | _ ->
      printf "error: expected only one argument (directory)";
      exit 1
  in
  let summaryLines = Common.readFileLines (List.fold_left Filename.concat dir ["testsuite"; "summary.txt"]) in
  let categories = ["testsuite"; "libs"; "examples"] in
  let stats = collectStats categories summaryLines in
  printf "%s %s\n" dir (Stats.toString categories stats);
  if stats.Stats.unknownLines > 0 then
    printf "warning: %d unrecognized lines found\n" stats.Stats.unknownLines

