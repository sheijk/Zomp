open Printf

let linkIfExists filesAndTitles =
  let rec loop = function
    | (file, title) :: remFiles ->
      if Sys.file_exists file then
        sprintf "<a href=\"../../%s\">%s</a>" file title
      else
        loop remFiles
    | [] ->
      let missingMessage, title =
        match filesAndTitles with
          | [] -> "No files to link to", "no title"
          | [file, title] -> sprintf "File %s does not exist" file, title
          | _ -> sprintf "Files %s do not exist" (String.concat ", " (List.map fst filesAndTitles)), snd (List.hd filesAndTitles)
      in
      sprintf "<span class=\"missing-file\" title=\"%s\">%s</span>" missingMessage title
  in
  loop filesAndTitles

let inHtmlTag tag args f =
  printf "<%s %s>\n" tag args;
  f();
  printf "</%s>\n" tag

let readFileIfExists fileName =
  try begin
    let ch = open_in fileName in
    let lines = ref [] in
    try
      while true do
        lines := input_line ch :: !lines
      done;
      ""
    with End_of_file ->
      String.concat "\n" (List.rev !lines)
  end with Sys_error _ ->
    ""

let groupBy compare pairs =
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) pairs in
  let lastKey, groups, lastGroupElements =
    List.fold_left
      (fun (currentKey, prevGroups, groupElements) (key, element) ->
        if currentKey = key then
          (currentKey, prevGroups, element :: groupElements)
        else
          (key, (currentKey, groupElements) :: prevGroups, [element]))
      (fst (List.hd sorted), [], []) sorted
  in
  List.rev ((lastKey, lastGroupElements) :: groups)

let groupFilesByDir files =
  let dirsAndBasenames = List.map
    (fun fileName ->
      Filename.dirname fileName, Filename.basename fileName)
    files
  in
  groupBy String.compare dirsAndBasenames

let produceReport title files =
  let groupedFiles = groupFilesByDir files in
  let changes = ref [] in
  let testResultRowsByDir =
    List.map
      (fun (dirname, files) ->
        let succeededTests = ref 0 in
        let reportRows = List.map
          (fun fileBaseName ->
            let fileName = Filename.concat dirname fileBaseName in
            let sourceLink = linkIfExists [sprintf "%s.zomp" fileName, fileBaseName] in

            let resultInfo =
              let resultFileContent = readFileIfExists (fileName ^ ".result") in
              if Sys.file_exists (fileName ^ ".result") then begin
                let cssClass =
                  let logChangedTestResult kind =
                    changes := (kind, dirname, fileBaseName) :: !changes;
                  in
                  match resultFileContent with
                    | "failed" ->
                      "failed"
                    | "failed!" ->
                      logChangedTestResult `Broke;
                      "failed changed"
                    | "ok" ->
                      incr succeededTests;
                      "ok"
                    | "ok!" ->
                      incr succeededTests;
                      logChangedTestResult `Fixed;
                      "ok changed"
                    | _ ->
                      "failed"
                in
                sprintf "<th class=\"%s\">%s</th>" cssClass resultFileContent;
              end else begin
                sprintf "<th class=\"failed\">not run</th>"
              end
            in

            let reportLink = linkIfExists [
              fileName ^ ".testreport", "report";
              fileName ^ ".compile_output", "compilation"
            ] in

            [sprintf "<th>&nbsp;&nbsp;&nbsp;&nbsp;%s</th>" sourceLink;
             sprintf "%s" resultInfo;
             sprintf "<th>%s</th>" reportLink;])
          files
        in
        dirname, reportRows, !succeededTests)
      groupedFiles
  in

  let totalTests = List.length files in
  let succeededTests = List.fold_left
    (fun totalSucceeded (_, _, succeeded) -> succeeded + totalSucceeded)
    0
    testResultRowsByDir
  in

  printf "<h1>%s</h1>\n" title;
  if (succeededTests = totalTests) then
    printf "<p class=\"summary\">%d/%d succeeded</p>\n" succeededTests totalTests
  else
    printf "<p class=\"summary\"><span class=\"failed\">%d/%d</span> succeeded</p>\n" succeededTests totalTests;

  if (List.length !changes > 0) then begin
    printf "<div class=\"changed\">\n";
    printf "  Changed tests<br />\n";
    printf "  <ul>\n";
    List.iter
      (fun (kind, dirname, fileBaseName) ->
        printf "    <li class=\"%s\">%s</li>\n"
          (match kind with
            | `Broke -> "failed"
            | `Fixed -> "ok")
          (Filename.concat dirname fileBaseName))
      !changes;
    printf "  </ul>\n";
    printf "</div>\n";
  end;

  let printReportLine reportRows =
    print_string "  <tr>\n";
    List.iter (printf "    %s\n") reportRows;
    print_string "  </tr>\n"
  in
  print_string "<table class=\"test-results\">\n";
  List.iter
    (fun (basedir, testResultRows, succeeded) ->
      let styledBaseDir = sprintf "<span class=\"dirname\">%s</span>" basedir in
      let styledSummary =
        let total = List.length testResultRows in
        sprintf "<span class=\"short-summary\">(%d/%d)</span>" succeeded total
      in
      printf "  <tr><th>%s %s</th><th></th><th></th><th></th></tr>\n"
        styledBaseDir styledSummary;
      List.iter printReportLine testResultRows)
    testResultRowsByDir;
  print_string "</table>\n"
    
let () =
  match Array.to_list Sys.argv with
    | _ :: title :: files ->
      produceReport title files
    | _ ->
      printf "%s: error: expected 'make_report title files*'" Sys.argv.(0);

