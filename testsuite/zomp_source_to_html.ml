(**
  This program will take a Zomp source file and produce an HTML snippet with
  lexical highlighting. The result will be printed to stdout.
 *)
open Printf

let () =
  let printLineAndDiagnostics line =
    printf "  <li><code>%s</code>" line;
    printf "</li>\n";
  in

  let fileName = Sys.argv.(1) in
  let source = Common.readFile fileName in
  let write, getLines = Basics.makeHtmlSourceWriter() in
  Basics.parseCommentsAndStrings write fileName source;
  let lines = getLines() in
  List.iter printLineAndDiagnostics lines

