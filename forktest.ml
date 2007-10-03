
open Printf

let successSignal = Sys.sigquit

let println message =
  printf "[%d] %s\n" (Unix.getpid()) message;
  flush stdout
    
let processStatusString = function
  | Unix.WEXITED returnCode -> sprintf "exited with %d" returnCode
  | Unix.WSIGNALED signalNum -> sprintf "signal %d" signalNum
  | Unix.WSTOPPED signalNum -> sprintf "stopped by %d" signalNum

let guardProcess childId redoJobF =
  let onOk _ =
(*     println "Received signal from guarded process: operation succeeded"; *)
    exit 0;
  in
(*   println (sprintf "Process %d guarding %d" (Unix.getpid()) childId); *)
  Sys.set_signal successSignal (Sys.Signal_handle onOk);
  let childid, status = Unix.waitpid [] childId in
  assert( childId = childid );
(*   println( sprintf "child %d died: %s" childid (processStatusString status) ); *)
  None
    
let rec dojobsave unsafeF () =
  let originalPid = Unix.getpid() in
(*   println(sprintf "Now forking process %d" originalPid); *)
  let id = Unix.fork() in
  if id <> 0 then begin
    assert( originalPid = (Unix.getpid()) );
    guardProcess id dojobsave
  end else begin
    let result = unsafeF() in
    Unix.kill originalPid successSignal;
    Some result
  end

let mainProcess num () =
  let pause seconds =
    Unix.sleep seconds;
  in
  Random.self_init();
  let success = Random.bool() in
  if success then begin
    pause 1;
    num * 4
  end else begin
    pause 2;
    exit 1;
  end

let rec readint prompt =
  printf "%s > " prompt; flush stdout;
  let input = read_line() in
  try
    int_of_string input
  with
    | _ -> readint "retry"

let () =
  let rec step () =
    let num = readint "enter number" in
    begin match dojobsave (mainProcess num) () with
      | Some result -> println (sprintf "Execution of unsafe job done, returned %d" result)
      | None -> println "Operation could not be executed"
    end;
    step()
  in
  step()

    
