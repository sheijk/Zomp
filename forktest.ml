
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
    println "Received signal from guarded process: operation succeeded";
    exit 0;
  in
  println (sprintf "Process %d guarding %d" (Unix.getpid()) childId);
  Sys.set_signal successSignal (Sys.Signal_handle onOk);
  let childid, status = Unix.waitpid [] childId in
  assert( childId = childid );
  println( sprintf "child %d died: %s" childid (processStatusString status) );
  redoJobF()

let mainProcess guardId =
  let pause seconds =
    println (sprintf "Sleeping for %d seconds" seconds);
    Unix.sleep seconds;
  in
  Random.init guardId;
  let success = Random.bool() in
  if success then begin
    pause 2;
    println "Execution succeeded";
    Unix.kill guardId successSignal;
    123
  end else begin
    pause 5;
    println "Execution failed";
    exit 1;
  end

let rec dojobsave () =
  let originalPid = Unix.getpid() in
  println(sprintf "Now forking process %d" originalPid);
  let id = Unix.fork() in
  if id <> 0 then begin
    assert( originalPid = (Unix.getpid()) );
    guardProcess id dojobsave
  end else begin
    mainProcess originalPid
  end

let () =
  println "Starting execution of unsafe job";
  let result = dojobsave() in
  println (sprintf "Execution of unsafe job done, returned %d" result)

    
