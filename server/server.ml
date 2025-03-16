open! Core
open! Async

let main ~dir:_ = Deferred.unit

let command =
  Async.Command.async
    ~summary:"Remote execution"
    ~readme:(fun () -> "Does remote execution.")
    (let%map_open.Command dir =
       flag "-dir" (required string) ~doc:"Where to watch files"
     in
     fun () -> main ~dir)
;;

let () = Command_unix.run ~version:"0.0" command
