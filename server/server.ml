open! Core

let main ~dir:_ = ()

let command =
  Command.basic
    ~summary:"Remote execution"
    ~readme:(fun () -> "Does remote execution.")
    (let%map_open.Command dir =
       flag "-dir" (required string) ~doc:"Where to watch files"
     in
     fun () -> main ~dir)
;;

let () = Command_unix.run ~version:"0.0" command

