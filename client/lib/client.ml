open! Core
open! Async
open Enuwatch

module Gitignore = Gitignore

let rec listen_to_fs msg_box =
  let%bind events = Mvar.take msg_box in
  printf "events:";
  Array.iter
    ~f:(fun event -> print_s (Fswatch.Event.sexp_of_t event))
    events;
  listen_to_fs msg_box
;;

let main ~dir =
  let t = FsTree.of_fs_dir dir in
  print_s (FsTree.sexp_of_t t);
  match Fswatch.init_library () with
  | Fswatch.Status.FSW_OK ->
    let handle, msg_box = Fswatch_async.init_session Fswatch.Monitor.System_default in
    Fswatch.add_path handle dir;
    don't_wait_for (Fswatch_async.start_monitor handle ());
    listen_to_fs msg_box
  | error ->
    print_s [%message "fswatch couldn't be initialized" (error : Fswatch.Status.t)];
    Deferred.unit
;;

let command =
  (* let gitignore = Gitignore.read () |> Option.map ~f:Gitignore.of_string in *)
  (* printf !"%{sexp: Gitignore.Parser.t option}\n" gitignore; *)
  Command.async
    ~summary:"Filesystem watcher with timing comparison"
    ~readme:(fun () -> "Watches a directory tree and compares sync vs async performance.")
    (let%map_open.Command dir =
       flag "-dir" (required Filename_unix.arg_type) ~doc:"DIR Directory to watch"
     in
     fun () -> main ~dir)
;;
