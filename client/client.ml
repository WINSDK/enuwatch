open! Core
open! Async
open Enuwatch

(* let f = *)
(*   let leaf1 = create_leaf "hello" in *)
(*   let leaf2 = create_leaf "world" in *)
(*   let root = create_node [| leaf1; leaf2 |] in *)
(*   let hash_incr = tree_hash root in *)
(*   let observer = Inc.observe hash_incr in *)
(*   Observer.on_update_exn observer ~f:(function *)
(*     | Observer.Update.Changed (_, h) -> printf "Root hash updated: %d\n" h *)
(*     | Observer.Update.Initialized h -> printf "Root hash: %d\n" h *)
(*     | _ -> ()); *)
(*   (1* Function to update a leaf's value *1) *)
(*   let update_leaf tree new_val = *)
(*     match tree with *)
(*     | Leaf var -> Var.set var new_val *)
(*     | _ -> () *)
(*   in *)
(*   Inc.stabilize (); *)
(*   update_leaf leaf1 "goodbye"; *)
(*   Inc.stabilize () *)
(* ;; *)

let main ~dir =
  let tree = Var.create (FsTree.create dir) in
  let hash = FsTree.hash (Var.watch tree) in
  Observer.on_update_exn (Incr.observe hash) ~f:(function
    | Observer.Update.Changed (_, h) -> printf "Root hash updated: %d\n" h
    | Observer.Update.Initialized h -> printf "Root hash: %d\n" h
    | _ -> ());
  Incr.stabilize ();
  Deferred.unit
;;

let command =
  Async.Command.async
    ~summary:"Filesystem watcher with timing comparison"
    ~readme:(fun () -> "Watches a directory tree and compares sync vs async performance.")
    (let%map_open.Command dir =
       flag "-dir" (required Filename_unix.arg_type) ~doc:"DIR Directory to watch"
     in
     fun () -> main ~dir)
;;

let () = Command_unix.run ~version:"0.0" command
