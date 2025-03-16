open! Core
open! Async
open Enuwatch

module Test = struct
  module Inc = Incremental.Make ()
  module Var = Inc.Var
  module Observer = Inc.Observer

  type tree =
    | Leaf of string Var.t
    | Node of tree array

  let hash s =
    printf "hashing '%s'\n" s;
    String.hash s
  ;;

  let rec tree_hash t =
    match t with
    | Leaf var -> Var.watch var |> Inc.map ~f:hash
    | Node children -> Array.map children ~f:tree_hash |> Inc.sum_int

  let create_leaf initial = Leaf (Var.create initial)
  let create_node l = Node l

  let f =
    let leaf1 = create_leaf "hello" in
    let leaf2 = create_leaf "world" in
    let root = create_node [| leaf1; leaf2 |] in

    let hash_incr = tree_hash root in
    let observer = Inc.observe hash_incr in
    Observer.on_update_exn observer ~f:(function
      | Observer.Update.Changed (_, h) -> printf "Root hash updated: %d\n" h
      | Observer.Update.Initialized h -> printf "Root hash: %d\n" h
      | _ -> ()
    );

    (* Function to update a leaf's value *)
    let update_leaf tree new_val =
      match tree with
      | Leaf var -> Var.set var new_val
      | _ -> ()
    in

    Inc.stabilize ();
    update_leaf leaf1 "goodbye";
    Inc.stabilize ();
end

let main ~dir =
  let _tree = file_tree dir in
  (* print_s (Entry.sexp_of_t tree); *)
  Test.f;
  Deferred.unit
;;

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
