open! Core
open! Incremental.Let_syntax

(*
   Example Workflow
   1: Modify tree
   2: Stablize ->
   3. Calls all observers
   4. Observers will compute some value (i.e. the tree's hash) 
   5. Change in tree structure or in computing a leaf will trigger recalculating parent hashes.

   Similarity:
     - uses hashes
     - computed as: matching_nodes (a, b) / node_count(a)
*)

module Incr = Incremental.Make ()
module Map_incr = Incr_map.Make (Incr)
module Var = Incr.Var
module Observer = Incr.Observer

let ( / ) = Filename.concat

module FsTree = struct
  type kind =
    | File
    | Directory
    | Unknown
  [@@deriving sexp_of, equal]

  type meta =
    { kind : kind
    ; permissions : int
    }
  [@@deriving sexp_of, equal, fields ~getters]

  type 't tkind =
    | File
    | Directory of 't list
    | Unknown
  [@@deriving sexp_of, compare]

  type tree =
    { name : string
    ; permissions : int
    ; kind : tree tkind
    }
  [@@deriving sexp_of, compare]

  type t =
    { base_dir : string
    ; pmap : meta String.Map.t
    ; tree : tree
    }
  [@@deriving sexp_of]

  let stat path =
    match Core_unix.stat path with
    | stats -> stats
    | exception _ ->
      { st_dev = 0
      ; st_ino = 0
      ; st_kind = Core_unix.S_BLK
      ; st_perm = 0
      ; st_nlink = 0
      ; st_uid = 0
      ; st_gid = 0
      ; st_rdev = 0
      ; st_size = Int64.of_int_exn 0
      ; st_atime = 0.0
      ; st_mtime = 0.0
      ; st_ctime = 0.0
      }
  ;;

  let strip_basename basename path =
    let basename = Filename.parts basename in
    let path = Filename.parts path in
    let l1 = List.length basename in
    "." :: List.drop path l1
  ;;

  let add_pmap t (stat : Core_unix.stats) path =
    let path = strip_basename t.base_dir path |> Filename.of_parts in
    let kind : kind =
      match stat.st_kind with
      | Core_unix.S_REG -> File
      | Core_unix.S_DIR -> Directory
      | _ -> Unknown
    in
    let meta = { kind; permissions = stat.st_perm } in
    let pmap = Map.add_exn t.pmap ~key:path ~data:meta in
    { t with pmap }
  ;;

  (*
     file:
      add_tree (dir a (dir b (file x.txt, file y.txt))) a/b/c.txt
      add_tree (dir b (file x.txt, file y.txt)) b/c.txt
      add_tree (file x.txt, file y.txt) c.txt
      (file c.txt, file x.txt, file y.txt)

    dir:
      add_tree (dir a (dir b (file x.txt, file y.txt))) a/b/c
      add_tree (dir b (file x.txt, file y.txt)) b/c
      add_tree (file x.txt, file y.txt) c
      (dir c (), file x.txt, file y.txt)
  *)
  let add_tree t (stats : Core_unix.stats) path =
    let find_e name = fun e -> String.( = ) e.name name in
    let entry name =
      let kind =
        match stats.st_kind with
        | Core_unix.S_REG -> File
        | Core_unix.S_DIR -> Directory []
        | _ -> Unknown
      in
      { name; permissions = stats.st_perm; kind }
    in
    let rec aux t parts =
      match parts with
      | [] -> failwith "missing filename"
      (* We're adding a file or directory here last *)
      | [ name ] ->
        (match t.kind with
         | File | Unknown -> failwith "conflict: cannot add to non-directory"
         | Directory entries ->
           (* Check if entry with this name already exists *)
           if Option.is_some (List.find entries ~f:(find_e name))
           then failwith "conflict: entry with this name already exists"
           else (
             let entry = entry name in
             { t with kind = Directory (entry :: entries) }))
      (* Need to traverse the tree *)
      | part :: next :: rest when String.( = ) part t.name ->
        (match t.kind with
         | File | Unknown -> failwith "conflict: cannot traverse through non-directory"
         | Directory entries ->
           (* Find the next entry in the path *)
           (match List.find entries ~f:(find_e next) with
            | Some entry ->
              (* Found the next entry, continue *)
              let updated_entry = aux entry (next :: rest) in
              let updated_entries =
                List.map entries ~f:(fun e -> if find_e next e then updated_entry else e)
                |> List.sort ~compare:compare_tree
              in
              { t with kind = Directory updated_entries }
            | None ->
              if List.is_empty rest
              then (
                (* There's no parts, add it as a new entry *)
                let entry = entry next in
                { t with kind = Directory (entry :: entries) })
              else failwith "missing entrmediate directory (shouldn't happen)"))
      | part :: _ ->
        raise_s [%message "part doesn't match tree name" (part : string) ~name:t.name]
    in
    let parts = strip_basename t.base_dir path in
    let tree = aux t.tree parts in
    { t with tree }
  ;;

  let add t path =
    let stats = stat path in
    let t = add_pmap t stats path in
    add_tree t stats path
  ;;

  let create base_dir =
    let stats = stat base_dir in
    let tree = { name = "."; permissions = stats.st_perm; kind = Directory [] } in
    let t = { base_dir; pmap = String.Map.empty; tree } in
    add_pmap t stats base_dir
  ;;

  let of_fs_dir base_dir =
    let rec aux t ~parent entry =
      let path = parent / entry in
      let stat = stat path in
      let t = add_pmap t stat path in
      let t = add_tree t stat path in
      match stat.st_kind with
      | Core_unix.S_DIR ->
        let entries = Sys_unix.readdir path in
        Array.sort entries ~compare:String.compare;
        Array.fold entries ~init:t ~f:(aux ~parent:path)
      | _ -> t
    in
    Array.fold
      (Sys_unix.readdir base_dir)
      ~init:(create base_dir)
      ~f:(aux ~parent:base_dir)
  ;;
end
