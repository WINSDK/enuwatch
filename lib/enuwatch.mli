(**
Keep 100 most recently opened files in memory. 
Have some kind of max acceptable file size range where
the max mem usage is (size_range / 2) * 100, i.e. a steep curve.
have a diffable datastructure of the file tree
* Multiple tags: [windows, macos linux,, ..maybe_more?]
* Builds go in /var/enuwatch/848d8d8g787as99779a8d32

diffmap

file {
  name: string,
  hash: u64,
  permissions: ???
}

dir {
  name: string
  children: file list
  sum_of_children: usize
  permissions: ???
}

two kinds of diffs: meta diff (also includes hash) and a content diff.
content diff: requires you have both trees in storage / memory.
            : (path * string Diff.t) list

Merging strat:
* Local delete, overwrites remote. Delete's to non-existing nodes don't do anything

+-client---------server-+
|
|      meta_tree
|      ---------> server performs matching
|                 ... Either create a new entry or update an existing tree's metadata.
|
| diff(meta_tree, unit | closest)
|      <--------- client knows what files / dirs have changed. Computes content diff
|
| diff(local_tree, remote_tree)
|      ---------> server write different file content's. Trigger command execution.
|
| diff(local_tree, updated_local_tree)
| diff(local_meta, updated_local_meta)
|      ---------> server write different file content's. Trigger command execution.
|     
|
+-----------------------+
*)
open! Core
open! Async

module Inc : Incremental.S
module Var = Inc.Var
module Observer = Inc.Observer

module FsTree : sig
  type meta =
    { name : string
    ; permissions : int
    }
  [@@deriving sexp_of, fields ~getters]

  type t =
    | File of (meta * bytes Var.t)
    | Directory of (meta * t array)
  [@@deriving sexp_of]

  val create : string -> t

  val hash : t -> int Inc.t
end
