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

module Incr : Incremental.S
module Var = Incr.Var
module Observer = Incr.Observer

module FsTree : sig
  type meta =
    { name : string
    ; permissions : int
    }
  [@@deriving sexp_of, bin_io, fields ~getters, diff]

  type kind =
    | File
    | Directory
    | Unknown
  [@@deriving sexp_of]

  type t = (kind * meta) String.Map.t
  [@@deriving sexp_of]

  val create : string -> t

  val hash : t Incr.t -> int Incr.t
end
