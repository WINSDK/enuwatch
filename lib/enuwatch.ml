open! Core

(*
   Example Workflow
   1: Modify tree
   2: Stablize ->
   3. Calls all observers
   4. Observers will compute some value (i.e. the tree's hash) 
   5. Change in tree structure or in computing a leaf will trigger recalculating parent hashes.
*)

module Inc = Incremental.Make ()
module Var = Inc.Var
module Observer = Inc.Observer

module FsTree = struct
  type meta =
    { name : string
    ; permissions : int
    }
  [@@deriving sexp_of, bin_io, fields ~getters]

  type t =
    | File of (meta * bytes Var.t)
    | Directory of (meta * t array)

  let rec sexp_of_t = function
    | File (meta, _) -> [%sexp_of: meta] meta
    | Directory (meta, children) -> [%sexp_of: meta * t array] (meta, children)
  ;;

  let create path =
    let ( / ) = Filename.concat in
    let parse ~f ~parent entry =
      let path = parent / entry in
      let fd = Core_unix.openfile ~mode:[Core_unix.O_RDONLY] path in
      let stat = Core_unix.fstat fd in
      let meta = { name = entry; permissions = stat.st_perm } in
      match stat.st_kind with
      | Core_unix.S_REG ->
        let buf = Bytes.create (Int.of_int64_trunc stat.st_size) in
        Core_unix.read fd ~buf |> ignore;
        Core_unix.close fd;
        File (meta, Var.create buf)
      | Core_unix.S_DIR ->
        Core_unix.close fd;
        let children = f path in
        Directory (meta, children)
      (* Unsupported inode kinds are stored as empty file's *)
      | _ -> File (meta, Var.create (Bytes.create 0))
    in
    let rec build_tree path =
      let entries = Sys_unix.ls_dir path |> List.to_array in
      Array.sort entries ~compare:String.compare;
      Array.map entries ~f:(parse ~f:build_tree ~parent:path)
    in
    let path = Filename_unix.realpath path in
    let children = build_tree path in
    let parent, entry = Filename.split path in
    parse ~f:(Fn.const children) ~parent entry
  ;;

  let rec hash = function
    | File (_, data) -> Var.watch data |> Inc.map ~f:(fun b ->
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b |> String.hash)
    | Directory (_, children) -> Array.map children ~f:hash |> Inc.sum_int
  ;;
end

let%expect_test "file_tree creates correct tree structure" =
  let base_dir = Core_unix.mkdtemp "exp_test" in
  let ( / ) = Filename.concat in
  Core_unix.mkdir (base_dir / "dir1") ~perm:0o755;
  Core_unix.mkdir (base_dir / "dir2") ~perm:0o755;
  Core_unix.mkdir (base_dir / "dir1" / "subdir") ~perm:0o755;
  let write_file path content =
    let path = base_dir / path in
    let oc = Out_channel.create path in
    Out_channel.output_string oc content;
    Out_channel.close oc
  in
  write_file "file1.txt" "Hello, world!";
  write_file ("dir1" / "file2.txt") "Content of file2";
  write_file ("dir1" / "subdir" / "file3.txt") "Nested file";
  write_file ("dir2" / "file4.txt") "Another file";
  (* Change the mkdtemp name to something consistent *)
  let meta_map m = { m with FsTree.name = "exp_test" } in
  let tree =
    match FsTree.create base_dir with
    | FsTree.Directory (meta, t) -> FsTree.Directory (meta_map meta, t)
    | FsTree.File (meta, t) -> FsTree.File (meta_map meta, t)
  in
  print_s ([%sexp_of: FsTree.t] tree);
  [%expect
    {|
    (((name exp_test) (permissions 448))
     ((((name dir1) (permissions 493))
       (((name file2.txt) (permissions 420))
        (((name subdir) (permissions 493))
         (((name file3.txt) (permissions 420))))))
      (((name dir2) (permissions 493)) (((name file4.txt) (permissions 420))))
      ((name file1.txt) (permissions 420))))
    |}];
  let dir_count = ref 0 in
  let file_count = ref 0 in
  let rec count_entries entry =
    match entry with
    | FsTree.File _ -> incr file_count
    | FsTree.Directory (_, children) ->
      incr dir_count;
      Array.iter children ~f:count_entries
  in
  count_entries tree;
  printf "Directory count: %d\n" !dir_count;
  [%expect {| Directory count: 4 |}];
  printf "File count: %d\n" !file_count;
  [%expect {| File count: 4 |}];
  Sys_unix.command (sprintf "rm -rf %s" base_dir) |> ignore;
;;
