open! Core

(*
   Example Workflow
   1: Modify tree
   2: Stablize ->
   3. Calls all observers
   4. Observers will compute some value (i.e. the tree's hash) 
   5. Change in tree structure or in computing a leaf will trigger recalculating parent hashes.
*)

module Incr = Incremental.Make ()
module Var = Incr.Var
module Observer = Incr.Observer

module FsTree = struct
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

  type t = (kind * meta) String.Map.t [@@deriving sexp_of]

  let create path =
    let ( / ) = Filename.concat in
    let rec parse ~parent acc entry =
      let path = parent / entry in
      let stat = Core_unix.stat path in
      let meta = { name = entry; permissions = stat.st_perm } in
      match stat.st_kind with
      | Core_unix.S_REG -> Map.add_exn acc ~key:path ~data:(File, meta)
      | Core_unix.S_DIR ->
        let acc = Map.add_exn acc ~key:path ~data:(Directory, meta) in
        let entries = Sys_unix.readdir path in
        Array.sort entries ~compare:String.compare;
        Array.fold entries ~init:acc ~f:(parse ~parent:path)
      (* Unsupported inode kinds are stored as empty file's *)
      | _ -> Map.add_exn acc ~key:path ~data:(Unknown, meta)
    in
    let parent, entry = Filename.split path in
    parse ~parent String.Map.empty entry
  ;;

  let hash t =
    let hash ~key:path ~data:(kind, meta) h_old =
      printf "hashing %s\n" path;
      let h_new =
        match kind with
        | File ->
          let data = In_channel.read_all path in
          Int.( lxor ) (String.hash data) (Int.hash meta.permissions)
        | Directory -> Int.hash meta.permissions
        | Unknown -> 0
      in
      Int.( lxor ) h_old h_new
    in
    Incr_map.unordered_fold t ~init:0 ~add:hash ~remove:hash
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
  let tree = FsTree.create base_dir in
  (* Change the mkdtemp name to something consistent *)
  let tree =
    Map.update tree ("./" ^ base_dir) ~f:(function
      | Some (Directory, meta) -> Directory, { meta with name = "exp_test" }
      | _ -> failwith "unreachable")
    |> String.Map.map_keys_exn
         ~f:(String.substr_replace_first ~pattern:("./" ^ base_dir) ~with_:".")
  in
  print_s ([%sexp_of: FsTree.t] tree);
  [%expect
    {|
    ((. (Directory ((name exp_test) (permissions 448))))
     (./dir1 (Directory ((name dir1) (permissions 493))))
     (./dir1/file2.txt (File ((name file2.txt) (permissions 420))))
     (./dir1/subdir (Directory ((name subdir) (permissions 493))))
     (./dir1/subdir/file3.txt (File ((name file3.txt) (permissions 420))))
     (./dir2 (Directory ((name dir2) (permissions 493))))
     (./dir2/file4.txt (File ((name file4.txt) (permissions 420))))
     (./file1.txt (File ((name file1.txt) (permissions 420)))))
    |}];
  let dir_count = ref 0 in
  let file_count = ref 0 in
  Map.iter tree ~f:(fun (kind, _meta) ->
    match kind with
    | FsTree.File -> incr file_count
    | FsTree.Directory -> incr dir_count
    | FsTree.Unknown -> ());
  printf "Directory count: %d\n" !dir_count;
  [%expect {| Directory count: 4 |}];
  printf "File count: %d\n" !file_count;
  [%expect {| File count: 4 |}];
  Sys_unix.command (sprintf "rm -rf %s" base_dir) |> ignore
;;
