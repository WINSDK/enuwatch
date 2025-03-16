open! Core

(* module Permissions = struct *)
(*   type kind = Read | Write | Execute *)
(*   [@@deriving sexp] *)

(*   type t = { owner : kind; group : kind; other : kind } *)
(*   [@@deriving sexp] *)

(*
1: Modify tree
2: Stablize ->
3. Calls all observers
4. Observers will compute some value (i.e. the tree's hash) 
5. Change in tree structure or in computing a leaf will trigger recalculating parent hashes.
  *)

module Entry = struct
  type 'a kind =
    | File
    | Dir of { children : 'a list }
  [@@deriving sexp]

  type t =
    { kind : t kind
    ; name : string
    ; hash : int
    ; permissions : int
    }
  [@@deriving sexp, fields ~getters]
end

module Inc = Incremental.Make ()
module Var = Inc.Var

let file_tree path =
  let ( / ) = Filename.concat in
  let parse parent_path entry ~f =
    let path = parent_path / entry in
    let stat = Core_unix.stat path in
    let hash, kind =
      match stat.st_kind with
      | Core_unix.S_REG ->
        let content = In_channel.read_all path in
        String.hash content, Entry.File
      | Core_unix.S_DIR ->
        let children = f path in
        let hash = List.sum (module Int) children ~f:Entry.hash in
        hash, Entry.Dir { children }
      | _ -> 0, Entry.File (* Unsupported things are saved as 0 sized file's *)
    in
    { Entry.kind; name = entry; hash; permissions = stat.st_perm }
  in
  let rec build_tree path =
    let entries = Sys_unix.ls_dir path |> List.sort ~compare:String.compare in
    List.map entries ~f:(fun entry -> parse path entry ~f:build_tree)
  in
  let path = Filename_unix.realpath path in
  let children = build_tree path in
  let parent_path, entry = Filename.split path in
  parse parent_path entry ~f:(Fn.const children)
;;

let%expect_test "file_tree creates correct tree structure" =
  let base_dir = Core_unix.mkdtemp "exp_test" in
  Core_unix.mkdir (Filename.concat base_dir "dir1") ~perm:0o755;
  Core_unix.mkdir (Filename.concat base_dir "dir2") ~perm:0o755;
  Core_unix.mkdir (Filename.concat base_dir "dir1/subdir") ~perm:0o755;
  let write_file path content =
    let path = Filename.concat base_dir path in
    let oc = Out_channel.create path in
    Out_channel.output_string oc content;
    Out_channel.close oc
  in
  write_file "file1.txt" "Hello, world!";
  write_file "dir1/file2.txt" "Content of file2";
  write_file "dir1/subdir/file3.txt" "Nested file";
  write_file "dir2/file4.txt" "Another file";
  let tree = file_tree base_dir in
  (* Change the mkdtemp name to something consistent *)
  let tree = { tree with name = "exp_test" } in
  print_s (Entry.sexp_of_t tree);
  [%expect
    {|
    ((kind
      (Dir
       (children
        (((kind
           (Dir
            (children
             (((kind File) (name file2.txt) (hash 688532149) (permissions 420))
              ((kind
                (Dir
                 (children
                  (((kind File) (name file3.txt) (hash 731139842)
                    (permissions 420))))))
               (name subdir) (hash 731139842) (permissions 493))))))
          (name dir1) (hash 1419671991) (permissions 493))
         ((kind
           (Dir
            (children
             (((kind File) (name file4.txt) (hash 725108470) (permissions 420))))))
          (name dir2) (hash 725108470) (permissions 493))
         ((kind File) (name file1.txt) (hash 891273215) (permissions 420))))))
     (name exp_test) (hash 3036053676) (permissions 448))
    |}];
  let dir_count = ref 0 in
  let file_count = ref 0 in
  let rec count_entries entry =
    match Entry.kind entry with
    | Entry.File -> incr file_count
    | Entry.Dir { children } ->
      incr dir_count;
      List.iter children ~f:count_entries
  in
  count_entries tree;
  printf "Directory count: %d\n" !dir_count;
  [%expect {| Directory count: 4 |}];
  printf "File count: %d\n" !file_count;
  [%expect {| File count: 4 |}];
  Sys_unix.command (sprintf "rm -rf %s" base_dir) |> ignore
;;
