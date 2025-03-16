open! Core

(*
   LRU Cache Implementation
   - Stores file contents as string values keyed by filename
   - Maintains a FIFO for tracking usage order
   - Automatically evicts least recently used entries when capacity is reached
*)

type t =
  { capacity : int
  ; cache : (string, string) Hashtbl.t
  ; order : string Queue.t (* Front is least recently used, back is most recently used *)
  }

(* Create a new cache with specified capacity *)
let create ~capacity =
  { capacity; cache = Hashtbl.create (module String); order = Queue.create () }
;;

(* Queue manipulation helpers *)
let remove_from_queue q key =
  let keys = Queue.to_list q in
  Queue.clear q;
  List.iter keys ~f:(fun k -> if not (String.equal k key) then Queue.enqueue q k)
;;

let update_order t key =
  remove_from_queue t.order key;
  Queue.enqueue t.order key
;;

let evict_if_needed t =
  if Queue.length t.order > t.capacity
  then (
    let lru = Queue.dequeue_exn t.order in
    Hashtbl.remove t.cache lru)
;;

let get t key =
  match Hashtbl.find t.cache key with
  | Some data ->
    update_order t key;
    Some data
  | None ->
    let data = In_channel.read_all key in
    Hashtbl.set t.cache ~key ~data;
    update_order t key;
    evict_if_needed t;
    Some data
;;

let%expect_test "Cache basic operations" =
  let base_dir = Core_unix.mkdtemp "cache_test" in
  let ( / ) = Filename.concat in
  let write_file path content =
    let path = base_dir / path in
    let oc = Out_channel.create path in
    Out_channel.output_string oc content;
    Out_channel.close oc;
    path
  in
  (* Create test files *)
  let file1 = write_file "c1" "content1" in
  let file2 = write_file "c2" "content2" in
  let file3 = write_file "c3" "content3" in
  let file4 = write_file "c4" "content4" in
  (* Create cache for testing *)
  let cache = create ~capacity:3 in
  let current_size t = Hashtbl.length t.cache in
  let current_order t =
    Queue.to_list t.order
    |> List.map ~f:(String.chop_prefix_exn ~prefix:base_dir)
    |> List.map ~f:(String.( ^ ) "cache_test")
  in
  (*--------------------------*)
  (* Test 1: Initial caching *)
  (*--------------------------*)
  let c1 = Option.value_exn (get cache file1) in
  let c2 = Option.value_exn (get cache file2) in
  let c3 = Option.value_exn (get cache file3) in
  printf "After adding file1, file2, file3:\n";
  printf "file1: %s\nfile2: %s\nfile3: %s\n" c1 c2 c3;
  printf "Cache size: %d\n" (current_size cache);
  printf "Cache order: %s\n" (String.concat ~sep:", " (current_order cache));
  [%expect
    {|
    After adding file1, file2, file3:
    file1: content1
    file2: content2
    file3: content3
    Cache size: 3
    Cache order: cache_test/c1, cache_test/c2, cache_test/c3
    |}];
  (*------------------------------------*)
  (* Test 2: Updating access order     *)
  (*------------------------------------*)
  let _ = Option.value_exn (get cache file1) in
  printf "\nAfter accessing file1 again:\n";
  printf "Cache order: %s\n" (String.concat ~sep:", " (current_order cache));
  [%expect
    {|
    After accessing file1 again:
    Cache order: cache_test/c2, cache_test/c3, cache_test/c1
    |}];
  (*------------------------------------*)
  (* Test 3: Eviction when full        *)
  (*------------------------------------*)
  let c4 = Option.value_exn (get cache file4) in
  printf "\nAfter adding file4 (eviction should occur):\n";
  printf "file4: %s\n" c4;
  printf "Cache size: %d\n" (current_size cache);
  printf "Cache order: %s\n" (String.concat ~sep:", " (current_order cache));
  [%expect
    {|
    After adding file4 (eviction should occur):
    file4: content4
    Cache size: 3
    Cache order: cache_test/c3, cache_test/c1, cache_test/c4
    |}];
  (*------------------------------------*)
  (* Test 4: Re-reading evicted file   *)
  (*------------------------------------*)
  let c2_again = Option.value_exn (get cache file2) in
  printf "\nAfter accessing file2 again (should be re-read):\n";
  printf "file2: %s\n" c2_again;
  printf "Cache size: %d\n" (current_size cache);
  printf "Cache order: %s\n" (String.concat ~sep:", " (current_order cache));
  [%expect
    {|
    After accessing file2 again (should be re-read):
    file2: content2
    Cache size: 3
    Cache order: cache_test/c1, cache_test/c4, cache_test/c2
    |}];
  (* Cleanup temporary files *)
  List.iter [ file1; file2; file3; file4 ] ~f:Sys_unix.remove
;;
