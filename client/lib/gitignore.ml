open! Core

let global () =
  let prog =
    Core_unix.create_process
      ~prog:"git"
      ~args:[ "config"; "--global"; "core.excludesFiles" ]
  in
  let stdout = Core_unix.in_channel_of_descr prog.stdout in
  match Core_unix.waitpid prog.pid with
  | Error #Core_unix.Exit_or_signal.error -> None
  | Ok _ ->
    let path = In_channel.input_all stdout in
    Some (String.strip path)
;;

let local () =
  let ( / ) = Filename.concat in
  let rec find path target =
    let entries = Sys_unix.ls_dir path in
    if List.exists ~f:(String.( = ) target) entries
    then Some (Filename.concat path target)
    else if String.( = ) path Filename.root
    then None
    else (
      let path = path / Filename.parent_dir_name in
      let path = Filename_unix.realpath path in
      find path target)
  in
  let cwd = Sys_unix.getcwd () in
  let%bind.Option git_path = find cwd ".git" in
  let%bind.Option ignore_path = find cwd ".gitignore" in
  let d1 = List.length (Filename.parts git_path) in
  let d2 = List.length (Filename.parts ignore_path) in
  (* If .gitignore is above the .git directory in the tree, no match is found *)
  if d2 < d1 then None else Some ignore_path
;;

let read () =
  match local (), global () with
  | Some l, Some g ->
    let l = In_channel.read_all l in
    let g = In_channel.read_all g in
    Some (l ^ g)
  | None, Some g -> Some (In_channel.read_all g)
  | Some l, None -> Some (In_channel.read_all l)
  | None, None -> None
;;

module Parser = struct
  open Angstrom
  open Angstrom.Let_syntax

  (** Check whether a path [p] in [glob p] matches. *)
  type glob = string -> bool

  let is_ws = function
    | ' ' | '\t' -> true
    | _ -> false
  ;;

  let is_eol = function
    | '\n' -> true
    | _ -> false
  ;;

  let ws = skip_while (fun c -> is_ws c || is_eol c)

  let text =
    let%map content = take_while1 (fun c -> not (is_eol c)) in
    String.strip content
  ;;

  let lex p = p <* ws
  let some x = Some x
  let none _ = None

  let glob ?anchored () =
    let%bind text = text in
    match Re.Glob.glob ~pathname:true ~double_asterisk:true ?anchored text with
    | glob ->
      let re = Re.compile glob in
      return (Re.execp re)
    | exception Re.Glob.Parse_error -> fail "glob parse error"
  ;;

  let pattern =
    let neg g = char '!' *> (Fn.compose not <$> g) <?> "negation" in
    let anch = char '/' *> glob ~anchored:true () <?> "anchored rule" in
    let normal = glob () <?> "rule" in
    neg (anch <|> normal) <|> anch <|> normal
  ;;

  let comment = char '#' *> text
  let pattern_line = lift2 Fn.const (lex pattern) (lex (option None (comment >>| some)))

  let line =
    match%bind peek_char_fail with
    | '#' -> lex comment >>| none
    | _ -> pattern_line >>| some
  ;;

  let parser = ws *> many line

  let run (str : string) =
    match parse_string ~consume:Prefix parser str with
    | Ok patterns -> patterns |> List.filter_opt
    | Error error -> raise_s [%message "parsing gitignore" (error : string)]
  ;;
end

let of_string (str : string) = Parser.run str

let%test_module "gitignore" =
  (module struct
    let test ~pattern ~path expected =
      let patterns = of_string pattern in
      if List.length patterns = 0 then failwith "Couldn't parse pattern";
      let matches = List.exists patterns ~f:(fun p -> p path) in
      if Bool.(matches <> expected)
      then
        failwithf
          "Pattern '%s' matching '%s': expected %b but got %b"
          pattern
          path
          expected
          matches
          ()
    ;;

    let%test_unit "basic patterns" =
      test ~pattern:"*.o" ~path:"file.o" true;
      test ~pattern:"*.o" ~path:"file.c" false;
      test ~pattern:"build/" ~path:"build/output.txt" true;
      test ~pattern:"build/" ~path:"source/file.txt" false;
      test ~pattern:"build/" ~path:"source/build/file.txt" true
    ;;

    let%test_unit "negation paterns" =
      test ~pattern:"!*.o" ~path:"file.c" true;
      test ~pattern:"!*.o" ~path:"file.o" false;
      test ~pattern:"!!*.o" ~path:"file.o" true
    ;;

    let%test_unit "anchored paterns" =
      test ~pattern:"*.o" ~path:"file.c" false;
      test ~pattern:"/build/" ~path:"build/output.txt" true;
      test ~pattern:"/build/" ~path:"output/build/output.txt" false
    ;;

    let%test_unit "recursive patterns" =
      test ~pattern:"**/logs" ~path:"logs" true;
      test ~pattern:"**/logs" ~path:"project/logs" true;
      test ~pattern:"**/logs" ~path:"project/subdir/logs" true;
      test ~pattern:"**/logs" ~path:"project/log" false
    ;;

    let%test_unit "multiple patterns" =
      test ~pattern:"*.o\n*.a\n" ~path:"file.o" true;
      test ~pattern:"*.o\n*.a\n" ~path:"file.a" true;
      test ~pattern:"*.o\n*.a\n" ~path:"file.c" false
    ;;

    let%test_unit "comments" =
      test ~pattern:"# This is a comment\n*.o" ~path:"file.o" true;
      test ~pattern:"*.o\n# Ignore libraries\n*.a" ~path:"file.a" true
    ;;
  end)
;;
