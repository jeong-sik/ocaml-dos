(* 게임이 바꾼 파일을 호스트 폴더 하나에 되돌려 쓴다.

   기계는 마운트한 파일을 메모리에만 들고 있어서, 게임 안에서 저장해도
   끝내면 사라진다. [--save-dir DIR] 을 주면 시작할 때 DIR 의 파일을
   [--dir] 위에 싣고, 끝날 때 시작과 내용이 다른 파일만 DIR 에 쓴다. 게임
   폴더([--dir])는 건드리지 않는다 — 원본은 그대로 두고 바뀐 것만 따로
   모은다.

   - 이름: 이 기계는 게스트가 만든 이름을 경로째 표에 둔다(디렉터리 모델이
     없다). 호스트 파일 이름으로 쓸 수 없는 이름(경로 구분자, 드라이브,
     [..])은 쓰지 않고 알린다. DIR 밖에 쓰지 않기 위해서다.
   - 지운 파일: 게임이 지운 파일이 DIR 에 있으면 지운다. 게임 폴더에서 온
     원본은 지우지 않고 알린다 — 다음에 시작하면 다시 보인다.
   - 열린 채인 파일: 핸들을 닫을 때 표에 들어가므로, 끝날 때 열려 있던
     파일의 마지막 쓰기는 들어가지 않는다. *)

type t = {
  dir : string;
  start : (string, string) Hashtbl.t;  (** 시작할 때 파일마다의 내용 *)
  originals : string list;  (** [--dir]·[--mount] 에서 온 이름 — 지우지 않는다 *)
  mutable written : string list;
  mutable removed : string list;
  mutable skipped : string list;  (** 호스트 이름으로 못 쓰는 이름 *)
  mutable kept : string list;  (** 게임이 지웠지만 남겨 둔 원본 *)
  mutable errors : string list;
}

let plain_name name =
  name <> "" && name <> Filename.current_dir_name && name <> Filename.parent_dir_name
  && String.for_all (fun c -> c <> '/' && c <> '\\' && c <> ':' && c <> '\000') name

let dir_perms = 0o755

(* DIR 이 없으면 만든다. 부모가 없으면 만들지 않는다. *)
let prepare dir =
  match Sys.is_directory dir with
  | true -> Ok ()
  | false -> Error (dir ^ " is not a directory")
  | exception Sys_error _ -> (
    match Sys.mkdir dir dir_perms with
    | () -> Ok ()
    | exception Sys_error e -> Error e)

let contents m =
  let table = Hashtbl.create 64 in
  List.iter
    (fun name -> Option.iter (Hashtbl.replace table name) (Dos_machine.read_mounted m name))
    (Dos_machine.mounted_names m);
  table

let create dir ~originals m =
  { dir; start = contents m; originals; written = []; removed = []; skipped = []; kept = [];
    errors = [] }

let write_back sd m =
  let final = contents m in
  Hashtbl.iter
    (fun name data ->
      if Hashtbl.find_opt sd.start name <> Some data then
        if not (plain_name name) then sd.skipped <- name :: sd.skipped
        else
          match Host_file.write_atomically (Filename.concat sd.dir name) data with
          | () -> sd.written <- name :: sd.written
          | exception Sys_error e -> sd.errors <- e :: sd.errors)
    final;
  Hashtbl.iter
    (fun name _ ->
      if not (Hashtbl.mem final name) then begin
        let path = Filename.concat sd.dir name in
        (if plain_name name && Sys.file_exists path then
           match Sys.remove path with
           | () -> sd.removed <- name :: sd.removed
           | exception Sys_error e -> sd.errors <- e :: sd.errors);
        if List.mem name sd.originals then sd.kept <- name :: sd.kept
      end)
    sd.start

let names l = String.concat " " (List.sort compare l)

let report sd =
  if sd.written <> [] then
    Printf.eprintf "wrote %d changed file(s) to %s: %s\n" (List.length sd.written) sd.dir
      (names sd.written);
  if sd.removed <> [] then Printf.eprintf "removed from %s: %s\n" sd.dir (names sd.removed);
  if sd.kept <> [] then
    Printf.eprintf "deleted by the game, original kept (it comes back next start): %s\n"
      (names sd.kept);
  if sd.skipped <> [] then
    Printf.eprintf "not written (not a plain file name): %s\n" (names sd.skipped);
  List.iter (Printf.eprintf "save-dir error: %s\n") (List.rev sd.errors)
