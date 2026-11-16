(* 기계 상태 한 칸을 파일에 둔다. F11 이 쓰고 Shift+F11 이 읽는다.

   상태에는 CPU·메모리·화면·마운트한 파일이 다 들어간다
   ([Dos_snapshot]). dosplay 는 게임이 쓴 파일을 호스트 폴더로 되돌려
   쓰지 않으므로, 게임 안에서 한 저장도 이 상태 파일에만 남는다.

   쓰기는 옆 파일에 쓴 뒤 이름을 바꾼다. 쓰다가 멈춰도 앞서 저장한 상태는
   그대로다. *)

type t = {
  path : string;
  mutable saves : int;
  mutable loads : int;
  mutable errors : string list;  (** 최근 것이 앞 *)
}

let create path = { path; saves = 0; loads = 0; errors = [] }

let read_file path =
  In_channel.with_open_bin path In_channel.input_all

let write_atomically path data =
  let tmp = path ^ ".tmp" in
  Out_channel.with_open_bin tmp (fun oc -> output_string oc data);
  Sys.rename tmp path

let note q e = q.errors <- e :: q.errors

let save q m =
  match Dos_snapshot.save m with
  | Error e -> note q ("save: " ^ Dos_snapshot.save_error_to_string e)
  | Ok bytes -> (
    match write_atomically q.path bytes with
    | () -> q.saves <- q.saves + 1
    | exception Sys_error e -> note q ("save: " ^ e))

let restore path =
  match read_file path with
  | exception Sys_error e -> Error e
  | bytes -> Result.map_error Dos_snapshot.error_to_string (Dos_snapshot.restore bytes)

(* 실패하면 지금 기계를 그대로 둔다. *)
let load q =
  match restore q.path with
  | Ok m ->
    q.loads <- q.loads + 1;
    Some m
  | Error e ->
    note q ("load: " ^ e);
    None

let report q =
  if q.saves > 0 || q.loads > 0 then
    Printf.eprintf "state %s: saved %d time(s), loaded %d time(s)\n" q.path q.saves q.loads;
  List.iter (Printf.eprintf "state error: %s\n") (List.rev q.errors)
