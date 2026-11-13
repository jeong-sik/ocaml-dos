(* 호스트 파일 읽기·쓰기. 쓰기는 옆 파일([path] ^ [tmp_suffix])에 쓴 뒤
   이름을 바꾼다 — 쓰다가 멈춰도 원래 파일은 그대로다. *)

let tmp_suffix = ".tmp"

let read path = In_channel.with_open_bin path In_channel.input_all

let write_atomically path data =
  let tmp = path ^ tmp_suffix in
  Out_channel.with_open_bin tmp (fun oc -> output_string oc data);
  Sys.rename tmp path
