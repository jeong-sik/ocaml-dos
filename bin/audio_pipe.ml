(* 표본을 외부 재생 프로그램의 표준 입력으로 흘린다(RFC 0006 단계 6).

   쓰기는 막히지 않는다. 재생기가 못 따라와 파이프가 차면 [max_queue]
   만큼만 들고 있고, 넘치는 몫은 오래된 표본부터 버린다. 표본은 2바이트라
   재생기가 받는 바이트 열의 짝이 어긋나지 않게 버린다 — 반쪽만 보낸
   표본의 나머지 바이트는 버리지 않는다. 재생기가 끝나면(EPIPE) 소리만
   멈추고 게임은 계속 돈다. *)

let sample_bytes = 2

type t = {
  fd : Unix.file_descr;
  max_queue : int;              (* 바이트, 짝수 *)
  mutable queue : Bytes.t;      (* 아직 못 보낸 바이트 *)
  mutable written : int;        (* 재생기에 넘긴 바이트 수 *)
  mutable dropped : int;        (* 버린 표본 수 *)
  mutable closed : string option;  (* 재생기가 닫은 까닭 *)
}

(* 공백으로 나눈다. 셸을 거치지 않아서 따옴표·변수는 풀지 않는다. *)
let words cmd = List.filter (( <> ) "") (String.split_on_char ' ' cmd)

let start ~cmd ~max_queue_samples =
  match words cmd with
  | [] -> Error "empty --audio-cmd"
  | prog :: _ as argv -> (
    (* 재생기가 먼저 끝나면 쓰기가 SIGPIPE 대신 EPIPE 로 돌아오게 한다 *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let r, w = Unix.pipe ~cloexec:true () in
    let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0 in
    match Unix.create_process prog (Array.of_list argv) r null null with
    | (_ : int) ->
      (* 기다리지 않는다: 재생기는 입력이 끝나면 남은 소리를 마저 내고
         끝나고, dosplay 도 곧 끝난다. *)
      Unix.close r;
      Unix.close null;
      Unix.set_nonblock w;
      Ok
        { fd = w; max_queue = max_queue_samples * sample_bytes; queue = Bytes.empty;
          written = 0; dropped = 0; closed = None }
    | exception Unix.Unix_error (e, _, _) ->
      List.iter Unix.close [ r; w; null ];
      Error (prog ^ ": " ^ Unix.error_message e))

(* 앞에서 [n] 바이트를 버린다. 보낸 바이트 수가 홀수면 첫 바이트는 반쪽
   보낸 표본의 나머지라 남기고, 그 뒤에서 짝수 바이트를 버린다. *)
let drop_front t n =
  let keep = t.written land 1 in
  let n = n land lnot 1 in
  let len = Bytes.length t.queue in
  let rest = Bytes.sub t.queue (keep + n) (len - keep - n) in
  t.queue <- Bytes.cat (Bytes.sub t.queue 0 keep) rest;
  t.dropped <- t.dropped + (n / sample_bytes)

let flush t =
  let len = Bytes.length t.queue in
  if len > 0 then
    match Unix.single_write t.fd t.queue 0 len with
    | n ->
      t.written <- t.written + n;
      t.queue <- Bytes.sub t.queue n (len - n)
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()
    | exception Unix.Unix_error (e, _, _) ->
      t.closed <- Some (Unix.error_message e);
      t.dropped <- t.dropped + (len / sample_bytes);
      t.queue <- Bytes.empty

let push t samples =
  match t.closed with
  | Some _ -> t.dropped <- t.dropped + (Bytes.length samples / sample_bytes)
  | None ->
    t.queue <- Bytes.cat t.queue samples;
    let over = Bytes.length t.queue - t.max_queue in
    if over > 0 then drop_front t (over + (over land 1));
    flush t

let close t = try Unix.close t.fd with Unix.Unix_error _ -> ()

let report t =
  if t.dropped > 0 then Printf.eprintf "audio: %d samples dropped (the player fell behind)\n" t.dropped;
  Option.iter (Printf.eprintf "audio: the player stopped taking samples: %s\n") t.closed
