(* dosplay 가 터미널과 주고받는 입출력: 게임용 설정으로 바꾸고 되돌리기,
   입력 바이트 읽기, 끝낼 때 터미널이 다 처리했는지 맞추기. *)

let read_size = 4096

let enter_terminal =
  String.concat ""
    [ "\027[?1049h";  (* 다른 화면으로 *)
      "\027[?25l";  (* 커서 숨김 *)
      "\027[>1u";  (* Kitty 키보드: disambiguate 를 스택에 넣는다 *)
      "\027[?1003h";  (* 버튼 없이 움직여도 알린다 *)
      "\027[?1006h";  (* SGR 마우스 *)
      "\027[?1016h";  (* 좌표를 픽셀로 *)
      "\027[14t";  (* 텍스트 영역 픽셀 크기를 묻는다 *)
      "\027[18t" (* 칸 수를 묻는다 *) ]

let leave_terminal =
  String.concat ""
    [ "\027[?1016l"; "\027[?1006l"; "\027[?1003l";
      "\027[<u";  (* 넣었던 키보드 플래그를 뺀다 *)
      "\027_Ga=d,d=A,q=2\027\\";  (* 그림을 모두 지운다 *)
      "\027[?25h"; "\027[?1049l" ]

let out s =
  print_string s;
  flush stdout

let raw_mode saved =
  { saved with
    Unix.c_icanon = false; c_echo = false; c_isig = false; c_ixon = false;
    c_icrnl = false; c_vmin = 0; c_vtime = 0 }

let read_available () =
  let buf = Bytes.create read_size in
  let acc = Buffer.create 64 in
  let rec go () =
    match Unix.select [ Unix.stdin ] [] [] 0.0 with
    | [], _, _ -> ()
    | _ :: _, _, _ ->
      let n = Unix.read Unix.stdin buf 0 read_size in
      Buffer.add_subbytes acc buf 0 n;
      if n = read_size then go ()
  in
  (try go () with Unix.Unix_error (Unix.EINTR, _, _) -> ());
  Buffer.contents acc

(* 끝낼 때 DA1 을 묻는다. 터미널은 받은 차례대로 답하므로, DA1 답이 오면
   그 앞의 그래픽 이스케이프는 다 처리됐다 — 마지막 프레임 파일도 읽고
   지웠다. 그 사이 온 답(그래픽 에러 등)은 여기서 버린다. 버리지 않으면
   터미널 설정을 되돌린 뒤 셸 입력으로 샌다. DA1 은 VT100 부터 있는
   질문이라 답하지 않는 터미널은 드물다. 그래도 [exit_sync_timeout] 넘게
   기다리지는 않는다. *)
let ask_attributes = "\027[c"
let exit_sync_timeout = 1.0

let drain_until_attributes () =
  let deadline = Unix.gettimeofday () +. exit_sync_timeout in
  let rec go tail =
    let left = deadline -. Unix.gettimeofday () in
    if left > 0.0 then
      match Unix.select [ Unix.stdin ] [] [] left with
      | [], _, _ -> ()
      | _ :: _, _, _ -> (
        (* 읽을 수 있다는데 아무것도 없으면 터미널이 닫혔다(EOF). *)
        match read_available () with
        | "" -> ()
        | got ->
          let events, tail = Dosterm.parse (tail ^ got) in
          if not (List.mem Dosterm.Device_attributes events) then go tail)
  in
  go ""

(* 터미널을 게임용으로 바꾸고 [f] 를 부른 뒤 되돌린다. 되돌리다 난 예외가
   [f] 의 예외를 덮지 않게 여기서 삼킨다. 비차단 모드는 셸과 같은 터미널
   파일에 걸리므로 반드시 푼다. 터미널이 다 처리한 뒤 남은 임시 파일을
   지운다. *)
let with_terminal o f =
  let saved = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW (raw_mode saved);
  out enter_terminal;
  Unix.set_nonblock Unix.stdout;
  Fun.protect f ~finally:(fun () ->
      (try Unix.clear_nonblock Unix.stdout with Unix.Unix_error _ -> ());
      (try Frame_out.finish o with Unix.Unix_error _ -> ());
      (try out (leave_terminal ^ ask_attributes) with Sys_error _ -> ());
      (try drain_until_attributes () with Unix.Unix_error _ -> ());
      (try Unix.tcsetattr Unix.stdin Unix.TCSANOW saved with Unix.Unix_error _ -> ());
      Frame_out.cleanup o)
