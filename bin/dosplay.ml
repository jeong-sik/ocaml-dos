(* 터미널에서 DOS 프로그램을 한다. 화면은 Kitty 그래픽 프로토콜로 그리고,
   키는 Kitty 키보드 프로토콜(disambiguate), 마우스는 SGR 픽셀 모드로
   받는다. Ghostty·kitty 처럼 두 프로토콜을 아는 터미널이 대상이다. F12 로
   끝낸다.

   기계는 벽시계에 맞춰 돈다: 흐른 시간만큼의 CPU 사이클(4.77MHz)을
   돌리고, 화면은 그 사이에 초당 [--fps] 번까지 그린다. 그리기가 늦어도
   게스트 시간은 밀리지 않는다. 호스트가 에뮬레이션 자체를 못 따라가면
   느려질 뿐 빨라지지는 않는다. 바뀐 프레임만 보낸다. *)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      really_input_string ic (in_channel_length ic))

(* PIT 입력 클록(Hz). CPU 클록은 그 [Dos_ports.cpu_cycles_per_pit_tick] 배다. *)
let pit_hz = 1_193_182
let cpu_hz = pit_hz * Dos_ports.cpu_cycles_per_pit_tick

let default_fps = 30

(* 게스트가 벽시계보다 이만큼(초) 넘게 뒤처지면 그 빚은 버린다. 호스트가
   잠깐 멈춘 뒤 게스트를 한꺼번에 몰아 돌리지 않기 위해서다. *)
let max_lag = 0.25

(* ESC 한 바이트가 이만큼의 프레임 동안 뒤따르는 바이트 없이 남으면 Esc 키다.
   Kitty 프로토콜을 쓰는 터미널은 Esc 를 [CSI 27 u] 로 보내므로 이 길을 타지
   않는다. *)
let lone_escape_frames = 2
let read_size = 4096

(* 답을 받기 전 칸 크기. 흔한 고정폭 글꼴의 비율이다. *)
let default_cols = 80
let default_rows = 24
let default_cell_w = 8
let default_cell_h = 16

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

(* 터미널을 게임용으로 바꾸고 [f] 를 부른 뒤 되돌린다. 되돌리다 난 예외가
   [f] 의 예외를 덮지 않게 여기서 삼킨다. *)
let with_terminal f =
  let saved = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW (raw_mode saved);
  out enter_terminal;
  Fun.protect f ~finally:(fun () ->
      (try out leave_terminal with Sys_error _ -> ());
      try Unix.tcsetattr Unix.stdin Unix.TCSANOW saved with Unix.Unix_error _ -> ())

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

type state = {
  m : Dos_machine.t;
  fps : int;
  mutable cols : int;
  mutable rows : int;
  mutable area : (int * int) option;  (** 텍스트 영역 픽셀 크기 *)
  mutable tail : string;
  mutable tail_age : int;
  mutable buttons : int;
  mutable image_id : int;
  mutable box : int * int;  (** 그림이 차지한 픽셀 *)
  mutable last : string;
  mutable redraw : bool;
  mutable quit : bool;
  mutable unmapped : int;
  mutable guest_cycles : int;  (** 돌린 CPU 사이클의 합 *)
}

let cell st =
  match st.area with
  | Some (w, h) -> (max 1 (w / max 1 st.cols), max 1 (h / max 1 st.rows))
  | None -> (default_cell_w, default_cell_h)

let apply st = function
  | Dosterm.Key w -> Dos_machine.push_key st.m w
  | Dosterm.Unmapped _ -> st.unmapped <- st.unmapped + 1
  | Dosterm.Quit -> st.quit <- true
  | Dosterm.Text_area { width; height } ->
    st.area <- Some (width, height);
    st.redraw <- true
  | Dosterm.Cells { cols; rows } ->
    st.cols <- cols;
    st.rows <- rows;
    st.redraw <- true
  | Dosterm.Mouse ev ->
    st.buttons <- Dosterm.buttons st.buttons ev;
    let fw, _ = Dos_machine.frame_dims st.m in
    let space = Dosterm.mouse_space ~mode:(Dos_machine.video_mode st.m) ~frame_width:fw in
    let x, y = Dosterm.to_guest ~box:st.box ~space (ev.x, ev.y) in
    Dos_machine.set_mouse st.m ~x ~y ~buttons:st.buttons

let poll_input st =
  let events, tail = Dosterm.parse (st.tail ^ read_available ()) in
  List.iter (apply st) events;
  if tail <> "" && tail = st.tail then st.tail_age <- st.tail_age + 1 else st.tail_age <- 0;
  st.tail <- tail;
  if st.tail_age >= lone_escape_frames then begin
    List.iter (apply st) (Dosterm.lone_escape st.tail);
    st.tail <- "";
    st.tail_age <- 0
  end

(* 게스트를 [target] 사이클까지 돌린다. *)
let run_to st target =
  while st.guest_cycles < target && not (Dos_machine.exited st.m) do
    st.guest_cycles <- st.guest_cycles + Dos_machine.step st.m
  done

let draw st =
  let rgb = Dos_machine.frame_rgb st.m in
  if st.redraw || rgb <> st.last then begin
    let width, height = Dos_machine.frame_dims st.m in
    let cell_w, cell_h = cell st in
    let cols, rows = Dosterm.fit ~cols:st.cols ~rows:st.rows ~cell_w ~cell_h in
    let id = if st.image_id = 1 then 2 else 1 in
    out
      ("\027[H" ^ Dosterm.image ~id ~width ~height ~cols ~rows rgb
     ^ Dosterm.delete_image st.image_id);
    st.image_id <- id;
    st.box <- (cols * cell_w, rows * cell_h);
    st.last <- rgb;
    st.redraw <- false
  end

(* 돌린 시간(벽시계 초)을 돌려준다. [origin] 은 게스트 사이클 0 에 해당하는
   벽시계다. 버린 빚만큼 뒤로 민다. *)
let play st =
  let frame = 1.0 /. float_of_int st.fps in
  let start = Unix.gettimeofday () in
  let origin = ref start in
  let next_draw = ref start in
  let cycles_at now = int_of_float ((now -. !origin) *. float_of_int cpu_hz) in
  while not (st.quit || Dos_machine.exited st.m) do
    poll_input st;
    let now = Unix.gettimeofday () in
    let behind = float_of_int (cycles_at now - st.guest_cycles) /. float_of_int cpu_hz in
    if behind > max_lag then origin := !origin +. (behind -. max_lag);
    run_to st (cycles_at now);
    let now = Unix.gettimeofday () in
    if now >= !next_draw then begin
      draw st;
      next_draw := Float.max (!next_draw +. frame) now
    end;
    let now = Unix.gettimeofday () in
    if !next_draw > now then Unix.sleepf (!next_draw -. now)
  done;
  Unix.gettimeofday () -. start

let mount_dir m dir =
  Array.iter
    (fun name ->
      let path = Filename.concat dir name in
      if Sys.file_exists path && not (Sys.is_directory path) then
        Dos_machine.mount_file m (String.uppercase_ascii name) (read_file path))
    (Sys.readdir dir)

let () =
  let com = ref "" and exe = ref "" and dirs = ref [] and mounts = ref [] in
  let fps = ref default_fps and mouse = ref true in
  let specs =
    [ ("--com", Arg.Set_string com, "PATH  COM 이미지");
      ("--exe", Arg.Set_string exe, "PATH  MZ EXE 이미지");
      ("--dir", Arg.String (fun d -> dirs := d :: !dirs),
       "DIR  폴더의 파일을 모두 대문자 이름으로 마운트한다");
      ("--mount", Arg.String (fun s -> mounts := s :: !mounts), "NAME=PATH  파일 하나를 마운트");
      ("--fps", Arg.Set_int fps, "N  초당 프레임(기본 30)");
      ("--no-mouse", Arg.Clear mouse, "  마우스 드라이버를 붙이지 않는다") ]
  in
  Arg.parse specs (fun a -> raise (Arg.Bad ("unexpected " ^ a)))
    "dosplay [--dir DIR] (--com PATH | --exe PATH) — Kitty 그래픽 터미널에서 실행, F12 로 끝";
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout) then begin
    prerr_endline "dosplay needs a terminal on stdin and stdout";
    exit 2
  end;
  let m = Dos_machine.create () in
  List.iter (mount_dir m) (List.rev !dirs);
  List.iter
    (fun spec ->
      match String.index_opt spec '=' with
      | Some i ->
        Dos_machine.mount_file m (String.sub spec 0 i)
          (read_file (String.sub spec (i + 1) (String.length spec - i - 1)))
      | None -> prerr_endline ("bad --mount (want NAME=PATH): " ^ spec))
    (List.rev !mounts);
  if !com <> "" then Dos_machine.load_com m (read_file !com)
  else if !exe <> "" then Dos_machine.load_exe m (read_file !exe)
  else begin
    prerr_endline "need --com PATH or --exe PATH";
    exit 2
  end;
  if !mouse then Dos_machine.attach_mouse m;
  let st =
    { m; fps = max 1 !fps; cols = default_cols; rows = default_rows; area = None;
      tail = ""; tail_age = 0; buttons = 0; image_id = 2; box = (0, 0); last = "";
      redraw = true; quit = false; unmapped = 0; guest_cycles = 0 }
  in
  let wall = with_terminal (fun () -> play st) in
  if Dos_machine.exited m then
    Printf.eprintf "program exited with code %d\n" (Dos_machine.exit_code m)
  else prerr_endline "stopped with F12";
  (* 게스트 시간이 벽시계보다 많이 짧으면 이 호스트가 실시간을 못 따라갔다. *)
  Printf.eprintf "guest time %.2fs, wall time %.2fs\n"
    (float_of_int st.guest_cycles /. float_of_int cpu_hz) wall;
  if st.unmapped > 0 then
    Printf.eprintf "%d key(s) had no BIOS word and were dropped\n" st.unmapped
