(* 터미널에서 DOS 프로그램을 한다. 화면은 Kitty 그래픽 프로토콜로 그리고,
   키는 Kitty 키보드 프로토콜(disambiguate), 마우스는 SGR 픽셀 모드로
   받는다. Ghostty·kitty 처럼 두 프로토콜을 아는 터미널이 대상이다. F12 로
   끝낸다.

   기계는 벽시계에 맞춰 돈다: 흐른 시간만큼의 CPU 사이클(4.77MHz)을
   돌리고, 화면은 그 사이에 초당 [--fps] 번까지 그린다. 터미널이 늦으면
   프레임을 건너뛸 뿐 게스트 시간은 밀리지 않는다. 호스트가 에뮬레이션
   자체를 못 따라가면 느려질 뿐 빨라지지는 않는다. 바뀐 프레임만 보낸다. *)

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

(* 답을 받기 전 칸 크기. 흔한 고정폭 글꼴의 비율이다. *)
let default_cols = 80
let default_rows = 24
let default_cell_w = 8
let default_cell_h = 16


type state = {
  mutable m : Dos_machine.t;  (** Shift+F11 이 저장한 기계로 바꾼다 *)
  out : Frame_out.t;
  state_file : Quicksave.t option;  (** [--state] — 없으면 F11 은 버리는 키다 *)
  fps : int;
  mutable cols : int;
  mutable rows : int;
  mutable area : (int * int) option;  (** 텍스트 영역 픽셀 크기 *)
  mutable tail : string;
  mutable tail_age : int;
  mutable buttons : int;
  mutable box : int * int;  (** 그림이 차지한 픽셀 *)
  mutable last : string;
  mutable last_digest : int;
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
  | Dosterm.Save_state -> (
    match st.state_file with
    | Some q -> Quicksave.save q st.m
    | None -> st.unmapped <- st.unmapped + 1)
  | Dosterm.Load_state -> (
    match st.state_file with
    | Some q ->
      Option.iter
        (fun m ->
          st.m <- m;
          st.redraw <- true)
        (Quicksave.load q)
    | None -> st.unmapped <- st.unmapped + 1)
  | Dosterm.Text_area { width; height } ->
    st.area <- Some (width, height);
    st.redraw <- true
  | Dosterm.Cells { cols; rows } ->
    st.cols <- cols;
    st.rows <- rows;
    st.redraw <- true
  | Dosterm.Graphics { id; reply } -> Frame_out.on_reply st.out ~id reply
  (* DA1 은 끝낼 때만 묻는다. 그 전에 온 답은 다른 프로그램이 물은 것이다. *)
  | Dosterm.Device_attributes -> ()
  | Dosterm.Mouse ev ->
    st.buttons <- Dosterm.buttons st.buttons ev;
    let fw, _ = Dos_machine.frame_dims st.m in
    let space = Dosterm.mouse_space ~mode:(Dos_machine.video_mode st.m) ~frame_width:fw in
    let x, y = Dosterm.to_guest ~box:st.box ~space (ev.x, ev.y) in
    Dos_machine.set_mouse st.m ~x ~y ~buttons:st.buttons

let poll_input st =
  let events, tail = Dosterm.parse (st.tail ^ Terminal.read_available ()) in
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

(* 프레임을 만드는 일([frame_rgb])은 비싸다. 터미널이 앞 그림을 다 받았고
   프레임 지문이 바뀌었을 때만 만든다. 앞의 것과 다를 때만 보낸다. *)
let draw st =
  Frame_out.tick st.out;
  let digest = Dos_machine.frame_digest st.m in
  if (not (Frame_out.busy st.out)) && (st.redraw || digest <> st.last_digest) then begin
    st.last_digest <- digest;
    let rgb = Dos_machine.frame_rgb st.m in
    if st.redraw || rgb <> st.last then begin
      let width, height = Dos_machine.frame_dims st.m in
      let cell_w, cell_h = cell st in
      let cols, rows = Dosterm.fit ~cols:st.cols ~rows:st.rows ~cell_w ~cell_h in
      Frame_out.post st.out { rgb; width; height; cols; rows };
      st.box <- (cols * cell_w, rows * cell_h);
      st.last <- rgb;
      st.redraw <- false
    end
  end

(* 돌린 시간(벽시계 초)을 돌려준다. [origin] 은 게스트 사이클 0 에 해당하는
   벽시계다. 버린 빚만큼 뒤로 민다. *)
(* 터미널 창을 닫으면 SIGHUP 이 온다. 기본 동작대로 바로 죽으면 터미널
   설정을 되돌리지 못하고 /tmp 의 임시 파일도 남는다. 신호는 표시만 하고
   루프가 보통처럼 끝나 [Terminal.with_terminal] 의 정리를 거치게 한다. *)
let stop_signal = ref None

let watch_signals () =
  List.iter
    (fun (signal, name) ->
      Sys.set_signal signal (Sys.Signal_handle (fun _ -> stop_signal := Some name)))
    [ (Sys.sighup, "SIGHUP"); (Sys.sigterm, "SIGTERM") ]

let play st =
  let frame = 1.0 /. float_of_int st.fps in
  let start = Unix.gettimeofday () in
  let origin = ref start in
  let next_draw = ref start in
  let cycles_at now = int_of_float ((now -. !origin) *. float_of_int cpu_hz) in
  while not (st.quit || Option.is_some !stop_signal || Dos_machine.exited st.m) do
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
    Frame_out.pump st.out;
    (* 다음 그리기까지 잔다. 키가 오거나, 쓰던 그림을 터미널이 더 받을 수
       있게 되면 일찍 깬다. *)
    let now = Unix.gettimeofday () in
    if !next_draw > now then
      let writes = if Frame_out.writing st.out then [ Unix.stdout ] else [] in
      match Unix.select [ Unix.stdin ] writes [] (!next_draw -. now) with
      | _ -> ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
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
  let fps = ref default_fps and mouse = ref true and state = ref "" in
  let specs =
    [ ("--com", Arg.Set_string com, "PATH  COM 이미지");
      ("--exe", Arg.Set_string exe, "PATH  MZ EXE 이미지");
      ("--dir", Arg.String (fun d -> dirs := d :: !dirs),
       "DIR  폴더의 파일을 모두 대문자 이름으로 마운트한다");
      ("--mount", Arg.String (fun s -> mounts := s :: !mounts), "NAME=PATH  파일 하나를 마운트");
      ("--fps", Arg.Set_int fps, "N  초당 프레임(기본 30)");
      ("--no-mouse", Arg.Clear mouse, "  마우스 드라이버를 붙이지 않는다");
      ("--state", Arg.Set_string state,
       "FILE  F11 이 상태를 저장하고 Shift+F11 이 읽는 파일. --com·--exe 없이 주면 거기서 이어 한다") ]
  in
  Arg.parse specs (fun a -> raise (Arg.Bad ("unexpected " ^ a)))
    "dosplay [--dir DIR] (--com PATH | --exe PATH | --state FILE) — Kitty 그래픽 터미널에서 실행, F12 로 끝";
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout) then begin
    prerr_endline "dosplay needs a terminal on stdin and stdout";
    exit 2
  end;
  let fresh () =
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
    else Dos_machine.load_exe m (read_file !exe);
    if !mouse then Dos_machine.attach_mouse m;
    m
  in
  let m =
    if !com <> "" || !exe <> "" then fresh ()
    else if !state <> "" then (
      match Quicksave.restore !state with
      | Ok m -> m
      | Error e ->
        prerr_endline ("cannot resume from " ^ !state ^ ": " ^ e);
        exit 2)
    else begin
      prerr_endline "need --com PATH, --exe PATH or --state FILE";
      exit 2
    end
  in
  let state_file = if !state = "" then None else Some (Quicksave.create !state) in
  let o = Frame_out.create () in
  let st =
    { m; out = o; state_file; fps = max 1 !fps; cols = default_cols; rows = default_rows; area = None;
      tail = ""; tail_age = 0; buttons = 0; box = (0, 0); last = ""; last_digest = 0;
      redraw = true; quit = false; unmapped = 0; guest_cycles = 0 }
  in
  watch_signals ();
  let wall = Terminal.with_terminal o (fun () -> play st) in
  (match !stop_signal with
   | Some name -> Printf.eprintf "stopped by %s\n" name
   | None ->
     if Dos_machine.exited st.m then
       Printf.eprintf "program exited with code %d\n" (Dos_machine.exit_code st.m)
     else prerr_endline "stopped with F12");
  (* 게스트 시간이 벽시계보다 많이 짧으면 이 호스트가 실시간을 못 따라갔다. *)
  Printf.eprintf "guest time %.2fs, wall time %.2fs, frames sent %d (%d via temp files)\n"
    (float_of_int st.guest_cycles /. float_of_int cpu_hz) wall o.sent o.via_files;
  Option.iter (Printf.eprintf "temp-file frames not used: %s\n") o.gave_up;
  Option.iter Quicksave.report st.state_file;
  if st.unmapped > 0 then
    Printf.eprintf "%d key(s) had no BIOS word and were dropped\n" st.unmapped
