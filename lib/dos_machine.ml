(* DOS 기계 — 8086 + 1MB RAM + 장치 포트 + BIOS/DOS 인터럽트 표면 +
   COM/MZ-EXE 로더.

   구조: 상태는 Dos_state, 포트는 Dos_ports, BIOS 는 Dos_bios, DOS 는
   Dos_dos 가 맡는다. 이 파일은 그것들을 잇고 바깥에 내보이는 얼굴이다.

   결정론: 같은 이미지 + 같은 키 순서 = 같은 화면. 시각도 난수도
   호스트에서 오지 않는다. *)

type t = Dos_state.t

open Dos_state

let starve_threshold = 2000

let create () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  let ports = Dos_ports.create ~mem ~video in
  (* EGA/VGA 평면 모드에서는 0xA0000 이 평면 넷을 겹쳐 둔 창이다 —
     그래픽 컨트롤러를 지나야 하고, 읽기는 래치를 채우는 부수효과가
     있다. 나머지 주소는 그대로 1MB 배열이다. *)
  let read a =
    let a = a land 0xfffff in
    if Dos_video.owns_address video a then Dos_video.mem_read video a
    else Char.code (Bytes.get mem a)
  in
  let write a v =
    let a = a land 0xfffff in
    if Dos_video.owns_address video a then Dos_video.mem_write video a v
    else Bytes.set mem a (Char.chr (v land 0xff))
  in
  let cpu =
    (* 1990년대 DOS 게임은 286 이상에서 돌았고 Borland 컴파일러가 186
       명령을 낸다. 실칩 8086 검증은 Cpu86 쪽 스위트가 I8086 으로 따로
       돈다. *)
    Cpu86.create ~model:Cpu86.I80186 ~read ~write
      ~port_in:(fun p -> Dos_ports.port_in ports p)
      ~port_out:(fun p v -> Dos_ports.port_out ports p v)
      ()
  in
  let t =
    {
      mem; cpu; ports;
      exited = false; exit_code = 0; video;
      host_files = Hashtbl.create 8;
      handles = Hashtbl.create 4;
      (* DOS 예약 핸들 0-4(stdin/stdout/stderr/aux/prn)는 피해서 준다 *)
      next_handle = 5;
      fcbs = Hashtbl.create 4;
      dta = 0x80; psp_seg = 0;
      kbd_wait = false; kbd_requests = 0; last_tick = 0; pending_irq0 = false;
      free_base = 0x1000; free_top = 0x9FFF; blocks = []; find_queue = [];
      stubs = []; exec_frames = []; last_child_code = 0;
      epoch_year = 1990; epoch_month = 1; epoch_day = 1;
      epoch_hour = 8; epoch_min = 0; epoch_sec = 0;
      ems_next_handle = 1;
      ems_pages = Hashtbl.create 2;
      ems_mapped = Array.make 4 (0, 0);
      mouse = {
        mouse_present = false; mouse_x = 0; mouse_y = 0;
        mouse_buttons = 0; mouse_visible = false;
        mouse_dx = 0; mouse_dy = 0;
      };
    }
  in
  Dos_bios.install t;
  Cpu86.set_int_hook cpu (fun vec ->
      (* ROM 스텁을 지나온 호출(실벡터 + 0x80)을 실벡터로 되돌린다. *)
      let raw = vec land 0xff in
      let v =
        if raw >= 0x80 && List.mem (raw - 0x80) Dos_bios.host_served then
          raw - 0x80
        else raw
      in
      (* 게스트가 건 벡터가 먼저다. 비어 있거나 아직 우리 스텁이면
         호스트 구현으로 간다 — 스텁으로 다시 들어가면 무한루프다. *)
      if (not (Dos_bios.ivt_is_own_stub t v)) && deliver_ivt t v then ()
      else
        match v with
        | 0x20 -> Dos_dos.terminate t
        | 0x21 -> Dos_dos.service t
        | 0x27 -> Dos_dos.int27 t
        | 0x67 -> Dos_ems.service t
        | 0x10 | 0x11 | 0x12 | 0x16 | 0x1A | 0x33 -> Dos_bios.service t v
        | _ -> ());
  t

(* ---------- 로더 ----------

   몸통은 Dos_dos 에 있다 — EXEC(AH=4Bh) 자식을 같은 코드로 싣기
   때문이다. 여기선 루트 프로그램을 싣는 얼굴만 남긴다. *)

let load_com t image = Dos_dos.load_com t image

let load_exe t image = Dos_dos.load_exe t image

(* ---------- 파일 마운트 ---------- *)

let mount_file t name data =
  Hashtbl.replace t.host_files (String.uppercase_ascii name)
    (Bytes.of_string data)

let read_mounted t name =
  Option.map Bytes.to_string
    (Hashtbl.find_opt t.host_files (String.uppercase_ascii name))

let mounted_names t =
  List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) t.host_files [])

(* ---------- 실행 ---------- *)

let step t =
  if t.exited then 2
  else begin
    Dos_ports.set_now t.ports (Cpu86.cycles t.cpu);
    let interval = Dos_ports.cycles_per_tick t.ports in
    let cyc = Cpu86.cycles t.cpu in
    if cyc - t.last_tick >= interval then begin
      t.last_tick <- cyc;
      t.pending_irq0 <- true
    end;
    (* IRQ0 은 IF 가 켜져 있고 PIC 마스크가 풀렸을 때만 들어간다. 못
       넣은 틱은 버리지 않고 들고 있다가 다음 기회에 넣는다 — 버리면
       임계구역이 긴 프로그램에서 시계가 느려진다. HLT 는 그 인터럽트로
       깨어난다. *)
    if t.pending_irq0 && interrupts_enabled t
       && not (Dos_ports.irq0_masked t.ports) then begin
      t.pending_irq0 <- false;
      Cpu86.wake t.cpu;
      ignore (deliver_ivt t 8)
    end;
    Cpu86.step t.cpu
  end

let exited t = t.exited
let exit_code t = t.exit_code
let halted t = Cpu86.halted t.cpu

let run t ~max_steps =
  let n = ref 0 in
  while (not t.exited) && (not (halted t)) && !n < max_steps do
    ignore (step t);
    incr n
  done

let run_until t ~max_steps ~stop =
  let n = ref 0 and stopped = ref false in
  while (not t.exited) && (not !stopped) && !n < max_steps do
    ignore (step t);
    incr n;
    if stop t then stopped := true
  done;
  !n

(* ---------- 키 ---------- *)

let push_key t word = push_key t word

(* 굶주림을 보고 하나씩 넣는다. 키를 미리 다 밀어 넣으면 앞선 메뉴의
   "아무 키나" 루프가 전부 먹어 치운다 — 게임이 어떤 상태일 때 넣을지는
   [not_before] 스텝으로 묶는다(실측: ZZT 사전 메뉴). *)
type key_plan = { word : int; not_before : int }

let run_with_keys ?(on_step = fun _ _ -> ()) ?(on_key = fun _ _ -> ())
    t ~max_steps ~keys =
  let queue = Queue.create () in
  List.iter (fun k -> Queue.push k queue) keys;
  let n = ref 0 and starve = ref 0 in
  while (not t.exited) && !n < max_steps do
    on_step t !n;
    ignore (step t);
    incr n;
    if t.kbd_wait then begin
      incr starve;
      match Queue.peek_opt queue with
      | Some k when !starve >= starve_threshold && !n >= k.not_before ->
        ignore (Queue.pop queue);
        push_key t k.word;
        on_key k.word !n;
        starve := 0
      | _ -> ()
    end
    else starve := 0
  done;
  !n

(* ASCII 한 글자를 BIOS 링에 넣는다. 스캔 코드는 US 자판 기준이고,
   글자 키가 아닌 것(방향키 등)은 워드를 직접 넣어야 한다. *)
let scancode_of_ascii c =
  match c with
  | '\r' | '\n' -> 0x1C
  | '\027' -> 0x01
  | ' ' -> 0x39
  | '\b' -> 0x0E
  | '\t' -> 0x0F
  | 'a' .. 'z' | 'A' .. 'Z' ->
    let letters = "qwertyuiopasdfghjklzxcvbnm" in
    let codes =
      [| 0x10; 0x11; 0x12; 0x13; 0x14; 0x15; 0x16; 0x17; 0x18; 0x19;
         0x1E; 0x1F; 0x20; 0x21; 0x22; 0x23; 0x24; 0x25; 0x26;
         0x2C; 0x2D; 0x2E; 0x2F; 0x30; 0x31; 0x32 |]
    in
    (match String.index_opt letters (Char.lowercase_ascii c) with
     | Some i -> codes.(i)
     | None -> 0)
  | '1' .. '9' -> 0x02 + (Char.code c - Char.code '1')
  | '0' -> 0x0B
  | _ -> 0

(* 이름이 있는 키 — BIOS 가 INT 16h 로 돌려주는 워드(스캔 코드 lsl 8 lor
   ASCII) 그대로다. 글자가 아닌 키는 ASCII 자리가 0 이고, 그래서 게스트가
   "확장 키" 로 알아본다. 표에 없는 키는 이름으로 부를 수 없다 — 워드를
   직접 넣는 길([push_key])은 그대로 열려 있다. *)
let named_keys =
  [ ("up", 0x4800); ("down", 0x5000); ("left", 0x4B00); ("right", 0x4D00);
    ("home", 0x4700); ("end", 0x4F00); ("pgup", 0x4900); ("pgdn", 0x5100);
    ("insert", 0x5200); ("delete", 0x5300);
    ("enter", 0x1C0D); ("return", 0x1C0D); ("esc", 0x011B);
    ("space", 0x3920); ("tab", 0x0F09); ("backtab", 0x0F00);
    ("backspace", 0x0E08);
    ("f1", 0x3B00); ("f2", 0x3C00); ("f3", 0x3D00); ("f4", 0x3E00);
    ("f5", 0x3F00); ("f6", 0x4000); ("f7", 0x4100); ("f8", 0x4200);
    ("f9", 0x4300); ("f10", 0x4400) ]

let key_of_string name =
  let lower = String.lowercase_ascii (String.trim name) in
  match List.assoc_opt lower named_keys with
  | Some w -> Ok w
  | None ->
    if String.length name = 1 then begin
      let c = name.[0] in
      let sc = scancode_of_ascii c in
      (* 스캔 코드가 0 이면 US 자판에 그 글자의 자리가 없다. 자리 없는
         글자를 0 번 스캔 코드로 넣으면 게스트가 엉뚱한 키로 읽는다. *)
      if sc = 0 then Error (Printf.sprintf "no key for character %C" c)
      else Ok ((sc lsl 8) lor Char.code c)
    end
    else
      Error
        (Printf.sprintf "unknown key %S — name one of [%s] or one character"
           name
           (String.concat " " (List.map fst named_keys)))

let key_to_string word =
  match List.find_opt (fun (_, w) -> w = word) named_keys with
  | Some (n, _) -> n
  | None ->
    let a = word land 0xff in
    if a >= 0x20 && a < 0x7f then String.make 1 (Char.chr a)
    else Printf.sprintf "%04x" word

let push_ascii t c =
  let ascii = if c = '\n' then Char.code '\r' else Char.code c in
  push_key t ((scancode_of_ascii c lsl 8) lor ascii)

let type_string t s = String.iter (fun c -> push_ascii t c) s

let kbd_waiting t = t.kbd_wait
let input_requests t = t.kbd_requests

(* ---------- 화면 ---------- *)

let frame_dims t = Dos_video.dims t.video
let screen_text t = Dos_render.text_ascii t.mem ~cols:(cols t)
let screen_text_utf8 t = Dos_render.text_utf8 t.mem ~cols:(cols t)

let frame_rgb t =
  let pal = Dos_ports.palette t.ports in
  match Dos_video.kind t.video with
  | Dos_video.Text -> Dos_render.rgb_text t.mem ~cols:(cols t)
  | Dos_video.Cga4 ->
    Dos_render.rgb_cga4 t.mem ~mode:(Dos_video.mode t.video)
      ~color_select:(Dos_video.cga_color_select t.video)
  | Dos_video.Cga2 -> Dos_render.rgb_cga2 t.mem
  | Dos_video.Planar ->
    let w, h = Dos_video.dims t.video in
    Dos_render.rgb_planar (Dos_video.planes t.video) ~w ~h
      ~attr:(Dos_video.attr_palette t.video) ~pal
  | Dos_video.Linear256 -> Dos_render.rgb_vga13 t.mem pal

let frame_ppm t =
  let w, h = frame_dims t in
  Printf.sprintf "P6\n%d %d\n255\n%s" w h (frame_rgb t)

(* ---------- 관측·주입 ---------- *)

let cpu_of t = t.cpu
let psp_seg_of t = t.psp_seg
let mem_read t a = rd8 t a
let mem_write t a v = wr8 t a v
let tick_count t = rd16 t 0x46C lor (rd16 t 0x46E lsl 16)
let speaker_on t = Dos_ports.speaker_on t.ports
let video_mode t = Dos_video.mode t.video
let screen_digest t = Dos_video.screen_digest t.video
let pixel t ~x ~y = Dos_video.get_pixel t.video ~x ~y

let set_clock t ~year ~month ~day ~hour ~minute ~second =
  t.epoch_year <- year;
  t.epoch_month <- month;
  t.epoch_day <- day;
  t.epoch_hour <- hour;
  t.epoch_min <- minute;
  t.epoch_sec <- second

let attach_mouse t = t.mouse.mouse_present <- true

let set_mouse t ~x ~y ~buttons =
  t.mouse.mouse_dx <- t.mouse.mouse_dx + (x - t.mouse.mouse_x);
  t.mouse.mouse_dy <- t.mouse.mouse_dy + (y - t.mouse.mouse_y);
  t.mouse.mouse_x <- x;
  t.mouse.mouse_y <- y;
  t.mouse.mouse_buttons <- buttons

let free_paras t = Dos_dos.largest_free t
