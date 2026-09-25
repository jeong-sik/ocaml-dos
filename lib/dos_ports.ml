(* 게스트가 보는 하드웨어 포트. 실기에서 이 포트가 무엇을 돌려주는지가
   게임의 진행 조건이 된다 — 값이 변하지 않으면 변화를 기다리는 루프가
   영원히 안 풀린다.

   시간에 기대는 답(PIT 카운터, CGA 재주사 비트, 스피커 리프레시 비트)은
   벽시계가 아니라 CPU 누적 사이클에서 만든다. 같은 이미지 + 같은 입력 =
   같은 실행이라는 계약을 포트도 지켜야 한다. *)

(* 4.77MHz CPU 와 1.193182MHz PIT 의 비 — 반올림해서 4. PIT 한 틱이
   CPU 사이클 4 다. 분주비 65536 이면 262144 사이클 = 18.2Hz. *)
let cpu_cycles_per_pit_tick = 4

(* 포트 0x61 bit4(리프레시)가 뒤집히는 주기. 실기 15.09kHz 는 CPU
   사이클로 약 158. 지연 루프가 이 비트를 세며 시간을 잰다. *)
let refresh_toggle_cycles = 158

(* YM3812(OPL2) 타이머 한 스텝 — T1 은 80µs, T2 는 320µs 를 CPU
   사이클(4.77MHz)로 환산한 값. *)
let opl_t1_step = 382
let opl_t2_step = 1527

let crtc_regs = 25
let dac_entries = 256

type t = {
  mem : Bytes.t;
  video : Dos_video.t;          (** 비디오 어댑터의 포트는 여기로 넘긴다 *)
  mutable now : int;                    (** CPU 누적 사이클 *)
  (* PIT 채널 0 *)
  mutable pit0_divisor : int;           (** 0 은 65536 을 뜻한다 *)
  mutable pit0_write_hi : bool;         (** lo/hi 두 바이트 쓰기의 순번 *)
  mutable pit0_read_hi : bool;
  mutable pit0_latched : int option;
  mutable pit0_access : int;            (** 1=lo 2=hi 3=lo/hi *)
  (* PIC *)
  mutable pic_mask : int;               (** IMR — bit0 이 IRQ0 *)
  (* VGA DAC *)
  pal : (int * int * int) array;
  mutable dac_write_index : int;
  mutable dac_read_index : int;
  mutable dac_phase : int;              (** 0=R 1=G 2=B *)
  mutable dac_mask : int;
  (* CRTC *)
  mutable crtc_index : int;
  crtc : int array;
  (* 기타 *)
  mutable scancode : int;
  mutable port_b : int;                 (** 0x61 래치 *)
  mutable cga_status : bool;
  mutable mode_control : int;           (** 0x3D8 *)
  mutable color_select : int;           (** 0x3D9 *)
  mutable cmos_index : int;
  (* OPL2 — 감지 계약만 지닌다(FMDRV.COM 실측). 소리는 없다. *)
  mutable opl_reg : int;                (** 0x388 로 고른 레지스터 *)
  mutable opl_t1_count : int;           (** 레지스터 02h *)
  mutable opl_t2_count : int;           (** 레지스터 03h *)
  mutable opl_t1_at : int;              (** 만료 사이클 — 0 은 미무장 *)
  mutable opl_t2_at : int;
  mutable opl_t1_exp : bool;
  mutable opl_t2_exp : bool;
}

(* 기본 VGA 256색: 0-15 CGA, 16-31 회색 램프, 32-247 6x6x6 RGB 큐브,
   248-255 검정. 게임이 DAC 을 다시 쓰지 않을 때의 근삿값이다 —
   표준 DAC 초기값과 미세한 차이가 있을 수 있다. *)
let cga_palette = [|
  (0x00, 0x00, 0x00); (0x00, 0x00, 0xAA); (0x00, 0xAA, 0x00); (0x00, 0xAA, 0xAA);
  (0xAA, 0x00, 0x00); (0xAA, 0x00, 0xAA); (0xAA, 0x55, 0x00); (0xAA, 0xAA, 0xAA);
  (0x55, 0x55, 0x55); (0x55, 0x55, 0xFF); (0x55, 0xFF, 0x55); (0x55, 0xFF, 0xFF);
  (0xFF, 0x55, 0x55); (0xFF, 0x55, 0xFF); (0xFF, 0xFF, 0x55); (0xFF, 0xFF, 0xFF) |]

(* DAC 은 채널당 6비트다. CGA 색의 8비트 값을 옮길 때 나누는 수는
   255 이어야 한다 — 0xAA 로 나누면 밝은 색(0xFF)이 94 가 되어 범위를
   넘고, 렌더러가 그 값을 8비트로 되돌릴 때 380 이 나온다. *)
let default_vga_pal i =
  if i < 16 then
    let r, g, b = cga_palette.(i) in
    ((r * 63) / 255, (g * 63) / 255, (b * 63) / 255)
  else if i < 32 then
    let v = ((i - 16) * 63) / 15 in (v, v, v)
  else if i < 248 then begin
    let j = i - 32 in
    let r = j / 36 and g = (j / 6) mod 6 and b = j mod 6 in
    (r * 63 / 5, g * 63 / 5, b * 63 / 5)
  end
  else (0, 0, 0)

let create ~mem ~video =
  {
    mem;
    video;
    now = 0;
    pit0_divisor = 0;
    pit0_write_hi = false;
    pit0_read_hi = false;
    pit0_latched = None;
    pit0_access = 3;
    pic_mask = 0;
    pal = Array.init dac_entries default_vga_pal;
    dac_write_index = 0;
    dac_read_index = 0;
    dac_phase = 0;
    dac_mask = 0xFF;
    crtc_index = 0;
    crtc = Array.make crtc_regs 0;
    scancode = 0;
    port_b = 0;
    cga_status = false;
    mode_control = 0x29;
    color_select = 0;
    cmos_index = 0;
    opl_reg = 0;
    opl_t1_count = 0;
    opl_t2_count = 0;
    opl_t1_at = 0;
    opl_t2_at = 0;
    opl_t1_exp = false;
    opl_t2_exp = false;
  }

(* 모드를 세우면 BIOS 가 DAC 을 기본값으로 다시 싣는다. 앞 프로그램이
   페이드아웃으로 DAC 을 전부 0 으로 만들고 끝나도, 다음 프로그램이
   모드만 세우고 팔레트를 안 건드리면 기본색이 보인다 — 실기 계약이다
   (삼국지3: OPEN.EXE 가 검게 페이드한 뒤 AX=0012h 로 끝나고, MAIN.EXE
   의 카피프로텍션 화면은 DAC 을 쓰지 않는다). 쓰기 순번과 PEL 마스크도
   처음 상태로 돌린다. *)
let reset_dac t =
  Array.iteri (fun i _ -> t.pal.(i) <- default_vga_pal i) t.pal;
  t.dac_write_index <- 0;
  t.dac_read_index <- 0;
  t.dac_phase <- 0;
  t.dac_mask <- 0xFF

let set_now t n = t.now <- n
let palette t = t.pal
let set_scancode t sc = t.scancode <- sc land 0xff
let irq0_masked t = t.pic_mask land 1 <> 0
let speaker_on t = t.port_b land 0x03 = 0x03

let divisor t = if t.pit0_divisor = 0 then 0x10000 else t.pit0_divisor

let cycles_per_tick t = divisor t * cpu_cycles_per_pit_tick

(* 채널 0 은 되풀이 모드로 센다: 분주비에서 지난 만큼을 뺀 나머지. *)
let pit0_count t =
  let d = divisor t in
  let elapsed = (t.now / cpu_cycles_per_pit_tick) mod d in
  (d - elapsed) land 0xffff

(* CRTC 커서 레지스터(0x0E/0x0F)는 화면 시작부터의 글자 수다. BIOS 는
   같은 값을 BDA 0x450/0x451 에 행·열로 둔다 — 커서의 진실 원천이
   둘로 갈라지지 않게 여기서 같이 맞춘다. *)
let sync_cursor_to_bda t =
  let linear = ((t.crtc.(0x0E) land 0xff) lsl 8) lor (t.crtc.(0x0F) land 0xff) in
  let cols = max 1 (Dos_video.text_cols t.video) in
  Bytes.set t.mem 0x450 (Char.chr (linear mod cols));
  Bytes.set t.mem 0x451 (Char.chr ((linear / cols) land 0xff))

let port_in t p =
  match Dos_video.port_in t.video p with
  | Some v -> v
  | None ->
  match p land 0xffff with
  | 0x21 -> t.pic_mask
  | 0x40 ->
    let v = match t.pit0_latched with Some v -> v | None -> pit0_count t in
    (match t.pit0_access with
     | 1 -> t.pit0_latched <- None; v land 0xff
     | 2 -> t.pit0_latched <- None; (v lsr 8) land 0xff
     | _ ->
       if t.pit0_read_hi then begin
         t.pit0_read_hi <- false;
         t.pit0_latched <- None;
         (v lsr 8) land 0xff
       end else begin
         t.pit0_read_hi <- true;
         v land 0xff
       end)
  | 0x60 -> t.scancode
  | 0x61 ->
    (* bit4 는 메모리 리프레시 — 실기에서 쉬지 않고 뒤집힌다. 고정값을
       주면 이 비트를 세는 지연 루프가 안 끝난다. *)
    let refresh = (t.now / refresh_toggle_cycles) land 1 in
    (t.port_b land 0xEF) lor (refresh lsl 4)
  | 0x64 -> 0x14                     (* 출력 버퍼 빔, 입력 버퍼 빔 *)
  | 0x70 -> t.cmos_index
  | 0x71 -> 0x00
  (* 조이스틱 미장착: 축 비트 0(방전) 이면 축 카운트 루프가 즉시
     통과한다. 0xFF 를 주면 카운터가 랩할 때까지 갇힌다(ZZT 실측). *)
  | 0x201 -> 0x00
  | 0x3C6 -> t.dac_mask
  | 0x3C7 -> if t.dac_phase = 0 then 0x00 else 0x03
  | 0x3C8 -> t.dac_write_index
  | 0x3C9 ->
    let r, g, b = t.pal.(t.dac_read_index land 0xff) in
    let v = match t.dac_phase with 0 -> r | 1 -> g | _ -> b in
    t.dac_phase <- t.dac_phase + 1;
    if t.dac_phase > 2 then begin
      t.dac_phase <- 0;
      t.dac_read_index <- (t.dac_read_index + 1) land 0xff
    end;
    v land 0x3f
  | 0x3B4 | 0x3D4 -> t.crtc_index
  | 0x3B5 | 0x3D5 ->
    if t.crtc_index < crtc_regs then t.crtc.(t.crtc_index) else 0xff
  | 0x3D8 -> t.mode_control
  | 0x3D9 -> t.color_select
  | 0x3BA | 0x3DA ->
    (* 재주사 상태: 읽을 때마다 bit0(디스플레이 인에이블)을 뒤집는다.
       CRT 루틴이 VRAM 직접 쓰기 전에 "clear 대기 → set 대기" 상승
       에지를 본다 — 고정값이면 한쪽 대기가 안 풀린다(ZZT 실측).
       bit3(수직 귀선)은 그보다 느리게 켜진다. *)
    (* 실기에서는 이 레지스터를 읽으면 속성 컨트롤러의 index/data
       번갈이도 index 로 돌아간다. 그 부수효과에 기대는 코드가 있다. *)
    Dos_video.reset_attr_flip t.video;
    t.cga_status <- not t.cga_status;
    let vsync = if (t.now / 70000) land 7 = 0 then 0x08 else 0x00 in
    (if t.cga_status then 0x01 else 0x00) lor vsync
  | 0x388 ->
    (* 만료는 읽는 순간에 갱신한다 — 감지 루틴은 쓰기와 읽기 사이에
       지연 루프를 두고 그 시간에 타이머가 끝나기를 기대한다. 만료한
       적 있으면 상위 두 비트(0xC0)와 타이머 플래그를 함께 띄운다. *)
    if t.opl_t1_at > 0 && t.now >= t.opl_t1_at then begin
      t.opl_t1_exp <- true;
      t.opl_t1_at <- 0                   (* 만료는 한 번 — 스탬프를 소비한다 *)
    end;
    if t.opl_t2_at > 0 && t.now >= t.opl_t2_at then begin
      t.opl_t2_exp <- true;
      t.opl_t2_at <- 0
    end;
    let flags =
      (if t.opl_t1_exp then 1 else 0) lor (if t.opl_t2_exp then 2 else 0)
    in
    if flags <> 0 then 0xC0 lor flags else 0x00
  | _ -> 0xff

let port_out t p v =
  let v = v land 0xff in
  if Dos_video.port_out t.video p v then ()
  else
  match p land 0xffff with
  | 0x20 -> ()                        (* EOI — 우선순위 모델이 없다 *)
  | 0x21 -> t.pic_mask <- v
  | 0x40 ->
    (match t.pit0_access with
     | 1 -> t.pit0_divisor <- (t.pit0_divisor land 0xff00) lor v
     | 2 -> t.pit0_divisor <- (t.pit0_divisor land 0x00ff) lor (v lsl 8)
     | _ ->
       if t.pit0_write_hi then begin
         t.pit0_divisor <- (t.pit0_divisor land 0x00ff) lor (v lsl 8);
         t.pit0_write_hi <- false
       end else begin
         t.pit0_divisor <- (t.pit0_divisor land 0xff00) lor v;
         t.pit0_write_hi <- true
       end)
  | 0x43 ->
    (* 제어 워드: bit7-6 채널, bit5-4 접근 모드(0=래치). 채널 0 만
       센다 — 채널 1(리프레시)·2(스피커)는 소리를 안 내므로 무시한다. *)
    if v lsr 6 = 0 then begin
      let access = (v lsr 4) land 3 in
      if access = 0 then t.pit0_latched <- Some (pit0_count t)
      else begin
        t.pit0_access <- access;
        t.pit0_write_hi <- false;
        t.pit0_read_hi <- false;
        t.pit0_latched <- None
      end
    end
  | 0x61 -> t.port_b <- v
  | 0x70 -> t.cmos_index <- v
  | 0x3C6 -> t.dac_mask <- v
  | 0x3C7 -> t.dac_read_index <- v; t.dac_phase <- 0
  | 0x3C8 -> t.dac_write_index <- v; t.dac_phase <- 0
  | 0x3C9 ->
    let i = t.dac_write_index land 0xff in
    let r, g, b = t.pal.(i) in
    let c = v land 0x3f in
    t.pal.(i) <-
      (match t.dac_phase with
       | 0 -> (c, g, b)
       | 1 -> (r, c, b)
       | _ -> (r, g, c));
    t.dac_phase <- t.dac_phase + 1;
    if t.dac_phase > 2 then begin
      t.dac_phase <- 0;
      t.dac_write_index <- (i + 1) land 0xff
    end
  | 0x3B4 | 0x3D4 -> t.crtc_index <- v
  | 0x3B5 | 0x3D5 ->
    if t.crtc_index < crtc_regs then begin
      t.crtc.(t.crtc_index) <- v;
      if t.crtc_index = 0x0E || t.crtc_index = 0x0F then sync_cursor_to_bda t;
      (* 커서 모양(시작·끝 스캔라인)도 BIOS 가 BDA 에 둔다 *)
      if t.crtc_index = 0x0A then Bytes.set t.mem 0x461 (Char.chr v);
      if t.crtc_index = 0x0B then Bytes.set t.mem 0x460 (Char.chr v)
    end
  | 0x3D8 -> t.mode_control <- v
  | 0x3D9 -> t.color_select <- v; Dos_video.set_cga_color_select t.video v
  | 0x388 -> t.opl_reg <- v              (* 레지스터 선택 *)
  | 0x389 ->
    (match t.opl_reg with
     | 0x02 -> t.opl_t1_count <- v
     | 0x03 -> t.opl_t2_count <- v
     | 0x04 ->
       (* bit0/1 = 타이머 시작(플래그를 지우고 다시 만다), bit7 = 플래그
         리셋(감지 루틴이 reg4=0x80 을 쓰는 쪽). bit5/6(마스크)는 플래그가
         아니라 IRQ 만 가린다 — 감지 루틴은 마스크를 건 채 플래그를
         기대한다. *)
       if v land 0x80 <> 0 then begin
         t.opl_t1_exp <- false;
         t.opl_t2_exp <- false
       end;
       if v land 1 <> 0 then begin
         t.opl_t1_exp <- false;
         t.opl_t1_at <- t.now + (opl_t1_step * (t.opl_t1_count + 1))
       end;
       if v land 2 <> 0 then begin
         t.opl_t2_exp <- false;
         t.opl_t2_at <- t.now + (opl_t2_step * (t.opl_t2_count + 1))
       end
     | _ -> ())                        (* 음색 레지스터 — 소리가 없어 무시 *)
  | _ -> ()

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

(* One range per field, named by both [write_state] and [read_state], so a
   value the writer accepts is one the reader accepts. *)
let cycle = C.count
let pit_access = C.range 0 3
let dac_phase_r = C.range 0 2

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. Keep [write_state] and [read_state] in one order. *)
let write_state w t =
  let { mem = _; video = _ (* the machine's: it writes them itself *);
        now; pit0_divisor; pit0_write_hi; pit0_read_hi; pit0_latched;
        pit0_access; pic_mask; pal; dac_write_index; dac_read_index;
        dac_phase; dac_mask; crtc_index; crtc; scancode; port_b; cga_status;
        mode_control; color_select; cmos_index; opl_reg; opl_t1_count;
        opl_t2_count; opl_t1_at; opl_t2_at; opl_t1_exp; opl_t2_exp } = t
  in
  let put what r v = C.put w ~what r v in
  put "ports.now" cycle now;
  put "pit0_divisor" C.word pit0_divisor;
  C.put_bool w pit0_write_hi;
  C.put_bool w pit0_read_hi;
  (match pit0_latched with
   | None -> C.put_bool w false
   | Some v -> C.put_bool w true; put "pit0_latched" C.word v);
  put "pit0_access" pit_access pit0_access;
  put "pic_mask" C.byte pic_mask;
  put "DAC size" C.count (Array.length pal);
  Array.iter
    (fun (r, g, b) -> put "DAC red" C.byte r; put "DAC green" C.byte g; put "DAC blue" C.byte b)
    pal;
  put "dac_write_index" C.byte dac_write_index;
  put "dac_read_index" C.byte dac_read_index;
  put "dac_phase" dac_phase_r dac_phase;
  put "dac_mask" C.byte dac_mask;
  put "crtc_index" C.byte crtc_index;
  C.put_int_array w ~what:"crtc" C.byte crtc;
  put "scancode" C.byte scancode;
  put "port_b" C.byte port_b;
  C.put_bool w cga_status;
  put "mode_control" C.byte mode_control;
  put "color_select" C.byte color_select;
  put "cmos_index" C.byte cmos_index;
  put "opl_reg" C.byte opl_reg;
  put "opl_t1_count" C.byte opl_t1_count;
  put "opl_t2_count" C.byte opl_t2_count;
  put "opl_t1_at" cycle opl_t1_at;
  put "opl_t2_at" cycle opl_t2_at;
  C.put_bool w opl_t1_exp;
  C.put_bool w opl_t2_exp

let read_state r t =
  let get what rng = C.get r ~what rng in
  t.now <- get "ports.now" cycle;
  t.pit0_divisor <- get "pit0_divisor" C.word;
  t.pit0_write_hi <- C.get_bool r;
  t.pit0_read_hi <- C.get_bool r;
  t.pit0_latched <- (if C.get_bool r then Some (get "pit0_latched" C.word) else None);
  t.pit0_access <- get "pit0_access" pit_access;
  t.pic_mask <- get "pic_mask" C.byte;
  if get "DAC size" C.count <> Array.length t.pal then C.fail "DAC size differs";
  Array.iteri
    (fun i _ ->
      let red = get "DAC red" C.byte in
      let green = get "DAC green" C.byte in
      let blue = get "DAC blue" C.byte in
      t.pal.(i) <- (red, green, blue))
    t.pal;
  t.dac_write_index <- get "dac_write_index" C.byte;
  t.dac_read_index <- get "dac_read_index" C.byte;
  t.dac_phase <- get "dac_phase" dac_phase_r;
  t.dac_mask <- get "dac_mask" C.byte;
  t.crtc_index <- get "crtc_index" C.byte;
  C.fill_int_array r ~what:"crtc" C.byte t.crtc;
  t.scancode <- get "scancode" C.byte;
  t.port_b <- get "port_b" C.byte;
  t.cga_status <- C.get_bool r;
  t.mode_control <- get "mode_control" C.byte;
  t.color_select <- get "color_select" C.byte;
  t.cmos_index <- get "cmos_index" C.byte;
  t.opl_reg <- get "opl_reg" C.byte;
  t.opl_t1_count <- get "opl_t1_count" C.byte;
  t.opl_t2_count <- get "opl_t2_count" C.byte;
  t.opl_t1_at <- get "opl_t1_at" cycle;
  t.opl_t2_at <- get "opl_t2_at" cycle;
  t.opl_t1_exp <- C.get_bool r;
  t.opl_t2_exp <- C.get_bool r
