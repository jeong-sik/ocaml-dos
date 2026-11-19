(* 게스트가 보는 하드웨어 포트. 실기에서 이 포트가 무엇을 돌려주는지가
   게임의 진행 조건이 된다 — 값이 변하지 않으면 변화를 기다리는 루프가
   영원히 안 풀린다.

   시간에 기대는 답(PIT 카운터, CGA 재주사 비트, 스피커 리프레시 비트)은
   벽시계가 아니라 CPU 누적 사이클에서 만든다. 같은 이미지 + 같은 입력 =
   같은 실행이라는 계약을 포트도 지켜야 한다. *)

(* ---------- CPU 속도 ----------

   게스트가 시간을 재는 길(IRQ0, PIT 카운터, 리프레시 비트, 재주사 비트,
   OPL 타이머, 시각)은 모두 실제 시간에 묶여 있다. CPU 가 빠르면 같은
   시간에 사이클이 더 돈다. 그래서 속도를 "PIT 한 틱(1/1193182초)에 CPU
   사이클 몇 개" 라는 정수 하나로 둔다. 정수라서 PIT 틱 경계가 사이클
   경계와 어긋나지 않는다. IBM PC 는 14.31818MHz 수정을 CPU 는 3 으로,
   PIT 는 12 로 나눠 쓰므로 4 다. 분주비 65536 이면 262144 사이클 = 18.2Hz.

   사이클 값은 8086 명령표에서 온다([Cpu86]). 그래서 속도는 "8086 으로
   치면 몇 MHz" 다. 같은 MHz 의 286·386 은 한 사이클에 일을 더 한다. *)

type speed = int

let pit_hz = 1_193_182
let pc_speed = 4

(* MHz 를 정수 배수로 바꿀 때 float → int 변환이 넘치지 않게 둔 상한
   (약 1.19GHz). *)
let max_speed = 1000

let speed_hz k = k * pit_hz

let speed_of_mhz mhz =
  let range () =
    Printf.sprintf "%g MHz is outside %.2f..%.0f MHz" mhz
      (float_of_int (speed_hz 1) /. 1e6) (float_of_int (speed_hz max_speed) /. 1e6)
  in
  match Float.classify_float mhz with
  | FP_nan | FP_infinite -> Error (range ())
  | FP_normal | FP_subnormal | FP_zero ->
    let k = Float.round (mhz *. 1e6 /. float_of_int pit_hz) in
    if k < 1. || k > float_of_int max_speed then Error (range ()) else Ok (int_of_float k)

(* 아래 시간 값은 PC 의 4.77MHz 사이클로 적었다. 기계의 속도에 맞춰
   늘리는 일은 [at_speed] 가 한다. *)

(* 포트 0x61 bit4(리프레시)가 뒤집히는 주기. AT 이후 기계에서 이 비트는
   PIT 채널 1 의 리프레시 요청마다 뒤집힌다. 채널 1 의 분주비는 18 이라
   PIT 18 틱 = 15.0857µs(216 / 14.31818MHz) 마다, CPU 사이클로 18 × 4 = 72
   다(PCTIM003 7.4·7.37 절). 지연 루프가 이 비트를 세며 시간을 잰다. *)
let refresh_toggle_cycles = 72

(* 포트 0x61 의 비트. 읽을 때 bit4 는 리프레시, bit5 는 PIT 채널 2 의 OUT
   이다. 나머지 비트는 쓴 값을 돌려준다. *)
let gate2_bit = 0x01
let refresh_bit = 0x10
let out2_bit = 0x20

(* YM3812(OPL2) 타이머 한 스텝 — T1 은 80µs, T2 는 320µs 를 CPU
   사이클(4.77MHz)로 환산한 값. *)
let opl_t1_step = 382
let opl_t2_step = 1527

(* 포트 201h: 버튼 넷이 떼어지고(bit4~7 = 1) 축 넷이 방전된(bit0~3 = 0)
   상태. *)
let game_port_released = 0xF0

let dac_entries = 256

(* OPL2 타이머 하나의 플래그. 마스크된 타이머는 넘쳐도 플래그를 세우지
   않고, 마스크를 걸면 선 플래그도 지운다(DOSBox Staging opl.cpp
   Timer::SetMask). 그래서 마스크된 채 선 플래그는 없다. *)
type opl_flag = Opl_clear | Opl_raised | Opl_masked

type t = {
  mem : Bytes.t;
  video : Dos_video.t;          (** 비디오 어댑터의 포트는 여기로 넘긴다 *)
  mutable speed : speed;  (** PIT 한 틱의 CPU 사이클. 스냅숏을 되살릴 때만 바뀐다 *)
  mutable now : int;                    (** CPU 누적 사이클 *)
  pit2 : Dos_pit.t;                     (** PIT 채널 2 — GATE 는 0x61 bit0 *)
  pit0 : Dos_pit.t;                     (** PIT 채널 0 — OUT 상승이 IRQ0 *)
  mutable irq0_at : int option;         (** 다음 OUT0 상승의 CPU 사이클 *)
  pic : Dos_pic.t;
  dma : Dos_dma.t;                      (** 포트 00h-0Fh 와 페이지 레지스터 *)
  mutable sb : Dos_sb.t option;         (** 220h 의 Sound Blaster. 스냅숏이 꽂고 뺀다 *)
  kbc : Dos_kbc.t;                      (** 포트 0x60·0x64 *)
  (* VGA DAC *)
  pal : (int * int * int) array;
  mutable dac_write_index : int;
  mutable dac_read_index : int;
  mutable dac_phase : int;              (** 0=R 1=G 2=B *)
  mutable dac_mask : int;
  (* CRTC *)
  (* 기타 *)
  mutable port_b : int;                 (** 0x61 래치 *)
  mutable cga_status : bool;
  mutable mode_control : int;           (** 0x3D8 *)
  mutable color_select : int;           (** 0x3D9 *)
  cmos : Dos_cmos.t;  (** 포트 70h·71h *)
  (* OPL2 — 타이머 둘과 상태 포트만 있다. 소리는 없다. *)
  mutable opl_reg : int;                (** 0x388 로 고른 레지스터 *)
  mutable opl_t1_count : int;           (** 레지스터 02h *)
  mutable opl_t2_count : int;           (** 레지스터 03h *)
  mutable opl_t1_at : int option;       (** 다음 넘침 사이클. None 이면 멈춤 *)
  mutable opl_t2_at : int option;
  mutable opl_t1_exp : opl_flag;
  mutable opl_t2_exp : opl_flag;
  opl : Dos_opl.t;                      (** 음색 레지스터. 02h-04h 는 위의 타이머 몫이다 *)
  mutable mpu : Dos_mpu401.t option;    (** 330h 의 MPU-401. 스냅숏이 꽂고 뺀다 *)
  mutable audio : Dos_audio.t option;
  (** RFC 0006 의 소리 표본. 켠 기계만 있다. 기계 상태가 아니라서 스냅숏에
      들지 않는다. *)
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

(* 읽기-되돌림 명령의 비트(82C54 데이터시트 Figure 10) *)
let read_back_no_count = 0x20
let read_back_no_status = 0x10
let read_back_channel2 = 0x08
let read_back_channel0 = 0x02

(* 채널 0 의 GATE 는 PC 에서 늘 높다. *)
let channel0_gate = true

(* IBM 호환 BIOS 는 POST 에서 채널 0 을 제어 워드 36h(하위·상위 바이트,
   모드 3, 바이너리)와 카운트 0(= 65536)으로 건다(skiselev/8088_bios
   src/bios.asm "channel 0, LSB & MSB, mode 3, binary", DOSBox-X
   src/hardware/timer.cpp 의 pit[0].mode = 3). SeaBIOS 는 모드 2 를 쓴다.
   기계는 그 POST 가 끝난 상태로, 틱 0 에 카운트를 실은 채 켜진다. *)
let bios_channel0_control = 0x36
let bios_channel0_count = 0

let channel0_after_bios () =
  let pit = Dos_pit.create () in
  Dos_pit.control pit ~tick:0 bios_channel0_control;
  Dos_pit.write pit ~tick:0 ~gate:channel0_gate (bios_channel0_count land 0xff);
  Dos_pit.write pit ~tick:0 ~gate:channel0_gate (bios_channel0_count lsr 8);
  pit

(* [speed] 는 PIT 한 틱의 CPU 사이클이다. *)
let rise_cycle ~speed pit ~tick =
  Option.map (fun r -> r * speed) (Dos_pit.next_rise pit ~tick)

let create ~speed ~mpu401 ~mem ~video ~sound_blaster ~cmos =
  let pit0 = channel0_after_bios () in
  {
    mem;
    video;
    speed;
    now = 0;
    pit2 = Dos_pit.create ();
    pit0;
    irq0_at = rise_cycle ~speed pit0 ~tick:0;
    pic = Dos_pic.create ();
    dma = Dos_dma.create ();
    sb = (if sound_blaster then Some (Dos_sb.create ()) else None);
    kbc = Dos_kbc.create ();
    pal = Array.init dac_entries default_vga_pal;
    dac_write_index = 0;
    dac_read_index = 0;
    dac_phase = 0;
    dac_mask = 0xFF;
    port_b = 0;
    cga_status = false;
    mode_control = 0x29;
    color_select = 0;
    cmos;
    opl_reg = 0;
    opl_t1_count = 0;
    opl_t2_count = 0;
    opl_t1_at = None;
    opl_t2_at = None;
    opl_t1_exp = Opl_clear;
    opl_t2_exp = Opl_clear;
    opl = Dos_opl.create ();
    mpu = (if mpu401 then Some (Dos_mpu401.create ()) else None);
    audio = None;
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

let cpu_hz t = speed_hz t.speed
let at_speed t pc_cycles = pc_cycles * t.speed / pc_speed

(* 포트 0x3DA bit3(수직 귀선)은 VGA 한 프레임에 한 번 켜진다. 줄 수와
   귀선 자리는 SeaBIOS stdvgamodes.c 의 모드별 CRTC 표에서 셌다. 세는
   법은 DOSBox vga_draw.cpp 와 같다.
   - 프레임 줄 수 = 수직 전체(06h, 07h 비트 0·5) + 2
   - 귀선 시작 = 10h(07h 비트 2·7)
   - 귀선 끝 = 시작 + ((11h 아래 4 비트 - 시작) land 0Fh)
   한 줄은 800 도트 / 25.175MHz = 31.78µs 다. 9 도트 글자의 텍스트
   모드는 900 도트 / 28.322MHz 라서 0.0005% 짧지만 같은 길이로 둔다.
   시간 값이 줄 단위라서 [at_speed] 대신 [cpu_hz] 로 사이클을 낸다. *)
type vga_frame = { lines : int; retrace_start : int; retrace_lines : int }

let vga_dot_hz = 25_175_000
let vga_line_dots = 800

(* 200·400 줄 모드: VT 1BFh, VRS 19Ch, VRE Eh. 70.09Hz *)
let frame_400 = { lines = 449; retrace_start = 412; retrace_lines = 2 }

(* 350 줄 모드(0Fh·10h): VT 1BFh, VRS 183h, VRE 5h. 70.09Hz *)
let frame_350 = { lines = 449; retrace_start = 387; retrace_lines = 2 }

(* 480 줄 모드(11h·12h): VT 20Bh, VRS 1EAh, VRE Ch. 59.94Hz *)
let frame_480 = { lines = 525; retrace_start = 490; retrace_lines = 2 }

(* 모르는 모드는 [Dos_video.spec] 처럼 텍스트(400 줄)로 본다. *)
let frame_of_mode = function
  | 0x0F | 0x10 -> frame_350
  | 0x11 | 0x12 -> frame_480
  | _ -> frame_400

(* 프레임 첫 줄 첫머리부터 [line] 번째 줄까지의 CPU 사이클. 프레임은
   사이클 0 에서 시작한다. *)
let line_cycles t line = cpu_hz t * line * vga_line_dots / vga_dot_hz

let in_vertical_retrace t =
  let f = frame_of_mode (Dos_video.mode t.video) in
  let pos = t.now mod line_cycles t f.lines in
  pos >= line_cycles t f.retrace_start
  && pos < line_cycles t (f.retrace_start + f.retrace_lines)


(* ---------- OPL2 타이머 ---------- *)

(* 상태(0x388 읽기)와 레지스터 04h 의 비트. Jeffrey S. Lee,
   "Programming the AdLib/Sound Blaster FM Music Chips" 2.0 의 표다.
   상태의 아래 비트는 OPL2 에서 06h 다. 이 값을 06h 와 AND 해 0 이면
   OPL3 로 본다(YMF289B 데이터시트, ModdingWiki "OPL chip" 에서 재인용).
   DOSBox Staging opl.cpp Opl::PortRead 도 06h 를 더한다. *)
let opl_status_irq = 0x80
let opl_status_t1 = 0x40
let opl_status_t2 = 0x20
let opl2_status_low = 0x06
let opl_ctl_reset = 0x80
let opl_ctl_mask_t1 = 0x40
let opl_ctl_mask_t2 = 0x20
let opl_ctl_start_t2 = 0x02
let opl_ctl_start_t1 = 0x01

(* 타이머는 레지스터 값에서 한 칸씩 올라가 256 에서 넘친다. *)
let opl_overflow = 256

let opl_period ~step ~count = step * (opl_overflow - count)

let opl_raise = function
  | Opl_clear | Opl_raised -> Opl_raised
  | Opl_masked -> Opl_masked

let opl_clear = function
  | Opl_clear | Opl_raised -> Opl_clear
  | Opl_masked -> Opl_masked

let opl_mask ~masked flag =
  match masked, flag with
  | true, (Opl_clear | Opl_raised | Opl_masked) -> Opl_masked
  | false, Opl_masked -> Opl_clear
  | false, Opl_clear -> Opl_clear
  | false, Opl_raised -> Opl_raised

(* [at] 에 넘칠 타이머가 [now] 까지 넘쳤으면 [now] 뒤 첫 주기 끝으로
   옮기고 플래그를 세운다. 여러 번 넘쳤어도 플래그는 하나다. *)
let opl_timer_update ~step ~count ~now (at, flag) =
  match at with
  | Some due when now >= due ->
    let p = opl_period ~step ~count in
    (Some (due + (p * (((now - due) / p) + 1))), opl_raise flag)
  | Some _ | None -> (at, flag)

(* 레지스터 04h 의 시작·마스크 비트를 타이머 하나에 건다. 돌던 타이머는
   시작 비트를 다시 써도 그대로 돈다. 새로 시작하면 DOSBox 처럼 지난 칸
   경계부터 세고 플래그를 지운다. *)
let opl_timer_control ~step ~count ~now ~on ~masked (at, flag) =
  let at, flag =
    match on, at with
    | true, Some _ -> (at, flag)
    | true, None -> (Some ((now / step * step) + opl_period ~step ~count), opl_clear flag)
    | false, (Some _ | None) -> (None, flag)
  in
  (at, opl_mask ~masked flag)

(* 넘침은 읽는 때가 아니라 넘친 때 일어난다. 상태를 읽거나 레지스터를
   쓰기 전에 그때까지 지난 넘침을 반영한다. *)
let opl_update t =
  let at, flag =
    opl_timer_update ~step:(at_speed t opl_t1_step) ~count:t.opl_t1_count ~now:t.now (t.opl_t1_at, t.opl_t1_exp)
  in
  t.opl_t1_at <- at;
  t.opl_t1_exp <- flag;
  let at, flag =
    opl_timer_update ~step:(at_speed t opl_t2_step) ~count:t.opl_t2_count ~now:t.now (t.opl_t2_at, t.opl_t2_exp)
  in
  t.opl_t2_at <- at;
  t.opl_t2_exp <- flag

let opl_control t v =
  if v land opl_ctl_reset <> 0 then begin
    (* bit7 이 서면 플래그만 지우고 나머지 비트는 보지 않는다. *)
    t.opl_t1_exp <- opl_clear t.opl_t1_exp;
    t.opl_t2_exp <- opl_clear t.opl_t2_exp
  end
  else begin
    let at, flag =
      opl_timer_control ~step:(at_speed t opl_t1_step) ~count:t.opl_t1_count ~now:t.now
        ~on:(v land opl_ctl_start_t1 <> 0) ~masked:(v land opl_ctl_mask_t1 <> 0)
        (t.opl_t1_at, t.opl_t1_exp)
    in
    t.opl_t1_at <- at;
    t.opl_t1_exp <- flag;
    let at, flag =
      opl_timer_control ~step:(at_speed t opl_t2_step) ~count:t.opl_t2_count ~now:t.now
        ~on:(v land opl_ctl_start_t2 <> 0) ~masked:(v land opl_ctl_mask_t2 <> 0)
        (t.opl_t2_at, t.opl_t2_exp)
    in
    t.opl_t2_at <- at;
    t.opl_t2_exp <- flag
  end

let opl_status t =
  opl_update t;
  let bit flag b =
    match flag with
    | Opl_raised -> opl_status_irq lor b
    | Opl_clear | Opl_masked -> 0
  in
  opl2_status_low lor bit t.opl_t1_exp opl_status_t1 lor bit t.opl_t2_exp opl_status_t2

let opl_write t v =
  opl_update t;
  match t.opl_reg with
  | 0x02 -> t.opl_t1_count <- v
  | 0x03 -> t.opl_t2_count <- v
  | 0x04 -> opl_control t v
  | reg -> Dos_opl.write t.opl reg v   (* 음색 레지스터 *)

let set_now t n = t.now <- n
let palette t = t.pal
let pic t = t.pic
let dma t = t.dma
let sound_blaster t = t.sb
let opl t = t.opl

(* Sound Blaster 2.0 의 FM 은 카드의 228h·229h 에도 보인다 — 388h·389h 의
   OPL2 와 같은 칩이다(DOSBox 도 base+8 에 OPL 을 단다). *)
let sb_fm_address = 0x228
let sb_fm_data = 0x229
let opl_address = 0x388
let opl_data = 0x389

let fm_alias t p =
  match t.sb with
  | None -> p
  | Some _ -> if p = sb_fm_address then opl_address else if p = sb_fm_data then opl_data else p

(* DSP 가 IRQ 선을 내렸는데 CPU 가 아직 요청을 안 받았으면 요청을
   거둔다(DOSBox PIC_DeActivateIRQ). 22Eh 를 읽은 뒤, 리셋·새 전송 뒤다. *)
let follow_sb_line t sb =
  if not (Dos_sb.irq_line sb) then Dos_pic.lower_irq t.pic Dos_pic.Sound_blaster

let sb_port_in t p =
  match t.sb with
  | None -> None
  | Some sb ->
    let v = Dos_sb.port_in sb p in
    follow_sb_line t sb;
    v

let sb_port_out t p v =
  match t.sb with
  | None -> false
  | Some sb ->
    let taken = Dos_sb.port_out sb ~hz:(cpu_hz t) ~now:t.now p v in
    follow_sb_line t sb;
    taken
let kbc t = t.kbc
let speaker_on t = t.port_b land 0x03 = 0x03

let pit_tick t = t.now / t.speed
let gate2 t = t.port_b land gate2_bit <> 0

(* 포트 0x61 bit1: 스피커 데이터. 스피커는 이 비트와 채널 2 의 OUT 의
   AND 를 따른다(Dos_audio.speaker). *)
let speaker_data_bit = 0x02

(* 채널 2 를 부르거나 0x61 을 바꾸기 전에 지금까지의 스피커 소리를 표본으로
   만든다. 채널 2 를 지금 시각으로 부르면 지난 구간이 남지 않는다
   (Dos_pit.next_change) — 읽기도 그렇다. *)
let render_audio t ~until =
  match t.audio with
  | None -> ()
  | Some a ->
    let speaker =
      Dos_audio.speaker t.pit2 ~data:(t.port_b land speaker_data_bit <> 0) ~speed:t.speed
    in
    (* the DSP's DAC holds between the changes it tells of (on_output) *)
    let sb = match t.sb with None -> 0 | Some sb -> Dos_audio.sb_level (Dos_sb.output sb) in
    Dos_audio.render a ~until (fun c ->
        let level, next = speaker c in
        (level + sb, next))

let catch_up_audio t = render_audio t ~until:t.now

let start_audio t ~rate =
  t.audio <- Some (Dos_audio.create ~cpu_hz:(cpu_hz t) ~rate ~start:t.now);
  Option.iter (fun sb -> Dos_sb.on_output sb (fun at -> render_audio t ~until:at)) t.sb

(* A DSP event due by now runs at the start of the next step, with its
   own cycle: the samples stop short of it until then. *)
let take_audio t =
  match t.audio with
  | None -> None
  | Some a ->
    let until = match t.sb with None -> t.now | Some sb -> min t.now (Dos_sb.due_at sb) in
    render_audio t ~until;
    Some (Dos_audio.take a)

let audio_clipped t = Option.map Dos_audio.clipped t.audio

(* 채널 0 에 사건(제어 워드, 카운트 쓰기)이 있으면 다음 상승을 다시 잡는다. *)
let pit0_changed t = t.irq0_at <- rise_cycle ~speed:t.speed t.pit0 ~tick:(pit_tick t)

let irq0_due t =
  match t.irq0_at with
  | Some at when t.now >= at -> pit0_changed t; true
  | Some _ | None -> false

let[@inline] rtc_irq_due t = Dos_cmos.irq_rises t.cmos ~now:t.now

(* RTC 가 줄을 내렸는데 CPU 가 아직 요청을 안 받았으면 요청을 거둔다
   (DSP 의 [follow_sb_line] 과 같다). 레지스터 C 를 읽은 뒤, PIE 를 끈
   뒤다. *)
let follow_rtc_line t =
  if not (Dos_cmos.irq_line t.cmos) then Dos_pic.lower_irq t.pic Dos_pic.Rtc

(* BIOS 데이터 영역에서 쪽과 커서가 사는 자리(SeaBIOS std/bda.h 의
   video_pagesize·video_pagestart·cursor_pos[8]·video_page). 쪽 p 의 커서는
   [bda_cursor_pos] + 2p 에 열, 그 다음 바이트에 행이다. *)
let bda_page_size = 0x44C
let bda_page_start = 0x44E
let bda_cursor_pos = 0x450
let bda_active_page = 0x462
let page_count = 8

(* CRTC 커서 레지스터(0x0E/0x0F)는 화면 메모리 처음부터의 글자 수다. BIOS
   는 보이는 쪽의 커서를 그 쪽 처음부터의 열·행으로 BDA 에 둔다 — 커서의
   진실 원천이 둘로 갈라지지 않게 여기서 같이 맞춘다. 쪽 처음(0x44E)은
   바이트 수라 2 로 나눠 글자 수로 뺀다. 보이는 쪽 번호가 0-7 밖이면 맞출
   칸이 없다. *)
let sync_cursor_to_bda t =
  let reg i = Dos_video.crtc t.video i land 0xff in
  let byte a = Char.code (Bytes.get t.mem a) in
  let page = byte bda_active_page in
  if page < page_count then begin
    let start = (byte bda_page_start lor (byte (bda_page_start + 1) lsl 8)) / 2 in
    let linear = (((reg 0x0E lsl 8) lor reg 0x0F) - start) land 0xffff in
    let cols = max 1 (Dos_video.text_cols t.video) in
    let slot = bda_cursor_pos + (2 * page) in
    Bytes.set t.mem slot (Char.chr (linear mod cols));
    Bytes.set t.mem (slot + 1) (Char.chr ((linear / cols) land 0xff))
  end

(* CRTC 는 비디오 어댑터가 가진다. 커서 위치(0x0E/0x0F)와 모양(0x0A/0x0B)
   은 BIOS 도 BDA 에 두므로, 어댑터가 쓰기를 받은 뒤 여기서 BDA 를 맞춘다. *)
let crtc_written t v =
  match Dos_video.crtc_index t.video with
  | 0x0E | 0x0F -> sync_cursor_to_bda t
  | 0x0A -> Bytes.set t.mem 0x461 (Char.chr v)
  | 0x0B -> Bytes.set t.mem 0x460 (Char.chr v)
  | _ -> ()

let mpu_port_in t p =
  match t.mpu with
  | None -> None
  | Some mpu -> Dos_mpu401.port_in mpu p

(* 이 기계에는 MIDI 장치가 없다. MIDI 바이트는 여기서 버린다. 330h·331h
   는 아래 [port_out] 의 다른 장치가 받지 않는다. *)
let mpu_port_out t p v =
  match t.mpu with
  | None -> ()
  | Some mpu ->
    (match Dos_mpu401.port_out mpu p v with
     | None | Some (Dos_mpu401.Taken | Dos_mpu401.Midi_out _) -> ())

let port_in t p =
  match Dos_video.port_in t.video p with
  | Some v -> v
  | None ->
  match Dos_dma.port_in t.dma p with
  | Some v -> v
  | None ->
  match sb_port_in t (p land 0xffff) with
  | Some v -> v
  | None ->
  match mpu_port_in t (p land 0xffff) with
  | Some v -> v
  | None ->
  match fm_alias t (p land 0xffff) with
  | 0x20 -> Dos_pic.read_command t.pic Dos_pic.Master
  | 0x21 -> Dos_pic.read_mask t.pic Dos_pic.Master
  | 0xA0 -> Dos_pic.read_command t.pic Dos_pic.Slave
  | 0xA1 -> Dos_pic.read_mask t.pic Dos_pic.Slave
  | 0x40 -> Dos_pit.read t.pit0 ~tick:(pit_tick t)
  | 0x42 ->
    catch_up_audio t;
    Dos_pit.read t.pit2 ~tick:(pit_tick t)
  | 0x60 -> Dos_kbc.read_data t.kbc ~hz:(cpu_hz t) ~now:t.now
  | 0x61 ->
    catch_up_audio t;
    (* bit4 는 메모리 리프레시 — 실기에서 쉬지 않고 뒤집힌다. 고정값을
       주면 이 비트를 세는 지연 루프가 안 끝난다. bit5 는 채널 2 의 OUT
       이다 — 채널 2 를 모드 0 으로 걸고 이 비트를 기다리는 지연 루프가
       있다(SeaBIOS src/hw/timer.c tsctimer_setup). *)
    let refresh = if (t.now / at_speed t refresh_toggle_cycles) land 1 = 0 then 0 else refresh_bit in
    let out2 = if Dos_pit.out t.pit2 ~tick:(pit_tick t) then out2_bit else 0 in
    (t.port_b land lnot (refresh_bit lor out2_bit)) lor refresh lor out2
  | 0x64 -> Dos_kbc.status t.kbc
  | 0x70 -> Dos_cmos.index t.cmos
  | 0x71 ->
    let v = Dos_cmos.read t.cmos ~now:t.now ~cpu_hz:(cpu_hz t) in
    follow_rtc_line t;
    v
  (* 조이스틱 미장착: 축 비트 0(방전) 이면 축 카운트 루프가 즉시
     통과한다. 0xFF 를 주면 카운터가 랩할 때까지 갇힌다(ZZT 실측).
     버튼 비트(4~7)는 0 이 눌림이라 1 로 둔다(86Box joystick_standard.c
     는 0xF0 에서 시작해 누른 버튼의 비트를 지운다). *)
  | 0x201 -> game_port_released
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
  | 0x3D8 -> t.mode_control
  | 0x3D9 -> t.color_select
  | 0x3BA | 0x3DA ->
    (* 재주사 상태: 읽을 때마다 bit0(디스플레이 인에이블)을 뒤집는다.
       CRT 루틴이 VRAM 직접 쓰기 전에 "clear 대기 → set 대기" 상승
       에지를 본다 — 고정값이면 한쪽 대기가 안 풀린다(ZZT 실측).
       bit3(수직 귀선)은 [in_vertical_retrace] 가 프레임 시각으로 정한다. *)
    (* 실기에서는 이 레지스터를 읽으면 속성 컨트롤러의 index/data
       번갈이도 index 로 돌아간다. 그 부수효과에 기대는 코드가 있다. *)
    Dos_video.reset_attr_flip t.video;
    t.cga_status <- not t.cga_status;
    let vsync = if in_vertical_retrace t then 0x08 else 0x00 in
    (if t.cga_status then 0x01 else 0x00) lor vsync
  | 0x388 -> opl_status t
  | _ -> 0xff

let port_out t p v =
  let v = v land 0xff in
  if Dos_video.port_out t.video p v then begin
    match p land 0xffff with
    | 0x3B5 | 0x3D5 -> crtc_written t v
    | _ -> ()
  end
  else if Dos_dma.port_out t.dma p v then ()
  else if sb_port_out t (p land 0xffff) v then ()
  else begin
  mpu_port_out t (p land 0xffff) v;
  match fm_alias t (p land 0xffff) with
  | 0x20 -> Dos_pic.write_command t.pic Dos_pic.Master v
  | 0x21 -> Dos_pic.write_mask t.pic Dos_pic.Master v
  | 0xA0 -> Dos_pic.write_command t.pic Dos_pic.Slave v
  | 0xA1 -> Dos_pic.write_mask t.pic Dos_pic.Slave v
  | 0x40 ->
    Dos_pit.write t.pit0 ~tick:(pit_tick t) ~gate:channel0_gate v;
    pit0_changed t
  | 0x42 ->
    catch_up_audio t;
    Dos_pit.write t.pit2 ~tick:(pit_tick t) ~gate:(gate2 t) v
  | 0x43 ->
    catch_up_audio t;
    (* 제어 워드: bit7-6 채널(3 은 읽기-되돌림 명령), bit5-4 접근
       모드(0=래치). 채널 1 은 DRAM 리프레시라 게스트가 쓰지 않는다 — 이
       기계에는 채널 1 이 없다. *)
    (match v lsr 6 with
     | 0 ->
       Dos_pit.control t.pit0 ~tick:(pit_tick t) v;
       pit0_changed t
     | 1 -> ()
     | 2 -> Dos_pit.control t.pit2 ~tick:(pit_tick t) v
     | _ ->
       (* 읽기-되돌림: bit 5 가 0 이면 카운트, bit 4 가 0 이면 상태를 잡는다.
          bit 1·2·3 이 채널 0·1·2 를 고른다. 잡기만 해서 OUT 은 그대로다. *)
       let count = v land read_back_no_count = 0
       and status = v land read_back_no_status = 0 in
       let tick = pit_tick t in
       if v land read_back_channel0 <> 0 then Dos_pit.read_back t.pit0 ~tick ~count ~status;
       if v land read_back_channel2 <> 0 then Dos_pit.read_back t.pit2 ~tick ~count ~status)
  | 0x60 -> Dos_kbc.write_data t.kbc ~hz:(cpu_hz t) ~now:t.now v
  | 0x61 ->
    catch_up_audio t;
    let was = gate2 t in
    t.port_b <- v;
    let now_high = gate2 t in
    if was <> now_high then Dos_pit.gate_edge t.pit2 ~tick:(pit_tick t) ~high:now_high
  | 0x64 -> Dos_kbc.write_command t.kbc ~hz:(cpu_hz t) v
  | 0x70 -> Dos_cmos.select t.cmos v
  | 0x71 ->
    Dos_cmos.write t.cmos v ~now:t.now ~cpu_hz:(cpu_hz t);
    follow_rtc_line t
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
  | 0x3D8 -> t.mode_control <- v
  | 0x3D9 -> t.color_select <- v; Dos_video.set_cga_color_select t.video v
  | 0x388 -> t.opl_reg <- v              (* 레지스터 선택 *)
  | 0x389 -> opl_write t v
  | _ -> ()
  end

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

(* One range per field, named by both [write_state] and [read_state], so a
   value the writer accepts is one the reader accepts. *)
let cycle = C.count
let speed_r = C.range 1 max_speed
let dac_phase_r = C.range 0 2
let opl_flag_r = C.range 0 2

let opl_flag_tag = function Opl_clear -> 0 | Opl_raised -> 1 | Opl_masked -> 2

let opl_flag_of_tag = function
  | 0 -> Opl_clear
  | 1 -> Opl_raised
  | 2 -> Opl_masked
  | n -> C.fail (Printf.sprintf "OPL flag %d" n)

let put_opl_at w = function
  | None -> C.put_bool w false
  | Some at -> C.put_bool w true; C.put w ~what:"opl next overflow" cycle at

let get_opl_at r =
  if C.get_bool r then Some (C.get r ~what:"opl next overflow" cycle) else None

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. Keep [write_state] and [read_state] in one order. *)
let write_state w t =
  let { mem = _; video = _ (* the machine's: it writes them itself *);
        speed; now; pit0; irq0_at; pic; dma; sb; kbc; pal; dac_write_index; dac_read_index;
        dac_phase; dac_mask; port_b; cga_status;
        mode_control; color_select; cmos; opl_reg; opl_t1_count;
        opl_t2_count; opl_t1_at; opl_t2_at; opl_t1_exp; opl_t2_exp; opl; pit2; mpu;
        audio = _ (* an observer, not machine state *) } = t
  in
  let put what r v = C.put w ~what r v in
  put "speed" speed_r speed;
  put "ports.now" cycle now;
  Dos_pit.write_state w pit2;
  Dos_pit.write_state w pit0;
  (match irq0_at with
   | None -> C.put_bool w false
   | Some at -> C.put_bool w true; put "irq0_at" cycle at);
  Dos_pic.write_state w pic;
  Dos_dma.write_state w dma;
  (match sb with
   | None -> C.put_bool w false
   | Some s -> C.put_bool w true; Dos_sb.write_state w s);
  Dos_kbc.write_state w kbc;
  put "DAC size" C.count (Array.length pal);
  Array.iter
    (fun (r, g, b) -> put "DAC red" C.byte r; put "DAC green" C.byte g; put "DAC blue" C.byte b)
    pal;
  put "dac_write_index" C.byte dac_write_index;
  put "dac_read_index" C.byte dac_read_index;
  put "dac_phase" dac_phase_r dac_phase;
  put "dac_mask" C.byte dac_mask;
  put "port_b" C.byte port_b;
  C.put_bool w cga_status;
  put "mode_control" C.byte mode_control;
  put "color_select" C.byte color_select;
  Dos_cmos.write_state w cmos;
  put "opl_reg" C.byte opl_reg;
  put "opl_t1_count" C.byte opl_t1_count;
  put "opl_t2_count" C.byte opl_t2_count;
  put_opl_at w opl_t1_at;
  put_opl_at w opl_t2_at;
  put "opl_t1_exp" opl_flag_r (opl_flag_tag opl_t1_exp);
  put "opl_t2_exp" opl_flag_r (opl_flag_tag opl_t2_exp);
  Dos_opl.write_state w opl;
  match mpu with
  | None -> C.put_bool w false
  | Some m -> C.put_bool w true; Dos_mpu401.write_state w m

let read_state r t =
  let get what rng = C.get r ~what rng in
  t.speed <- get "speed" speed_r;
  t.now <- get "ports.now" cycle;
  Dos_pit.read_state r t.pit2;
  Dos_pit.read_state r t.pit0;
  t.irq0_at <- (if C.get_bool r then Some (get "irq0_at" cycle) else None);
  Dos_pic.read_state r t.pic;
  Dos_dma.read_state r t.dma;
  t.sb <-
    (if C.get_bool r then begin
       let s = Dos_sb.create () in
       Dos_sb.read_state r s;
       Some s
     end
     else None);
  Dos_kbc.read_state r t.kbc ~hz:(cpu_hz t);
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
  t.port_b <- get "port_b" C.byte;
  t.cga_status <- C.get_bool r;
  t.mode_control <- get "mode_control" C.byte;
  t.color_select <- get "color_select" C.byte;
  Dos_cmos.read_state r t.cmos;
  t.opl_reg <- get "opl_reg" C.byte;
  t.opl_t1_count <- get "opl_t1_count" C.byte;
  t.opl_t2_count <- get "opl_t2_count" C.byte;
  t.opl_t1_at <- get_opl_at r;
  t.opl_t2_at <- get_opl_at r;
  t.opl_t1_exp <- opl_flag_of_tag (get "opl_t1_exp" opl_flag_r);
  t.opl_t2_exp <- opl_flag_of_tag (get "opl_t2_exp" opl_flag_r);
  Dos_opl.read_state r t.opl;
  t.mpu <-
    (if C.get_bool r then begin
       let m = Dos_mpu401.create () in
       Dos_mpu401.read_state r m;
       Some m
     end
     else None)
