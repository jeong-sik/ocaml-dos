(* 기계의 상태와 그 상태를 만지는 기본기. INT 표면(Dos_bios, Dos_dos)과
   배선(Dos_machine)이 이 타입을 함께 쓴다.

   결정론: 시계도 난수도 호스트에서 오지 않는다. 날짜·시각은 고정된
   기준시각에 CPU 누적 사이클로 환산한 경과를 더해 만든다. 같은 이미지에
   같은 키를 같은 순서로 넣으면 같은 화면이 나온다. *)

let vram_base = 0xB8000

(* DOS 핸들 표(JFT). 실기 DOS 는 PSP 마다 20칸짜리 표(PSP:18h, 크기는
   PSP:32h)를 두고, 0-4 는 stdin/stdout/stderr/stdaux/stdprn 이 이미
   차지한다. AH=3Ch/3Dh/45h 는 가장 낮은 빈 칸 번호를 돌려주고, 빈 칸이
   없으면 CF=1, AX=4 (too many open files) 로 답한다. AH=46h 는 CX 가 표
   밖이면 AX=6 (invalid handle). 출처: Ralf Brown's Interrupt List,
   INT 21h AH=3Ch/3Dh/45h/46h, 그리고 PSP 형식 표. AH=67h(표 늘리기)는 아직
   모델 밖이라 표는 20칸 고정이다. *)
let first_file_handle = 5
let max_handles = 20

(* [data] is shared: every handle opened on the same [hname] (through
   [Dos_dos.open_handle], not [dup]) points at the same ref, so a write
   through one is a read through the other -- real DOS keeps one file on
   disk regardless of how many times it is open. [pos] is each handle's
   own, since two independent opens read and write at their own offsets.
   [dup] (AH=45h/46h) instead shares the whole record under a second
   number, [pos] included -- that is a different number naming the same
   open file, not a second open. *)
type handle = {
  hname : string;
  data : Bytes.t ref;
  mutable pos : int;
}

(* INT 33h AX=0x0C 로 건 마우스 이벤트 핸들러의 등록. [mouse_handler_mask]
   0 은 미등록 — 마스크가 0 이면 주소가 남아 있어도 부르지 않는다. 세 값은
   16비트(Cpu86.set_reg16·set_seg 가 마스크한다). *)
(* AX=05h·06h 가 돌려주는 버튼 하나의 기록: 마지막 호출 뒤 누른(뗀) 횟수와
   마지막으로 누른(뗀) 자리. *)
type button_log = { mutable times : int; mutable last_x : int; mutable last_y : int }

(* 왼쪽·오른쪽·가운데. 버튼 i 는 버튼 상태의 비트 i 다. *)
let mouse_button_count = 3

let new_button_logs () =
  Array.init mouse_button_count (fun _ -> { times = 0; last_x = 0; last_y = 0 })

(* AX=07h·08h 의 범위. 드라이버가 상주할 때, 리셋할 때, INT 10h 로 모드를
   세울 때 가상 화면 전체({!Dos_mouse_space})로 돌아간다. *)
let clamp_to (lo, hi) v = max lo (min hi v)

(* 커서 깃발(HelpPC 2.10 INT 33,1): 0 이면 커서가 보이고 음수면 숨는다.
   드라이버를 리셋하면 -1 이다. *)
let cursor_flag_after_reset = -1

(* AX=09h 그래픽 커서는 열여섯 줄이고 줄마다 16비트 워드 하나다. 화면
   마스크 열여섯 워드 뒤에 커서 마스크 열여섯 워드가 온다(RBIL INT 33/0009
   표 03170). *)
let cursor_rows = 16

type graphics_cursor =
  | Default_arrow
      (** 리셋 뒤 드라이버에 든 화살표다. 비트맵은 Dos_mouse_cursor 의
          default_screen_mask·default_cursor_mask(DOSBox-X mouse.cpp)다. *)
  | Custom_cursor of {
      hot_x : int;  (** BX 그대로 *)
      hot_y : int;  (** CX 그대로 *)
      screen_mask : int array;  (** [cursor_rows] 워드, 화면과 AND *)
      cursor_mask : int array;  (** [cursor_rows] 워드, 그 뒤 XOR *)
    }

(* 화살표의 핫스폿은 왼쪽 위 (0, 0)이다(DOSBox-X mouse.cpp 의 리셋이
   hotx·hoty 를 0 으로 둔다). *)
let default_hot_spot = (0, 0)

(* AX=0Ah 텍스트 커서(RBIL INT 33/000A). 소프트웨어 커서는 커서 자리의
   글자·속성 워드를 화면 마스크와 AND 한 뒤 커서 마스크와 XOR 한다.
   하드웨어 커서는 CRTC 커서의 시작·끝 스캔 줄이다. *)
type text_cursor =
  | Software_text of { screen_mask : int; cursor_mask : int }
  | Hardware_text of { first_line : int; last_line : int }

(* 리셋 뒤 텍스트 커서는 속성 바이트만 뒤집는 소프트웨어 커서다(DOSBox-X
   mouse.cpp defaultTextAndMask 77FFh·defaultTextXorMask 7700h). *)
let default_text_cursor = Software_text { screen_mask = 0x77FF; cursor_mask = 0x7700 }

(* 드라이버가 화면에 그려 둔 커서와 그 밑에 있던 것. 숨기거나 옮기기 전에
   되돌린다(DOSBox-X mouse.cpp backData·backposx·backposy). *)
type drawn_cursor =
  | Not_drawn
  | Drawn_text of { page : int; col : int; row : int; under : int }
      (** 칸 하나, [under] 는 그 칸의 글자·속성 워드 *)
  | Drawn_graphics of { left : int; top : int; width : int; height : int; under : int array }
      (** 화면 안으로 자른 사각형. [under] 는 위 줄부터 줄마다 [width] 점 *)

(* 움직임 설정 — AX=0Fh 미키/화소 비율, 1Ah 감도, 13h·1Ah 두 배 속도 문턱
   (RBIL INT 33/000F, 001A, 0013). *)
type mouse_motion = {
  mickeys_x : int;  (** 가로 8 화소당 미키 *)
  mickeys_y : int;  (** 세로 8 화소당 미키 *)
  speed_x : int;  (** 1Ah 가로 감도, 1..100 *)
  speed_y : int;
  double_speed : int;  (** 미키/초 *)
}

(* 리셋 값. 비율은 RBIL INT 33/000F("default 8", "default 16")와 HelpPC
   INT 33,0, 문턱 64 는 RBIL INT 33/0013, 감도 50 은 CuteMouse ctmouse.asm
   기본값(sensval 49, 1Bh 가 1 을 더해 낸다)이다. *)
let default_motion =
  { mickeys_x = 8; mickeys_y = 16; speed_x = 50; speed_y = 50; double_speed = 64 }

let lowest_speed = 1
let highest_speed = 100

(* 0Fh 비율은 8 화소당 미키다 *)
let pixels_per_ratio = 8

(* 커서가 [pixels] 움직인 만큼의 미키. 드라이버는 미키를 세고 커서를
   미키 × 8 / 비율만큼 옮긴다(CuteMouse updateposition, DOSBox-X
   Mouse_CursorMoved). 이 기계는 하네스가 커서 자리를 주니 거꾸로 화소 × 비율
   / 8 이다. 감도는 미키를 세기 전에 들어가므로 여기에 끼지 않는다. 8 로
   나눈 나머지(0..7)는 [carry] 로 다음 움직임에 넘겨 느린 움직임이 사라지지
   않게 한다. *)
let mickeys_of_pixels ~per_8 ~carry pixels =
  let v = (pixels * per_8) + carry in
  let m =
    if v >= 0 then v / pixels_per_ratio
    else -(((-v) + pixels_per_ratio - 1) / pixels_per_ratio)
  in
  (m, v - (m * pixels_per_ratio))

type mouse = {
  mutable mouse_present : bool;
  mutable mouse_x : int;
  mutable mouse_y : int;
  mutable mouse_buttons : int;
  mutable mouse_cursor_flag : int;  (** 0 이하. 0 이면 보인다 *)
  mutable mouse_graphics_cursor : graphics_cursor;
  mutable mouse_text_cursor : text_cursor;
  mutable mouse_dx : int;  (** 미키 *)
  mutable mouse_dy : int;
  mutable mouse_motion : mouse_motion;
  mutable mouse_carry_x : int;  (** [mickeys_of_pixels] 의 나머지 *)
  mutable mouse_carry_y : int;
  mutable mouse_handler_mask : int;
  mutable mouse_handler_seg : int;
  mutable mouse_handler_off : int;
  mouse_presses : button_log array;
  mouse_releases : button_log array;
  mutable mouse_range_x : int * int;
  mutable mouse_range_y : int * int;
  mutable mouse_drawn : drawn_cursor;  (** {!Dos_mouse_cursor} 가 쓴다 *)
}

(* ---------- AH=4Bh EXEC 의 부모 프레임 ----------

   자식을 띄울 때 부모의 CPU·프로세스 상태를 통째로 찍어둔다. 레지스터
   세그먼트 IP 플래그를 값으로 저장한다 — Cpu86.t 를 통째로 복제하지
   않는 이유는 사이클 누적 같은 기계 전체의 상태가 부모의 것이어야
   하기 때문이다. 스냅숏의 IP 는 이미 INT 21h 명령 다음을 가리킨다
   (fetch 가 IP 를 전진시킨 뒤 훅이 돈다). 그 지점이 부모의 재개점이다. *)
type cpu_snapshot = {
  snap_regs : int array;                  (** 8 × 16비트 범용 *)
  snap_segs : int array;                  (** 4 × 세그먼트 *)
  snap_ip : int;
  snap_flags : int;                       (* Cpu86.flags 합성 값 *)
}

type service_return =
  | Direct_return
  | Interrupt_return of { frame_ss : int; frame_sp : int; status_mask : int }

type exec_frame = {
  parent : cpu_snapshot;
  parent_service_return : service_return;
  parent_psp : int;
  parent_dta : int;
  parent_free_base : int;
  parent_free_top : int;
  parent_blocks : (int * int) list;
  parent_handles : (int * handle) list;   (** 자식 종료 시 이 목록 밖의 핸들을 닫는다 *)
}

(* A suspended host service is plain data, including its exact return
   boundary. IRQ handlers can run above that boundary, and may themselves
   suspend, without letting the original caller execute its next opcode. *)
type line_input = { text : string; echo_widths : int list }

(* CON's cooked line buffer includes the terminal CR. The DOS-compatible
   LINEBUFSIZECON contract is 128 bytes:
   https://github.com/FDOS/kernel/blob/master/hdr/kbd.h *)
let console_line_capacity = 128

type dos_input_request =
  | Read_character of { echo : bool }
  | Read_buffered_line of { base : int; capacity : int; line : line_input }
  | Read_console_line of { destination : int; length : int; line : line_input }

(* 멈춘 BIOS·DOS 서비스가 기다리는 것. [Bios_wait] 은 입력이 아니라 INT 15h
   AH=86h 가 RTC 의 끝 깃발(40:A1h bit 7)을 기다리는 것이다 — 기다리는 동안
   IRQ 가 들어오는 길이 같아서 여기에 있다. *)
type input_request = Bios_key | Bios_wait | Dos_input of dos_input_request

type service_result = Completed | Control_transferred | Awaiting_input of input_request

type input_continuation = {
  request : input_request;
  return_cs : int;
  return_ip : int;
  return_ss : int;
  return_sp : int;
  return_control_flags : int;  (* caller IF/TF, before the service's wait loop *)
  service_return : service_return;
}

(* What AH=4Eh found and AH=4Fh still has to give: a file by its path key,
   or a directory entry by the name it shows ([.], [..] or its name). *)
type find_hit = File_hit of string | Dir_hit of string

(* 이 기계가 받지 못한 INT 호출의 갈래. 기계 동작은 바꾸지 않고 관찰자에게만
   알린다 — 게임을 돌려 무엇이 빠졌는지 찾는 진단용이다. *)
type unserved =
  | No_function  (** 호스트 서비스(21h·10h·15h·16h·1Ah·2Fh·33h)에 이 기능 갈래가 없다 *)
  | Iret_stub    (** 프로그램이 부른 INT 가 ROM 의 IRET 스텁으로 갔다 *)

type t = {
  mem : Bytes.t;                              (** 1MB *)
  cpu : Cpu86.t;
  ports : Dos_ports.t;
  mutable exited : bool;
  mutable exit_code : int;
  video : Dos_video.t;
  host_files : (string, Bytes.t) Hashtbl.t;
        (** 하네스가 마운트한 파일 — 키는 {!Dos_path.key} *)
  dirs : (string, unit) Hashtbl.t;
        (** 루트 밖의 디렉터리 — 키는 {!Dos_path.key}. 마운트한 파일의 위
            디렉터리와 게스트가 AH=39h 로 만든 것 *)
  mutable cwd : Dos_path.t;                   (** C: 의 현재 디렉터리 *)
  handles : (int, handle) Hashtbl.t;
        (** 핸들 번호 [first_file_handle] .. [max_handles - 1] 만 쓴다 *)
  open_files : (string, Bytes.t ref) Hashtbl.t;
        (** 이름 하나가 지금 열려 있는 동안의 공유 버퍼. [handles] 의 어떤
            항목도, [exec_frames] 의 어떤 부모 핸들도 그 이름을 더는 물지
            않으면 지운다 — 다음 open 은 [host_files] 에서 새로 시작한다. *)
  fcbs : (int, Bytes.t * int) Hashtbl.t;
  mutable dta : int;                          (** 전송 주소(물리) *)
  mutable psp_seg : int;
  mutable kbd_wait : bool;                    (** 키를 기다리다 굶었다 *)
  mutable ext_scan_pending : int;             (** INT 21h 바이트 읽기용 확장키 스캔 대기 *)
  mutable input_continuations : input_continuation list;
  mutable console_pending : string;  (* cooked AH=3F line bytes not yet returned *)
  mutable kbd_requests : int;                 (** 빈 링을 만난 횟수 *)
  mutable free_base : int;                    (** 할당 가능 첫 세그먼트 *)
  mutable free_top : int;                     (** 할당 가능 상한 세그먼트 *)
  mutable blocks : (int * int) list;          (** (세그먼트, paras) *)
  mutable find_queue : find_hit list;         (** findnext 가 남긴 것 *)
  mutable stubs : (int * int) list;           (** (벡터, ROM 스텁 오프셋) *)
  mutable exec_frames : exec_frame list;      (** EXEC 로 띄운 자식의 부모 프레임 *)
  mutable last_child_code : int;              (** AH=4Dh 가 돌려줄 마지막 자식 코드 *)
  (* 기준 시각 — RTC(INT 1Ah 02h·04h)는 여기에 경과를 더해 만든다. 부팅
     때 BIOS 틱과 DOS 날짜도 이 시각에서 채운다. *)
  mutable epoch_year : int;
  mutable epoch_month : int;
  mutable epoch_day : int;
  mutable epoch_hour : int;
  mutable epoch_min : int;
  mutable epoch_sec : int;
  mutable dos_days : int;
      (** DOS 의 날짜: 1980-01-01 부터 센 날 수(MS-DOS DAYCNT). 시각은 BIOS
          틱에서 읽는다(Dos_clock). *)
  (* EMS 4.0 스텁(Dos_ems)의 상태 *)
  ems_pages : (int, Bytes.t array) Hashtbl.t;
        (** 핸들(0 .. [Dos_ems.max_handle]) → 논리 페이지 배열(각 16KB).
            운영체제 핸들 0 은 늘 있다. *)
  ems_mapped : (int * int) option array;
        (** 물리 페이지 0-3 → 지금 겹쳐진 (핸들, 논리 페이지). [None] 은
            비어 있다. *)
  ems_saved : (int, (int * int) option array) Hashtbl.t;
        (** 핸들 → AH=47h 가 저장한 [ems_mapped]. AH=48h 가 꺼내 간다.
            있는 페이지만 가리킨다. *)
  ems_names : (int, string) Hashtbl.t;
        (** 핸들 → AH=53h 로 붙인 이름. 여덟 바이트이고, 첫 NUL 뒤는 모두
            NUL 이다. 이름이 없는(모두 NUL) 핸들은 없고, 같은 이름은 둘
            없다. *)
  xms : Dos_xms.t;                            (** XMS 드라이버(INT 2Fh AH=43h) *)
  mouse : mouse;
  mutable on_unserved : unserved -> vector:int -> ax:int -> unit;
        (** 관찰자. 기계 상태가 아니어서 스냅숏에 싣지 않는다 *)
}

let report_unserved t kind ~vector = t.on_unserved kind ~vector ~ax:(Cpu86.reg16 t.cpu 0)

(* ---------- 메모리 ---------- *)

(* 화면의 열 수는 모드가 정한다 — 40 열 화면을 80 으로 계산하면 커서와
   스크롤이 한 줄씩 어긋난다. *)
let cols t = max 1 (Dos_video.text_cols t.video)
let rows t = Dos_video.text_rows t.video

let rd8 t a = Char.code (Bytes.get t.mem (a land 0xfffff))
let wr8 t a v = Bytes.set t.mem (a land 0xfffff) (Char.chr (v land 0xff))
let rd16 t a = rd8 t a lor (rd8 t (a + 1) lsl 8)
let wr16 t a v = wr8 t a v; wr8 t (a + 1) (v lsr 8)

(* The 8086 forms a physical address as segment*16 + offset over 20 address
   lines, so FFFF:0010 and above wrap to the bottom of memory (the high
   memory area exists only on a 286+ with the A20 line enabled, which this
   machine does not model). Every INT 21h buffer and FCB address built here
   is therefore a 20-bit physical address. *)
let phys_mask = 0xfffff
let physical seg off = ((seg lsl 4) + off) land phys_mask

(* ---------- DOS 데이터 영역 ----------

   MS-DOS 4.0 CONST2.ASM SWAP_START 의 앞 두 바이트: ErrorMode(INT 24h
   처리 중), INDOS(DOS 안에 있는 깊이). PRINT 같은 TSR 은 ErrorMode 가
   INDOS 바로 앞이라고 믿는다. INT 21h AH=34h 가 INDOS 주소를 준다.
   게스트 메모리에 두어 스냅숏이 메모리째 싣는다. 자리는 IVT·BDA 와 첫
   PSP(1000h) 사이의 빈 곳이다.

   INDOS 는 [input_continuations] 에서 나온다. DISP.ASM 은 01h..0Ch 를
   포함한 거의 모든 기능에서 INDOS 를 올리고, 끝나면 내린다. 이 기계의
   서비스는 차단 읽기 말고는 게스트 코드를 끼우지 않고 끝나니, 게스트가
   볼 수 있는 값은 기다리는 DOS 읽기의 수다. INT 24h 를 모델링하지 않아
   ErrorMode 는 늘 0 이다.

   그 뒤는 확장 에러(AH=59h)다: WPErr, EXTERR_LOCUS, EXTERR, EXTERR_ACTION,
   EXTERR_CLASS, EXTERRPT. 0x0C..0x16(DTA, 현재 PSP 등)는 호스트 상태에
   있어 비워 둔다. 0x17 은 CNTCFLAG(AH=33h 확장 Ctrl-Break 깃발)다.
   Swap_Always 의 USER_IN_AX(0x1A)에 기능을 기억한다. *)
let sda_seg = 0x0070
let sda_error_mode = 0
let sda_in_dos = 1
let sda_wp_err = 2
let sda_exterr_locus = 3
let sda_exterr = 4
let sda_exterr_action = 6
let sda_exterr_class = 7
let sda_exterr_pointer = 8
let sda_cntc_flag = 0x17
let sda_user_in_ax = 0x1A
let byte_max = 0xFF
let sda off = physical sda_seg off

(* CONST2.ASM 에서 SWAP_START 앞: VERFLG, CharCo, chSwitch, AllocMethod,
   fShare, DIFFNAM, MYNAME(16), CritPatch(10) 해서 32 바이트, 그 뒤 EVEN.
   그 앞 크기를 몰라 EVEN 은 0 바이트로 친다. 게스트가 주소로 찾는 값이
   아니어서 자리는 이 기계가 고른 것이다. [sda] 는 SWAP_START 기준이라
   음수 오프셋도 받는다. *)
let pre_swap_verify = -32
let pre_swap_switch_char = -30
let pre_swap_alloc_method = -29
let default_switch_char = Char.code '/'

(* CONST2.ASM 초기값 중 0 이 아닌 것: WPErr <-1>, chSwitch <'/'> *)
let init_dos_data t =
  wr8 t (sda sda_wp_err) byte_max;
  wr8 t (sda pre_swap_switch_char) default_switch_char

let set_locus t l = wr8 t (sda sda_exterr_locus) (Dos_error.locus_code l)

(* DISP.ASM DSKROUT: AH 가 0 이거나 0Ch 보다 크고 59h 가 아닌 기능에 들어갈
   때. 50h·51h·62h·33h·64h 는 그 앞에서 따로 끝나 여기에 오지 않는다. *)
let get_extended_error = 0x59
let last_console_call = 0x0C
let max_function = 0x6C
let early_calls = [ 0x33; 0x50; 0x51; 0x62; 0x64 ]

let disk_route ah =
  (ah = 0 || ah > last_console_call)
  && ah <> get_extended_error && ah <= max_function && not (List.mem ah early_calls)

let enter_call t =
  let ax = Cpu86.reg16 t.cpu 0 in
  if disk_route (ax lsr 8) then begin
    wr8 t (sda sda_user_in_ax) (ax land 0xff);
    wr8 t (sda (sda_user_in_ax + 1)) (ax lsr 8);
    set_locus t Dos_error.Unknown_locus;
    wr8 t (sda sda_error_mode) 0;
    wr8 t (sda sda_wp_err) byte_max
  end

let set_input_continuations t waits =
  t.input_continuations <- waits;
  let in_dos = List.length (List.filter (fun w ->
      match w.request with Dos_input _ -> true | Bios_key | Bios_wait -> false) waits) in
  wr8 t (physical sda_seg sda_in_dos) (min in_dos byte_max)

let finish_service_return t = function
  | Direct_return -> ()
  | Interrupt_return { frame_ss; frame_sp; status_mask } ->
    let addr n = physical frame_ss ((frame_sp + n) land 0xffff) in
    let saved = rd8 t (addr 4) lor (rd8 t (addr 5) lsl 8) in
    let flags = (saved land lnot status_mask) lor (Cpu86.flags t.cpu land status_mask) in
    wr8 t (addr 4) flags;
    wr8 t (addr 5) (flags lsr 8)

let seg_off t sreg reg = physical (Cpu86.seg t.cpu sreg) (Cpu86.reg16 t.cpu reg)

(* Copies [len] bytes of [src] into guest memory at physical [dst], wrapping
   at 1MB like [wr8]. A buffer that starts near the top of memory continues
   at physical 0 instead of raising. The bytes go through the CPU's bus, as
   DOS's own REP MOVS would: in a planar mode the A0000h window is the
   video planes, not the RAM under it (#117). *)
let blit_to_mem t src src_off dst len =
  for i = 0 to len - 1 do
    Cpu86.bus_write t.cpu ((dst + i) land phys_mask) (Char.code (Bytes.get src (src_off + i)))
  done

(* The guest byte at physical [a] as the CPU reads it -- the counterpart of
   [blit_to_mem] for bytes DOS takes from a guest buffer. *)
let bus_rd8 t a = Cpu86.bus_read t.cpu (a land phys_mask)

(* DS:DX 의 ASCIIZ — INT 21h 파일명 계열이 쓴다. *)
let asciiz_at t base =
  let b = Buffer.create 16 in
  let i = ref 0 in
  let stop = ref false in
  while (not !stop) && !i < 128 do
    let c = rd8 t (base + !i) in
    if c = 0 then stop := true
    else begin Buffer.add_char b (Char.chr c); incr i end
  done;
  Buffer.contents b

(* ---------- 플래그 도우미 ---------- *)

(* 플래그로 결과를 돌려주는 서비스(INT 21h 의 CF, INT 16h AH=01h 의
   ZF)는 현재 플래그를 고치는 것으로 충분하다 — IVT 가 아직 우리
   스텁이면 훅이 서비스를 곧장 부르고(프레임 push 없음) iret 도 없
   다. 스택의 프레임을 손대면 게스트의 push 값이 오염된다(삼국지3
   MAIN 의 push cs 값 11ad 가 11ac 로 깎힌 실측 — CF 비트 클리어로).
   게스트가 벡터를 훅해 체인하는 경로가 생기면 그때 프레임 워드
   수정이 필요해진다. *)
let set_cf t on =
  let f = Cpu86.flags t.cpu in
  Cpu86.set_flags t.cpu
    (if on then f lor Cpu86.f_carry else f land lnot Cpu86.f_carry)

let ok t = set_cf t false

(* MS_CODE.ASM SYS_RET_ERR: ETAB_LK 이 진짜 코드를 EXTERR 에 두고, 이
   기능이 돌려줄 수 없는 코드면 바꾼다. ErrorMap·CAL_LK 이 표에서
   class·action·locus 를 채운다. 기능 번호는 USER_IN_AX 의 AH 다. *)
let fail t code =
  let fn = rd8 t (sda (sda_user_in_ax + 1)) in
  wr8 t (sda sda_exterr) (code land 0xff);
  wr8 t (sda (sda_exterr + 1)) (code lsr 8);
  (match Dos_error.info code with
   | None -> ()
   | Some { Dos_error.error_class; action; locus } ->
     wr8 t (sda sda_exterr_class) (Dos_error.class_code error_class);
     wr8 t (sda sda_exterr_action) (Dos_error.action_code action);
     Option.iter (set_locus t) locus);
  Cpu86.set_reg16 t.cpu 0 (Dos_error.returned_code ~fn code);
  set_cf t true

(* ---------- 시계 ---------- *)

let days_in_month y m =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | _ -> if (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 then 29 else 28

let elapsed_seconds t = Cpu86.cycles t.cpu / Dos_ports.cpu_hz t.ports

(* 기준 시각 + 경과. 윤년까지 세되 달력 라이브러리는 쓰지 않는다 —
   게스트가 보는 건 DOS 의 날짜 필드뿐이다. *)
let now_fields t =
  let total = (t.epoch_hour * 3600) + (t.epoch_min * 60) + t.epoch_sec
              + elapsed_seconds t in
  let days = total / 86400 in
  let rest = total mod 86400 in
  let y = ref t.epoch_year and m = ref t.epoch_month and d = ref t.epoch_day in
  for _ = 1 to days do
    incr d;
    if !d > days_in_month !y !m then begin
      d := 1; incr m;
      if !m > 12 then begin m := 1; incr y end
    end
  done;
  (!y, !m, !d, rest / 3600, rest mod 3600 / 60, rest mod 60)

(* BIOS 데이터 영역의 틱(0x40:0x6C, 32 비트)과 자정 넘김(0x40:0x70). *)
let bda_ticks = 0x46C
let bda_midnight = 0x470
let read_ticks t = rd16 t bda_ticks lor (rd16 t (bda_ticks + 2) lsl 16)
let write_ticks t v =
  wr16 t bda_ticks (v land 0xFFFF);
  wr16 t (bda_ticks + 2) ((v lsr 16) land 0xFFFF)

(* ---------- 쪽·커서·화면 ---------- *)

(* 텍스트 화면은 쪽 여덟 장을 가진다. BIOS 는 쪽마다 커서를 따로 두고
   (BDA 0x450 + 2 × 쪽), 보이는 쪽(0x462)의 커서만 CRTC 에도 쓴다(SeaBIOS
   vgabios.c set_cursor_pos). 커서의 진실 원천은 BDA 다. CRTC 를 같이
   맞춰 두지 않으면 게스트가 CRTC 를 직접 읽을 때 딴 자리를 본다. 쪽
   번호가 0-7 밖이면 커서를 바꾸지 않고, 읽으면 (0, 0) 이다. *)
let active_page t = rd8 t Dos_ports.bda_active_page
let cursor_slot page = Dos_ports.bda_cursor_pos + (2 * page)

(* 쪽 [page] 의 (r, c) 칸이 0xB800 세그먼트 안에서 차지하는 오프셋.
   BIOS 는 B800:오프셋 으로 쓰므로 오프셋이 16비트에서 돈다(SeaBIOS
   vgafb.c text_address). 쪽 크기는 모드를 세울 때 BDA 0x44C 에 적힌다. *)
let segment_mask = 0xFFFF

let page_offset t ~page r c =
  ((rd16 t Dos_ports.bda_page_size * page) + (((r * cols t) + c) * 2)) land segment_mask

(* 쪽 하나의 바이트 수(SeaBIOS vgabios.c calc_page_size). 텍스트는 칸마다
   두 바이트를 2KB 로, 16색·256색은 화면 점 수 / 8 을 8KB 로 올린다. CGA
   는 16KB 다. *)
let text_page_align = 0x800
let graphics_page_align = 0x2000
let cga_page_size = 0x4000
let dots_per_plane_byte = 8
let round_up n unit = (n + unit - 1) / unit * unit

let page_size_of t =
  match Dos_video.kind t.video with
  | Dos_video.Text -> round_up (cols t * rows t * 2) text_page_align
  | Dos_video.Cga4 | Dos_video.Cga2 -> cga_page_size
  | Dos_video.Planar | Dos_video.Linear256 ->
    let w, h = Dos_video.dims t.video in
    round_up (w * h / dots_per_plane_byte) graphics_page_align

(* 쪽 오프셋(바이트)을 CRTC 시작 주소로 바꾸는 비율. SeaBIOS stdvga.c
   stdvga_set_displaystart 가 오프셋 × stdvga_vram_ratio / 4 를 쓴다. *)
let display_ratio = function
  | Dos_video.Text | Dos_video.Cga4 -> 2
  | Dos_video.Cga2 | Dos_video.Planar -> 4
  | Dos_video.Linear256 -> 1
let display_ratio_unit = 4

(* 모드를 세운 뒤: 쪽 크기를 다시 적고, 0 쪽을 보이며, 쪽 여덟 장의
   커서를 (0,0)으로 둔다(SeaBIOS vgabios.c vga_set_mode). *)
let reset_pages t =
  wr16 t Dos_ports.bda_page_size (page_size_of t);
  wr16 t Dos_ports.bda_page_start 0;
  wr8 t Dos_ports.bda_active_page 0;
  for page = 0 to Dos_ports.page_count - 1 do wr16 t (cursor_slot page) 0 done

let get_cursor_on t ~page =
  if page < Dos_ports.page_count then (rd8 t (cursor_slot page), rd8 t (cursor_slot page + 1))
  else (0, 0)

let set_cursor_on t ~page col row =
  if page < Dos_ports.page_count then begin
    let w = cols t in
    let col = max 0 (min (w - 1) col) and row = max 0 (min (rows t - 1) row) in
    wr8 t (cursor_slot page) col;
    wr8 t (cursor_slot page + 1) row;
    if page = active_page t then begin
      let linear = page_offset t ~page row col / 2 in
      Dos_ports.port_out t.ports 0x3D4 0x0E;
      Dos_ports.port_out t.ports 0x3D5 (linear lsr 8);
      Dos_ports.port_out t.ports 0x3D4 0x0F;
      Dos_ports.port_out t.ports 0x3D5 (linear land 0xff)
    end
  end

(* INT 10h AH=05h — 보이는 쪽 바꾸기(SeaBIOS vgabios.c set_active_page).
   CRTC 시작 주소를 그 쪽 처음으로 옮기고, BDA 에 쪽 번호와 시작 오프셋을
   적고, 그 쪽의 커서를 CRTC 에 쓴다. 0-7 밖이면 아무것도 하지 않는다. *)
let select_page t page =
  if page < Dos_ports.page_count then begin
    let off = page_offset t ~page 0 0 in
    let start = off * display_ratio (Dos_video.kind t.video) / display_ratio_unit in
    Dos_ports.port_out t.ports 0x3D4 0x0C;
    Dos_ports.port_out t.ports 0x3D5 ((start lsr 8) land 0xff);
    Dos_ports.port_out t.ports 0x3D4 0x0D;
    Dos_ports.port_out t.ports 0x3D5 (start land 0xff);
    wr16 t Dos_ports.bda_page_start off;
    wr8 t Dos_ports.bda_active_page page;
    let col, row = get_cursor_on t ~page in
    set_cursor_on t ~page col row
  end

(* 쪽을 말하지 않는 길(teletype, INT 21h 출력, AH=06h/07h)은 보이는 쪽을 쓴다. *)
let get_cursor t = get_cursor_on t ~page:(active_page t)   (* (열, 행) *)
let set_cursor t col row = set_cursor_on t ~page:(active_page t) col row

let cell_addr t ~page r c = vram_base + page_offset t ~page r c
let read_cell_on t ~page r c = rd16 t (cell_addr t ~page r c)
let write_cell_on t ~page r c v = wr16 t (cell_addr t ~page r c) v
let read_cell t r c = read_cell_on t ~page:(active_page t) r c
let write_cell t r c v = write_cell_on t ~page:(active_page t) r c v

(* ---------- 그래픽 모드의 글자 ----------

   그래픽 모드에서 BIOS 와 DOS 의 글자 출력은 글꼴 표의 점을 화면에
   찍는다. 글꼴 표는 게스트 메모리에 있고 벡터로 찾는다. VGA BIOS 는
   모드를 세울 때 INT 43h 를 그 모드의 글자 높이 표로 돌려 둔다. CGA
   모드(4~6)는 128 이상 글자를 INT 1Fh 의 8x8 표에서 찾는다. 프로그램이
   벡터를 자기 표로 바꾸면 그 글꼴로 그린다(DOSBox int10_char.cpp
   WriteChar 와 같다). *)

let font_vector = 0x43
let cga_high_font_vector = 0x1F
let cga_high_first = 0x80

let graphics t =
  match Dos_video.kind t.video with
  | Dos_video.Text -> false
  | Dos_video.Cga4 | Dos_video.Cga2 | Dos_video.Planar | Dos_video.Linear256 -> true

let vector_address t v = (rd16 t ((v * 4) + 2) lsl 4) + rd16 t (v * 4)

let glyph t ch =
  let height = Dos_video.char_height t.video in
  let base =
    match Dos_video.kind t.video with
    | (Dos_video.Cga4 | Dos_video.Cga2) when ch >= cga_high_first ->
      vector_address t cga_high_font_vector + ((ch - cga_high_first) * height)
    | Dos_video.Text | Dos_video.Cga4 | Dos_video.Cga2 | Dos_video.Planar
    | Dos_video.Linear256 ->
      vector_address t font_vector + (ch * height)
  in
  fun line -> rd8 t (base + line)

(* 글자의 바탕: 256색 모드는 주어진 색(AH=09h 의 BH)이다. 그 밖의 모드는
   0 이고, XOR 쓰기면 XOR 0 이라 바탕이 그대로 남는다. *)
let glyph_background t ~fg ~back256 =
  match Dos_video.kind t.video with
  | Dos_video.Linear256 -> back256
  | Dos_video.Text | Dos_video.Cga4 | Dos_video.Cga2 | Dos_video.Planar ->
    fg land Dos_video.xor_pixel

let write_glyph t ~col ~row ch ~fg ~bg =
  Dos_gtext.draw t.video ~glyph:(glyph t (ch land 0xff)) ~col ~row ~fg ~bg

(* 창 스크롤 — INT 10h AH=06/07 과 teletype 의 줄 넘침이 함께 쓴다.
   [lines]=0 은 창 전체를 지운다. 그래픽 모드에서 [attr] 는 빈 행을 채울
   색이다. 텍스트 모드에서는 창의 아래·오른쪽 끝을 화면 안으로
   자른다(SeaBIOS vgabios.c verify_scroll). 자르지 않으면 다음 쪽을
   덮는다. *)
let scroll_window_on t ~page ~up ~lines ~top ~left ~bottom ~right ~attr =
  if graphics t then
    Dos_gtext.scroll t.video ~up ~lines ~top ~left ~bottom ~right ~fill:attr
  else
  let bottom = min bottom (rows t - 1) and right = min right (cols t - 1) in
  let blank = (attr lsl 8) lor 0x20 in
  let lines = if lines = 0 then bottom - top + 1 else lines in
  let read r c = read_cell_on t ~page r c and write r c v = write_cell_on t ~page r c v in
  if up then
    for r = top to bottom do
      for c = left to right do
        let src = r + lines in
        write r c (if src > bottom then blank else read src c)
      done
    done
  else
    for r = bottom downto top do
      for c = left to right do
        let src = r - lines in
        write r c (if src < top then blank else read src c)
      done
    done

let scroll_window t = scroll_window_on t ~page:(active_page t)

(* 글자를 찍는 두 길. AH=13h 는 텍스트 모드에서도 속성을 쓴다
   ([Attribute]). teletype(AH=0Eh)과 DOS 출력은 텍스트 모드에서 글자만
   바꾸고 칸의 속성을 그대로 둔다([Teletype]). 그래픽 모드에서는 둘 다
   값을 색으로 쓴다(DOSBox int10_char.cpp: INT10_TeletypeOutput 은
   텍스트 모드에서 속성을 쓰지 않고, INT10_WriteString 은 쓴다). *)
type ink = Attribute of int | Teletype of int

(* 텍스트 모드에서 칸의 속성을 두고 글자만 바꾼다. 그래픽 모드는 [color]
   로 그린다. *)
let write_char_only t ~page ~col ~row ch ~color =
  if graphics t then
    write_glyph t ~col ~row ch ~fg:color ~bg:(glyph_background t ~fg:color ~back256:0)
  else
    write_cell_on t ~page row col
      ((read_cell_on t ~page row col land 0xFF00) lor (ch land 0xff))

(* 글자 하나 찍기. 마지막 줄을 넘으면 그 쪽을 한 줄 올린다. 넘침을
   클램프로 막으면 글자가 마지막 칸에 계속 덮어써져 출력이 통째로
   사라진다. 새 줄은 그래픽 모드면 검정, 텍스트 모드면 이 글자를 찍기 전
   커서 칸의 속성으로 채운다(DOSBox teletype_output_attr 이 AH=08h 로
   읽어 쓰는 값). 그래픽 모드의 글자는 쪽과 상관없이 보이는 화면에 찍는다. *)
let output_char_on t ~page ch ink =
  let col0, row0 = get_cursor_on t ~page in
  let col = ref col0 and row = ref row0 in
  (match ch with
   | 0x0D -> col := 0
   | 0x0A -> incr row
   | 0x08 -> if !col > 0 then decr col
   | 0x09 -> col := min (cols t - 1) ((!col + 8) / 8 * 8)
   | 0x07 -> ()                       (* 벨 — 소리는 내지 않는다 *)
   | _ ->
     (match ink with
      | Teletype color -> write_char_only t ~page ~col:!col ~row:!row ch ~color
      | Attribute attr ->
        if graphics t then
          write_glyph t ~col:!col ~row:!row ch ~fg:attr
            ~bg:(glyph_background t ~fg:attr ~back256:0)
        else write_cell_on t ~page !row !col ((attr lsl 8) lor (ch land 0xff)));
     incr col;
     if !col >= cols t then begin col := 0; incr row end);
  if !row >= rows t then begin
    let fill = if graphics t then 0 else read_cell_on t ~page row0 col0 lsr 8 in
    scroll_window_on t ~page ~up:true ~lines:1 ~top:0 ~left:0 ~bottom:(rows t - 1)
      ~right:(cols t - 1) ~attr:fill;
    row := rows t - 1
  end;
  set_cursor_on t ~page !col !row

let put_char_on t ~page ch attr = output_char_on t ~page ch (Attribute attr)
let put_char t ch attr = put_char_on t ~page:(active_page t) ch attr
let teletype t ch color = output_char_on t ~page:(active_page t) ch (Teletype color)

let put_string t s attr = String.iter (fun c -> put_char t (Char.code c) attr) s

(* ---------- BIOS 키 링 ---------- *)

(* 0x40:0x1E-0x3D 의 16워드 링이 키의 단일 진실 원천이다. 호스트 큐를
   따로 두면 게스트가 링을 직접 다룰 때 두 사본이 갈라진다(실측: AH=00
   이 호스트 큐만 비워 ZZT 가 AH=01 폴링을 영원히 돌았다).
   게스트는 포인터를 직접 쓸 수 있다(키 버퍼 비우기) — 범위 밖 값은
   '빈 버퍼' 로 읽는다. *)
let ring_lo = 0x1E
let ring_hi = 0x3C

let clamp_ptr v = if v < ring_lo || v > ring_hi then ring_lo else v

let bda_ring_head = 0x41A
let bda_ring_tail = 0x41C
let ring_head t = clamp_ptr (rd16 t bda_ring_head)
let ring_tail t = clamp_ptr (rd16 t bda_ring_tail)
let key_pending t = ring_head t <> ring_tail t

let peek_key t = rd16 t (0x400 + ring_head t)

let pop_key t =
  let h = ring_head t in
  let w = rd16 t (0x400 + h) in
  wr16 t 0x41A (if h >= ring_hi then ring_lo else h + 2);
  w

(* 링에 워드 하나 — BIOS 키보드 인터럽트와 INT 16h AH=05h 가 넣는다.
   꽉 차면 버린다(실기 BIOS 는 삑 소리를 낸다). *)
let ring_push t word =
  let head = ring_head t and tail = ring_tail t in
  let nxt = if tail >= ring_hi then ring_lo else tail + 2 in
  if nxt <> head then begin
    wr16 t (0x400 + tail) word;
    wr16 t 0x41C nxt
  end

(* ---------- 인터럽트 전달 ---------- *)

(* 게스트 IVT 핸들러에 진짜 인터럽트 프레임(flags/cs/ip)을 밀어 넣는다.
   false = 그 벡터는 게스트가 안 걸었다(호스트가 서빙할 몫).
   실기 순서: 현재 플래그를 그대로 밀고 '그 다음에' IF·TF 를 끈다.
   끄지 않으면 핸들러 도중에 다음 틱이 들어와 프레임이 쌓인다. IRET 이
   밀어둔 값을 되돌리므로 IF 는 핸들러가 끝나면 살아난다. *)
let push_word t w =
  Cpu86.set_reg16 t.cpu 4 (Cpu86.reg16 t.cpu 4 - 2);
  let base = Cpu86.seg t.cpu 2 lsl 4 and o = Cpu86.reg16 t.cpu 4 in
  wr8 t (base + o) (w land 0xff);
  wr8 t (base + ((o + 1) land 0xffff)) (w lsr 8)

let deliver_ivt t v =
  let off = rd16 t (v * 4) and sg = rd16 t ((v * 4) + 2) in
  if off <> 0 || sg <> 0 then begin
    let flags = Cpu86.flags t.cpu in
    let push = push_word t in
    push flags;
    push (Cpu86.seg t.cpu 1);
    push (Cpu86.dump_ip t.cpu);
    Cpu86.set_flags t.cpu
      (flags land lnot Cpu86.f_interrupt land lnot Cpu86.f_trap);
    Cpu86.set_seg t.cpu 1 sg;
    Cpu86.set_ip t.cpu off;
    true
  end
  else false

(* 마우스 이벤트 핸들러(INT 33h AX=0x0C 등록)를 far call 로 부른다.
   인터럽트가 아니라 CALL 이라 플래그를 밀지 않고 IF·TF 도 안 건드린다 —
   핸들러는 RETF 로 돌아온다. 레지스터(AX 이벤트, BX 단추, CX·DX 좌표,
   SI·DI 이동량)는 부르는 쪽이 미리 채운다. 호출자는 run 사이에만 불리므로
   (지금은 Dos_machine.set_mouse) CPU 는 명령 경계에 있다. *)
let deliver_call t seg off =
  let push = push_word t in
  push (Cpu86.seg t.cpu 1);
  push (Cpu86.dump_ip t.cpu);
  Cpu86.set_seg t.cpu 1 seg;
  Cpu86.set_ip t.cpu off

(* 게스트가 키를 물었는데 링이 비어 있었다. 래치는 읽기가 성공해야
   내려가므로 "지금 기다리는가" 만 알려주고 "몇 번 물었는가" 는 못
   알려준다 — 한 구간 안에서 물었다가 받아간 것을 보려면 계수가 있어야
   한다. *)
let starve t =
  t.kbd_wait <- true;
  t.kbd_requests <- t.kbd_requests + 1

let interrupts_enabled t = Cpu86.flags t.cpu land Cpu86.f_interrupt <> 0
