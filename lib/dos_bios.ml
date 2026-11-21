(* BIOS 표면 — ROM 코드, 인터럽트 벡터표, BIOS 데이터 영역, 그리고
   INT 10h(비디오)·11h(장비)·12h(메모리)·15h(시스템)·16h(키보드)·1Ah(시각)·
   33h(마우스) 서비스.

   부팅 관문의 대부분은 "실기 하드웨어 계약이 비어 있었다" 였다. 벡터가
   0:0 이면 체인 방식 런타임이 0000:0000 으로 떨어지고, BDA 가 0 이면
   CRT 루틴이 없는 포트를 폴링한다. 여기 있는 값은 전부 실기 문서와
   실측에서 정한 것이다. *)

open Dos_state

(* 호스트 구현으로 가야 하는 벡터. 스텁을 까는 [install] 과 서비스를
   고르는 [Dos_machine.host_service] 가 모두 [host_vector] 하나에서
   나온다. 전용 스텁이 없으면 전벡터 IRET 스텁이 배달을 삼켜 서비스가
   조용히 무시된다(실측: TSR 의 INT 27h). 두 목록이 따로 있을 때 11h·
   12h·1Ah 가 스텁 목록에서 빠져 한 번도 불리지 않았다. *)
type bios_service =
  | Keyboard_irq | Rtc_irq | Video | Equipment | Memory_size | System | Keyboard | Time_of_day
  | Mouse
type host_vector =
  | Bios of bios_service
  | Dos_divide_overflow
  | Dos_terminate
  | Dos_call
  | Dos_keep
  | Multiplex   (* INT 2Fh: XMS 가 43h 로 답한다 *)
  | Ems

let host_vector = function
  | 0x00 -> Some Dos_divide_overflow
  | 0x09 -> Some (Bios Keyboard_irq)
  | 0x10 -> Some (Bios Video)
  | 0x11 -> Some (Bios Equipment)
  | 0x12 -> Some (Bios Memory_size)
  | 0x15 -> Some (Bios System)
  | 0x16 -> Some (Bios Keyboard)
  | 0x1A -> Some (Bios Time_of_day)
  | 0x20 -> Some Dos_terminate
  | 0x21 -> Some Dos_call
  | 0x27 -> Some Dos_keep
  | 0x2F -> Some Multiplex
  | 0x33 -> Some (Bios Mouse)
  | 0x67 -> Some Ems
  | 0x70 -> Some (Bios Rtc_irq)
  | _ -> None

(* ROM 안의 자리 배치. EMM 디바이스 헤더는 0(이름 칸이 세그먼트+0x0A
   라는 관례 때문에 0 번지여야 한다), INT 8 루틴은 0x40, IRET 스텁은
   0x20, 하드웨어 IRQ 기본 루틴은 0x30, 호스트 서빙 벡터의 되돌림 스텁은
   0x100 부터 16바이트씩. *)
let rom_seg = 0xF000
let timer_vector = 0x08
let emm_entry_off = 0x0000
let iret_stub_off = 0x0020
let hw_irq_off = 0x0030
let bios_int8_off = 0x0040
(* F000:0022 의 미디어 ID 바이트. INT 21h AH=1Bh·1Ch 가 DS:BX 로 가리킨다.
   MS-DOS 4.0 은 DOS 데이터의 고정 칸(FATBYTE)을 가리키고 사용자는 이 한
   바이트만 읽어야 한다(MISC.ASM $SLEAZEFUNC). F8h 는 하드 디스크다. IRET
   스텁(0x20)과 하드웨어 IRQ 루틴(0x30) 사이의 빈 곳이다. *)
let media_id_off = 0x0022
let media_id_fixed_disk = 0xF8
let stub_area = 0x0100
let stub_stride = 16
let rom_font_off = 0xFA6E

(* F000:FFFE 의 기계 모델 바이트. FCh 는 AT 다(SeaBIOS src/config.h
   BUILD_MODEL_ID, 28aa46349b97). 게임은 이 바이트로 PCjr(FDh)·Tandy
   (FFh)를 가린다. *)
let model_byte_off = 0xFFFE
let at_model_byte = 0xFC
(* 나라별 표(Dos_country): 대문자 루틴과 INT 21h AH=38h·63h·65h 가
   가리키는 표들. 스텁 자리(0x100 부터 16 바이트씩) 위의 빈 곳이다. *)
let country_off = 0x0400
let rom_font_first = 0x20
let rom_font_last = 0x7E

(* VGA BIOS 의 글꼴 표: INT 43h·1Fh 가 가리키고 AX=1130h 가 알려 준다.
   키는 글자 높이다. VGA ROM 의 8x14·8x16 글꼴이 없어서 8x8 글꼴을
   세로로 늘려 채운다 — 행·열 자리는 맞고 글자 모양만 다르다. 256자 표
   가운데 0x20~0x7E 만 채운다. 8x8 표의 뒤 절반(128자)이 INT 1Fh 표다. *)
let font_tables = [ (8, 0xA000); (14, 0xB000); (16, 0xC000) ]
let font_glyph_rows = 8
let font8_off = List.assoc font_glyph_rows font_tables
let cga_high_font_off = font8_off + (cga_high_first * font_glyph_rows)

(* AX=1130h BH=5·7 의 9점 대체 글꼴 목록. 목록 끝 표시(0) 하나뿐이라
   대체할 글자가 없다. *)
let alt_font_off = 0xD000

(* 호스트 구현으로 되돌리는 사설 벡터 번호 — 실벡터 + 0x80. *)
let private_vector v = v + 0x80

let country, country_rom = Dos_country.image ~segment:rom_seg ~at:country_off

(* The XMS control function (Dos_xms), far-called at F000:[xms_entry_off]
   after INT 2Fh AX=4310h. XMS 3.0 "IMPLEMENTATION NOTES" ask it to start
   with a short jump over three NOPs: a hooking program patches a far
   jump over those five bytes and chains to entry + 5. Its service id is
   43h, the multiplex number XMS answers to. It names the stub in
   [stubs] and picks the private vector; IVT[43h] is left alone (it is
   the graphics font pointer). *)
let xms_control_id = 0x43
let xms_hook_bytes = 5

let xms_control =
  "\xeb\x03\x90\x90\x90"                (* jmp short +3; nop; nop; nop *)
  ^ "\xcd" ^ String.make 1 (Char.chr (private_vector xms_control_id))
                                     (* int 0C3h — the host serves it *)
  ^ "\xcb"                            (* retf *)

let set_ivt t v off sg =
  wr16 t (v * 4) off;
  wr16 t ((v * 4) + 2) sg

(* VGA BIOS 는 모드를 세울 때 INT 43h 를 그 모드의 글자 높이 표로 돌린다. *)
let set_font_vector t =
  set_ivt t font_vector (List.assoc (Dos_video.char_height t.video) font_tables) rom_seg

(* AH=0Ah 는 그래픽 모드에서 색을 받지 않는다. 모드마다 가장 밝은 색으로
   그린다(DOSBox int10_char.cpp WriteChar). *)
let default_glyph_color t =
  match Dos_video.kind t.video with
  | Dos_video.Cga4 -> 0x03
  | Dos_video.Cga2 -> 0x01
  | Dos_video.Text | Dos_video.Planar | Dos_video.Linear256 -> 0x0F

(* 하루의 틱 수(SeaBIOS src/std/bda.h TICKS_PER_DAY). *)
let ticks_per_day = 0x1800B0

let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

(* IRQ0 에서 도는 ROM 루틴: 0x46C 틱을 올리고 INT 1Ch 사용자 훅을 부른
   뒤 PIC 에 EOI 를 보내고 IRET. 실기 BIOS 와 같은 사슬이다.

   틱이 하루치 이상이 되면 0 으로 돌리고 0x470 을 하나 올린다(SeaBIOS
   src/clock.c clock_update). DOS 는 INT 1Ah AH=00h 가 돌려주는 이 값을
   보고 날짜를 넘긴다.

   AX·DX·DS 를 저장한다. INT 1Ch 훅이 바꾼 레지스터는 인터럽트를 받은
   프로그램에 보이면 안 된다. SeaBIOS 는 handle_08 이 0 으로 채운 별도
   레지스터 묶음으로 INT 1Ch 를 불러 이 격리를 얻는다. Castle Adventure
   의 INT 1Ch 훅은 DX 를 저장하지 않고 `xor dx,dx` 로 지우므로, DX 를
   저장하지 않으면 틱이 끼어든 곳의 DX 가 0 이 된다. *)
let bios_int8 =
  "\x1e\x50\x52"                    (* push ds; push ax; push dx *)
  ^ "\x31\xc0\x8e\xd8"              (* xor ax,ax; mov ds,ax *)
  ^ "\xa1\x6c\x04\x40\xa3\x6c\x04"  (* mov ax,[046C]; inc ax; mov [046C],ax *)
  ^ "\x75\x04"                      (* jnz +4 — 자리올림 없으면 고워드 skip *)
  ^ "\xff\x06\x6e\x04"              (* inc word [046E] *)
  ^ "\x81\x3e\x6e\x04" ^ le16 (ticks_per_day lsr 16)     (* cmp word [046E],hi *)
  ^ "\x72\x16"                      (* jb hook — 하루가 안 됐다 *)
  ^ "\x77\x08"                      (* ja midnight *)
  ^ "\x81\x3e\x6c\x04" ^ le16 (ticks_per_day land 0xffff) (* cmp word [046C],lo *)
  ^ "\x72\x0c"                      (* jb hook *)
  ^ "\x31\xc0"                      (* midnight: xor ax,ax *)
  ^ "\xa3\x6c\x04\xa3\x6e\x04"      (* mov [046C],ax; mov [046E],ax *)
  ^ "\xfe\x06\x70\x04"              (* inc byte [0470] *)
  ^ "\xcd\x1c"                      (* hook: int 1Ch — 사용자 틱 훅 *)
  ^ "\xb0\x20\xe6\x20"              (* mov al,20h; out 20h,al (EOI) *)
  ^ "\x5a\x58\x1f\xcf"              (* pop dx; pop ax; pop ds; iret *)

(* IRQ0-7 의 벡터(08h-0Fh). 쓰는 장치가 없는 줄의 기본 루틴은 EOI 를
   보내고 IRET 한다 — SeaBIOS post.c 가 08h-0Fh 를 entry_hwpic1 로 채우고,
   handle_hwpic1 은 pic_eoi1(20h 를 포트 20h 로)만 한다. IMR 은 건드리지
   않는다. IRET 스텁이면 프로그램이 벡터를 걸기 전에 들어온 IRQ 가 ISR
   비트를 남겨, 그 줄이 다시 들어오지 않는다(#110). *)
let hw_irq_first_vector = 0x08
let hw_irq_lines = 8

let hw_irq_default =
  "\x50"                            (* push ax *)
  ^ "\xb0\x20\xe6\x20"              (* mov al,20h; out 20h,al (EOI) *)
  ^ "\x58\xcf"                      (* pop ax; iret *)

let is_hw_irq_vector v = v >= hw_irq_first_vector && v < hw_irq_first_vector + hw_irq_lines

(* IRQ8-15 의 벡터(70h-77h). 두 번째 8259 의 줄이라 처리기는 두 칩 모두에
   EOI 를 보낸다, 두 번째 칩 먼저(SeaBIOS src/hw/pic.h pic_eoi2,
   28aa46349b97). 쓰는 장치가 없는 줄의 기본 루틴은 그것만 한다(SeaBIOS
   handle_hwpic2). *)
let slave_irq_first_vector = Dos_pic.vector Dos_pic.Rtc
let is_slave_irq_vector v =
  v >= slave_irq_first_vector && v < slave_irq_first_vector + hw_irq_lines

let slave_eoi =
  "\xb0\x20\xe6\xa0"              (* mov al,20h; out 0A0h,al (두 번째 칩 EOI) *)
  ^ "\xe6\x20"                      (* out 20h,al (첫 칩 EOI) *)

let hw_irq2_default = "\x50" ^ slave_eoi ^ "\x58\xcf"   (* push ax ... pop ax; iret *)


(* The XMS control function goes on the first paragraph after the INT 8
   routine, so a longer routine moves it instead of being overwritten. *)
let paragraph = 16
let xms_entry_off =
  (bios_int8_off + String.length bios_int8 + paragraph - 1) / paragraph * paragraph

let next_paragraph off = (off + paragraph - 1) / paragraph * paragraph

(* IRQ9-15 루틴은 XMS 진입점 뒤 문단, 스텁 영역(0100h) 앞에 둔다. IRQ8 은
   호스트가 받는다(INT 70h, [rtc_irq]). *)
let hw_irq2_off = next_paragraph (xms_entry_off + String.length xms_control)

(* INT 1Ah AH=00h(틱 읽기)는 읽기 전에 인터럽트를 잠깐 허용한다. SeaBIOS
   handle_1a00 이 먼저 yield() 를 부르고 yield() 는 check_irqs() 의
   "sti ; nop ; rep ; nop ; cli ; cld" 로 보류된 IRQ 를 받게 한다(src/clock.c,
   src/stacks.c). 인터럽트를 끈 채 틱이 늘기를 기다리는 지연 루프가 이 창에서
   IRQ0 을 받아야 틱이 오른다(Arcy 2). 창은 게스트 코드여야 한다: IRQ0 은
   게스트 ISR(INT 1Ch 사용자 훅 포함)을 부른다.

   기계는 이 호출을 호스트가 바로 답하지 않고 이 루틴으로 보낸다. 창을 연 뒤
   int 사설벡터 로 호스트가 읽고 iret 로 돌아간다. 사설 벡터는 int 명령이
   등록한 스텁 자리에 있어야 알아본다([private_service_vector]). *)
let time_of_day_vector = 0x1A
let () = assert (host_vector time_of_day_vector = Some (Bios Time_of_day))

let irq_window_prefix = "\xFB\x90\xFA"        (* sti; nop; cli *)
let irq_window =
  irq_window_prefix ^ "\xCD" ^ String.make 1 (Char.chr (private_vector time_of_day_vector))
  ^ "\xCF"                                     (* iret *)
let irq_window_int_off = String.length irq_window_prefix

let irq_window_off = next_paragraph (hw_irq2_off + String.length hw_irq2_default)
let () = assert (irq_window_off + String.length irq_window <= stub_area)

(* 호스트가 답하기 전에 인터럽트 창을 여는 호출. *)
let opens_irq_window ~vector ~ah =
  match host_vector vector with
  | Some (Bios Time_of_day) -> ah = 0x00
  | _ -> false

(* 시각을 틱으로: SeaBIOS src/hw/timer.c ticks_from_ms. PM 타이머
   3579545Hz 를 거쳐 PIT 분주비 65536 으로 나누고 둘 다 올림한다. *)
let pmtimer_hz = 3579545
let pmtimer_to_pit = 3
let pit_tick_interval = 65536
let ms_per_second = 1000
let ceil_div a b = (a + b - 1) / b

let ticks_of_time ~hour ~minute ~second =
  let ms = (((hour * 60) + minute) * 60 + second) * ms_per_second in
  let pm = ceil_div (ms * pmtimer_hz) pit_tick_interval in
  ceil_div pm (ms_per_second * pmtimer_to_pit)

(* 부팅 때 BIOS 가 RTC 를 읽어 틱을 채운다(SeaBIOS src/clock.c
   clock_setup). 자정 넘김 표시는 지운다. now_fields 의 시는 0..23 이라
   틱은 하루를 넘지 않는다(23:59:59 가 1573025). *)
let set_ticks_from_clock t =
  let _, _, _, hour, minute, second = now_fields t in
  write_ticks t (ticks_of_time ~hour ~minute ~second);
  wr8 t bda_midnight 0

(* CuteMouse setupvideo(ctmouse.asm): 범위를 지금 모드의 가상 화면 전체로,
   커서를 그 가운데로. 드라이버가 상주할 때, 00h·21h 리셋, INT 10h 모드
   세우기가 부른다(HelpPC 2.10 INT 33,0: "mouse is positioned to screen
   center", "max width and height are set to maximum for video mode"). *)
(* 드라이버가 커서를 옮기면(04h, 화면 맞추기) 움직임 나머지를 버린다 —
   CuteMouse 의 rounderr 를 setpos_04 와 setupvideo 가 지운다. *)
let clear_mickey_carry m =
  m.mouse_carry_x <- 0;
  m.mouse_carry_y <- 0

let mouse_fit_screen t =
  let m = t.mouse in
  let space = Dos_mouse_space.current t.mem in
  m.mouse_range_x <- Dos_mouse_space.range_x space;
  m.mouse_range_y <- Dos_mouse_space.range_y space;
  let x, y = Dos_mouse_space.center space in
  m.mouse_x <- x;
  m.mouse_y <- y;
  clear_mickey_carry m

let equipment_word =
  (* bit0 디스켓 있음, bit1 코프로세서 없음(8087 미장착 — ESC 명령은
     아무 일도 하지 않는다), bits4-5 = 10b 컬러 80x25, bits14-15 프린터 1 개 *)
  0x0001 lor 0x0020 lor 0x4000

(* 비디오 BIOS 의 정적 기능 표(RBIL INT 10/AH=1Bh, 표 00045). INT 10h
   AH=1Bh 가 이 자리를 가리킨다. 실기 VGA BIOS 는 C000 세그먼트에 살고
   DOSBox-staging 도 이 표를 C000 에 둔다. 앞 256 바이트는 ROM 머리글
   자리로 비워 둔다. *)
let video_rom_seg = 0xC000
let static_table_off = 0x0100

(* 모드 0-7·0Dh-13h — Dos_video 의 모드표가 아는 번호다. SeaBIOS 가
   stdvga 모드 목록으로 만드는 값과 같다. DOSBox-staging 은 PCjr·Tandy
   모드(8-0Ch)까지 켠 FFh FFh 0Fh 를 낸다. *)
let static_modes = [ 0xFF; 0xE0; 0x0F ]
let static_scan_lines = 0x07           (* 200·350·400 *)

(* 글꼴은 ROM 8x8 하나뿐이다. AH=11h 의 글꼴 싣기가 없다. *)
let static_char_blocks = 1

(* 기능 지원 깃발(표 00046). 이 기계가 하는 것만 켠다. *)
let fn_gray_summing = 0x0002           (* AH=12h BL=33h, AX=101Bh *)
let fn_palette_loading = 0x0008        (* AH=12h BL=31h *)
let fn_cursor_emulation = 0x0010       (* AH=12h BL=34h — 비트만 저장한다 *)
let fn_ega_palette = 0x0020            (* AX=1000h·1002h·1007h·1009h *)
let fn_color_palette = 0x0040          (* AX=1010h·1012h·1015h·1017h *)
let fn_dcc = 0x0800                    (* AX=1A00h *)
let static_functions =
  fn_gray_summing lor fn_palette_loading lor fn_cursor_emulation
  lor fn_ega_palette lor fn_color_palette lor fn_dcc

let static_table =
  static_modes
  @ [ 0; 0; 0; 0; static_scan_lines; static_char_blocks; static_char_blocks;
      static_functions land 0xff; static_functions lsr 8; 0; 0; 0; 0 ]

(* EGA/VGA BIOS 데이터: 0x487 은 EGA 기타 정보(bit 0 = 커서 모양 흉내 끔),
   0x488 은 스위치, 0x489 는 VGA 모드 세우기 깃발(bit 7·4 = 200·400 주사선,
   bit 3 = 기본 팔레트 싣기 끔). 비트 자리는 DOSBox-staging int10.cpp 와
   같다. SeaBIOS 1231h 는 0x487 bit 3 을 바꾸지만 정작 그쪽 모드 세우기는
   0x489 bit 3 을 본다. *)
let bda_ega_misc = 0x487
let bda_switches = 0x488
let bda_vga_flags = 0x489
let ega_misc_no_cursor_emulation = 0x01
let vga_flag_200_lines = 0x80
let vga_flag_400_lines = 0x10
let vga_flag_keep_palette = 0x08
(* bit 1 = 회색 합산 켬, bit 2 = 흑백 모니터. 둘 중 하나라도 서 있으면
   AX=1010h·1012h 가 색을 회색으로 바꿔 싣는다(DOSBox-staging int10_pal.cpp
   INT10_SetSingleDACRegister 의 0x06 마스크). 모드 세우기는 bit 1 만 본다. *)
let vga_flag_gray_summing = 0x02
let vga_flag_mono_display = 0x04
let switches_kept = 0xF0
let switches_cga_emulation = 0x08
let switches_ega_enhanced = 0x09

(* 부팅과 모드 세우기가 적는 값. SeaBIOS(vgainit.c init_bios_area, vgabios.c
   vga_set_mode)와 DOSBox-staging(int10.cpp INT10_Seg40Init)이 같다.
   - 0x487 60h: bit 5-6 = 256KB. 모드 세우기는 bit 7 에 AL bit 7(화면
     보존)을 옮기고, 커서 흉내 끔(bit 0) 같은 다른 비트는 지운다.
   - 0x488 F9h: 위 니블 = 연결 단자 비트, 아래 니블 = 스위치 9h.
   - 0x489 51h: VGA 켬(bit 0), 400 줄(bit 4), DCC 바꾸기 켬(bit 6). 모드
     세우기는 bit 7(200 줄)만 지운다. *)
let ega_misc_256k = 0x60
let ega_misc_screen_kept = 0x80
let switches_post = 0xF9
let vga_flags_post = 0x51
let bda_crtc_port = 0x463             (* CRTC 어드레스 포트, 워드 *)

let install t =
  (* EMM 디바이스 헤더(Dos_ems). 진입 스텁 "int 사설벡터; iret" 뒤에,
     DOS 디바이스 관례대로 +0x0A 에 드라이버 이름이 온다. EMS 런타임은
     INT 21h AH=35h AL=67h 로 이 칸을 읽어 드라이버를 알아본다(삼국지3
     실측). *)
  wr8 t (rom_seg * 16) 0xCD;
  wr8 t ((rom_seg * 16) + 1) ((0x67 + 0x80) land 0xff);
  wr8 t ((rom_seg * 16) + 2) 0xCF;
  String.iteri
    (fun i c -> wr8 t ((rom_seg * 16) + 0x0A + i) (Char.code c))
    Dos_ems.emm_name;
  Bytes.blit (Bytes.of_string bios_int8) 0 t.mem
    ((rom_seg * 16) + bios_int8_off)
    (String.length bios_int8);
  wr8 t ((rom_seg * 16) + iret_stub_off) 0xCF;   (* IRET 스텁 *)
  wr8 t ((rom_seg * 16) + media_id_off) media_id_fixed_disk;
  wr8 t ((rom_seg * 16) + model_byte_off) at_model_byte;
  String.iteri
    (fun i c -> wr8 t ((rom_seg * 16) + country_off + i) (Char.code c))
    country_rom;
  Bytes.blit_string xms_control 0 t.mem
    ((rom_seg * 16) + xms_entry_off)
    (String.length xms_control);
  t.stubs <- (xms_control_id, xms_entry_off + xms_hook_bytes) :: t.stubs;
  Bytes.blit_string hw_irq_default 0 t.mem
    ((rom_seg * 16) + hw_irq_off)
    (String.length hw_irq_default);
  Bytes.blit_string hw_irq2_default 0 t.mem
    ((rom_seg * 16) + hw_irq2_off)
    (String.length hw_irq2_default);
  Bytes.blit_string irq_window 0 t.mem ((rom_seg * 16) + irq_window_off) (String.length irq_window);
  t.stubs <- (time_of_day_vector, irq_window_off + irq_window_int_off) :: t.stubs;
  set_ivt t timer_vector bios_int8_off rom_seg;  (* 타이머 IRQ0 *)
  (* POST 는 부팅 모드를 세우며 화면을 지운다: 공백, 속성 07h. 이게
     없으면 화면 메모리가 0(검정 위 검정)이라, 칸의 속성을 두는 teletype
     과 DOS 출력이 보이지 않는다. *)
  Dos_video.set_mode t.video (Dos_video.mode t.video) ~clear:true;
  (* BIOS 데이터 영역 — 실기 부팅 값. CRT 루틴이 화면 폴링 포트를
     [0x40:0x63](CRTC 베이스) + 6 으로 계산한다: 0 이면 포트 6 을 읽어
     영원히 갇힌다(실측). 컬러 80x25 텍스트 기준. *)
  wr16 t 0x410 equipment_word;
  wr16 t 0x413 Dos_cmos.base_memory_kb;  (* 기억용량 KB — CMOS 15h·16h 와 같다 *)
  set_ticks_from_clock t;
  wr8 t 0x417 0;                    (* 시프트 상태 *)
  wr8 t 0x418 0;
  wr16 t 0x41A ring_lo;
  wr16 t 0x41C ring_lo;
  wr16 t 0x480 ring_lo;             (* 키 버퍼 시작 *)
  wr16 t 0x482 (ring_hi + 2);       (* 키 버퍼 끝 *)
  wr8 t 0x449 3;                    (* 비디오 모드 3 *)
  wr16 t 0x44A (cols t);
  wr16 t 0x44C 0x1000;              (* 페이지 크기(바이트) 4KB *)
  wr16 t 0x44E 0;                   (* 페이지 시작 오프셋 *)
  wr16 t bda_crtc_port 0x3D4;       (* CRTC 어드레스 포트(컬러) *)
  wr8 t 0x465 0x29;                 (* CRT 모드 레지스터 사본 *)
  wr8 t 0x484 (rows t - 1);
  wr16 t 0x485 (Dos_video.char_height t.video);  (* 글자 높이 *)
  wr8 t 0x462 0;                    (* 활성 페이지 *)
  wr8 t 0x460 7; wr8 t 0x461 6;     (* 커서 모양 *)
  set_cursor t 0 0;
  List.iteri (fun i b -> wr8 t ((video_rom_seg * 16) + static_table_off + i) b) static_table;
  wr8 t bda_ega_misc ega_misc_256k;
  wr8 t bda_switches switches_post;
  wr8 t bda_vga_flags vga_flags_post;
  (* 호스트가 서빙하는 벡터도 IVT 에 실주소가 있어야 한다 — Turbo
     Pascal 의 Intr 썽크는 INT 명령 없이 IVT 를 직접 읽어 그 주소로
     retf 한다(실측: 빈 IVT[10h] 로 0000:0000 추락). 스텁 본체는
     "int 사설벡터; iret" 이고, 사설 벡터는 비어 있어 호스트 훅이 받는다. *)
  (* EMS 는 헤더 0 번지의 스텁으로 간다 — EMS 런타임이 세그먼트+0x0A 를
     읽기 때문이다. 나머지 벡터는 전부 IRET 스텁으로 채운다. 실기 BIOS 는
     모든 벡터가 ROM/DOS 를 가리켜서, 프로그램이 훅을 걸며 되읽은 "옛
     벡터" 가 0:0 이 아니다. 하나라도 비워 두면 체인 복귀가 0000:0000
     으로 떨어진다. *)
  let off = ref stub_area in
  for v = 0 to 255 do
    match host_vector v with
    | Some Ems ->
      set_ivt t v emm_entry_off rom_seg;
      t.stubs <- (v, emm_entry_off) :: t.stubs
    | Some (Bios _ | Dos_divide_overflow | Dos_terminate | Dos_call | Dos_keep | Multiplex) ->
      let a = (rom_seg * 16) + !off in
      wr8 t a 0xCD;
      wr8 t (a + 1) (private_vector v);
      wr8 t (a + 2) 0xCF;
      set_ivt t v !off rom_seg;
      t.stubs <- (v, !off) :: t.stubs;
      off := !off + stub_stride
    | None ->
      (* 쓰는 장치가 없는 하드웨어 IRQ 줄은 EOI 루틴, 그 밖은 IRET 스텁 *)
      if v <> timer_vector then
        set_ivt t v
          (if is_hw_irq_vector v then hw_irq_off
           else if is_slave_irq_vector v then hw_irq2_off
           else iret_stub_off)
          rom_seg
  done;
  (* IBM PC BIOS(5150)는 8x8 글꼴 표를 F000:FA6E 에 둔다. 그래픽 모드에서
     글자를 직접 찍는 프로그램이 BIOS 를 안 거치고 이 표를 읽는다 —
     삼국지3 MAIN.EXE 의 글자 루틴이 es=0F000h, si=0FA6Eh+ch*8 로
     글리프를 가져온다(실측). 표가 비면 글리프가 전부 0 이라 아무 것도
     안 그린다. 0x20-0x7E 만 심는다 — Font8x8 이 가진 범위고,
     0x7E*8 을 더해도 1MB 경계 안(0xFFE66)이다. 한글·그림문자는 게임이
     자체 데이터 파일에서 가져오므로 여기 없어도 된다. *)
  for ch = rom_font_first to rom_font_last do
    Array.iteri
      (fun row b ->
        wr8 t ((rom_seg * 16) + rom_font_off + (ch * 8) + row) b)
      (Font8x8.glyph ch)
  done;
  List.iter
    (fun (height, off) ->
      for ch = rom_font_first to rom_font_last do
        let g = Font8x8.glyph ch in
        for line = 0 to height - 1 do
          wr8 t ((rom_seg * 16) + off + (ch * height) + line)
            g.(line * font_glyph_rows / height)
        done
      done)
    font_tables;
  wr8 t ((rom_seg * 16) + alt_font_off) 0;
  set_ivt t cga_high_font_vector cga_high_font_off rom_seg;
  set_font_vector t;
  (* 상주한 마우스 드라이버는 설치할 때 부팅 모드에 맞춘다 *)
  mouse_fit_screen t

(* IVT[v] 가 아직 우리 스텁이면(게스트가 AH=25h 로 안 바꿨으면) 호스트
   구현으로 바로 간다 — deliver 로 다시 스텁에 들어가면 무한루프다. *)
let ivt_is_own_stub t v =
  List.exists
    (fun (sv, soff) ->
      sv = v
      && rd16 t (v * 4) = soff
      && rd16 t ((v * 4) + 2) = rom_seg)
    t.stubs

let ivt_is_iret_stub t v =
  rd16 t (v * 4) = iret_stub_off && rd16 t ((v * 4) + 2) = rom_seg

let private_service_vector t raw =
  List.find_map
    (fun (v, off) ->
      if raw = private_vector v && Cpu86.seg t.cpu 1 = rom_seg
         && Cpu86.dump_ip t.cpu = off + 2
      then Some v else None)
    t.stubs

let to_bcd n = ((n / 10) * 16) + (n mod 10)

(* ---------- INT 10h 비디오 ---------- *)

let alt_select_done = 0x12

(* 회색 = 0.30 R + 0.59 G + 0.11 B 를 256 단위로 반올림한다(SeaBIOS
   stdvga_perform_gray_scale_summing, DOSBox-staging
   INT10_PerformGrayScaleSumming 이 같은 식이다). 세 무게의 합이 256 이라
   6비트 색에서 나온 값은 6비트를 넘지 않는다. 두 구현의 0x3F 자르기는
   여기서 닿지 않아 두지 않는다. *)
let gray_red = 77
let gray_green = 151
let gray_blue = 28
let gray_round = 0x80
let gray_shift = 8

let gray (r, g, b) =
  let v = ((gray_red * r) + (gray_green * g) + (gray_blue * b) + gray_round) lsr gray_shift in
  (v, v, v)

(* DAC [first] 부터 [count] 자리를 회색으로. 번호는 DAC 끝에서 0 으로 돈다. *)
let sum_to_gray pal ~first ~count =
  let n = Array.length pal in
  for i = 0 to min count n - 1 do
    let k = (first + i) mod n in
    pal.(k) <- gray pal.(k)
  done

let set_bda_bit t addr bit on =
  let v = rd8 t addr in
  wr8 t addr (if on then v lor bit else v land lnot bit)

(* 모드 세우기 뒤의 0x487·0x488·0x489(SeaBIOS vga_set_mode) *)
let modeset_bda t ~screen_kept =
  wr8 t bda_ega_misc (ega_misc_256k lor (if screen_kept then ega_misc_screen_kept else 0));
  wr8 t bda_switches switches_post;
  wr8 t bda_vga_flags (rd8 t bda_vga_flags land lnot vga_flag_200_lines)

(* AH=12h BL=10h: BH = 1 이면 흑백(CRTC 3B4h), CL = 스위치, CH = 연결 단자 *)
let crtc_mono = 0x3B4
let switch_bits = 0x0F
let feature_shift = 4

(* 디스플레이 조합 코드(AX=1A00h) 08h = VGA 컬러 *)
let dcc_vga_color = 0x08

(* INT 10h AH=1Bh BX=0 이 ES:DI 에 쓰는 64 바이트(RBIL 표 00040) *)
let state_size = 64
let state_bda_low = 0x449              (* 모드 ~ 3x9 사본, 30 바이트 *)
let state_bda_low_len = 30
let state_bda_rows = 0x484             (* 행 수 - 1, 글자 높이, 3 바이트 *)
let state_bda_rows_len = 3
let at_bda_low = 0x04
let at_bda_rows = 0x22
let at_dcc = 0x25                      (* 26h(다른 화면)는 00h = 없음, 지운 값 그대로 *)
let at_colors = 0x27
let at_pages = 0x29
let at_scan_lines = 0x2A
let at_misc = 0x2D
let at_video_mem = 0x31
let video_mem_256k = 3

let scan_200 = 0
let scan_350 = 1
let scan_400 = 2
let scan_480 = 3

(* 지금 모드의 색 수, 쪽 수, 주사선 번호. 쪽 수는 DOSBox-staging
   int10_modes.cpp ModeList_VGA 의 ptotal 이다. 흑백 모드(7, 0Fh)의 색
   수는 0 이다(RBIL 표 00040 27h). 텍스트 그림은 늘 400 줄이다. *)
let mode_shape t =
  match Dos_video.kind t.video with
  | Dos_video.Text -> ((if Dos_video.mode t.video = 7 then 0 else 16), 8, scan_400)
  | Dos_video.Cga4 -> (4, 1, scan_200)
  | Dos_video.Cga2 -> (2, 1, scan_200)
  | Dos_video.Planar ->
    (match Dos_video.mode t.video with
     | 0x0D -> (16, 8, scan_200)
     | 0x0E -> (16, 4, scan_200)
     | 0x0F -> (0, 2, scan_350)
     | 0x10 -> (16, 2, scan_350)
     | 0x11 -> (2, 1, scan_480)
     | _ -> (16, 1, scan_480)          (* 12h — 16색 모드는 0Dh-12h 뿐이다 *)
    )
  | Dos_video.Linear256 -> (256, 1, scan_200)

(* 상태 깃발(표 00041). bit 1-3(회색 합산, 흑백 모니터, 기본 팔레트 싣기
   끔)은 BDA 0x489 의 같은 비트다. bit 4 는 커서 모양 흉내가 켜져 있으면
   (0x487 bit 0 이 0) 선다. bit 5(1 = 깜박임, 0 = 바탕 밝기)는 텍스트
   모드에서 선다. Dos_render 가 속성 bit 7 을 바탕색에 쓰지 않기 때문이다.
   깜박이게 그리지는 않는다. bit 0 을 정하는 BDA 자리는 확인하지 못해 0 으로
   둔다. *)
let misc_cursor_emulation = 0x10
let misc_blink = 0x20

let state_misc t =
  (rd8 t bda_vga_flags
   land (vga_flag_gray_summing lor vga_flag_mono_display lor vga_flag_keep_palette))
  lor (if rd8 t bda_ega_misc land ega_misc_no_cursor_emulation = 0 then misc_cursor_emulation
       else 0)
  lor (match Dos_video.kind t.video with
       | Dos_video.Text -> misc_blink
       | Dos_video.Cga4 | Dos_video.Cga2 | Dos_video.Planar | Dos_video.Linear256 -> 0)

(* ES:DI 부터 64 바이트. 주소는 세그먼트 안에서 돈다. 정해지지 않은 칸은
   0 이다(SeaBIOS handle_101b 처럼 먼저 다 지운다). *)
let write_state_info t ~seg ~off =
  let put i v = wr8 t ((seg lsl 4) + ((off + i) land 0xFFFF)) (v land 0xff) in
  let put16 i v = put i v; put (i + 1) (v lsr 8) in
  for i = 0 to state_size - 1 do put i 0 done;
  put16 0 static_table_off;
  put16 2 video_rom_seg;
  for i = 0 to state_bda_low_len - 1 do put (at_bda_low + i) (rd8 t (state_bda_low + i)) done;
  for i = 0 to state_bda_rows_len - 1 do put (at_bda_rows + i) (rd8 t (state_bda_rows + i)) done;
  put at_dcc dcc_vga_color;
  let colors, pages, scan = mode_shape t in
  put16 at_colors colors;
  put at_pages pages;
  put at_scan_lines scan;
  put at_misc (state_misc t);
  put at_video_mem video_mem_256k

(* 속성 컨트롤러의 색 번호 레지스터 00h~0Fh *)
let palette_regs = 16

(* 포트 0x3D9 의 비트: 하위 4 비트가 바탕색, 비트 4 가 밝기, 비트 5 가
   팔레트(Dos_render.cga4_colors). *)
let cga_color_select_port = 0x3D9
let cga_background_bits = 0x1F
let cga_palette_bit = 0x20

let video t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 and al = Cpu86.reg8 cpu 0 in
  match ah with
  | 0x00 ->
    (* AL 의 최상위 비트는 "화면을 지우지 말라" 는 뜻이다. *)
    let mode = al land 0x7f in
    (* 마우스 드라이버는 모드를 세우기 전에 커서를 숨긴다(CuteMouse
       int10handler 가 INT 33h 02h 를 부른다). 화면을 지우지 않는 모드
       세우기라도 커서 자국이 남지 않는다. *)
    Dos_mouse_cursor.restore t;
    Dos_video.set_mode t.video mode ~clear:(al land 0x80 = 0);
    modeset_bda t ~screen_kept:(al land 0x80 <> 0);
    (* 실기 VGA BIOS 는 모드를 세울 때 DAC 을 기본값으로 다시 싣는다.
       AL bit7(화면 보존)과는 상관없다. 막는 길은 BDA 0x489 bit3
       (기본 팔레트 싣기 금지, AX=1201h BL=31h 가 켜는 비트) 하나다. *)
    if rd8 t bda_vga_flags land vga_flag_keep_palette = 0 then begin
      Dos_ports.reset_dac t.ports;
      if rd8 t bda_vga_flags land vga_flag_gray_summing <> 0 then
        let pal = Dos_ports.palette t.ports in
        sum_to_gray pal ~first:0 ~count:(Array.length pal)
    end;
    wr8 t 0x449 mode;
    wr16 t 0x44A (cols t);
    reset_pages t;
    wr8 t 0x484 (rows t - 1);
    wr16 t 0x485 (Dos_video.char_height t.video);
    set_font_vector t;
    set_cursor t 0 0;
    (* 마우스 드라이버가 모드 세우기를 가로챈다(CuteMouse int10handler):
       커서를 숨기고(깃발 -1) 새 모드의 가상 화면에 맞춘다. 커서 모양,
       핸들러, 버튼 기록은 그대로다. *)
    if t.mouse.mouse_present then begin
      t.mouse.mouse_cursor_flag <- cursor_flag_after_reset;
      mouse_fit_screen t
    end
  | 0x01 ->
    (* 커서 모양: CH=시작 스캔라인, CL=끝 *)
    Dos_ports.port_out t.ports 0x3D4 0x0A;
    Dos_ports.port_out t.ports 0x3D5 (Cpu86.reg8 cpu 5);
    Dos_ports.port_out t.ports 0x3D4 0x0B;
    Dos_ports.port_out t.ports 0x3D5 (Cpu86.reg8 cpu 1)
  | 0x02 -> set_cursor_on t ~page:(Cpu86.reg8 cpu 7) (Cpu86.reg8 cpu 2) (Cpu86.reg8 cpu 6)
  | 0x03 ->
    let col, row = get_cursor_on t ~page:(Cpu86.reg8 cpu 7) in
    Cpu86.set_reg8 cpu 2 col;
    Cpu86.set_reg8 cpu 6 row;
    Cpu86.set_reg8 cpu 5 (rd8 t 0x461);
    Cpu86.set_reg8 cpu 1 (rd8 t 0x460)
  | 0x05 -> select_page t al
  | 0x06 | 0x07 ->
    scroll_window t ~up:(ah = 0x06) ~lines:al
      ~top:(Cpu86.reg8 cpu 5) ~left:(Cpu86.reg8 cpu 1)
      ~bottom:(Cpu86.reg8 cpu 6) ~right:(Cpu86.reg8 cpu 2)
      ~attr:(Cpu86.reg8 cpu 7)
  | 0x08 ->
    let page = Cpu86.reg8 cpu 7 in
    let col, row = get_cursor_on t ~page in
    Cpu86.set_reg16 cpu 0 (read_cell_on t ~page row col)
  | (0x09 | 0x0A) when graphics t ->
    (* 그래픽 모드: 글꼴 표의 점을 CX 번 찍는다. AH=09h 는 BL 이 색이고
       bit7 이면 XOR 이다. AH=0Ah 는 모드의 기본색을 쓴다. 256색 모드의
       바탕은 BH 다. 커서는 움직이지 않는다. *)
    let fg = if ah = 0x09 then Cpu86.reg8 cpu 3 else default_glyph_color t in
    let bg = glyph_background t ~fg ~back256:(Cpu86.reg8 cpu 7) in
    let col, row = get_cursor t in
    let r = ref row and c = ref col in
    for _ = 1 to max 1 (Cpu86.reg16 cpu 1) do
      if !r < rows t then begin
        write_glyph t ~col:!c ~row:!r al ~fg ~bg;
        incr c;
        if !c >= cols t then begin c := 0; incr r end
      end
    done
  | 0x09 | 0x0A ->
    (* BH 쪽의 커서 자리에 같은 글자를 CX 번. 커서는 움직이지 않는다.
       AH=09 는 BL 을 속성으로 쓰고, AH=0A 는 화면의 속성을 남긴다. *)
    let page = Cpu86.reg8 cpu 7 in
    let col, row = get_cursor_on t ~page in
    let n = max 1 (Cpu86.reg16 cpu 1) in
    let r = ref row and c = ref col in
    for _ = 1 to n do
      if !r < rows t then begin
        let attr =
          if ah = 0x09 then Cpu86.reg8 cpu 3
          else (read_cell_on t ~page !r !c) lsr 8
        in
        write_cell_on t ~page !r !c ((attr lsl 8) lor al);
        incr c;
        if !c >= cols t then begin c := 0; incr r end
      end
    done
  | 0x0E ->
    (* 텍스트 모드는 칸의 속성을 둔다. 그래픽 모드에서 BL 은 글자 색
       전체다(256색 모드면 0~255). *)
    teletype t al (Cpu86.reg8 cpu 3)
  | 0x0C ->
    (* 점 찍기: AL=색, CX=x, DX=y. BH(쪽)는 무시하고 늘 0 쪽에 찍는다. *)
    Dos_video.put_pixel t.video ~x:(Cpu86.reg16 cpu 1) ~y:(Cpu86.reg16 cpu 2)
      ~color:al
  | 0x0D ->
    Cpu86.set_reg8 cpu 0
      (Dos_video.get_pixel t.video ~x:(Cpu86.reg16 cpu 1)
         ~y:(Cpu86.reg16 cpu 2))
  | 0x0F ->
    (* AL bit 7 = 마지막 모드 세우기가 화면을 지우지 않았다(0x487 bit 7) *)
    Cpu86.set_reg8 cpu 0 (Dos_video.mode t.video lor (rd8 t bda_ega_misc land ega_misc_screen_kept));
    Cpu86.set_reg8 cpu 4 (cols t);
    Cpu86.set_reg8 cpu 7 (rd8 t 0x462)
  | 0x10 ->
    let pal = Dos_ports.palette t.ports in
    let summing = rd8 t bda_vga_flags land (vga_flag_gray_summing lor vga_flag_mono_display) <> 0 in
    let load rgb = if summing then gray rgb else rgb in
    (match al with
     | 0x00 ->
       (* 색 번호 하나를 DAC 자리로: BL=색 번호, BH=DAC 자리 *)
       Dos_video.set_attr_palette t.video (Cpu86.reg8 cpu 3)
         (Cpu86.reg8 cpu 7)
     | 0x01 -> Dos_video.set_overscan t.video (Cpu86.reg8 cpu 7)
     | 0x02 ->
       (* 열여섯 색 + 테두리를 한 번에: ES:DX 가 17 바이트. 마지막 바이트는
          테두리(속성 11h)다 — 그다음 번호인 10h(모드 제어)가 아니다
          (RBIL INT 10/AX=1002h, SeaBIOS stdvga_set_all_palette_reg). *)
       let src = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 2 in
       for i = 0 to palette_regs - 1 do
         Dos_video.set_attr_palette t.video i (rd8 t (src + i))
       done;
       Dos_video.set_overscan t.video (rd8 t (src + palette_regs))
     | 0x03 ->
       (* BL bit 0: 1 이면 깜박임, 0 이면 바탕 열여섯 색(SeaBIOS
          stdvga_set_palette_blinking) *)
       Dos_video.set_text_blink t.video (Cpu86.reg8 cpu 3 land 1 <> 0)
     | 0x07 ->
       (* 속성 레지스터는 00h~14h 다. 그 밖의 BL 은 BH 를 두고 돌아간다. *)
       let attr = Dos_video.attr_palette t.video in
       let bl = Cpu86.reg8 cpu 3 in
       if bl < Array.length attr then Cpu86.set_reg8 cpu 7 attr.(bl)
     | 0x08 -> Cpu86.set_reg8 cpu 7 (Dos_video.overscan t.video)
     | 0x09 ->
       let dst = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 2 in
       let attr = Dos_video.attr_palette t.video in
       for i = 0 to palette_regs - 1 do wr8 t (dst + i) attr.(i) done;
       wr8 t (dst + palette_regs) (Dos_video.overscan t.video)
     | 0x10 ->
       (* 한 색: BX=번호, DH=R CH=G CL=B *)
       let idx = Cpu86.reg16 cpu 3 land 0xff in
       pal.(idx) <-
         load
           (Cpu86.reg8 cpu 6 land 0x3f, Cpu86.reg8 cpu 5 land 0x3f,
            Cpu86.reg8 cpu 1 land 0x3f)
     | 0x12 ->
       (* 여러 색: BX=첫 번호, CX=개수, ES:DX=R,G,B 배열 *)
       let first = Cpu86.reg16 cpu 3 land 0xff in
       let count = Cpu86.reg16 cpu 1 in
       let src = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 2 in
       for i = 0 to count - 1 do
         let k = (first + i) land 0xff in
         pal.(k) <-
           load
             (rd8 t (src + (i * 3)) land 0x3f,
              rd8 t (src + (i * 3) + 1) land 0x3f,
              rd8 t (src + (i * 3) + 2) land 0x3f)
       done
     | 0x1B ->
       (* BX 부터 CX 자리를 회색으로. 깃발과 상관없이 바꾼다 *)
       sum_to_gray pal ~first:(Cpu86.reg16 cpu 3) ~count:(Cpu86.reg16 cpu 1)
     | 0x15 ->
       let r, g, b = pal.(Cpu86.reg16 cpu 3 land 0xff) in
       Cpu86.set_reg8 cpu 6 r; Cpu86.set_reg8 cpu 5 g; Cpu86.set_reg8 cpu 1 b
     | 0x1A ->
       (* 색 페이지 상태 읽기(VGA 전용). BL=페이징 방식, BH=현재 쪽(RBIL INT
          10/AX=101Ah). 게임은 BX=FFFFh 로 불러 BX 가 바뀌었는지로 VGA 인지
          가린다 — 이 서브기능이 없는 BIOS 는 BX 를 그대로 둔다. *)
       Cpu86.set_reg8 cpu 3 (Dos_video.color_paging t.video);
       Cpu86.set_reg8 cpu 7 (Dos_video.color_page t.video)
     | 0x17 ->
       let first = Cpu86.reg16 cpu 3 land 0xff in
       let count = Cpu86.reg16 cpu 1 in
       let dst = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 2 in
       for i = 0 to count - 1 do
         let r, g, b = pal.((first + i) land 0xff) in
         wr8 t (dst + (i * 3)) r;
         wr8 t (dst + (i * 3) + 1) g;
         wr8 t (dst + (i * 3) + 2) b
       done
     | _ -> ())
  | 0x1B ->
    (* 기능·상태 정보. BX=0 만 정의돼 있다(RBIL). 다른 BX 는 AL=0 으로
       돌아간다(DOSBox-staging int10.cpp). *)
    if Cpu86.reg16 cpu 3 = 0 then begin
      write_state_info t ~seg:(Cpu86.seg cpu 0) ~off:(Cpu86.reg16 cpu 7);
      Cpu86.set_reg8 cpu 0 0x1B
    end
    else Cpu86.set_reg8 cpu 0 0
  | 0x11 ->
    (* AL=30h 폰트 정보: BH 가 고른 표를 ES:BP 에, 화면 글꼴의 높이를 CX
       에, 행-1 을 DL 에 준다(RBIL INT 10/AX=1130h). 비디오 초기화가 이
       포인터로 분기한다(실측: 빈 값이면 추락). *)
    if al = 0x30 then begin
      let rom off = Some (rom_seg, off) in
      let vector v = Some (rd16 t ((v * 4) + 2), rd16 t (v * 4)) in
      let table =
        match Cpu86.reg8 cpu 7 with
        | 0x00 -> vector cga_high_font_vector
        | 0x01 -> vector font_vector
        | 0x02 -> rom (List.assoc 14 font_tables)
        | 0x03 -> rom rom_font_off
        | 0x04 -> rom cga_high_font_off
        | 0x05 | 0x07 -> rom alt_font_off
        | 0x06 -> rom (List.assoc 16 font_tables)
        | _ -> None
      in
      Option.iter
        (fun (sg, off) ->
          Cpu86.set_seg cpu 0 sg;
          Cpu86.set_reg16 cpu 5 off;
          Cpu86.set_reg16 cpu 1 (rd16 t 0x485);
          Cpu86.set_reg8 cpu 2 (rd8 t 0x484))
        table
    end
  | 0x12 ->
    (* 대체 선택. BL=10h 는 EGA/VGA 정보: BH = 컬러 0·흑백 1, BL=3(256KB),
       CX 는 0x488 의 두 니블(DOSBox-staging int10.cpp. SeaBIOS 는 CH=0).
       BL=30h·31h·33h·34h·36h 는 받으면 AL=12h, AL 이 범위 밖이면 AL=0 이다
       (RBIL INT 10/AH=12h BL=30h~36h). 그 밖의 BL 은 AL 을 두고 돌아간다. *)
    let al_in n f = if al > n then Cpu86.set_reg8 cpu 0 0 else (f (); Cpu86.set_reg8 cpu 0 alt_select_done) in
    (match Cpu86.reg8 cpu 3 with
     | 0x10 ->
       Cpu86.set_reg8 cpu 7 (if rd16 t bda_crtc_port = crtc_mono then 1 else 0);
       Cpu86.set_reg8 cpu 3 video_mem_256k;
       Cpu86.set_reg8 cpu 1 (rd8 t bda_switches land switch_bits);
       Cpu86.set_reg8 cpu 5 (rd8 t bda_switches lsr feature_shift)
     | 0x30 ->
       (* 다음 모드 세우기의 텍스트 주사선 수. BDA 만 바꾼다 — 텍스트 그림은
          늘 8x8 글자를 세로로 두 배 늘린 400 줄이다. *)
       al_in 2 (fun () ->
           let flags = rd8 t bda_vga_flags land lnot (vga_flag_200_lines lor vga_flag_400_lines) in
           let lines, switches =
             match al with
             | 0 -> (vga_flag_200_lines, switches_cga_emulation)
             | 1 -> (0, switches_ega_enhanced)
             | _ -> (vga_flag_400_lines, switches_ega_enhanced)
           in
           wr8 t bda_vga_flags (flags lor lines);
           wr8 t bda_switches ((rd8 t bda_switches land switches_kept) lor switches))
     | 0x31 ->
       al_in 1 (fun () -> set_bda_bit t bda_vga_flags vga_flag_keep_palette (al = 1))
     | 0x33 ->
       (* AL=0 이 합산을 켠다. 31h·34h 의 비트는 "끔" 이라 AL=1 이 세우고,
          이 비트는 "켬" 이라 AL=0 이 세운다. *)
       al_in 1 (fun () -> set_bda_bit t bda_vga_flags vga_flag_gray_summing (al = 0))
     | 0x34 ->
       (* 커서 모양 바꾸기(AH=01h)는 이 비트와 상관없이 CRTC 에 그대로 쓴다. *)
       al_in 1 (fun () -> set_bda_bit t bda_ega_misc ega_misc_no_cursor_emulation (al = 1))
     | 0x36 -> al_in 1 (fun () -> Dos_video.set_screen_blanked t.video (al = 1))
     | _ -> ())
  | 0x13 ->
    (* 문자열 쓰기: ES:BP=문자열, CX=길이, DH/DL=시작 행/열,
       AL bit0=커서 갱신, bit1=속성이 문자열 안에 섞여 있음 *)
    let src = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 5 in
    let n = Cpu86.reg16 cpu 1 in
    let attr = Cpu86.reg8 cpu 3 in
    let col0 = Cpu86.reg8 cpu 2 and row0 = Cpu86.reg8 cpu 6 in
    let page = Cpu86.reg8 cpu 7 in
    let saved = get_cursor_on t ~page in
    set_cursor_on t ~page col0 row0;
    for i = 0 to n - 1 do
      if al land 0x02 <> 0 then
        put_char_on t ~page (rd8 t (src + (i * 2))) (rd8 t (src + (i * 2) + 1))
      else put_char_on t ~page (rd8 t (src + i)) attr
    done;
    if al land 0x01 = 0 then set_cursor_on t ~page (fst saved) (snd saved)
  | 0x1A ->
    (* 디스플레이 조합 코드: BL = 지금 화면, BH = 다른 화면(00h = 없음) *)
    Cpu86.set_reg8 cpu 0 0x1A;
    Cpu86.set_reg8 cpu 3 dcc_vga_color;
    Cpu86.set_reg8 cpu 7 0x00
  | 0x0B ->
    (* CGA 색 선택. BH=0 은 BL 의 하위 5 비트(바탕색 4 비트 + 밝기)를,
       BH=1 은 BL 비트 0 으로 팔레트를 바꾼다. 나머지 비트는 그대로다.
       SeaBIOS vgasrc/stdvga.c(28aa46349b97)의
       stdvga_set_cga_background_color·stdvga_set_cga_palette 가 바꾸는
       자리와 같다. 그 BIOS 는 VGA 속성 레지스터로 흉내 내고, 이 기계는
       CGA 처럼 포트 0x3D9 로 그린다. 다른 BH 는 SeaBIOS 처럼 무시한다. *)
    let old = Dos_video.cga_color_select t.video and bl = Cpu86.reg8 cpu 3 in
    let set v = Dos_ports.port_out t.ports cga_color_select_port v in
    (match Cpu86.reg8 cpu 7 with
     | 0x00 -> set ((old land lnot cga_background_bits) lor (bl land cga_background_bits))
     | 0x01 ->
       set ((old land lnot cga_palette_bit) lor (if bl land 1 = 0 then 0 else cga_palette_bit))
     | _ -> ())
  | _ -> report_unserved t No_function ~vector:0x10

(* ---------- INT 16h 키보드 ---------- *)

let read_key t =
  if key_pending t then begin
    t.kbd_wait <- false;
    Cpu86.set_reg16 t.cpu 0 (pop_key t);
    Completed
  end
  else begin
    starve t;
    Awaiting_input Bios_key
  end

let keyboard t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 4 with
  | 0x01 | 0x11 ->
    if key_pending t then begin
      t.kbd_wait <- false;
      Cpu86.set_flags cpu (Cpu86.flags cpu land lnot Cpu86.f_zero);
      Cpu86.set_reg16 cpu 0 (peek_key t)
    end
    else begin
      (* 빈 링 폴링도 굶주림이다 — KeyPressed 루프는 AH=00 을 부르지도
         않는다(실측: ZZT 메뉴 대기). *)
      starve t;
      Cpu86.set_flags cpu (Cpu86.flags cpu lor Cpu86.f_zero);
    end
  | 0x02 | 0x12 ->
    Cpu86.set_reg8 cpu 0 (rd8 t 0x417);
    if Cpu86.reg8 cpu 4 = 0x12 then Cpu86.set_reg8 cpu 4 (rd8 t 0x418)
  | 0x05 ->
    (* 키 밀어넣기: CX=(스캔<<8)|ASCII. 꽉 찼으면 AL=1 *)
    let before = ring_tail t in
    ring_push t (Cpu86.reg16 cpu 1);
    Cpu86.set_reg8 cpu 0 (if ring_tail t = before then 1 else 0)
  | 0x03 -> ()                      (* 타이프매틱 속도 — 모델 밖 *)
  | _ -> report_unserved t No_function ~vector:0x16

(* ---------- INT 9 키보드 IRQ ---------- *)

let keyboard_data_port = 0x60
let bda_shift_flags = 0x417
let bda_shift_flags_left = 0x418

(* 수식키가 0040:0017 과 0040:0018 에서 세우는 비트. SeaBIOS src/std/bda.h
   의 KF0_* 와 같다: 0x417 은 bit1 왼쪽 shift, bit2 ctrl, bit3 alt 이고,
   0x418 은 bit0 왼쪽 ctrl, bit1 왼쪽 alt 다. *)
let modifier_bits = function
  | Dos_keymap.Shift -> (0x02, 0x00)
  | Dos_keymap.Ctrl -> (0x04, 0x01)
  | Dos_keymap.Alt -> (0x08, 0x02)

(* 실기 BIOS 의 INT 9 처럼 포트 0x60 을 읽어 시프트 상태나 링을 고치고
   EOI 를 보낸다. 스캔 코드를 글자로 옮기는 표 대신 make 바이트가 지닌
   워드를 넣는다 — 호스트가 누른 워드가 INT 16h 의 답이다. INT 9 를
   가로챈 게스트가 먼저 0x60 을 읽고 여기로 넘겨도 같은 바이트를 읽는다
   (Dos_kbc.read_data). *)
let keyboard_irq t =
  let code = Dos_ports.port_in t.ports keyboard_data_port in
  (match Dos_keymap.modifier_of_code code with
   | Some (m, edge) ->
     let flags, left = modifier_bits m in
     let apply a bits =
       let v = rd8 t a in
       wr8 t a (match edge with Dos_keymap.Down -> v lor bits | Dos_keymap.Up -> v land lnot bits)
     in
     apply bda_shift_flags flags;
     apply bda_shift_flags_left left
   | None -> Option.iter (ring_push t) (Dos_kbc.data_word (Dos_ports.kbc t.ports)));
  Dos_pic.end_of_interrupt (Dos_ports.pic t.ports)

(* ---------- INT 1Ah 시각 ---------- *)

let clock t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 4 with
  | 0x00 ->
    let ticks = read_ticks t in
    Cpu86.set_reg16 cpu 1 (ticks lsr 16);         (* CX = 상위 워드 *)
    Cpu86.set_reg16 cpu 2 (ticks land 0xFFFF);    (* DX = 하위 워드 *)
    Cpu86.set_reg8 cpu 0 (rd8 t bda_midnight);    (* AL = 자정 넘김 *)
    wr8 t bda_midnight 0
  | 0x01 ->
    write_ticks t ((Cpu86.reg16 cpu 1 lsl 16) lor Cpu86.reg16 cpu 2);
    wr8 t bda_midnight 0
  | 0x02 ->
    let _, _, _, h, mi, s = now_fields t in
    Cpu86.set_reg8 cpu 5 (to_bcd h);
    Cpu86.set_reg8 cpu 1 (to_bcd mi);
    Cpu86.set_reg8 cpu 6 (to_bcd s);
    Cpu86.set_reg8 cpu 2 0;
    set_cf t false
  | 0x04 ->
    let y, m, d, _, _, _ = now_fields t in
    Cpu86.set_reg8 cpu 5 (to_bcd (y / 100));
    Cpu86.set_reg8 cpu 1 (to_bcd (y mod 100));
    Cpu86.set_reg8 cpu 6 (to_bcd m);
    Cpu86.set_reg8 cpu 2 (to_bcd d);
    set_cf t false
  | _ -> report_unserved t No_function ~vector:0x1A

(* ---------- INT 33h 마우스 ---------- *)

(* 마우스는 하네스가 움직인다. 기본은 장착 — KOEI 계열 게임은 부팅에서
   AX=0 으로 드라이버를 검사하고, 없으면 키보드 모드로 강제한다.
   AX=0x0C 로 건 핸들러는 Dos_machine.set_mouse 가 이벤트와 함께 부른다. *)
let mouse_driver_installed = 0xFFFF
let mouse_buttons_reported = 2

(* AX=2Ah 가 DX 로 알리는 마우스 종류. 04h 는 IBM(PS/2) 마우스다(RBIL
   INT 33/002A 표 03179). 이 기계의 마우스는 하네스가 움직여 실제 포트가
   없다. DOSBox-X 와 같은 값을 준다. *)
let mouse_type_ps2 = 0x04

(* AX=24h 의 판 번호(BH.BL). CuteMouse 가 흉내 내는 7.05 를 쓴다 — 26h 가
   7.05 식이다(ctmouse.asm driverversion). DOSBox-X 는 8.05 를 준다. *)
let mouse_driver_version = 0x0705
let mouse_irq_ps2 = 0  (* RBIL INT 33/0024: CL = 0 은 PS/2 *)
let mouse_enabled = 0  (* AX=26h BX: 0 이면 드라이버가 켜져 있다 *)

(* 13h·1Ah 의 DX 0 은 기본 문턱이다(RBIL INT 33/0013 "0000h = default of 64") *)
let double_speed_of dx = if dx = 0 then default_motion.double_speed else dx

(* 00h 와 21h 가 함께 되돌리는 드라이버 상태. HelpPC 2.10 INT 33,0: 커서를
   숨기고 모양을 기본으로 하고, 사용자 핸들러 마스크를 0 으로 하고, 범위를
   모드 최대로 한다. 마스크가 0 이면 핸들러를 부르지 않는다 — 앞 프로그램이
   건 핸들러가 그 메모리를 놓은 뒤에 불리지 않는다. 범위와 커서 자리는
   [mouse_fit_screen] 이 맞춘다. 이동량도 0 으로 한다 — CuteMouse 21h 가
   지우는 칸(ClearArea3)에 들고, DOSBox-X Mouse_Reset 도 지운다. *)
let reset_mouse_driver t =
  let m = t.mouse in
  Dos_mouse_cursor.restore t;
  m.mouse_cursor_flag <- cursor_flag_after_reset;
  m.mouse_graphics_cursor <- Default_arrow;
  m.mouse_text_cursor <- default_text_cursor;
  m.mouse_handler_mask <- 0;
  m.mouse_dx <- 0;
  m.mouse_dy <- 0;
  m.mouse_motion <- default_motion;
  mouse_fit_screen t;
  List.iter
    (Array.iter (fun (l : button_log) -> l.times <- 0; l.last_x <- 0; l.last_y <- 0))
    [ m.mouse_presses; m.mouse_releases ]

(* 0Bh·27h: 지난 읽기 뒤 미키를 CX·DX 로 주고 0 으로 한다 *)
let take_mickeys cpu m =
  Cpu86.set_reg16 cpu 1 (m.mouse_dx land 0xffff);
  Cpu86.set_reg16 cpu 2 (m.mouse_dy land 0xffff);
  m.mouse_dx <- 0;
  m.mouse_dy <- 0

(* 0Ch·14h: CX 마스크, ES:DX 루틴을 사용자 핸들러로 건다 *)
let register_handler cpu m =
  m.mouse_handler_mask <- Cpu86.reg16 cpu 1;
  m.mouse_handler_seg <- Cpu86.seg cpu 0;
  m.mouse_handler_off <- Cpu86.reg16 cpu 2

let mouse t =
  let cpu = t.cpu in
  let m = t.mouse in
  match Cpu86.reg16 cpu 0 with
  | 0x00 ->
    Cpu86.set_reg16 cpu 0 (if m.mouse_present then mouse_driver_installed else 0x0000);
    Cpu86.set_reg16 cpu 3 (if m.mouse_present then mouse_buttons_reported else 0);
    reset_mouse_driver t
  | 0x21 ->
    (* SOFTWARE RESET (RBIL INT 33/0021): 00h 와 같되 하드웨어는 건드리지
       않는다. 드라이버가 없으면 AX 는 0021h 그대로다. *)
    if m.mouse_present then begin
      Cpu86.set_reg16 cpu 0 mouse_driver_installed;
      Cpu86.set_reg16 cpu 3 mouse_buttons_reported;
      reset_mouse_driver t
    end
  | 0x01 ->
    (* 깃발을 하나 올리되 0 에서 멈춘다. 02h 를 n 번 불렀으면 01h 도 n 번
       불러야 보인다(RBIL INT 33/0002, DOSBox-X mouse.cpp). *)
    if m.mouse_cursor_flag < 0 then m.mouse_cursor_flag <- m.mouse_cursor_flag + 1;
    Dos_mouse_cursor.draw t
  | 0x02 ->
    Dos_mouse_cursor.restore t;
    m.mouse_cursor_flag <- m.mouse_cursor_flag - 1
  | 0x03 ->
    let space = Dos_mouse_space.current t.mem in
    Cpu86.set_reg16 cpu 1 (Dos_mouse_space.cell_x space m.mouse_x);
    Cpu86.set_reg16 cpu 2 (Dos_mouse_space.cell_y space m.mouse_y);
    Cpu86.set_reg16 cpu 3 m.mouse_buttons
  | 0x04 ->
    m.mouse_x <- clamp_to m.mouse_range_x (Cpu86.reg16 cpu 1);
    m.mouse_y <- clamp_to m.mouse_range_y (Cpu86.reg16 cpu 2);
    clear_mickey_carry m;
    Dos_mouse_cursor.draw t
  | 0x0F ->
    (* DEFINE MICKEY/PIXEL RATIO: CX·DX 는 8 화소당 미키. 하나라도 0 이면
       무시한다(CuteMouse sensitivity_0F, DOSBox-X Mouse_SetMickeyPixelRate). *)
    let cx = Cpu86.reg16 cpu 1 and dx = Cpu86.reg16 cpu 2 in
    if cx <> 0 && dx <> 0 then
      m.mouse_motion <- { m.mouse_motion with mickeys_x = cx; mickeys_y = dx }
  | 0x13 ->
    (* DEFINE DOUBLE-SPEED THRESHOLD: DX, 0 이면 64(RBIL INT 33/0013) *)
    m.mouse_motion <- { m.mouse_motion with double_speed = double_speed_of (Cpu86.reg16 cpu 2) }
  | 0x1A ->
    (* SET MOUSE SENSITIVITY: BX·CX 감도, DX 문턱(RBIL INT 33/001A). 감도가
       1..100 밖이면 그 축은 그대로다(CuteMouse senscalc). 문턱은 13h 와
       같은 값이다 — CuteMouse 는 두 배 속도를 두지 않아 RBIL 을 따른다. *)
    let speed now v = if v >= lowest_speed && v <= highest_speed then v else now in
    let mo = m.mouse_motion in
    m.mouse_motion <-
      { mo with
        speed_x = speed mo.speed_x (Cpu86.reg16 cpu 3);
        speed_y = speed mo.speed_y (Cpu86.reg16 cpu 1);
        double_speed = double_speed_of (Cpu86.reg16 cpu 2) }
  | 0x1B ->
    (* RETURN MOUSE SENSITIVITY: BX·CX 감도, DX 문턱 *)
    let mo = m.mouse_motion in
    Cpu86.set_reg16 cpu 3 mo.speed_x;
    Cpu86.set_reg16 cpu 1 mo.speed_y;
    Cpu86.set_reg16 cpu 2 mo.double_speed
  | (0x05 | 0x06) as fn ->
    (* 버튼 BX 의 누름(05h)·뗌(06h) 기록. AX 는 지금 버튼 상태, BX 는 지난
       호출 뒤 횟수, CX·DX 는 마지막 자리. 횟수는 읽으면 0 이 된다
       (Ralf Brown INT 33/0005, 0006). 없는 버튼 번호면 AX 만 쓴다. *)
    Cpu86.set_reg16 cpu 0 m.mouse_buttons;
    let logs = if fn = 0x05 then m.mouse_presses else m.mouse_releases in
    let b = Cpu86.reg16 cpu 3 in
    if b < mouse_button_count then begin
      let l = logs.(b) in
      Cpu86.set_reg16 cpu 3 l.times;
      Cpu86.set_reg16 cpu 1 l.last_x;
      Cpu86.set_reg16 cpu 2 l.last_y;
      l.times <- 0
    end
  | (0x07 | 0x08) as fn ->
    (* 가로(07h)·세로(08h) 범위 CX..DX(Ralf Brown INT 33/0007, 0008).
       거꾸로 주면 바꿔 쓴다(HelpPC 2.10 INT 33,7). 범위는 움직임을 가두는
       창이라 지금 자리도 그 안으로 옮긴다. 텍스트 모드에서 셀 크기 배수로
       내리는 일(RBIL 의 메모)은 하지 않는다. *)
    let a = Cpu86.reg16 cpu 1 and b = Cpu86.reg16 cpu 2 in
    let range = (min a b, max a b) in
    if fn = 0x07 then begin
      m.mouse_range_x <- range;
      m.mouse_x <- clamp_to range m.mouse_x
    end
    else begin
      m.mouse_range_y <- range;
      m.mouse_y <- clamp_to range m.mouse_y
    end
  | 0x0B -> take_mickeys cpu m
  | 0x27 ->
    (* GET SCREEN/CURSOR MASKS AND MICKEY COUNTS(RBIL INT 33/0027): AX·BX 는
       0Ah 텍스트 커서 — 소프트웨어면 두 마스크, 하드웨어면 시작·끝 스캔 줄
       (v7.02+, CuteMouse cursor_27 의 startscan·endscan). CX·DX 는 0Bh 처럼
       미키를 가져간다. *)
    let first, second =
      match m.mouse_text_cursor with
      | Software_text { screen_mask; cursor_mask } -> (screen_mask, cursor_mask)
      | Hardware_text { first_line; last_line } -> (first_line, last_line)
    in
    Cpu86.set_reg16 cpu 0 first;
    Cpu86.set_reg16 cpu 3 second;
    take_mickeys cpu m
  | 0x31 ->
    (* GET CURRENT MINIMUM/MAXIMUM VIRTUAL COORDINATES(RBIL INT 33/0031):
       07h·08h 범위. AX·BX 가 가로·세로 아래 끝, CX·DX 가 위 끝이다. *)
    let x_lo, x_hi = m.mouse_range_x and y_lo, y_hi = m.mouse_range_y in
    Cpu86.set_reg16 cpu 0 x_lo;
    Cpu86.set_reg16 cpu 3 y_lo;
    Cpu86.set_reg16 cpu 1 x_hi;
    Cpu86.set_reg16 cpu 2 y_hi
  | 0x09 ->
    (* 그래픽 커서: BX·CX 핫스폿, ES:DX 마스크 서른두 워드. 오프셋은
       세그먼트 안에서 돈다. 돌려주는 값은 없다. *)
    let seg = Cpu86.seg cpu 0 lsl 4 and off = Cpu86.reg16 cpu 2 in
    let row k = rd16 t (seg + ((off + (2 * k)) land 0xffff)) in
    m.mouse_graphics_cursor <-
      Custom_cursor
        { hot_x = Cpu86.reg16 cpu 3; hot_y = Cpu86.reg16 cpu 1;
          screen_mask = Array.init cursor_rows row;
          cursor_mask = Array.init cursor_rows (fun k -> row (cursor_rows + k)) };
    Dos_mouse_cursor.draw t
  | 0x0A ->
    (* 텍스트 커서: BX=0 이면 CX·DX 가 마스크, 아니면 스캔 줄이다. RBIL 은
       BX 0·1 만 적는다. 그 밖의 값은 DOSBox-X 처럼 하드웨어로 본다. 하드웨어
       커서는 그리지 않는다(Dos_mouse_cursor). *)
    let cx = Cpu86.reg16 cpu 1 and dx = Cpu86.reg16 cpu 2 in
    m.mouse_text_cursor <-
      (match Cpu86.reg16 cpu 3 with
       | 0 -> Software_text { screen_mask = cx; cursor_mask = dx }
       | _ -> Hardware_text { first_line = cx; last_line = dx });
    Dos_mouse_cursor.draw t
  | 0x2A ->
    (* GET CURSOR HOT SPOT(RBIL INT 33/002A): AX 깃발, BX·CX 핫스폿, DX 종류.
       RBIL 은 깃발을 AX 로 적는다. DOSBox-X 는 AL 만 쓴다. *)
    let hot_x, hot_y =
      match m.mouse_graphics_cursor with
      | Default_arrow -> default_hot_spot
      | Custom_cursor { hot_x; hot_y; _ } -> (hot_x, hot_y)
    in
    Cpu86.set_reg16 cpu 0 (m.mouse_cursor_flag land 0xffff);
    Cpu86.set_reg16 cpu 3 hot_x;
    Cpu86.set_reg16 cpu 1 hot_y;
    Cpu86.set_reg16 cpu 2 mouse_type_ps2
  | 0x24 ->
    (* GET SOFTWARE VERSION, MOUSE TYPE, AND IRQ NUMBER(RBIL INT 33/0024):
       BH.BL 판, CH 종류, CL IRQ(PS/2 는 0). AX 는 그대로다. *)
    Cpu86.set_reg16 cpu 3 mouse_driver_version;
    Cpu86.set_reg8 cpu 5 mouse_type_ps2;
    Cpu86.set_reg8 cpu 1 mouse_irq_ps2
  | 0x26 ->
    (* GET MAXIMUM VIRTUAL COORDINATES(RBIL INT 33/0026): BX 꺼짐 깃발, CX·DX
       최대 좌표. 7.05 부터는 07h·08h 범위가 아니라 모드의 가상 화면 끝이다.
       1Fh(드라이버 끄기)가 없어 BX 는 늘 0 이다. *)
    let space = Dos_mouse_space.current t.mem in
    Cpu86.set_reg16 cpu 3 mouse_enabled;
    Cpu86.set_reg16 cpu 1 (snd (Dos_mouse_space.range_x space));
    Cpu86.set_reg16 cpu 2 (snd (Dos_mouse_space.range_y space))
  | 0x0C ->
    (* DEFINE INTERRUPT SUBROUTINE PARAMETERS — CX 마스크, ES:DX 루틴.
       돌려주는 값은 없다(Ralf Brown rb-5968: Return 없음). AX 를 건드리면
       없는 규약을 지어내는 것이니 저장만 한다. *)
    register_handler cpu m
  | 0x14 ->
    (* EXCHANGE INTERRUPT SUBROUTINES(RBIL INT 33/0014): 0Ch 처럼 걸고, 앞
       등록의 마스크를 CX, 루틴을 ES:DX 로 돌려준다(CuteMouse exchangeUIR_14,
       DOSBox-X). 마스크는 0Ch 가 받은 워드 그대로다. *)
    let old_mask = m.mouse_handler_mask
    and old_seg = m.mouse_handler_seg
    and old_off = m.mouse_handler_off in
    register_handler cpu m;
    Cpu86.set_reg16 cpu 1 old_mask;
    Cpu86.set_seg cpu 0 old_seg;
    Cpu86.set_reg16 cpu 2 old_off
  | _ -> report_unserved t No_function ~vector:0x33

(* ---------- 묶음 ---------- *)

(* ---------- INT 15h 시스템 ---------- *)

(* SeaBIOS src/system.c(28aa46349b97)를 따른다. 보호 모드는 없다. 그래서
   87h(블록 이동)·89h(보호 모드)·24h(A20)·C0h(구성 표)·E8h(메모리 지도)는
   "지원 안 함" 으로 답한다. 원래 AT BIOS 도 C0h 가 없었다. 88h 는 1MB 위
   RAM 의 크기를 KB 로 준다. 1MB 보드는 0 이고 16MB 보드(386)는 15360 으로,
   XMS 가 나눠 주는 크기(Dos_xms.capacity_kb)와 같다. *)
let extended_memory_kb t = (Bytes.length t.mem / 1024) - 1024
let unsupported_function = 0x86

(* ---------- RTC 기다리기: INT 15h 83h·86h 와 INT 70h ----------

   SeaBIOS src/clock.c(28aa46349b97)의 set_usertimer·clear_usertimer·
   handle_1583·handle_1586·handle_70 을 따른다. 기다리기는 BDA 에 산다
   (SeaBIOS src/std/bda.h): 40:98h 끝 깃발의 주소(오프셋, 세그먼트),
   40:9Ch 남은 µs(dword), 40:A0h 대기 깃발(bit 0 대기 중). RTC 의 주기
   인터럽트(IRQ8)마다 INT 70h 가 남은 시간을 977µs 씩 줄이고, 977µs 보다
   적게 남았으면 끝 깃발 바이트의 bit 7 을 세우고 대기를 지운다.
   기다리는 동안만 레지스터 B 의 PIE 를 켠다(SeaBIOS rtc_use·rtc_release.
   그 사용자 수는 이 기다리기 하나뿐이라 세지 않는다). 알람(AIE, INT 4Ah)은
   CMOS 가 AF 를 세우지 않아 오지 않는다.

   AH=86h 는 SeaBIOS 가 자기 스택의 바이트를 끝 깃발로 쓴다. 게스트가 볼 수
   있는 자리가 필요해서 DOSBox-staging src/ints/bios.h(a1b4dbbd8bbe)의
   BIOS_WAIT_FLAG_TEMP, 40:A1h 를 쓴다. DOSBox 는 한 틱에 997µs 를 빼지만
   여기는 SeaBIOS 의 977µs(1000000/1024 반올림)다. *)
let bda_wait_flag_ptr = 0x498
let bda_wait_timeout = 0x49C
let bda_rtc_wait_flag = 0x4A0
let bda_wait_temp = 0x4A1
let rws_wait_pending = 0x01
let wait_elapsed = 0x80
let usec_per_rtc = 977
let rtc_wait_function = 0x86
let clock_in_use = 0x83           (* SeaBIOS RET_ECLOCKINUSE *)
let set_interval = 0x00
let cancel_interval = 0x01

let cmos_index = 0x70
let cmos_data = 0x71
let cmos_status_b = 0x0B
let cmos_status_c = 0x0C
let rtc_b_pie = 0x40
let rtc_b_aie = 0x20
let rtc_c_pf = 0x40
let eoi_command = 0x20

let rd32 t a = rd16 t a lor (rd16 t (a + 2) lsl 16)
let wr32 t a v = wr16 t a (v land 0xFFFF); wr16 t (a + 2) ((v lsr 16) land 0xFFFF)

(* 포트로 읽고 쓴다 — 게스트의 IN·OUT 과 같은 길이라 IRQ8 줄도 따라간다. *)
let cmos_read t reg =
  Dos_ports.port_out t.ports cmos_index reg;
  Dos_ports.port_in t.ports cmos_data

let cmos_mask t reg ~off ~on =
  let v = cmos_read t reg in
  Dos_ports.port_out t.ports cmos_data ((v land lnot off) lor on)

let wait_pending t = rd8 t bda_rtc_wait_flag land rws_wait_pending <> 0

let set_usertimer t ~usecs ~seg ~off =
  if wait_pending t then false
  else begin
    wr8 t bda_rtc_wait_flag rws_wait_pending;
    wr16 t bda_wait_flag_ptr off;
    wr16 t (bda_wait_flag_ptr + 2) seg;
    wr32 t bda_wait_timeout usecs;
    cmos_mask t cmos_status_b ~off:0 ~on:rtc_b_pie;
    true
  end

let clear_usertimer t =
  if wait_pending t then begin
    wr8 t bda_rtc_wait_flag 0;
    cmos_mask t cmos_status_b ~off:rtc_b_pie ~on:0
  end

let interval t = (Cpu86.reg16 t.cpu 1 lsl 16) lor Cpu86.reg16 t.cpu 2

(* AH=83h: AL=00h 은 ES:BX 의 바이트를 끝 깃발로 걸고 바로 돌아온다,
   AL=01h 은 건 것을 지운다. 그 밖의 AL 은 SeaBIOS handle_1583XX 처럼
   AH=86h·CF 에 AL 을 하나 줄인다. *)
let event_wait t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 0 with
  | al when al = set_interval ->
    if set_usertimer t ~usecs:(interval t) ~seg:(Cpu86.seg cpu 0) ~off:(Cpu86.reg16 cpu 3)
    then set_cf t false
    else begin Cpu86.set_reg8 cpu 4 unsupported_function; set_cf t true end
  | al when al = cancel_interval -> clear_usertimer t; set_cf t false
  | al ->
    Cpu86.set_reg8 cpu 4 unsupported_function;
    set_cf t true;
    Cpu86.set_reg8 cpu 0 ((al - 1) land 0xFF)

let resume_wait t =
  if rd8 t bda_wait_temp land wait_elapsed <> 0 then begin
    set_cf t false;
    Completed
  end
  else Awaiting_input Bios_wait

(* AH=86h: CX:DX µs 를 기다린다. 기다리는 동안 IRQ 가 들어온다. *)
let rtc_wait t =
  wr8 t bda_wait_temp 0;
  if set_usertimer t ~usecs:(interval t) ~seg:0 ~off:bda_wait_temp then resume_wait t
  else begin
    Cpu86.set_reg8 t.cpu 4 clock_in_use;
    set_cf t true;
    Completed
  end

(* INT 70h. 레지스터 C 를 읽으면 깃발이 지워지고 IRQ8 이 내려간다. *)
let rtc_irq t =
  let b = cmos_read t cmos_status_b in
  let c = cmos_read t cmos_status_c in
  if b land (rtc_b_pie lor rtc_b_aie) <> 0 && c land rtc_c_pf <> 0 && wait_pending t then begin
    let left = rd32 t bda_wait_timeout in
    if left < usec_per_rtc then begin
      let a = (rd16 t (bda_wait_flag_ptr + 2) * 16) + rd16 t bda_wait_flag_ptr in
      wr8 t a (rd8 t a lor wait_elapsed);
      clear_usertimer t
    end
    else wr32 t bda_wait_timeout (left - usec_per_rtc)
  end;
  let pic = Dos_ports.pic t.ports in
  Dos_pic.write_command pic Dos_pic.Slave eoi_command;
  Dos_pic.write_command pic Dos_pic.Master eoi_command

let system t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 4 with
  | 0x88 -> Cpu86.set_reg16 cpu 0 (extended_memory_kb t); set_cf t false
  | 0x4F -> set_cf t true (* 키보드 가로채기: 그 스캔 코드를 그대로 쓰라 *)
  | 0x90 | 0x91 -> () (* 장치 바쁨·끝남 알림: SeaBIOS 도 아무것도 안 한다 *)
  | 0x84 when Cpu86.reg16 cpu 2 <= 1 ->
    (* 조이스틱 지원(RBIL INT 15/AH=84h). DX=0 은 게임 포트의 윗 네 비트(버튼),
       DX=1 은 네 축의 위치(AX BX CX DX)를 CF=0 으로 준다. 이 기계는 포트 201h 가
       조이스틱 없는 값(축 비트 0, 버튼 비트 1)이라 위치는 모두 0 이고 버튼은
       모두 뗀 상태다. RBIL 은 게임 포트가 없으면 DX=0 이 AL=00h 라고 적는다;
       이 모델은 BIOS 가 포트를 읽는 대로 포트 값을 준다. *)
    if Cpu86.reg16 cpu 2 = 0 then Cpu86.set_reg8 cpu 0 Dos_ports.game_port_released
    else List.iter (fun r -> Cpu86.set_reg16 cpu r 0) [ 0; 1; 2; 3 ];
    set_cf t false
  | 0x83 -> event_wait t
  | _ ->
    (* 집계는 AX 가 바뀌기 전에 올린다 *)
    report_unserved t No_function ~vector:0x15;
    Cpu86.set_reg8 cpu 4 unsupported_function;
    set_cf t true

let service t s =
  match s with
  | Keyboard_irq -> keyboard_irq t; Completed
  | Rtc_irq -> rtc_irq t; Completed
  | System when Cpu86.reg8 t.cpu 4 = rtc_wait_function -> rtc_wait t
  | Keyboard when Cpu86.reg8 t.cpu 4 = 0x00 || Cpu86.reg8 t.cpu 4 = 0x10 -> read_key t
  | Keyboard -> keyboard t; Completed
  | Video -> video t; Completed
  | Equipment -> Cpu86.set_reg16 t.cpu 0 (rd16 t 0x410); Completed
  | Memory_size -> Cpu86.set_reg16 t.cpu 0 (rd16 t 0x413); Completed
  | Time_of_day -> clock t; Completed
  | System -> system t; Completed
  | Mouse -> mouse t; Completed
