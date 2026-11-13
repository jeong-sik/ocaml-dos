(* BIOS 표면 — ROM 코드, 인터럽트 벡터표, BIOS 데이터 영역, 그리고
   INT 10h(비디오)·11h(장비)·12h(메모리)·16h(키보드)·1Ah(시각)·
   33h(마우스) 서비스.

   부팅 관문의 대부분은 "실기 하드웨어 계약이 비어 있었다" 였다. 벡터가
   0:0 이면 체인 방식 런타임이 0000:0000 으로 떨어지고, BDA 가 0 이면
   CRT 루틴이 없는 포트를 폴링한다. 여기 있는 값은 전부 실기 문서와
   실측에서 정한 것이다. *)

open Dos_state

(* 27h(옛 방식 상주 종료) 포함 — 호스트 구현으로 가야 하는 벡터에만
   전용 스텁을 준다. 전용 스텁이 없으면 전벡터 IRET 스텁이 배달을
   삼켜 서비스가 조용히 무시된다(실측: TSR 의 INT 27h). *)
let host_served = [ 0x10; 0x16; 0x20; 0x21; 0x27; 0x33; 0x67 ]

(* ROM 안의 자리 배치. EMM 디바이스 헤더는 0(이름 칸이 세그먼트+0x0A
   라는 관례 때문에 0 번지여야 한다), INT 8 루틴은 0x40, IRET 스텁은
   0x20, 호스트 서빙 벡터의 되돌림 스텁은 0x100 부터 16바이트씩. *)
let rom_seg = 0xF000
let emm_entry_off = 0x0000
let iret_stub_off = 0x0020
let bios_int8_off = 0x0040
let stub_area = 0x0100
let stub_stride = 16
let rom_font_off = 0xFA6E
let rom_font_first = 0x20
let rom_font_last = 0x7E

(* 호스트 구현으로 되돌리는 사설 벡터 번호 — 실벡터 + 0x80. *)
let private_vector v = v + 0x80

let set_ivt t v off sg =
  wr16 t (v * 4) off;
  wr16 t ((v * 4) + 2) sg

(* IRQ0 에서 도는 ROM 루틴: 0x46C 틱을 올리고 INT 1Ch 사용자 훅을 부른
   뒤 PIC 에 EOI 를 보내고 IRET. 실기 BIOS 와 같은 사슬이다. *)
let bios_int8 =
  "\x1e\x50"                        (* push ds; push ax *)
  ^ "\x31\xc0\x8e\xd8"              (* xor ax,ax; mov ds,ax *)
  ^ "\xa1\x6c\x04\x40\xa3\x6c\x04"  (* mov ax,[046C]; inc ax; mov [046C],ax *)
  ^ "\x75\x04"                      (* jnz +4 — 자리올림 없으면 고워드 skip *)
  ^ "\xff\x06\x6e\x04"              (* inc word [046E] *)
  ^ "\xcd\x1c"                      (* int 1Ch — 사용자 틱 훅 *)
  ^ "\xb0\x20\xe6\x20"              (* mov al,20h; out 20h,al (EOI) *)
  ^ "\x58\x1f\xcf"                  (* pop ax; pop ds; iret *)

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
  set_ivt t 0x08 bios_int8_off rom_seg;          (* 타이머 IRQ0 *)
  set_ivt t 0x67 emm_entry_off rom_seg;          (* EMS — 헤더 0 번지 *)
  (* BIOS 데이터 영역 — 실기 부팅 값. CRT 루틴이 화면 폴링 포트를
     [0x40:0x63](CRTC 베이스) + 6 으로 계산한다: 0 이면 포트 6 을 읽어
     영원히 갇힌다(실측). 컬러 80x25 텍스트 기준. *)
  wr16 t 0x410 equipment_word;
  wr16 t 0x413 640;                 (* 기억용량 KB *)
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
  wr8 t 0x484 (rows - 1);
  wr16 t 0x485 8;                   (* 글자 높이 *)
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
  let off = ref stub_area in
  List.iter
    (fun v ->
      let a = (rom_seg * 16) + !off in
      wr8 t a 0xCD;
      wr8 t (a + 1) (private_vector v);
      wr8 t (a + 2) 0xCF;
      set_ivt t v !off rom_seg;
      t.stubs <- (v, !off) :: t.stubs;
      off := !off + stub_stride)
    host_served;
  (* 0x67 은 위의 일괄 배정을 덮어써 헤더 0 번지로 — EMS 런타임이
     세그먼트+0x0A 를 읽기 때문이다. 되돌림 스텁 자리는 비워 둔다. *)
  set_ivt t 0x67 emm_entry_off rom_seg;
  t.stubs <- (0x67, emm_entry_off) :: t.stubs;
  (* 나머지 벡터는 전부 IRET 스텁으로 채운다. 실기 BIOS 는 모든 벡터가
     ROM/DOS 를 가리켜서, 프로그램이 훅을 걸며 되읽은 "옛 벡터" 가 0:0
     이 아니다. 하나라도 비워 두면 체인 복귀가 0000:0000 으로 떨어진다. *)
  for v = 0 to 255 do
    if not (List.mem v (0x08 :: host_served)) then
      set_ivt t v iret_stub_off rom_seg
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
  done

(* IVT[v] 가 아직 우리 스텁이면(게스트가 AH=25h 로 안 바꿨으면) 호스트
   구현으로 바로 간다 — deliver 로 다시 스텁에 들어가면 무한루프다. *)
let ivt_is_own_stub t v =
  List.exists
    (fun (sv, soff) ->
      sv = v
      && rd16 t (v * 4) = soff
      && rd16 t ((v * 4) + 2) = rom_seg)
    t.stubs

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

let video t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 and al = Cpu86.reg8 cpu 0 in
  match ah with
  | 0x00 ->
    (* AL 의 최상위 비트는 "화면을 지우지 말라" 는 뜻이다. *)
    let mode = al land 0x7f in
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
    wr8 t 0x484 (rows - 1);
    set_cursor t 0 0
  | 0x01 ->
    (* 커서 모양: CH=시작 스캔라인, CL=끝 *)
    Dos_ports.port_out t.ports 0x3D4 0x0A;
    Dos_ports.port_out t.ports 0x3D5 (Cpu86.reg8 cpu 5);
    Dos_ports.port_out t.ports 0x3D4 0x0B;
    Dos_ports.port_out t.ports 0x3D5 (Cpu86.reg8 cpu 1)
  | 0x02 -> set_cursor t (Cpu86.reg8 cpu 2) (Cpu86.reg8 cpu 6)
  | 0x03 ->
    let col, row = get_cursor t in
    Cpu86.set_reg8 cpu 2 col;
    Cpu86.set_reg8 cpu 6 row;
    Cpu86.set_reg8 cpu 5 (rd8 t 0x461);
    Cpu86.set_reg8 cpu 1 (rd8 t 0x460)
  | 0x05 -> wr8 t 0x462 al
  | 0x06 | 0x07 ->
    scroll_window t ~up:(ah = 0x06) ~lines:al
      ~top:(Cpu86.reg8 cpu 5) ~left:(Cpu86.reg8 cpu 1)
      ~bottom:(Cpu86.reg8 cpu 6) ~right:(Cpu86.reg8 cpu 2)
      ~attr:(Cpu86.reg8 cpu 7)
  | 0x08 ->
    let col, row = get_cursor t in
    Cpu86.set_reg16 cpu 0 (read_cell t row col)
  | 0x09 | 0x0A ->
    (* 커서 자리에 같은 글자를 CX 번. 커서는 움직이지 않는다.
       AH=09 는 BL 을 속성으로 쓰고, AH=0A 는 화면의 속성을 남긴다. *)
    let col, row = get_cursor t in
    let n = max 1 (Cpu86.reg16 cpu 1) in
    let r = ref row and c = ref col in
    for _ = 1 to n do
      if !r < rows then begin
        let attr =
          if ah = 0x09 then Cpu86.reg8 cpu 3
          else (read_cell t !r !c) lsr 8
        in
        write_cell t !r !c ((attr lsl 8) lor al);
        incr c;
        if !c >= cols t then begin c := 0; incr r end
      end
    done
  | 0x0E -> put_char t al (Cpu86.reg8 cpu 3 land 0x0f)
  | 0x0C ->
    (* 점 찍기: AL=색, CX=x, DX=y. BH 페이지는 한 장뿐이라 무시한다. *)
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
     | 0x02 ->
       (* 열여섯 색 + 테두리를 한 번에: ES:DX 가 17 바이트 *)
       let src = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 2 in
       for i = 0 to 16 do
         Dos_video.set_attr_palette t.video i (rd8 t (src + i))
       done
     | 0x07 ->
       Cpu86.set_reg8 cpu 7
         (Dos_video.attr_palette t.video).(Cpu86.reg8 cpu 3 land 0x1f)
     | 0x09 ->
       let dst = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 2 in
       let attr = Dos_video.attr_palette t.video in
       for i = 0 to 16 do wr8 t (dst + i) attr.(i) done
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
    (* AL=30h 폰트 정보: ES:BP=ROM 8x8 폰트, CX=높이, DL=행-1.
       비디오 초기화가 이 포인터로 분기한다(실측: 빈 값이면 추락). *)
    if al = 0x30 then begin
      Cpu86.set_seg cpu 0 rom_seg;
      Cpu86.set_reg16 cpu 5 0xFA6E;
      Cpu86.set_reg16 cpu 1 8;
      Cpu86.set_reg8 cpu 2 (rows - 1)
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
    let saved = get_cursor t in
    set_cursor t col0 row0;
    for i = 0 to n - 1 do
      if al land 0x02 <> 0 then
        put_char t (rd8 t (src + (i * 2))) (rd8 t (src + (i * 2) + 1))
      else put_char t (rd8 t (src + i)) attr
    done;
    if al land 0x01 = 0 then set_cursor t (fst saved) (snd saved)
  | 0x1A ->
    (* 디스플레이 조합 코드: BL = 지금 화면, BH = 다른 화면(00h = 없음) *)
    Cpu86.set_reg8 cpu 0 0x1A;
    Cpu86.set_reg8 cpu 3 dcc_vga_color;
    Cpu86.set_reg8 cpu 7 0x00
  | _ -> ()

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
    push_key t (Cpu86.reg16 cpu 1);
    Cpu86.set_reg8 cpu 0 (if ring_tail t = before then 1 else 0)
  | 0x03 -> ()                      (* 타이프매틱 속도 — 모델 밖 *)
  | _ -> ()

(* ---------- INT 1Ah 시각 ---------- *)

let clock t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 4 with
  | 0x00 ->
    Cpu86.set_reg16 cpu 1 (rd16 t 0x46E);   (* CX = 상위 워드 *)
    Cpu86.set_reg16 cpu 2 (rd16 t 0x46C);   (* DX = 하위 워드 *)
    Cpu86.set_reg8 cpu 0 (rd8 t 0x470);     (* AL = 자정 넘김 *)
    wr8 t 0x470 0
  | 0x01 ->
    wr16 t 0x46E (Cpu86.reg16 cpu 1);
    wr16 t 0x46C (Cpu86.reg16 cpu 2);
    wr8 t 0x470 0
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
  | _ -> ()

(* ---------- INT 33h 마우스 ---------- *)

(* 마우스는 하네스가 움직인다. 기본은 장착 — KOEI 계열 게임은 부팅에서
   AX=0 으로 드라이버를 검사하고, 없으면 키보드 모드로 강제한다.
   AX=0x0C 로 건 핸들러는 Dos_machine.set_mouse 가 이벤트와 함께 부른다. *)
let mouse t =
  let cpu = t.cpu in
  let m = t.mouse in
  match Cpu86.reg16 cpu 0 with
  | 0x00 ->
    Cpu86.set_reg16 cpu 0 (if m.mouse_present then 0xFFFF else 0x0000);
    Cpu86.set_reg16 cpu 3 (if m.mouse_present then 2 else 0);
    m.mouse_visible <- false
  | 0x01 -> m.mouse_visible <- true
  | 0x02 -> m.mouse_visible <- false
  | 0x03 ->
    Cpu86.set_reg16 cpu 1 m.mouse_x;
    Cpu86.set_reg16 cpu 2 m.mouse_y;
    Cpu86.set_reg16 cpu 3 m.mouse_buttons
  | 0x04 ->
    m.mouse_x <- Cpu86.reg16 cpu 1;
    m.mouse_y <- Cpu86.reg16 cpu 2
  | 0x0B ->
    Cpu86.set_reg16 cpu 1 (m.mouse_dx land 0xffff);
    Cpu86.set_reg16 cpu 2 (m.mouse_dy land 0xffff);
    m.mouse_dx <- 0;
    m.mouse_dy <- 0
  | 0x0C ->
    (* DEFINE INTERRUPT SUBROUTINE PARAMETERS — CX 마스크, ES:DX 루틴.
       돌려주는 값은 없다(Ralf Brown rb-5968: Return 없음). AX 를 건드리면
       없는 규약을 지어내는 것이니 저장만 한다. *)
    m.mouse_handler_mask <- Cpu86.reg16 cpu 1;
    m.mouse_handler_seg <- Cpu86.seg cpu 0;
    m.mouse_handler_off <- Cpu86.reg16 cpu 2
  | _ -> ()

(* ---------- 묶음 ---------- *)

let service t vec =
  if vec = 0x16 && (Cpu86.reg8 t.cpu 4 = 0x00 || Cpu86.reg8 t.cpu 4 = 0x10)
  then read_key t
  else begin
  (match vec with
  | 0x10 -> video t
  | 0x11 -> Cpu86.set_reg16 t.cpu 0 (rd16 t 0x410)
  | 0x12 -> Cpu86.set_reg16 t.cpu 0 (rd16 t 0x413)
  | 0x16 -> keyboard t
  | 0x1A -> clock t
  | 0x33 -> mouse t
  | _ -> ());
  Completed
  end
