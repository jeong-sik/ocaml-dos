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
let host_served = [ 0x10; 0x16; 0x20; 0x21; 0x27 ]

(* ROM 안의 자리 배치. INT 8 루틴은 0, IRET 스텁은 0x20, 호스트 서빙
   벡터의 되돌림 스텁은 0x100 부터 16바이트씩. *)
let rom_seg = 0xF000
let iret_stub_off = 0x0020
let stub_area = 0x0100
let stub_stride = 16

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

let install t =
  Bytes.blit (Bytes.of_string bios_int8) 0 t.mem (rom_seg * 16)
    (String.length bios_int8);
  wr8 t ((rom_seg * 16) + iret_stub_off) 0xCF;   (* IRET 스텁 *)
  set_ivt t 0x08 0x0000 rom_seg;                 (* 타이머 IRQ0 *)
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
  wr16 t 0x463 0x3D4;               (* CRTC 어드레스 포트(컬러) *)
  wr8 t 0x465 0x29;                 (* CRT 모드 레지스터 사본 *)
  wr8 t 0x484 (rows - 1);
  wr16 t 0x485 8;                   (* 글자 높이 *)
  wr8 t 0x462 0;                    (* 활성 페이지 *)
  wr8 t 0x460 7; wr8 t 0x461 6;     (* 커서 모양 *)
  set_cursor t 0 0;
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
  (* 나머지 벡터는 전부 IRET 스텁으로 채운다. 실기 BIOS 는 모든 벡터가
     ROM/DOS 를 가리켜서, 프로그램이 훅을 걸며 되읽은 "옛 벡터" 가 0:0
     이 아니다. 하나라도 비워 두면 체인 복귀가 0000:0000 으로 떨어진다. *)
  for v = 0 to 255 do
    if not (List.mem v (0x08 :: host_served)) then
      set_ivt t v iret_stub_off rom_seg
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

let to_bcd n = ((n / 10) * 16) + (n mod 10)

(* ---------- INT 10h 비디오 ---------- *)

let video t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 and al = Cpu86.reg8 cpu 0 in
  match ah with
  | 0x00 ->
    (* AL 의 최상위 비트는 "화면을 지우지 말라" 는 뜻이다. *)
    let mode = al land 0x7f in
    Dos_video.set_mode t.video mode ~clear:(al land 0x80 = 0);
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
    Cpu86.set_reg8 cpu 0 (Dos_video.mode t.video);
    Cpu86.set_reg8 cpu 4 (cols t);
    Cpu86.set_reg8 cpu 7 (rd8 t 0x462)
  | 0x10 ->
    let pal = Dos_ports.palette t.ports in
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
           (rd8 t (src + (i * 3)) land 0x3f,
            rd8 t (src + (i * 3) + 1) land 0x3f,
            rd8 t (src + (i * 3) + 2) land 0x3f)
       done
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
    (* 대체 선택: BL=10h 이면 EGA/VGA 정보. BH=0(컬러), BL=3(256KB) *)
    if Cpu86.reg8 cpu 3 = 0x10 then begin
      Cpu86.set_reg8 cpu 7 0;
      Cpu86.set_reg8 cpu 3 3;
      Cpu86.set_reg16 cpu 1 0
    end
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
    (* 디스플레이 조합 코드: 08h = VGA 컬러 *)
    Cpu86.set_reg8 cpu 0 0x1A;
    Cpu86.set_reg8 cpu 3 0x08;
    Cpu86.set_reg8 cpu 7 0x00
  | _ -> ()

(* ---------- INT 16h 키보드 ---------- *)

let keyboard t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 4 with
  | 0x00 | 0x10 ->
    (* 실기는 키가 올 때까지 블록한다. 빈 링은 하네스가 볼 수 있는
       '굶주림' 으로 알린다 — Turbo Pascal 의 ReadKey 는 AX=0 을 Break
       신호로 읽어 무한 재시도에 빠진다(실측). *)
    if key_pending t then begin
      t.kbd_wait <- false;
      Cpu86.set_reg16 cpu 0 (pop_key t)
    end
    else begin
      starve t;
      Cpu86.set_reg16 cpu 0 0
    end
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
      Cpu86.set_flags cpu (Cpu86.flags cpu lor Cpu86.f_zero)
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

(* 마우스는 하네스가 움직인다. 기본은 미장착 — 없는 장치를 있다고 하면
   게임이 커서를 기다린다. Dos_machine.attach_mouse 로 붙인다. *)
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
  | _ -> ()

(* ---------- 묶음 ---------- *)

let service t vec =
  match vec with
  | 0x10 -> video t
  | 0x11 -> Cpu86.set_reg16 t.cpu 0 (rd16 t 0x410)
  | 0x12 -> Cpu86.set_reg16 t.cpu 0 (rd16 t 0x413)
  | 0x16 -> keyboard t
  | 0x1A -> clock t
  | 0x33 -> mouse t
  | _ -> ()
