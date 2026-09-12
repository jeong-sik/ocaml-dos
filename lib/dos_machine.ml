(* DOS 머신 M2b. Cpu86 + 1MB RAM + 텍스트/VGA 비디오 + INT 표면 +
   COM/MZ-EXE 로더. INT 는 Cpu86 의 호스트 훅으로 받아 OCaml 에서
   서빙한다 — 코어의 스택 무변경 계약 그대로, 훅 안에서 레지스터와
   VRAM 만 바꾼다. *)

type t = {
  mem : Bytes.t;                (** 1MB *)
  cpu : Cpu86.t;
  mutable cursor : int;         (** 텍스트 화면 오프셋(셀 단위) *)
  mutable exited : bool;
  mutable exit_code : int;
  mutable vmode : int;          (** 3=텍스트, 0x13=VGA 256색 선형 *)
  mutable pal : (int * int * int) array;  (** 256색 DAC — 6비트/채널 *)
  host_files : (string, Bytes.t) Hashtbl.t;  (** 하네스가 마운트한 파일 *)
  handles : (int, Bytes.t * int) Hashtbl.t;  (** 열린 핸들 → (내용, 위치) *)
  mutable next_handle : int;
  fcbs : (int, Bytes.t * int) Hashtbl.t;  (** FCB 물리주소 → (내용, 위치) *)
  mutable dta : int;  (** INT 21h AH=1Ah 가 고르는 전송 주소(물리) *)
  mutable psp_seg : int;  (** 적재 시 DOS 가 정한 PSP 세그먼트(load_com 은 0) *)
  mutable kbd_wait : bool;  (** 직전 INT 16h AH=00 이 빈 링으로 즉시 복귀(굶주림) *)
  mutable last_tick : int;  (** 마지막 BIOS tick 갱신 시점의 누적 사이클 *)
}

let vram_base = 0xB8000
let cols = 80
let rows = 25

(* CGA 16색 — attribute 하위니블=전경, 상위니블=배경. *)
let cga_palette = [|
  (0x00, 0x00, 0x00); (0x00, 0x00, 0xAA); (0x00, 0xAA, 0x00); (0x00, 0xAA, 0xAA);
  (0xAA, 0x00, 0x00); (0xAA, 0x00, 0xAA); (0xAA, 0x55, 0x00); (0xAA, 0xAA, 0xAA);
  (0x55, 0x55, 0x55); (0x55, 0x55, 0xFF); (0x55, 0xFF, 0x55); (0x55, 0xFF, 0xFF);
  (0xFF, 0x55, 0x55); (0xFF, 0x55, 0xFF); (0xFF, 0xFF, 0x55); (0xFF, 0xFF, 0xFF) |]

(* 기본 VGA 256색: 0-15 CGA, 16-31 회색 램프, 32-247 6x6x6 RGB 큐브,
   248-255 검정. 게임이 INT 10h AH=10h 으로 DAC 을 다시 쓰지 않는
   프로그램용 근사 — 표준 DAC 초기값과 미세 차이가 있을 수 있다. *)
let default_vga_pal i =
  if i < 16 then
    let (r, g, b) = cga_palette.(i) in
    ((r * 63) / 0xAA, (g * 63) / 0xAA, (b * 63) / 0xAA)
  else if i < 32 then
    let v = ((i - 16) * 63) / 15 in (v, v, v)
  else if i < 248 then begin
    let j = i - 32 in
    let r = j / 36 and g = (j / 6) mod 6 and b = j mod 6 in
    (r * 63 / 5, g * 63 / 5, b * 63 / 5)
  end
  else (0, 0, 0)

let cpu_of t = t.cpu

let psp_seg_of t = t.psp_seg

let kbd_waiting t = t.kbd_wait

let mem_read t a = Char.code (Bytes.get t.mem (a land 0xfffff))

(* DS:DX 의 ASCIIZ 문자열 — INT 21h 파일명. create 의 INT 훅 클로저에서
   쓰므로 원시 Bytes 를 직접 받는다. *)
let read_asciiz_bytes mem cpu =
  let base = ((Cpu86.seg cpu 3 lsl 4) + Cpu86.reg16 cpu 2) land 0xfffff in
  let b = Buffer.create 16 in
  let i = ref 0 in
  let c = ref (Char.code (Bytes.get mem base)) in
  while !c <> 0 && !i < 128 do
    Buffer.add_char b (Char.chr !c);
    incr i;
    c := Char.code (Bytes.get mem ((base + !i) land 0xfffff))
  done;
  Buffer.contents b

(* FCB 의 8+3 이름을 "NAME.EXT" 로 — 스페이스는 잘라낸다. *)
let fcb_name_of mem fcb =
  let take n off =
    let b = Buffer.create 12 in
    for i = 0 to n - 1 do
      let c = Char.code (Bytes.get mem (fcb + off + i)) in
      if c <> 0x20 then Buffer.add_char b (Char.chr c)
    done;
    Buffer.contents b in
  let base = take 8 1 and ext = take 3 9 in
  if ext = "" then base else base ^ "." ^ ext

let fcb_read16 mem a =
  Char.code (Bytes.get mem a) lor (Char.code (Bytes.get mem (a + 1)) lsl 8)

let fcb_write16 mem a v =
  Bytes.set mem a (Char.chr (v land 0xff));
  Bytes.set mem (a + 1) (Char.chr ((v lsr 8) land 0xff))

(* 텍스트 한 글자 찍기 — INT 10h teletype 과 INT 21h AH=02 가 공유. *)
let put_char t ch attr =
  if ch = 0x0D then t.cursor <- (t.cursor / cols) * cols
  else if ch = 0x0A then t.cursor <- min (cols * rows) (t.cursor + cols)
  else begin
    let cell = vram_base + (t.cursor * 2) in
    Bytes.set t.mem cell (Char.chr ch);
    Bytes.set t.mem (cell + 1) (Char.chr attr);
    t.cursor <- min (cols * rows - 1) (t.cursor + 1)
  end

(* 인터럽트 프레임(flags/cs/ip push)을 게스트 IVT 핸들러에 전달.
   false = 그 벡터는 게스트가 안 걸었다(호스트 서빙 대상).
   INT 명령의 게스트 라우팅과 하드웨어 타이머 발화(IRQ0 → INT 8,
   BIOS INT 8 이 0x46C 를 올리고 INT 1Ch 를 호출) 가 같은 경로를 쓴다. *)
let deliver_ivt t v =
  let rd16 a = Char.code (Bytes.get t.mem a)
               lor (Char.code (Bytes.get t.mem (a + 1)) lsl 8) in
  let ivt_off = rd16 (v * 4) and ivt_seg = rd16 (v * 4 + 2) in
  if ivt_off <> 0 || ivt_seg <> 0 then begin
    (* 실기: INT 는 현재 플래그를 그대로 push 하고 '그 후' IF/TF 를
       지운다. IRET 은 푸시된 값 그대로 복원 — 마스킹하면 틱마다 IF 가
       영구히 꺼진 채 돌아온다. *)
    let flags = Cpu86.flags t.cpu in
    let pushw val16 =
      Cpu86.set_reg16 t.cpu 4 ((Cpu86.reg16 t.cpu 4) - 2);
      let sp = (Cpu86.seg t.cpu 2 lsl 4) + Cpu86.reg16 t.cpu 4 in
      Bytes.set t.mem sp (Char.chr (val16 land 0xff));
      Bytes.set t.mem (sp + 1) (Char.chr (val16 lsr 8)) in
    pushw flags;
    pushw (Cpu86.seg t.cpu 1);
    pushw (Cpu86.dump_ip t.cpu);
    Cpu86.set_seg t.cpu 1 ivt_seg;
    Cpu86.set_ip t.cpu ivt_off;
    true
  end else false

let create () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let read a = Char.code (Bytes.get mem (a land 0xfffff)) in
  let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff)) in
  let cpu =
    (* 0x3DA 상태 포트: 읽을 때마다 bit0(디스플레이 인에이블/재주사) 를
       토글. TP CRT 가 VRAM 직접 쓰기 전에 "clear 대기 → set 대기" 상승
       에지를 보고 — 고정값이면 한쪽 대기가 영원히 안 풀린다(실측:
       5fb6:05e9 루프). *)
    let cga_stat = ref false in
    Cpu86.create ~read ~write
      ~port_in:(fun p ->
          if p land 0xffff = 0x3DA then begin
            cga_stat := not !cga_stat;
            if !cga_stat then 0x01 else 0x00
          end
          (* 게임 포트 0x201: 조이스틱 미장착 — 축 비트 0(방전) 이면 TP 의
             축 카운트 루프가 즉시 통과한다(실기 미접속 동작). 0xFF 를
             돌려주면 카운트가 랩할 때까지 갇힌다(실측: ZZT 초기화). *)
          else if p land 0xff = 0x201 then 0x00 else 0xff)
      ~port_out:(fun _ _ -> ())
  in
  let t =
    { mem; cpu; cursor = 0; exited = false; exit_code = 0;
      vmode = 3; pal = Array.init 256 default_vga_pal;
      host_files = Hashtbl.create 8; handles = Hashtbl.create 4;
      (* DOS 예약 핸들 0-4 (stdin/stdout/stderr/aux/prn) 은 피해서
         배정한다. 0/1/2 는 콘솔로 특수 취급(0x40 참조). *)
      next_handle = 5;
      fcbs = Hashtbl.create 4; dta = 0x80; psp_seg = 0; last_tick = 0;
      kbd_wait = false }
  in
  (* BIOS ROM: F000:0000 에 INT 8 루틴(0x46C tick 범프 → INT 1Ch 호출 →
     PIC EOI → IRET), F000:0020 에 IRET 스텁. IVT[8]/[1Bh]/[1Ch] 을
     채운다 — 실기 BIOS 는 모든 벡터가 ROM/DOS 를 가리켜서 프로그램이
     훅 설치 때 INT 21h AH=35h 로 되읽은 "옛 벡터" 가 0:0 이 아니다.
     빈 IVT 로 두면 TP 등 체인 방식 런타임이 옛 벡터 0:0 으로 복귀해
     0000:0000 에 떨어진다(ZZT 실측: retf 후 cs=0000). *)
  let bios_int8 =
    "\x1e\x50"                         (* push ds; push ax *)
    ^ "\x31\xc0\x8e\xd8"            (* xor ax,ax; mov ds,ax *)
    ^ "\xa1\x6c\x04\x40\xa3\x6c\x04"  (* mov ax,[046C]; inc ax; mov [046C],ax *)
    ^ "\x75\x04"                      (* jnz +4 — 자리올림 없으면 고워드 skip *)
    ^ "\xff\x06\x6e\x04"            (* inc word [046E] *)
    ^ "\xcd\x1c"                      (* int 1Ch — 사용자 틱 훅 *)
    ^ "\xb0\x20\xe6\x20"            (* mov al,20h; out 20h,al (EOI) *)
    ^ "\x58\x1f\xcf"                 (* pop ax; pop ds; iret *)
  in
  Bytes.blit (Bytes.of_string bios_int8) 0 mem 0xF0000
    (String.length bios_int8);
  Bytes.set mem 0xF0020 '\xcf';         (* IRET 스텁 (vec 1Bh/1Ch 기본값) *)
  let set_ivt v off seg =
    Bytes.set mem (v * 4) (Char.chr (off land 0xff));
    Bytes.set mem (v * 4 + 1) (Char.chr (off lsr 8));
    Bytes.set mem (v * 4 + 2) (Char.chr (seg land 0xff));
    Bytes.set mem (v * 4 + 3) (Char.chr (seg lsr 8)) in
  set_ivt 0x08 0x0000 0xF000;            (* 타이머 IRQ0: ROM INT 8 루틴 *)
  (* BDA(BIOS Data Area, 0x40:xx) — 실기 부팅 값. TP CRT 가 화면 폴링
     포트를 [0x40:0x63](CRTC 베이스) + 6 으로 계산한다: 0 이면 포트 6 을
     읽어 영원히 갇힌다(실측: 5fb6:05e9). 컬러 80x25 텍스트 기준. *)
  let bda8 a v = Bytes.set mem a (Char.chr (v land 0xff)) in
  let bda16 a v = bda8 a v; bda8 (a + 1) (v lsr 8) in
  bda16 0x413 640;                      (* 기억용량 KB *)
  bda8 0x449 3;                         (* 비디오 모드 3 *)
  bda16 0x44A 80;                       (* 열 수 *)
  bda16 0x44C 0x2000;                   (* 페이지 크기(워드) 16KB *)
  bda16 0x463 0x3D4;                    (* CRTC 어드레스 포트(컬러) *)
  bda8 0x484 24;                        (* 행-1 *)
  bda16 0x480 0x001E;                   (* 키 버퍼 시작 오프셋 *)
  bda16 0x482 0x0000;                   (* 키 버퍼 시작 세그먼트 *)
  (* 실기처럼 나머지 벡터 전부 IRET 스텁으로 채운다. 예외는 호스트가
     서빙하는 {10h 비디오, 16h 키보드, 20h/21h DOS} — 비어있어야 INT 가
     호스트 훅으로 온다. TP 런타임은 부팅 때 IVT 전체를 훑어 저장하므로
     0:0 벡터가 하나라도 남으면 체인 복귀(retf) 때 0000:0000 으로
     떨어진다(ZZT 실측). *)
  for v = 0 to 255 do
    if not (List.mem v [ 0x08; 0x10; 0x16; 0x20; 0x21 ]) then set_ivt v 0x0020 0xF000
  done;
  Cpu86.set_int_hook cpu (fun vec ->
      (* 게스트 IVT 우선: 프로그램이 후킹한 벡터(0:vec*4 != 0:0)는 진짜
         인터럽트 프레임(flags/cs/ip push)으로 게스트 핸들러에 보내고
         IRET 이 돌아온다. 비어있으면 호스트 서빙(DOS/BIOS 표면).
         ZZT/TP 가 벡터를 설치하고 INT 21h AH=35 로 되읽는다(실측). *)
      let v = vec land 0xff in
      if deliver_ivt t v then ()
      else begin
      let ah = Cpu86.reg8 t.cpu 4 in
      (try if Sys.getenv "DOSDBG" <> "" then
         Printf.eprintf "INT %02x ah=%02x al=%02x @%04x:%04x dx=%04x ds=%04x\n%!"
           vec ah (Cpu86.reg8 t.cpu 0) (Cpu86.seg t.cpu 1) (Cpu86.dump_ip t.cpu) (Cpu86.reg16 t.cpu 2) (Cpu86.seg t.cpu 3)
       with Not_found -> ());
      (match vec with
       | 0x10 ->
         (match ah with
          | 0x0E (* teletype — BL 하위니블이 전경색 *) ->
            put_char t (Cpu86.reg8 t.cpu 0) (Cpu86.reg8 t.cpu 3 land 0x0f)
          | 0x0F (* 모드 읽기: AL=모드 3, AH=80 *) ->
            Cpu86.set_reg8 t.cpu 0 3;
            Cpu86.set_reg8 t.cpu 4 cols
          | 0x00 (* 모드 세팅: 3=텍스트, 13h=VGA 256색 *) ->
            let al = Cpu86.reg8 t.cpu 0 in
            if al = 0x13 then begin
              t.vmode <- 0x13;
              Bytes.fill t.mem 0xA0000 32000 '\000'
            end
            else t.vmode <- 3
          | 0x10 (* DAC 색 설정: AL=10h 개별 — BX=색, DH=R CH=G CL=B *) ->
            if Cpu86.reg8 t.cpu 0 = 0x10 then begin
              let idx = Cpu86.reg16 t.cpu 3 land 0xff in
              t.pal.(idx) <-
                (Cpu86.reg8 t.cpu 7 land 0x3f,
                 Cpu86.reg8 t.cpu 5 land 0x3f,
                 Cpu86.reg8 t.cpu 1 land 0x3f)
            end
          | _ -> ())
       | 0x21 ->
         (match ah with
          | 0x4C (* 종료 *) ->
            t.exited <- true;
            t.exit_code <- Cpu86.reg8 t.cpu 0
          | 0x35 (* get vector: AL=vec → ES:BX *) ->
            let v = Cpu86.reg8 t.cpu 0 land 0xff in
            let rd16 a = Char.code (Bytes.get t.mem a)
                         lor (Char.code (Bytes.get t.mem (a + 1)) lsl 8) in
            Cpu86.set_reg16 t.cpu 3 (rd16 (v * 4));
            Cpu86.set_seg t.cpu 0 (rd16 (v * 4 + 2))
          | 0x25 (* set vector: AL=vec, DS:DX *) ->
            let v = Cpu86.reg8 t.cpu 0 land 0xff in
            let wr16 a x =
              Bytes.set t.mem a (Char.chr (x land 0xff));
              Bytes.set t.mem (a + 1) (Char.chr ((x lsr 8) land 0xff)) in
            wr16 (v * 4) (Cpu86.reg16 t.cpu 2);
            wr16 (v * 4 + 2) (Cpu86.seg t.cpu 3)
          | 0x44 (* IOCTL — TP CRT 가 부팅 때 콘솔/파일 상태를 묻는다.
                    전부 실패시키면 핸들을 무효로 간주해 runtime error 006
                    로 죽는다(ZZT 실측). AL=00 device info: 콘솔(0/1/2) 은
                    bit7 문자디바이스+콘솔 비트, 파일은 0x02(읽기+쓰기).
                    AL=06/07 입출력 상태: 준비됨. 나머지는 invalid. *)
            -> (match Cpu86.reg8 t.cpu 0 with
             | 0x00 ->
               (match Cpu86.reg16 t.cpu 3 with
                | 0 | 1 | 2 ->
                  Cpu86.set_reg16 t.cpu 2 0x80D3;
                  Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
                | h ->
                  if Hashtbl.mem t.handles h then begin
                    Cpu86.set_reg16 t.cpu 2 0x0002;
                    Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
                  end else begin
                    Cpu86.set_reg16 t.cpu 0 6;
                    Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)
                  end)
             | 0x06 | 0x07 ->
               Cpu86.set_reg8 t.cpu 0 0xFF;
               Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
             | _ ->
               Cpu86.set_reg16 t.cpu 0 1;
               Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry))
          | 0x02 (* 문자 출력: DL *) ->
            put_char t (Cpu86.reg8 t.cpu 2) 0x07
          | 0x06 (* 직접 콘솔 출력: DL(≠FF). TP 런타임의 에러 메시지가
                    이 경로로 나온다 — 없으면 화면에 아무 흔적 없이 죽는다
                    (ZZT 실측: "Runtime error 006 at ..."). *) ->
            let dl = Cpu86.reg8 t.cpu 2 in
            if dl <> 0xFF then put_char t dl 0x07
          | 0x0F (* FCB open: DS:DX = FCB *) ->
            let fcb = (Cpu86.seg t.cpu 3 lsl 4) + Cpu86.reg16 t.cpu 2 in
            let name = String.uppercase_ascii (fcb_name_of mem fcb) in
            begin match Hashtbl.find_opt t.host_files name with
              | Some data ->
                Hashtbl.replace t.fcbs fcb (data, 0);
                (* 파일 크기(@0x10 dword) 기록, 레코드 크기(@0x0E)=128,
                   블록/레코드/랜덤은 0 으로. *)
                fcb_write16 mem (fcb + 0x0E) 128;
                fcb_write16 mem (fcb + 0x10) (Bytes.length data land 0xffff);
                fcb_write16 mem (fcb + 0x12) ((Bytes.length data lsr 16) land 0xffff);
                Bytes.set mem (fcb + 0x20) '\x00';
                Bytes.set mem (fcb + 0x21) '\x00';
                Bytes.set mem (fcb + 0x22) '\x00';
                Bytes.set mem (fcb + 0x23) '\x00';
                Cpu86.set_reg8 t.cpu 0 0
              | None -> Cpu86.set_reg8 t.cpu 0 0xFF
            end
          | 0x10 (* FCB close *) ->
            let fcb = (Cpu86.seg t.cpu 3 lsl 4) + Cpu86.reg16 t.cpu 2 in
            Hashtbl.remove t.fcbs fcb;
            Cpu86.set_reg8 t.cpu 0 0
          | 0x14 (* FCB 순차 읽기: 한 레코드 → DTA *) ->
            let fcb = (Cpu86.seg t.cpu 3 lsl 4) + Cpu86.reg16 t.cpu 2 in
            let recsize = max 1 (fcb_read16 mem (fcb + 0x0E)) in
            begin match Hashtbl.find_opt t.fcbs fcb with
              | Some (data, pos) ->
                let avail = Bytes.length data - pos in
                if avail <= 0 then Cpu86.set_reg8 t.cpu 0 1  (* EOF *)
                else begin
                  let take = min recsize avail in
                  Bytes.blit data pos mem t.dta take;
                  Hashtbl.replace t.fcbs fcb (data, pos + take);
                  (* 현재 레코드(+0x20) 진행 — 128 차면 블록(+0x0C) 증가 *)
                  let recno = Char.code (Bytes.get mem (fcb + 0x20)) + 1 in
                  if recno >= 128 then begin
                    fcb_write16 mem (fcb + 0x0C)
                      (fcb_read16 mem (fcb + 0x0C) + 1);
                    Bytes.set mem (fcb + 0x20) '\x00'
                  end
                  else Bytes.set mem (fcb + 0x20) (Char.chr recno);
                  Cpu86.set_reg8 t.cpu 0
                    (if take < recsize then 1 else 0)  (* 1=부분 레코드(EOF 근처) *)
                end
              | None -> Cpu86.set_reg8 t.cpu 0 0xFF
            end
          | 0x21 (* FCB 랜덤 읽기: FCB+0x21 레코드 번호 1개 → DTA *) ->
            let fcb = (Cpu86.seg t.cpu 3 lsl 4) + Cpu86.reg16 t.cpu 2 in
            let recsize = max 1 (fcb_read16 mem (fcb + 0x0E)) in
            let recno =
              Char.code (Bytes.get mem (fcb + 0x21))
              lor (Char.code (Bytes.get mem (fcb + 0x22)) lsl 8)
              lor (Char.code (Bytes.get mem (fcb + 0x23)) lsl 16) in
            begin match Hashtbl.find_opt t.fcbs fcb with
              | Some (data, pos) ->
                let start = pos in
                let off = recno * recsize in
                ignore start;
                if off >= Bytes.length data then Cpu86.set_reg8 t.cpu 0 1
                else begin
                  let take = min recsize (Bytes.length data - off) in
                  Bytes.blit data off mem t.dta take;
                  Hashtbl.replace t.fcbs fcb (data, off + take);
                  Cpu86.set_reg8 t.cpu 0
                    (if take < recsize then 1 else 0)
                end
              | None -> Cpu86.set_reg8 t.cpu 0 0xFF
            end
          | 0x1A (* set DTA: DS:DX *) ->
            t.dta <- (Cpu86.seg t.cpu 3 lsl 4) + Cpu86.reg16 t.cpu 2
          | 0x24 (* FCB 레코드 크기 설정: FCB+0x0E 를 이미 쓴 값으로 확정 *) ->
            Cpu86.set_reg8 t.cpu 0 0
          | 0x3D (* open: DS:DX ASCIIZ *) ->
            let name = String.uppercase_ascii (read_asciiz_bytes mem cpu) in
            begin match Hashtbl.find_opt t.host_files name with
              | Some data ->
                let h = t.next_handle in
                t.next_handle <- t.next_handle + 1;
                Hashtbl.replace t.handles h (data, 0);
                Cpu86.set_reg16 t.cpu 0 h;
                Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
              | None ->
                Cpu86.set_reg16 t.cpu 0 2;
                Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)
            end
          | 0x3F (* read: BX=핸들 CX=바이트 DS:DX=버퍼 *) ->
            let h = Cpu86.reg16 t.cpu 3 in
            let want = Cpu86.reg16 t.cpu 1 in
            let buf = Cpu86.reg16 t.cpu 2 in
            if h = 0 then begin
              (* stdin: 키는 INT 16h 경로 — 핸들 읽기는 즉시 EOF(0) *)
              Cpu86.set_reg16 t.cpu 0 0;
              Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
            end
            else begin match Hashtbl.find_opt t.handles h with
              | None ->
                Cpu86.set_reg16 t.cpu 0 6;
                Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)
              | Some (data, pos) ->
                let take = min want (max 0 (Bytes.length data - pos)) in
                Bytes.blit data pos t.mem
                  ((Cpu86.seg t.cpu 3 lsl 4) + buf) take;
                Hashtbl.replace t.handles h (data, pos + take);
                Cpu86.set_reg16 t.cpu 0 take;
                Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
            end
          | 0x3E (* close: BX=핸들 *) ->
            Hashtbl.remove t.handles (Cpu86.reg16 t.cpu 3);
            Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
          | 0x40 (* write: BX=핸들 CX=바이트 DS:DX=버퍼 → AX=쓴 수.
                    실기 DOS 는 부팅 때 stdin(0)/stdout(1)/stderr(2) 를
                    열어준다 — TP 런타임이 stderr 로 진단을 쓰는데 없으면
                    invalid handle(6) 로 즉사한다(ZZT 실측). 콘솔 쓰기는
                    화면에 찍는다. *)
            -> (match Cpu86.reg16 t.cpu 3 with
             | 1 | 2 ->
               let src = (Cpu86.seg t.cpu 3 lsl 4) + Cpu86.reg16 t.cpu 2 in
               for i = 0 to Cpu86.reg16 t.cpu 1 - 1 do
                 put_char t (Char.code (Bytes.get t.mem ((src + i) land 0xfffff))) 0x07
               done;
               Cpu86.set_reg16 t.cpu 0 (Cpu86.reg16 t.cpu 1);
               Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
             | h ->
               (match Hashtbl.find_opt t.handles h with
                | Some _ ->
                  (* 마운트된 파일은 읽기 전용 — 쓰기 거부 *)
                  Cpu86.set_reg16 t.cpu 0 5;
                  Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)
                | None ->
                  Cpu86.set_reg16 t.cpu 0 6;
                  Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)))
          | 0x42 (* lseek: AL=모드 BX=핸들 CX:DX=오프셋 → DX:AX *) ->
            let h = Cpu86.reg16 t.cpu 3 in
            let al = Cpu86.reg8 t.cpu 0 in
            let off32 =
              (Cpu86.reg16 t.cpu 1 lsl 16) lor Cpu86.reg16 t.cpu 2 in
            begin match Hashtbl.find_opt t.handles h with
              | None ->
                Cpu86.set_reg16 t.cpu 0 6;
                Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)
              | Some (data, pos) ->
                let signed =
                  if off32 >= 0x80000000 then off32 - 0x100000000 else off32 in
                let size = Bytes.length data in
                let newpos =
                  match al with
                  | 0 -> signed
                  | 1 -> pos + signed
                  | _ -> size + signed
                in
                if newpos < 0 || newpos > size then begin
                  Cpu86.set_reg16 t.cpu 0 6;
                  Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_carry)
                end
                else begin
                  Hashtbl.replace t.handles h (data, newpos);
                  Cpu86.set_reg16 t.cpu 0 (newpos land 0xffff);
                  Cpu86.set_reg16 t.cpu 2 ((newpos lsr 16) land 0xffff);
                  Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_carry)
                end
            end
          | _ -> ())
       | 0x16 ->
         (* BIOS 키 링(0x40:0x1E-0x3D) 을 단일 진실 원천으로 읽는다 —
            호스트 큐를 따로 두면 게스트가 링을 직접 다룰 때 두 사본이
            갈라진다(실측: AH=00 이 호스트 큐만 비워 ZZT 가 AH=01 폴링을
            영원히 돌았다). 헤드/테일 포인터(0x41A/0x41C) 규약 그대로. *)
         let ring_rd16 a =
           Char.code (Bytes.get t.mem a)
           lor (Char.code (Bytes.get t.mem (a + 1)) lsl 8) in
         let ring_wr16 a v =
           Bytes.set t.mem a (Char.chr (v land 0xff));
           Bytes.set t.mem (a + 1) (Char.chr ((v lsr 8) land 0xff)) in
         let head = ref (ring_rd16 0x41A) and tail = ring_rd16 0x41C in
         if !head < 0x1E || !head > 0x3C then head := 0x1E;
         let key_pending () = !head <> tail in
         let pop_key () =
           let w = ring_rd16 (0x400 + !head) in
           head := if !head >= 0x3C then 0x1E else !head + 2;
           ring_wr16 0x41A !head;
           w in
         (match ah with
          | 0x00 | 0x10 ->
            (* AX = (스캔<<8)|ASCII — BIOS 규약대로 전체 워드 반환.
               실기는 키가 올 때까지 블록한다 — 빈 링은 하네스가 볼 수
               있는 '굶주림' 상태로 알린다(TP ReadKey 는 AX=0 을 Break
               신호로 해석해 무한 재시도 루프에 빠진다, ZZT 실측). *)
            if key_pending () then begin
              t.kbd_wait <- false;
              Cpu86.set_reg16 t.cpu 0 (pop_key ())
            end
            else begin
              t.kbd_wait <- true;
              Cpu86.set_reg16 t.cpu 0 0
            end
          | 0x01 | 0x11 ->
            if key_pending () then begin
              Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_zero);
              Cpu86.set_reg16 t.cpu 0 (ring_rd16 (0x400 + !head))
            end
            else
              Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_zero)
          | _ -> ())
       | 0x20 (* PSP INT 20h — RET 로 돌아온 프로그램의 종료 *) ->
         t.exited <- true;
         t.exit_code <- 0
       | _ -> ())
      end);
  t

let load_com t image =
  Bytes.blit (Bytes.of_string image) 0 t.mem 0x100 (String.length image);
  (* PSP: INT 20h, 커맨드라인 길이 0. RET 복귀지 스택 top = 0x0000. *)
  Bytes.set t.mem 0x00 '\xcd';
  Bytes.set t.mem 0x01 '\x20';
  Bytes.set t.mem 0x80 '\x00';
  Cpu86.set_seg t.cpu 1 0;  (* CS *)
  Cpu86.set_seg t.cpu 3 0;  (* DS *)
  Cpu86.set_seg t.cpu 0 0;  (* ES *)
  Cpu86.set_seg t.cpu 2 0;  (* SS *)
  Cpu86.set_ip t.cpu 0x100;
  Cpu86.set_reg16 t.cpu 4 0xFFFE;  (* SP *)
  Bytes.set t.mem 0xFFFE '\x00';
  Bytes.set t.mem 0xFFFF '\x00';
  t.exited <- false;
  t.exit_code <- 0

(* MZ EXE 로더 — 실기 배치. PSP 세그먼트(0x1000) 앞, 이미지는 그 16
   paras 뒤(image_seg): PSP+0x00 INT 20h, +0x02 메모리 top, +0x0A 종료
   주소, +0x80 커맨드라인. DOS 는 DS/ES 를 PSP 세그먼트로 시작한다 —
   ZZT 엔트리가 mov cx,[PSP+0x0C] 로 바로 읽는다(실측). 이미지 시작을
   덮어쓰던 초판 PSP 심기의 파괴는 이 배치로 사라진다. *)
let load_exe t image =
  let u8 i = Char.code image.[i] in
  let u16 i = u8 i lor (u8 (i + 1) lsl 8) in
  if not (u8 0 = 0x4D && u8 1 = 0x5A) then invalid_arg "not an MZ image";
  let reloc_count = u16 0x06 in
  let header_bytes = u16 0x08 * 16 in
  let exe_ip = u16 0x14 in
  let exe_cs = u16 0x16 in
  let exe_sp = u16 0x10 in
  let exe_ss = u16 0x0E in
  (* 실기 DOS 배치: 프로그램을 conventional RAM memtop 아래 배정.
     LZEXE 스텁의 자기 재배치가 이 위치를 기준으로 원본 엔트리를 계산
     한다 (ZZT 실측: 낮은 고정 0x1000 에서 원본 엔트리 0xC000:0x192B 가
     빈 주소가 됨).
     상한: 이미지 시작 psp+0x10 부터 img_paras+minalloc 이 0xA000
     (VRAM 시작 세그) 을 넘지 않게 — 넘으면 INT 10h 모드 셋이 VRAM
     clear 하며 코드를 지운다 (mode 13h fill 0xA0000+32000 실측). *)
  let psp_seg =
    (* 이미지 크기 = 파일 전체 - 헤더(헤더는 게스트 메모리에 안 남는다) *)
    let img_paras = (String.length image - header_bytes + 15) / 16 in
    let minalloc = u16 0x0A in
    max 0x1000 (0x9FF0 - img_paras - minalloc)
  in
  let image_seg = psp_seg + 0x10 in
  let image_base = image_seg * 16 in
  Bytes.blit (Bytes.of_string image) header_bytes t.mem image_base
    (String.length image - header_bytes);
  for i = 0 to reloc_count - 1 do
    let e = u16 0x18 + (i * 4) in
    let off = u16 e and seg = u16 (e + 2) in
    let addr = image_base + seg * 16 + off in
    let old = Char.code (Bytes.get t.mem addr)
              lor (Char.code (Bytes.get t.mem (addr + 1)) lsl 8) in
    let v = old + image_seg in
    Bytes.set t.mem addr (Char.chr (v land 0xff));
    Bytes.set t.mem (addr + 1) (Char.chr ((v lsr 8) land 0xff))
  done;
  (* PSP — 실기 계약: INT 20h, memtop(비디오 0xA000 앞), 종료 주소 0:0,
     커맨드라인 길이 0. *)
  let psp = psp_seg * 16 in
  t.psp_seg <- psp_seg;
  (* 기본 DTA = PSP:0x80 (실기 DOS 규약 — AH=1Ah 로 안 바꾸면 이 자리) *)
  t.dta <- psp + 0x80;
  Bytes.set t.mem psp '\xcd';
  Bytes.set t.mem (psp + 0x01) '\x20';
  Bytes.set t.mem (psp + 0x02) '\xff';
  Bytes.set t.mem (psp + 0x03) '\x9f';
  Bytes.set t.mem (psp + 0x80) '\x00';
  Bytes.set t.mem (psp + 0x81) '\x0d';
  Cpu86.set_seg t.cpu 1 (image_seg + exe_cs);  (* CS *)
  Cpu86.set_ip t.cpu exe_ip;
  Cpu86.set_seg t.cpu 2 (image_seg + exe_ss);  (* SS *)
  Cpu86.set_reg16 t.cpu 4 exe_sp;
  Cpu86.set_seg t.cpu 3 psp_seg;               (* DS = PSP (DOS 표준) *)
  Cpu86.set_seg t.cpu 0 psp_seg;               (* ES = PSP *)
  t.exited <- false;
  t.exit_code <- 0

(* 하네스가 게임 데이터 파일을 마운트한다 — INT 21h open 이 이 이름으로
   찾는다. 이름은 대소문자 무시로 매칭. *)
let mount_file t name data =
  Hashtbl.replace t.host_files (String.uppercase_ascii name) (Bytes.of_string data)

let step t =
  if t.exited || Cpu86.halted t.cpu then 2
  else begin
    let used = Cpu86.step t.cpu in
    let cyc = Cpu86.cycles t.cpu in
    (* IRQ0 (18.2Hz): 하드웨어처럼 벡터 8 만 발화. ROM INT 8 루틴이
       0x46C 를 올리고 INT 1Ch 사용자 훅을 부른 뒤 EOI+IRET 한다 —
       게스트가 INT 8 을 훅했으면 게스트 핸들러가 그 사슬을 이어받는다. *)
    if cyc - t.last_tick >= 262087 then begin
      t.last_tick <- cyc;
      ignore (deliver_ivt t 8)
    end;
    used
  end

let run t ~max_steps =
  let n = ref 0 in
  while (not t.exited) && not (Cpu86.halted t.cpu) && !n < max_steps do
    ignore (step t);
    incr n
  done

let exited t = t.exited
let exit_code t = t.exit_code
let halted t = Cpu86.halted t.cpu

let screen_text t =
  let b = Buffer.create (cols * (rows + 1)) in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let ch = Char.code (Bytes.get t.mem (vram_base + ((r * cols + c) * 2))) in
      Buffer.add_char b (if ch >= 32 && ch < 127 then Char.chr ch else ' ')
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

(* 현재 비디오 모드의 프레임 크기 — 렌더러/하네스 계약. *)
let frame_dims t = if t.vmode = 0x13 then (320, 200) else (640, 400)

let frame_rgb t =
  if t.vmode = 0x13 then begin
    (* VGA 13h: 0xA0000 선형 320x200, DAC 은 6비트/채널. *)
    let img = Bytes.make (320 * 200 * 3) '\000' in
    for i = 0 to 320 * 200 - 1 do
      let r, g, b = t.pal.(Char.code (Bytes.get t.mem (0xA0000 + i))) in
      Bytes.set img (i * 3) (Char.chr ((r * 255) / 63));
      Bytes.set img (i * 3 + 1) (Char.chr ((g * 255) / 63));
      Bytes.set img (i * 3 + 2) (Char.chr ((b * 255) / 63))
    done;
    Bytes.to_string img
  end
  else begin
    (* 텍스트 640x400: 각 셀 8x8 글리프 스케일업. 속성 바이트의 하위니블=
       전경, 상위니블=배경. 폰트는 1비트/행 — MSB 가 왼쪽. *)
    let img = Bytes.make (640 * 400 * 3) '\000' in
    for r = 0 to rows - 1 do
      for c = 0 to cols - 1 do
        let cell = vram_base + ((r * cols + c) * 2) in
        let ch = Char.code (Bytes.get t.mem cell) in
        let attr = Char.code (Bytes.get t.mem (cell + 1)) in
        let fr, fg, fb = cga_palette.(attr land 0x0f) in
        let br, bg, bb = cga_palette.((attr lsr 4) land 0x07) in
        let glyph = Font8x8.glyph ch in
        for gy = 0 to 7 do
          let bits = glyph.(gy) in
          let ybase = ((r * 8 + gy) * 640 + c * 8) * 3 in
          for gx = 0 to 7 do
            let on = bits land (0x80 lsr gx) <> 0 in
            let i = ybase + (gx * 3) in
            let rr, gg, bb2 = if on then (fr, fg, fb) else (br, bg, bb) in
            Bytes.set img i (Char.chr rr);
            Bytes.set img (i + 1) (Char.chr gg);
            Bytes.set img (i + 2) (Char.chr bb2)
          done
        done
      done
    done;
    Bytes.to_string img
  end

(* BIOS 키 버퍼(0x40:0x1E-0x3D, 16워드 링): 헤드 0x1A, 테일 0x1C. 게스트가
   INT 9/16 을 후킹해 이 큐를 직접 폴링한다(ZZT/TP 실측) — 호스트 큐만
   채우면 게스트 핸들러가 못 읽는다. push 는 (스캔<<8|ascii) 워드. *)
let push_key t sc =
  let wr16 a x =
    Bytes.set t.mem a (Char.chr (x land 0xff));
    Bytes.set t.mem (a + 1) (Char.chr ((x lsr 8) land 0xff)) in
  let rd16 a =
    Char.code (Bytes.get t.mem a)
    lor (Char.code (Bytes.get t.mem (a + 1)) lsl 8) in
  (* BIOS 규약: 0x41A/0x41C 는 버퍼 내 '오프셋'(0x1E-0x3D) — 물리는 0x400+. *)
  let head_off = rd16 0x41A and tail_off = rd16 0x41C in
  if head_off = 0 then begin wr16 0x41A 0x1E; wr16 0x41C 0x1E end;
  let tail_off = if tail_off < 0x1E || tail_off > 0x3C then 0x1E else tail_off in
  let head_off = if head_off < 0x1E || head_off > 0x3C then 0x1E else head_off in
  let nxt = if tail_off >= 0x3C then 0x1E else tail_off + 2 in
  if nxt <> head_off then begin
    wr16 (0x400 + tail_off) sc;
    wr16 0x41C nxt
  end
