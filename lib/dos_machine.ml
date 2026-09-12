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
  keys : int Queue.t;           (** BIOS 스캔 코드 — 하네스가 넣는다 *)
  mutable vmode : int;          (** 3=텍스트, 0x13=VGA 256색 선형 *)
  mutable pal : (int * int * int) array;  (** 256색 DAC — 6비트/채널 *)
  host_files : (string, Bytes.t) Hashtbl.t;  (** 하네스가 마운트한 파일 *)
  handles : (int, Bytes.t * int) Hashtbl.t;  (** 열린 핸들 → (내용, 위치) *)
  mutable next_handle : int;
  fcbs : (int, Bytes.t * int) Hashtbl.t;  (** FCB 물리주소 → (내용, 위치) *)
  mutable dta : int;  (** INT 21h AH=1Ah 가 고르는 전송 주소(물리) *)
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

let create () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let read a = Char.code (Bytes.get mem (a land 0xfffff)) in
  let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff)) in
  let cpu =
    Cpu86.create ~read ~write
      ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> ())
  in
  let t =
    { mem; cpu; cursor = 0; exited = false; exit_code = 0; keys = Queue.create ();
      vmode = 3; pal = Array.init 256 default_vga_pal;
      host_files = Hashtbl.create 8; handles = Hashtbl.create 4; next_handle = 6;
      fcbs = Hashtbl.create 4; dta = 0x80; last_tick = 0 }
  in
  Cpu86.set_int_hook cpu (fun vec ->
      let ah = Cpu86.reg8 t.cpu 4 in
      (try if Sys.getenv "DOSDBG" <> "" then
         Printf.eprintf "INT %02x ah=%02x @%04x:%04x dx=%04x ds=%04x\n%!"
           vec ah (Cpu86.seg t.cpu 1) (Cpu86.dump_ip t.cpu) (Cpu86.reg16 t.cpu 2) (Cpu86.seg t.cpu 3)
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
          | 0x02 (* 문자 출력 *) ->
            put_char t (Cpu86.reg8 t.cpu 0) 0x07
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
            begin match Hashtbl.find_opt t.handles h with
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
         (match ah with
          | 0x00 | 0x10 ->
            if Queue.is_empty t.keys then Cpu86.set_reg8 t.cpu 0 0
            else Cpu86.set_reg8 t.cpu 0 (Queue.pop t.keys)
          | 0x01 | 0x11 ->
            if Queue.is_empty t.keys then
              Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_zero)
            else begin
              Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_zero);
              Cpu86.set_reg8 t.cpu 0 (Queue.peek t.keys)
            end
          | _ -> ())
       | 0x20 (* PSP INT 20h — RET 로 돌아온 프로그램의 종료 *) ->
         t.exited <- true;
         t.exit_code <- 0
       | _ -> ()));
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
  let psp_seg = 0x1000 in
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
    if cyc - t.last_tick >= 262087 then begin
      t.last_tick <- cyc;
      let v = Char.code (Bytes.get t.mem 0x46C)
              lor (Char.code (Bytes.get t.mem 0x46D) lsl 8)
              lor (Char.code (Bytes.get t.mem 0x46E) lsl 16) in
      let v = v + 1 in
      Bytes.set t.mem 0x46C (Char.chr (v land 0xff));
      Bytes.set t.mem 0x46D (Char.chr ((v lsr 8) land 0xff));
      Bytes.set t.mem 0x46E (Char.chr ((v lsr 16) land 0xff))
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

let push_key t sc = Queue.push sc t.keys
