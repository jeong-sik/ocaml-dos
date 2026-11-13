(* BDA 0x487·0x488·0x489 의 부팅 값과 모드 세우기 뒤 값, AH=0Fh 의 AL
   bit 7, AH=12h BL=10h 의 BH·CX. 값은 SeaBIOS vgainit.c·vgabios.c 와
   DOSBox-staging int10.cpp 가 같게 적는 것이다. *)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let int10 ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x10"
let store_ax a = "\xa3" ^ w16 a
let store_bx a = "\x89\x1e" ^ w16 a
let store_cx a = "\x89\x0e" ^ w16 a
let exit_0 = "\xb8\x00\x4c\xcd\x21"

let psp = 0x10000
let code_top = 0x280
let out = 0x300

let ega_misc = 0x487
let switches = 0x488
let vga_flags = 0x489
let crtc_port = 0x463

let run ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= code_top);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  before m;
  Dos_machine.run m ~max_steps:50_000;
  check "program exited" (Dos_machine.exited m);
  m

let bda m a = Dos_machine.mem_read m a
let rd m i = Dos_machine.mem_read m (psp + out + i)
let poke a v m = Dos_machine.mem_write m a v

let test_boot () =
  let m = run exit_0 in
  checki "부팅 0x487" (bda m ega_misc) 0x60;
  checki "부팅 0x488" (bda m switches) 0xF9;
  checki "부팅 0x489" (bda m vga_flags) 0x51

(* AH=0Fh 를 부르고 AX 를 out+i 에 *)
let get_mode i = int10 0x0F00 ^ store_ax (out + i)

let test_mode_set () =
  let m = run (int10 0x0013 ^ get_mode 0 ^ int10 0x0093 ^ get_mode 2 ^ exit_0) in
  checki "13h: AL" (rd m 0) 0x13;
  checki "93h: AL bit 7" (rd m 2) 0x93;
  checki "93h: AH 열" (rd m 3) 40;
  checki "93h 뒤 0x487" (bda m ega_misc) 0xE0;
  let m = run (int10 0x0093 ^ int10 0x0003 ^ get_mode 0 ^ exit_0) in
  checki "93h 다음 03h: AL" (rd m 0) 0x03;
  checki "93h 다음 03h: 0x487" (bda m ega_misc) 0x60;
  (* 모드 세우기 전에는 부팅 값 그대로, AL bit 7 없음 *)
  let m = run (get_mode 0 ^ exit_0) in
  checki "부팅 뒤 AH=0Fh AL" (rd m 0) 0x03

(* 모드 세우기는 0x487 의 다른 비트와 0x488 을 다시 적고, 0x489 는 bit 7 만
   지운다 *)
let test_mode_set_rewrites () =
  let m = run (int10 0x1201 ~bx:0x34 ^ int10 0x0003 ^ exit_0) in
  checki "34h 끔 뒤 모드 세우기: 0x487" (bda m ega_misc) 0x60;
  let m = run (int10 0x1201 ~bx:0x34 ^ exit_0) in
  checki "34h 끔: 0x487 bit 0" (bda m ega_misc) 0x61;
  let m = run (int10 0x1200 ~bx:0x30 ^ exit_0) in
  checki "30h 200 줄: 0x488" (bda m switches) 0xF8;
  checki "30h 200 줄: 0x489" (bda m vga_flags) 0xC1;
  let m = run (int10 0x1200 ~bx:0x30 ^ int10 0x0003 ^ exit_0) in
  checki "30h 200 줄 뒤 모드 세우기: 0x488" (bda m switches) 0xF9;
  checki "30h 200 줄 뒤 모드 세우기: 0x489" (bda m vga_flags) 0x41;
  let m = run ~before:(poke vga_flags 0xFF) (int10 0x0003 ^ exit_0) in
  checki "0x489 = FFh 뒤 모드 세우기" (bda m vga_flags) 0x7F;
  let m = run ~before:(poke switches 0x00) (int10 0x0003 ^ exit_0) in
  checki "0x488 = 0 뒤 모드 세우기" (bda m switches) 0xF9

(* BL=10h 는 BH 에 들어온 값과 상관없이 BH 를 쓴다(RBIL: BH=FFh 로 불러
   EGA 가 있는지 본다) *)
let ega_info = int10 0x1200 ~bx:0xFF10 ~cx:0xFFFF ^ store_bx out ^ store_cx (out + 2)

let test_ega_info () =
  let m = run (ega_info ^ exit_0) in
  checki "BL=10h BL" (rd m 0) 3;
  checki "BL=10h BH 컬러" (rd m 1) 0;
  checki "BL=10h CL 스위치" (rd m 2) 0x09;
  checki "BL=10h CH 연결 단자" (rd m 3) 0x0F;
  let m = run ~before:(poke switches 0x5A) (ega_info ^ exit_0) in
  checki "0x488 = 5Ah: CL" (rd m 2) 0x0A;
  checki "0x488 = 5Ah: CH" (rd m 3) 0x05;
  let m =
    run ~before:(fun m -> poke crtc_port 0xB4 m; poke (crtc_port + 1) 0x03 m) (ega_info ^ exit_0)
  in
  checki "CRTC 3B4h: BH 흑백" (rd m 1) 1;
  let m =
    run ~before:(fun m -> poke crtc_port 0xB4 m; poke (crtc_port + 1) 0x02 m) (ega_info ^ exit_0)
  in
  checki "CRTC 2B4h(아래 바이트만 같음): BH" (rd m 1) 0

let () =
  test_boot ();
  test_mode_set ();
  test_mode_set_rewrites ();
  test_ega_info ();
  if !failed = 0 then print_endline "dos vga bda: all passed"
  else begin
    Printf.eprintf "dos vga bda: %d failures\n%!" !failed;
    exit 1
  end
