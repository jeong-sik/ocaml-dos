(* INT 10h AH=1Bh BX=0 — 기능·상태 정보 64 바이트(RBIL 표 00040)와 그
   앞머리가 가리키는 정적 기능 표 16 바이트(표 00045). *)

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
let es_is_cs = "\x0e\x07"
let es_is seg = "\xb8" ^ w16 seg ^ "\x8e\xc0"
let di_is off = "\xbf" ^ w16 off
let store_al a = "\xa2" ^ w16 a
let store_bx a = "\x89\x1e" ^ w16 a
let exit_0 = "\xb8\x00\x4c\xcd\x21"

let psp = 0x10000
let code_top = 0x280
let al_at = 0x2F0
let buf = 0x300
let state_size = 64
let fill = 0xEE

(* AL 은 입력이 아니다. 0 이 아닌 값을 넣어 돌아오는 AL 을 가른다. *)
let state_info ?(bx = 0) () = es_is_cs ^ di_is buf ^ int10 0x1B55 ~bx ^ store_al al_at

let run ?(data = []) ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= code_top);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  List.iter (fun (addr, bytes) ->
      List.iteri (fun i v -> Dos_machine.mem_write m (addr + i) v) bytes) data;
  before m;
  Dos_machine.run m ~max_steps:50_000;
  check "program exited" (Dos_machine.exited m);
  m

(* 바깥 한 바이트까지 fill 로 덮어 둔 버퍼 *)
let filled = [ (psp + buf - 1, List.init (state_size + 2) (fun _ -> fill)) ]
let rd m i = Dos_machine.mem_read m (psp + buf + i)
let rd16 m i = rd m i lor (rd m (i + 1) lsl 8)
let al m = Dos_machine.mem_read m (psp + al_at)

let video_rom = 0xC0000
let static_off = 0x0100
let static_table =
  [ 0xFF; 0xE0; 0x0F; 0; 0; 0; 0; 0x07; 1; 1; 0x7A; 0x08; 0; 0; 0; 0 ]

let check_static name m =
  List.iteri (fun i want ->
      checki (Printf.sprintf "%s: 정적 표 %02Xh" name i)
        (Dos_machine.mem_read m (video_rom + static_off + i)) want)
    static_table

let test_static_table () =
  let m = run ~data:filled (state_info () ^ exit_0) in
  checki "AL = 1Bh" (al m) 0x1B;
  checki "00h 오프셋" (rd16 m 0) static_off;
  checki "02h 세그먼트" (rd16 m 2) (video_rom lsr 4);
  check_static "부팅 뒤" m;
  let m = run (int10 0x0013 ^ int10 0x0003 ^ exit_0) in
  check_static "모드 세우기 뒤" m

(* 모드 3, 커서 5행 18열. 앞 30 바이트는 BDA 0x449-0x466 사본이다. *)
let test_mode3 () =
  let m =
    run ~data:filled
      ~before:(fun m -> Dos_machine.mem_write m 0x466 0x5A)
      (int10 0x0200 ~dx:0x0512 ^ state_info () ^ exit_0)
  in
  checki "AL = 1Bh" (al m) 0x1B;
  for i = 0 to 29 do
    checki (Printf.sprintf "BDA %03Xh 사본" (0x449 + i)) (rd m (0x04 + i))
      (Dos_machine.mem_read m (0x449 + i))
  done;
  checki "04h 모드" (rd m 0x04) 3;
  checki "05h 열" (rd16 m 0x05) 80;
  checki "07h 쪽 크기" (rd16 m 0x07) 0x1000;
  checki "0Bh 0쪽 커서" (rd16 m 0x0B) 0x0512;
  checki "1Bh 커서 모양" (rd16 m 0x1B) 0x0607;
  checki "1Eh CRTC" (rd16 m 0x1E) 0x3D4;
  checki "21h 3x9(마지막 사본)" (rd m 0x21) 0x5A;
  checki "22h 행 - 1" (rd m 0x22) 24;
  checki "23h 글자 높이" (rd16 m 0x23) 8;
  checki "25h DCC" (rd m 0x25) 0x08;
  checki "26h 다른 DCC" (rd m 0x26) 0x00;
  checki "27h 색" (rd16 m 0x27) 16;
  checki "29h 쪽" (rd m 0x29) 8;
  checki "2Ah 400 줄" (rd m 0x2A) 2;
  checki "2Dh 깃발(커서 흉내·깜박임)" (rd m 0x2D) 0x30;
  checki "31h 256K" (rd m 0x31) 3;
  List.iter (fun i -> checki (Printf.sprintf "%02Xh 는 0" i) (rd m i) 0)
    ([ 0x2B; 0x2C; 0x2E; 0x2F; 0x30 ] @ List.init 14 (fun k -> 0x32 + k));
  checki "버퍼 앞은 그대로" (rd m (-1)) fill;
  checki "버퍼 뒤는 그대로" (rd m state_size) fill

(* 글자 높이의 윗 바이트까지 세 바이트를 옮긴다 *)
let test_rows_copy () =
  let m =
    run ~data:filled
      ~before:(fun m -> Dos_machine.mem_write m 0x484 0x31; Dos_machine.mem_write m 0x486 0x01)
      (state_info () ^ exit_0)
  in
  checki "22h = 0x484" (rd m 0x22) 0x31;
  checki "24h = 0x486" (rd m 0x24) 0x01

(* AX=1A00h 는 25h 와 같은 DCC 를 BL 에, 다른 화면 00h 를 BH 에 낸다 *)
let test_dcc () =
  let bx_at = al_at + 2 in
  let m = run (int10 0x1A00 ~bx:0xFFFF ^ store_al al_at ^ store_bx bx_at ^ exit_0) in
  checki "1A00h AL" (al m) 0x1A;
  checki "1A00h BL" (Dos_machine.mem_read m (psp + bx_at)) 0x08;
  checki "1A00h BH" (Dos_machine.mem_read m (psp + bx_at + 1)) 0x00

let test_other_bx () =
  let m = run ~data:filled (state_info ~bx:1 () ^ exit_0) in
  checki "BX=1: AL = 0" (al m) 0;
  for i = 0 to state_size - 1 do
    checki (Printf.sprintf "BX=1: %02Xh 그대로" i) (rd m i) fill
  done

(* 모드마다 (색, 쪽, 주사선, 깃발 bit 5). 쪽 수는 DOSBox-staging
   ModeList_VGA 의 ptotal 이다. *)
let shapes =
  [ (0x01, 16, 8, 2, true); (0x03, 16, 8, 2, true); (0x07, 0, 8, 2, true);
    (0x04, 4, 1, 0, false); (0x05, 4, 1, 0, false); (0x06, 2, 1, 0, false);
    (0x0D, 16, 8, 0, false); (0x0E, 16, 4, 0, false); (0x0F, 0, 2, 1, false);
    (0x10, 16, 2, 1, false); (0x11, 2, 1, 3, false); (0x12, 16, 1, 3, false);
    (0x13, 256, 1, 0, false) ]

let test_modes () =
  List.iter (fun (mode, colors, pages, scan, blink) ->
      let m = run ~data:filled (int10 mode ^ state_info () ^ exit_0) in
      let name what = Printf.sprintf "모드 %02Xh %s" mode what in
      checki (name "04h") (rd m 0x04) mode;
      checki (name "색") (rd16 m 0x27) colors;
      checki (name "쪽") (rd m 0x29) pages;
      checki (name "주사선") (rd m 0x2A) scan;
      checki (name "깜박임") (rd m 0x2D land 0x20) (if blink then 0x20 else 0))
    shapes

let test_misc () =
  let misc ~before = rd (run ~data:filled ~before (state_info () ^ exit_0)) 0x2D in
  let poke a v m = Dos_machine.mem_write m a v in
  checki "0x489 = FFh, 0x487 = FEh" (misc ~before:(fun m -> poke 0x489 0xFF m; poke 0x487 0xFE m)) 0x3E;
  checki "0x489 = F1h" (misc ~before:(poke 0x489 0xF1)) 0x30;
  checki "0x487 bit 0 = 1: 커서 흉내 끔" (misc ~before:(poke 0x487 0x01)) 0x20;
  checki "흑백 비트" (misc ~before:(poke 0x489 0x04)) 0x34;
  (* BIOS 호출로 바꾼 깃발 *)
  let m =
    run ~data:filled
      (int10 0x1200 ~bx:0x33 ^ int10 0x1201 ~bx:0x31 ^ int10 0x1201 ~bx:0x34
       ^ state_info () ^ exit_0)
  in
  checki "12h 33h·31h·34h 뒤" (rd m 0x2D) 0x2A

(* ES=2000h, DI=FFF0h: 64 바이트가 세그먼트 안에서 돈다 *)
let test_wrap () =
  let wrap_off = 0xFFF0 in
  let below = 0x20000 and above = 0x30000 in
  let m =
    run ~data:[ (above, List.init state_size (fun _ -> fill)) ]
      (es_is 0x2000 ^ di_is wrap_off ^ int10 0x1B00 ^ exit_0)
  in
  checki "2000:FFF0 오프셋" (Dos_machine.mem_read m (below + wrap_off)) (static_off land 0xff);
  checki "2000:FFF1 오프셋" (Dos_machine.mem_read m (below + wrap_off + 1)) (static_off lsr 8);
  checki "2000:0015 = 25h DCC" (Dos_machine.mem_read m (below + 0x25 - 0x10)) 0x08;
  checki "2000:0021 = 31h 256K" (Dos_machine.mem_read m (below + 0x31 - 0x10)) 3;
  for i = 0 to state_size - 1 do
    checki (Printf.sprintf "3000:%04X 그대로" i) (Dos_machine.mem_read m (above + i)) fill
  done

let () =
  test_static_table ();
  test_mode3 ();
  test_rows_copy ();
  test_dcc ();
  test_other_bx ();
  test_modes ();
  test_misc ();
  test_wrap ();
  if !failed = 0 then print_endline "dos vga state: all passed"
  else begin
    Printf.eprintf "dos vga state: %d failures\n%!" !failed;
    exit 1
  end
