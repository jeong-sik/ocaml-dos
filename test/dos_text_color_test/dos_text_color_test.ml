(* 텍스트 모드 색 — 속성 팔레트와 DAC 를 지나고, 속성 컨트롤러 10h 의
   깜박임 비트(INT 10h AX=1003h)가 바탕을 여덟 색과 열여섯 색 사이에서
   바꾼다. 1001h·1002h·1007h·1008h·1009h 는 SeaBIOS vgasrc/vgabios.c 와
   RBIL INT 10/AX=10xxh 를 따른다. *)

open Dos_state

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got 0x%x want 0x%x\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let b8 n = String.make 1 (Char.chr (n land 0xff))
let int10 ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x10"
let es_is_cs = "\x0e\x07"  (* push cs; pop es *)
let store_bh a = "\x88\x3e" ^ w16 a  (* mov [a],bh *)
let exit_0 = "\xb8\x00\x4c\xcd\x21"

(* 3DAh 를 읽어 3C0h 를 index 차례로 돌린 뒤 레지스터 [idx] 를 읽어 [a] 에 *)
let read_attr_reg idx a =
  String.concat ""
    [ "\xba" ^ w16 0x3DA; "\xec";
      "\xba" ^ w16 0x3C0; "\xb0" ^ b8 idx; "\xee";
      "\x42"; "\xec"; "\xa2" ^ w16 a ]

let psp = 0x10000
let out = 0x300
let table = 0x280
let dump = 0x2C0

let mode_control = 0x10
let overscan = 0x11

let peek m off = Dos_machine.mem_read m (psp + off)

let run_program ?(data = []) code =
  assert (0x100 + String.length code <= table);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  List.iter (fun (off, bytes) ->
      List.iteri (fun i v -> Dos_machine.mem_write m (psp + off + i) v) bytes) data;
  Dos_machine.run m ~max_steps:20_000;
  check "program exited" (Dos_machine.exited m);
  m

(* ---- 화면 ---- *)

(* Font8x8 은 ASCII 만 가진다. 글자색은 '#' 의 켜진 점에서 본다. *)
let solid = Char.code '#'
let blank = 0x20
let cols = 80
let cell_w = 8
let cell_h = 16
let vram = 0xB8000

let put_cell m k ~ch ~attr =
  Dos_machine.mem_write m (vram + (k * 2)) ch;
  Dos_machine.mem_write m (vram + (k * 2) + 1) attr

let pixel_at frame k ~dx ~dy =
  let x = ((k mod cols) * cell_w) + dx and y = ((k / cols) * cell_h) + dy in
  let i = ((y * cols * cell_w) + x) * 3 in
  (Char.code frame.[i], Char.code frame.[i + 1], Char.code frame.[i + 2])

(* 빈칸의 왼쪽 위 점은 바탕색이다 *)
let pixel frame k = pixel_at frame k ~dx:0 ~dy:0

(* '#' 의 첫 켜진 점(글자는 세로로 두 배라 행 하나가 두 줄) *)
let lit =
  let g = Font8x8.glyph solid in
  let gy = ref 0 in
  while g.(!gy) = 0 do incr gy done;
  let gx = ref 0 in
  while g.(!gy) land (0x80 lsr !gx) = 0 do incr gx done;
  (!gx, !gy * (cell_h / Array.length g))

let ink frame k = let dx, dy = lit in pixel_at frame k ~dx ~dy

(* 속성 a 마다 칸 2a 는 꽉 찬 글자(글자색), 2a+1 은 빈칸(바탕색) *)
let paint_all_attrs m =
  for a = 0 to 0xff do
    put_cell m (2 * a) ~ch:solid ~attr:a;
    put_cell m ((2 * a) + 1) ~ch:blank ~attr:a
  done

let test_font_assumption () =
  check "'#' 에는 켜진 점이 있다"
    (Array.exists (fun row -> row <> 0) (Font8x8.glyph solid));
  check "빈칸은 모든 점이 꺼져 있다"
    (Array.for_all (fun row -> row = 0) (Font8x8.glyph blank))

(* 팔레트를 안 건드린 화면은 예전 고정 CGA 표와 같은 색이다 *)
let test_default_matches_cga () =
  let m = Dos_machine.create () in
  paint_all_attrs m;
  let f = Dos_machine.frame_rgb m in
  let bad = ref 0 in
  for a = 0 to 0xff do
    if ink f (2 * a) <> Dos_ports.cga_palette.(a land 0x0f) then incr bad;
    if pixel f ((2 * a) + 1) <> Dos_ports.cga_palette.((a lsr 4) land 0x07) then incr bad
  done;
  checki "부팅 화면: 256 속성의 글자색·바탕색이 CGA 표와 같다" !bad 0

(* 1003h BL=0 이면 bit 7 이 바탕을 밝힌다 *)
let test_blink_off () =
  let m = run_program (int10 0x1003 ~bx:0 ^ exit_0) in
  paint_all_attrs m;
  let f = Dos_machine.frame_rgb m in
  let bad = ref 0 in
  for a = 0 to 0xff do
    if ink f (2 * a) <> Dos_ports.cga_palette.(a land 0x0f) then incr bad;
    if pixel f ((2 * a) + 1) <> Dos_ports.cga_palette.((a lsr 4) land 0x0f) then incr bad
  done;
  checki "깜박임 끔: 바탕이 열여섯 색" !bad 0;
  check "깜박임 끔: 9Eh 의 바탕은 밝은 파랑"
    (pixel f ((2 * 0x9E) + 1) = (0x55, 0x55, 0xFF))

(* 10h 의 값: 부팅·1003h·모드 세우기 *)
let test_mode_control () =
  let code =
    String.concat ""
      [ read_attr_reg mode_control out;
        int10 0x1003 ~bx:0; read_attr_reg mode_control (out + 1);
        int10 0x1003 ~bx:1; read_attr_reg mode_control (out + 2);
        int10 0x1003 ~bx:2; read_attr_reg mode_control (out + 3);
        int10 0x1003 ~bx:3; read_attr_reg mode_control (out + 4);
        int10 0x1003 ~bx:0; int10 0x0003; read_attr_reg mode_control (out + 5);
        int10 0x0013; read_attr_reg mode_control (out + 6);
        int10 0x0012; read_attr_reg mode_control (out + 7);
        int10 0x0007; read_attr_reg mode_control (out + 8);
        int10 0x0004; read_attr_reg mode_control (out + 9);
        int10 0x0003;
        exit_0 ]
  in
  let m = run_program code in
  checki "부팅 모드 3: 0Ch" (peek m out) 0x0C;
  checki "1003h BL=0: bit 3 만 지운다" (peek m (out + 1)) 0x04;
  checki "1003h BL=1: 다시 켠다" (peek m (out + 2)) 0x0C;
  checki "1003h BL=2: bit 0 만 본다(끔)" (peek m (out + 3)) 0x04;
  checki "1003h BL=3: bit 0 만 본다(켬)" (peek m (out + 4)) 0x0C;
  checki "모드 3 을 다시 세우면 켜진다" (peek m (out + 5)) 0x0C;
  checki "모드 13h: 41h" (peek m (out + 6)) 0x41;
  checki "모드 12h: 01h" (peek m (out + 7)) 0x01;
  checki "모드 7: 0Eh" (peek m (out + 8)) 0x0E;
  checki "모드 4: 01h" (peek m (out + 9)) 0x01

(* 글자색·바탕색이 속성 팔레트와 DAC 를 지난다 *)
let test_palette_and_dac () =
  let code =
    String.concat ""
      [ int10 0x1010 ~bx:1 ~dx:(63 lsl 8) ~cx:0;  (* DAC 1 = 빨강 *)
        int10 0x1000 ~bx:(1 lsl 8 lor 2);       (* 색 2 → DAC 1 *)
        exit_0 ]
  in
  let m = run_program code in
  put_cell m 0 ~ch:solid ~attr:0x01;
  put_cell m 1 ~ch:solid ~attr:0x02;
  put_cell m 2 ~ch:blank ~attr:0x10;
  put_cell m 3 ~ch:blank ~attr:0x20;
  put_cell m 4 ~ch:solid ~attr:0x03;
  let f = Dos_machine.frame_rgb m in
  check "글자색 1 은 DAC 1" (ink f 0 = (0xFF, 0, 0));
  check "글자색 2 는 팔레트를 지나 DAC 1" (ink f 1 = (0xFF, 0, 0));
  check "바탕색 1 은 DAC 1" (pixel f 2 = (0xFF, 0, 0));
  check "바탕색 2 는 팔레트를 지나 DAC 1" (pixel f 3 = (0xFF, 0, 0));
  check "안 바꾼 색 3 은 그대로" (ink f 4 = Dos_ports.cga_palette.(3))

(* 1002h 의 열일곱째 바이트는 테두리(11h)다. 10h 를 건드리지 않는다. *)
let test_all_palette () =
  let regs = List.init 16 (fun i -> 15 - i) @ [ 0x05 ] in
  let code =
    String.concat ""
      [ es_is_cs; int10 0x1002 ~dx:table;
        int10 0x1007 ~bx:mode_control; store_bh out;
        int10 0x1008; store_bh (out + 1);
        int10 0x1007 ~bx:0; store_bh (out + 2);
        es_is_cs; int10 0x1009 ~dx:dump;
        exit_0 ]
  in
  (* 1009h 가 쓰지 않은 바이트가 드러나게 덤프 자리를 미리 채운다 *)
  let unwritten = List.init (List.length regs) (fun _ -> 0xEE) in
  let m = run_program ~data:[ (table, regs); (dump, unwritten) ] code in
  checki "1002h 뒤에도 10h 는 0Ch" (peek m out) 0x0C;
  checki "1002h 가 테두리를 세운다(1008h)" (peek m (out + 1)) 0x05;
  checki "1002h 색 0 은 첫 바이트" (peek m (out + 2)) 15;
  checki "1009h 첫 바이트" (peek m dump) 15;
  checki "1009h 열여섯째 바이트" (peek m (dump + 15)) 0;
  checki "1009h 열일곱째 바이트는 테두리" (peek m (dump + 16)) 0x05;
  put_cell m 0 ~ch:solid ~attr:0x0F;
  put_cell m 1 ~ch:blank ~attr:0x80;
  let f = Dos_machine.frame_rgb m in
  check "색 15 → 팔레트 0 → 검정" (ink f 0 = (0, 0, 0));
  check "깜박임이 그대로라 80h 의 바탕은 색 0 → 팔레트 15 → 흰색"
    (pixel f 1 = (0xFF, 0xFF, 0xFF))

(* 1001h·1008h 와 포트, 1007h 의 범위 *)
let test_overscan_and_read () =
  let code =
    String.concat ""
      [ int10 0x1001 ~bx:(0x22 lsl 8); int10 0x1008; store_bh out;
        read_attr_reg overscan (out + 1);
        int10 0x1007 ~bx:(0x77 lsl 8 lor 0x15); store_bh (out + 2);
        int10 0x1007 ~bx:(0x77 lsl 8 lor 0xFF); store_bh (out + 3);
        int10 0x1007 ~bx:(0x77 lsl 8 lor 0x14); store_bh (out + 4);
        int10 0x1007 ~bx:(0x77 lsl 8 lor 0x11); store_bh (out + 5);
        exit_0 ]
  in
  let m = run_program code in
  checki "1001h → 1008h" (peek m out) 0x22;
  checki "1001h → 포트 11h" (peek m (out + 1)) 0x22;
  checki "1007h BL=15h: BH 를 둔다" (peek m (out + 2)) 0x77;
  checki "1007h BL=FFh: BH 를 둔다" (peek m (out + 3)) 0x77;
  checki "1007h BL=14h: 색 선택 0" (peek m (out + 4)) 0;
  checki "1007h BL=11h: 테두리" (peek m (out + 5)) 0x22

let save_exn m =
  match Dos_snapshot.save m with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let test_snapshot () =
  let m = run_program (int10 0x1003 ~bx:0 ^ int10 0x1001 ~bx:(0x09 lsl 8) ^ exit_0) in
  paint_all_attrs m;
  let saved = save_exn m in
  match Dos_snapshot.restore saved with
  | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
  | Ok r ->
    check "깜박임 끔이 돌아온다" (not (Dos_video.text_blink r.video));
    checki "테두리가 돌아온다" (Dos_video.overscan r.video) 0x09;
    check "같은 그림" (Dos_machine.frame_rgb r = Dos_machine.frame_rgb m)

let () =
  test_font_assumption ();
  test_default_matches_cga ();
  test_blink_off ();
  test_mode_control ();
  test_palette_and_dac ();
  test_all_palette ();
  test_overscan_and_read ();
  test_snapshot ();
  if !failed = 0 then print_endline "dos text color: all passed"
  else begin
    Printf.eprintf "dos text color: %d failures\n%!" !failed;
    exit 1
  end
