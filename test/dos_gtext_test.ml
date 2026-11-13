(* 그래픽 모드의 BIOS 글자 — INT 10h AH=09h·0Ah·0Eh·06h·07h·0Ch·11h 와
   DOS 출력.

   계약: 그래픽 모드에서 글자는 글꼴 표의 점으로 화면에 찍힌다. 글꼴은
   게스트 메모리에서 INT 43h(CGA 모드의 128 이상은 INT 1Fh)로 찾는다.
   AH=09h 는 BL 색, bit7 이면 XOR, 256색 모드의 바탕은 BH 다. AH=0Ah 는
   모드의 기본색을 쓴다. 480줄 모드는 8x16 글자 30행이다. 스크롤은 점을
   옮기고 빈 행을 채운다. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

let cpu m = m.Dos_state.cpu

let int10 m ~ax ?(bx = 0) ?(cx = 0) ?(dx = 0) () =
  let c = cpu m in
  Cpu86.set_reg16 c 0 ax;
  Cpu86.set_reg16 c 3 bx;
  Cpu86.set_reg16 c 1 cx;
  Cpu86.set_reg16 c 2 dx;
  ignore (Dos_bios.service m 0x10)

let machine mode =
  let m = Dos_machine.create () in
  int10 m ~ax:mode ();
  m

let locate m ~col ~row = int10 m ~ax:0x0200 ~dx:((row lsl 8) lor col) ()
let pixel m ~x ~y = Dos_video.get_pixel m.Dos_state.video ~x ~y
let plot m ~x ~y color = int10 m ~ax:(0x0C00 lor color) ~cx:x ~dx:y ()
let bda8 m a = Bytes.get_uint8 m.Dos_state.mem a
let bda16 m a = Bytes.get_uint16_le m.Dos_state.mem a

let on glyph_row dot = glyph_row land (0x80 lsr dot) <> 0

(* 글자 칸 하나를 기대와 견준다. [row_of line] 은 8x8 글리프의 몇째 줄이
   그 점 줄인지다. 다른 점이 처음 나온 자리를 알린다. *)
let check_cell name m ~x0 ~y0 ~height ~row_of ~glyph ~fg ~bg =
  let bad = ref None in
  for line = height - 1 downto 0 do
    for dot = 7 downto 0 do
      let want = if on glyph.(row_of line) dot then fg else bg in
      let got = pixel m ~x:(x0 + dot) ~y:(y0 + line) in
      if got <> want then bad := Some (dot, line, got, want)
    done
  done;
  match !bad with
  | None -> ()
  | Some (dot, line, got, want) ->
    check (Printf.sprintf "%s: dot %d line %d is %X, want %X" name dot line got want) false

let a_glyph = Font8x8.glyph (Char.code 'A')
let same line = line

(* ---------- 256색: AH=09h 의 색과 BH 바탕 ---------- *)

let test_vga256_write_char () =
  let m = machine 0x13 in
  locate m ~col:2 ~row:1;
  int10 m ~ax:(0x0900 lor Char.code 'A') ~bx:0x010E ~cx:2 ();
  for k = 0 to 1 do
    check_cell (Printf.sprintf "13h copy %d" k) m ~x0:((2 + k) * 8) ~y0:8 ~height:8
      ~row_of:same ~glyph:a_glyph ~fg:0x0E ~bg:0x01
  done;
  check "13h cursor stays" (bda8 m 0x450 = 2 && bda8 m 0x451 = 1);
  (* AH=0Ah 는 BL 을 보지 않고 흰색(0Fh)으로, 바탕은 BH 로 그린다. *)
  locate m ~col:0 ~row:3;
  int10 m ~ax:(0x0A00 lor Char.code 'A') ~bx:0x0522 ~cx:1 ();
  check_cell "13h AH=0Ah" m ~x0:0 ~y0:24 ~height:8 ~row_of:same ~glyph:a_glyph ~fg:0x0F
    ~bg:0x05;
  (* teletype 의 BL 은 256색 전부다. *)
  locate m ~col:0 ~row:5;
  int10 m ~ax:(0x0E00 lor Char.code 'A') ~bx:0x002C ();
  check_cell "13h teletype color above 0Fh" m ~x0:0 ~y0:40 ~height:8 ~row_of:same
    ~glyph:a_glyph ~fg:0x2C ~bg:0

(* ---------- 480줄: 30행, 8x16, teletype 과 스크롤 ---------- *)

let test_tall_mode_teletype () =
  let m = machine 0x12 in
  check_int "12h rows - 1" (bda8 m 0x484) 29;
  check_int "12h char height" (bda16 m 0x485) 16;
  plot m ~x:3 ~y:(16 + 5) 9;
  locate m ~col:0 ~row:29;
  check "12h cursor reaches row 29" (bda8 m 0x451 = 29);
  int10 m ~ax:(0x0E00 lor Char.code 'A') ~bx:0x000C ();
  check_cell "12h teletype" m ~x0:0 ~y0:(29 * 16) ~height:16 ~row_of:(fun l -> l / 2)
    ~glyph:a_glyph ~fg:0x0C ~bg:0;
  check "12h teletype moves the cursor" (bda8 m 0x450 = 1 && bda8 m 0x451 = 29);
  int10 m ~ax:0x0E0D ~bx:0x000C ();
  int10 m ~ax:0x0E0A ~bx:0x000C ();
  check_int "12h scroll lifts row 1 into row 0" (pixel m ~x:3 ~y:5) 9;
  check_cell "12h scroll lifts the last row" m ~x0:0 ~y0:(28 * 16) ~height:16
    ~row_of:(fun l -> l / 2) ~glyph:a_glyph ~fg:0x0C ~bg:0;
  check_cell "12h new last row is black" m ~x0:0 ~y0:(29 * 16) ~height:16 ~row_of:same
    ~glyph:(Array.make 16 0) ~fg:0 ~bg:0

(* ---------- 16색 평면: XOR 과 바탕 ---------- *)

let test_planar_xor () =
  let m = machine 0x0D in
  let draw bl = int10 m ~ax:(0x0900 lor Char.code 'A') ~bx:bl ~cx:1 () in
  (* 'A' 의 첫 줄 맨 왼쪽 점은 꺼져 있다 — 바탕 자리다. 그 점을 3 으로
     칠해 두고 바탕이 남는지 본 뒤 0 으로 되돌려 칸 전체를 견준다. *)
  check "A's top-left dot is background" (not (on a_glyph.(0) 0));
  plot m ~x:0 ~y:0 3;
  draw 0x8F;
  check_int "XOR keeps the background" (pixel m ~x:0 ~y:0) 3;
  plot m ~x:0 ~y:0 0;
  check_cell "XOR draws the glyph" m ~x0:0 ~y0:0 ~height:8 ~row_of:same ~glyph:a_glyph
    ~fg:0x0F ~bg:0;
  plot m ~x:0 ~y:0 3;
  draw 0x8F;
  check_int "second XOR keeps the background" (pixel m ~x:0 ~y:0) 3;
  plot m ~x:0 ~y:0 0;
  check_cell "second XOR erases the glyph" m ~x0:0 ~y0:0 ~height:8 ~row_of:same
    ~glyph:a_glyph ~fg:0 ~bg:0;
  plot m ~x:0 ~y:0 3;
  draw 0x0F;
  check_int "plain write clears the background" (pixel m ~x:0 ~y:0) 0;
  check_cell "plain write" m ~x0:0 ~y0:0 ~height:8 ~row_of:same ~glyph:a_glyph ~fg:0x0F
    ~bg:0

(* ---------- AH=0Ch 의 XOR ---------- *)

let test_pixel_xor () =
  List.iter
    (fun (mode, first, xor, want) ->
      let m = machine mode in
      plot m ~x:10 ~y:10 first;
      plot m ~x:10 ~y:10 xor;
      check_int (Printf.sprintf "mode %Xh AH=0Ch %02Xh then %02Xh" mode first xor)
        (pixel m ~x:10 ~y:10) want)
    [ (0x04, 1, 0x83, 2); (0x06, 1, 0x81, 0); (0x0D, 1, 0x83, 2); (0x12, 5, 0x8F, 0x0A);
      (0x13, 1, 0x83, 0x83) ]

(* ---------- 글꼴은 게스트 메모리의 벡터에서 ---------- *)

let set_vector m v ~seg ~off =
  Bytes.set_uint16_le m.Dos_state.mem (v * 4) off;
  Bytes.set_uint16_le m.Dos_state.mem ((v * 4) + 2) seg

let test_fonts_from_vectors () =
  let m = machine 0x04 in
  (* 128 이상은 INT 1Fh 표: 0060:0000 의 첫 글자(80h). *)
  let high = [| 0xFF; 0; 0; 0; 0; 0; 0; 0x81 |] in
  Array.iteri (fun i b -> Bytes.set_uint8 m.Dos_state.mem (0x600 + i) b) high;
  set_vector m 0x1F ~seg:0x60 ~off:0;
  int10 m ~ax:0x0980 ~bx:0x0003 ~cx:1 ();
  check_cell "CGA char 80h from INT 1Fh" m ~x0:0 ~y0:0 ~height:8 ~row_of:same ~glyph:high
    ~fg:3 ~bg:0;
  (* 128 미만은 INT 43h 표. *)
  let custom = [| 0xF0; 0x0F; 0xF0; 0x0F; 0xF0; 0x0F; 0xF0; 0x0F |] in
  Array.iteri
    (fun i b -> Bytes.set_uint8 m.Dos_state.mem (0x700 + (Char.code 'A' * 8) + i) b)
    custom;
  set_vector m 0x43 ~seg:0x70 ~off:0;
  locate m ~col:1 ~row:0;
  int10 m ~ax:(0x0900 lor Char.code 'A') ~bx:0x0002 ~cx:1 ();
  check_cell "CGA 'A' from INT 43h" m ~x0:8 ~y0:0 ~height:8 ~row_of:same ~glyph:custom
    ~fg:2 ~bg:0;
  (* 모드를 다시 세우면 INT 43h 가 ROM 표로 돌아온다. *)
  int10 m ~ax:0x13 ();
  int10 m ~ax:(0x0900 lor Char.code 'A') ~bx:0x0007 ~cx:1 ();
  check_cell "mode set restores INT 43h" m ~x0:0 ~y0:0 ~height:8 ~row_of:same
    ~glyph:a_glyph ~fg:7 ~bg:0

(* ---------- AX=1130h ---------- *)

let font_info m bh =
  let c = cpu m in
  Cpu86.set_seg c 0 0x1111;
  Cpu86.set_reg16 c 5 0x2222;
  int10 m ~ax:0x1130 ~bx:(bh lsl 8) ();
  (Cpu86.seg c 0, Cpu86.reg16 c 5, Cpu86.reg16 c 1, Cpu86.reg8 c 2)

let rom_byte m ~seg ~off = Bytes.get_uint8 m.Dos_state.mem ((seg * 16) + off)

let test_font_info () =
  let m = Dos_machine.create () in
  check_int "text char height" (bda16 m 0x485) 16;
  let seg, off, cx, dl = font_info m 3 in
  check "BH=3 is the ROM 8x8 at F000:FA6E" (seg = 0xF000 && off = 0xFA6E);
  check_int "CX is the screen font height" cx 16;
  check_int "DL is rows - 1" dl 24;
  let seg, off, _, _ = font_info m 6 in
  let ok = ref true in
  for line = 0 to 15 do
    if rom_byte m ~seg ~off:(off + (Char.code 'A' * 16) + line) <> a_glyph.(line / 2) then
      ok := false
  done;
  check "BH=6 table holds 'A' doubled" !ok;
  let seg14, off14, _, _ = font_info m 2 in
  let ok = ref true in
  for line = 0 to 13 do
    if rom_byte m ~seg:seg14 ~off:(off14 + (Char.code 'A' * 14) + line)
       <> a_glyph.(line * 8 / 14)
    then ok := false
  done;
  check "BH=2 table holds 'A' stretched to 14" !ok;
  let s1, o1, _, _ = font_info m 1 in
  check "BH=1 is INT 43h, the 8x16 table in text mode" (s1 = seg && o1 = off);
  let s0, o0, _, _ = font_info m 0 in
  let s4, o4, _, _ = font_info m 4 in
  check "BH=0 is INT 1Fh, the upper half of the 8x8 table" (s0 = s4 && o0 = o4);
  let s5, o5, _, _ = font_info m 5 in
  check_int "BH=5 alternate list is empty" (rom_byte m ~seg:s5 ~off:o5) 0;
  let s8, o8, _, _ = font_info m 8 in
  check "BH=8 leaves ES:BP" (s8 = 0x1111 && o8 = 0x2222);
  int10 m ~ax:0x0D ();
  let _, _, cx, _ = font_info m 1 in
  check_int "mode 0Dh font height" cx 8

(* ---------- DOS 출력 ---------- *)

let test_dos_output () =
  let m = machine 0x13 in
  (* mov ah,2; mov dl,'A'; int 21h; int 20h *)
  Dos_machine.load_com m "\xB4\x02\xB2\x41\xCD\x21\xCD\x20";
  Dos_machine.run m ~max_steps:10_000;
  check_cell "INT 21h AH=02h in mode 13h" m ~x0:0 ~y0:0 ~height:8 ~row_of:same ~glyph:a_glyph
    ~fg:7 ~bg:0

(* ---------- AH=06h·07h ---------- *)

let test_scroll () =
  let m = machine 0x13 in
  plot m ~x:9 ~y:11 0x11;
  plot m ~x:16 ~y:8 0x33;
  (* 0..1 행, 0..1 열 창을 한 행 올리고 2Ah 로 채운다. *)
  int10 m ~ax:0x0601 ~bx:0x2A00 ~cx:0x0000 ~dx:0x0101 ();
  check_int "scroll up lifts the dot" (pixel m ~x:9 ~y:3) 0x11;
  check_int "scroll up fills the new row" (pixel m ~x:9 ~y:11) 0x2A;
  check_int "scroll leaves dots outside the window" (pixel m ~x:16 ~y:8) 0x33;
  int10 m ~ax:0x0701 ~bx:0x0000 ~cx:0x0000 ~dx:0x0101 ();
  check_int "scroll down brings it back" (pixel m ~x:9 ~y:11) 0x11;
  check_int "scroll down fills the top row" (pixel m ~x:9 ~y:3) 0x00;
  int10 m ~ax:0x0600 ~bx:0x0500 ~cx:0x0000 ~dx:0x0101 ();
  check_int "AL=0 clears the window" (pixel m ~x:9 ~y:11) 0x05;
  (* 16색 모드에서 채우는 색의 bit7 은 XOR 이 아니다. *)
  let p = machine 0x0D in
  plot p ~x:9 ~y:11 3;
  int10 p ~ax:0x0600 ~bx:0x8500 ~cx:0x0000 ~dx:0x0101 ();
  check_int "planar fill does not XOR" (pixel p ~x:9 ~y:11) 0x05

let () =
  test_vga256_write_char ();
  test_tall_mode_teletype ();
  test_planar_xor ();
  test_pixel_xor ();
  test_fonts_from_vectors ();
  test_font_info ();
  test_dos_output ();
  test_scroll ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos graphics text: all passed\n"
