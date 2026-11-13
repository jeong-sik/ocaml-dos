(* 텍스트 모드의 teletype 과 DOS 출력 — 칸의 속성을 둔다.

   계약: INT 10h AH=0Eh 와 DOS 출력은 텍스트 모드에서 글자만 바꾸고 칸의
   속성을 그대로 둔다. 화면이 넘쳐 올라가면 새 줄은 커서 칸의 속성으로
   채운다. AH=09h 와 AH=13h 는 속성을 쓴다. DOS 줄 입력의 지우기도 속성을
   둔다. 부팅한 화면은 공백·속성 07h 다. *)

let failed = ref 0

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04X want %04X\n%!" name got want
  end

let cpu m = m.Dos_state.cpu

let int10 m ~ax ?(bx = 0) ?(cx = 0) ?(dx = 0) () =
  let c = cpu m in
  Cpu86.set_reg16 c 0 ax;
  Cpu86.set_reg16 c 3 bx;
  Cpu86.set_reg16 c 1 cx;
  Cpu86.set_reg16 c 2 dx;
  ignore (Dos_bios.service m 0x10)

let locate m ~col ~row = int10 m ~ax:0x0200 ~dx:((row lsl 8) lor col) ()
let cell m ~col ~row = Dos_state.read_cell m row col

(* 화면 전체를 [attr] 로 칠한다(AH=06h AL=0). *)
let paint m attr = int10 m ~ax:0x0600 ~bx:(attr lsl 8) ~cx:0 ~dx:0x184F ()

let blue = 0x1E
let red = 0x4F

let test_boot_screen () =
  let m = Dos_machine.create () in
  check_int "boot screen is blank grey" (cell m ~col:0 ~row:0) 0x0720;
  check_int "boot screen bottom right" (cell m ~col:79 ~row:24) 0x0720

let test_teletype_keeps_attribute () =
  let m = Dos_machine.create () in
  paint m blue;
  locate m ~col:0 ~row:0;
  (* BL=0: 속성을 쓴다면 검정 위 검정이 된다. *)
  int10 m ~ax:(0x0E00 lor Char.code 'A') ~bx:0x0000 ();
  check_int "AH=0Eh keeps the cell attribute" (cell m ~col:0 ~row:0) ((blue lsl 8) lor 0x41);
  (* 마지막 줄에서 줄바꿈하면 새 줄은 커서 칸의 속성이다. *)
  locate m ~col:5 ~row:24;
  int10 m ~ax:0x0920 ~bx:0x002F ~cx:1 ();
  int10 m ~ax:0x0E0A ~bx:0x0007 ();
  check_int "teletype scroll fills with the cursor cell attribute" (cell m ~col:0 ~row:24)
    0x2F20;
  check_int "the cursor cell moved up with the rest" (cell m ~col:5 ~row:23) 0x2F20;
  check_int "teletype scroll moved row 1 up" (cell m ~col:0 ~row:0) ((blue lsl 8) lor 0x20)

let test_attribute_writers () =
  let m = Dos_machine.create () in
  paint m blue;
  locate m ~col:0 ~row:2;
  int10 m ~ax:(0x0900 lor Char.code 'B') ~bx:0x002F ~cx:1 ();
  check_int "AH=09h writes the attribute" (cell m ~col:0 ~row:2) 0x2F42;
  (* AH=13h AL=0: ES:BP 의 문자열을 BL 속성으로 *)
  let text = "Hi" in
  String.iteri (fun i c -> Bytes.set m.Dos_state.mem (0x600 + i) c) text;
  let c = cpu m in
  Cpu86.set_seg c 0 0x60;
  Cpu86.set_reg16 c 5 0;
  int10 m ~ax:0x1300 ~bx:0x002F ~cx:(String.length text) ~dx:0x0300 ();
  check_int "AH=13h writes the attribute" (cell m ~col:1 ~row:3) 0x2F69

(* mov ah,2; mov dl,'C'; int 21h; int 20h *)
let dos_putc = "\xB4\x02\xB2\x43\xCD\x21\xCD\x20"

let test_dos_output_keeps_attribute () =
  let m = Dos_machine.create () in
  paint m red;
  locate m ~col:0 ~row:0;
  Dos_machine.load_com m dos_putc;
  Dos_machine.run m ~max_steps:10_000;
  check_int "INT 21h AH=02h keeps the cell attribute" (cell m ~col:0 ~row:0)
    ((red lsl 8) lor 0x43)

(* AH=0Ah 줄 입력: 'x' 를 치고 백스페이스로 지운 뒤 Enter. *)
let line_input_com =
  "\xB4\x0A\xBA\x00\x02"          (* mov ah,0Ah; mov dx,0200h *)
  ^ "\xC6\x06\x00\x02\x10"        (* mov byte [0200h],16 *)
  ^ "\xCD\x21\xCD\x20"

let test_line_erase_keeps_attribute () =
  let m = Dos_machine.create () in
  paint m red;
  locate m ~col:0 ~row:0;
  Dos_machine.load_com m line_input_com;
  Dos_machine.type_string m "x\b\r";
  Dos_machine.run m ~max_steps:100_000;
  check_int "line input erase keeps the cell attribute" (cell m ~col:0 ~row:0)
    ((red lsl 8) lor 0x20)

let () =
  test_boot_screen ();
  test_teletype_keeps_attribute ();
  test_attribute_writers ();
  test_dos_output_keeps_attribute ();
  test_line_erase_keeps_attribute ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos teletype: all passed\n"
