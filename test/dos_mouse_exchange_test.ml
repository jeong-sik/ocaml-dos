(* INT 33h 14h(핸들러 맞바꾸기), 27h(텍스트 커서 값과 미키), 31h(지금 범위).
   기대값은 RBIL INT 33/0014·0027·0031 과 CuteMouse ctmouse.asm(exchangeUIR_14,
   cursor_27, cursrange_31)이다. COM 이 부른 뒤 레지스터를 0x300 부터 적는다. *)

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
let mouse_call ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x33"
let store_ax a = "\xa3" ^ w16 a
let store_bx a = "\x89\x1e" ^ w16 a
let store_cx a = "\x89\x0e" ^ w16 a
let store_dx a = "\x89\x16" ^ w16 a
let store_es a = "\x8c\xc0" ^ store_ax a  (* mov ax,es; mov [a],ax *)
let es_is_cs = "\x0e\x07"  (* push cs; pop es *)
let exit_0 = "\xb8\x00\x4c\xcd\x21"

let psp_seg = 0x1000
let psp = psp_seg * 16
let flag = 0x2F0
let handler_a = 0x280
let handler_b = 0x290
let out = 0x300

(* 핸들러는 표시 바이트를 [flag] 에 쓰고 돌아간다: mov byte [flag],v; retf *)
let handler v = "\xc6\x06" ^ w16 flag ^ String.make 1 (Char.chr v) ^ "\xcb"
let mark_a = 0xAA
let mark_b = 0xBB

(* [flag] 가 0 이 아니게 될 때까지 돈다 *)
let wait_flag = "\xa0" ^ w16 flag ^ "\x08\xc0\x74\xf9"

let peek_word m off =
  Dos_machine.mem_read m (psp + off) lor (Dos_machine.mem_read m (psp + off + 1) lsl 8)

(* 코드 뒤에 handler_a·handler_b 를 제자리에 둔다 *)
let image code =
  let pad n s = s ^ String.make (n - String.length s) '\x00' in
  assert (0x100 + String.length code <= handler_a);
  pad (handler_b - 0x100) (pad (handler_a - 0x100) code ^ handler mark_a) ^ handler mark_b

let test_exchange () =
  let code =
    String.concat ""
      [ es_is_cs;
        mouse_call 0x14 ~cx:0x0002 ~dx:handler_a;
        store_cx out; store_es (out + 2); store_dx (out + 4);
        es_is_cs;
        mouse_call 0x14 ~cx:0x0006 ~dx:handler_b;
        store_cx (out + 6); store_es (out + 8); store_dx (out + 10);
        wait_flag;
        exit_0 ]
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m (image code);
  Dos_machine.run m ~max_steps:5000;
  check "누르기 전에는 기다린다" (not (Dos_machine.exited m));
  checki "부팅 뒤 14h: 앞 마스크 0" (peek_word m out) 0;
  checki "부팅 뒤 14h: 앞 세그먼트 0" (peek_word m (out + 2)) 0;
  checki "부팅 뒤 14h: 앞 오프셋 0" (peek_word m (out + 4)) 0;
  checki "두 번째 14h: 앞 마스크" (peek_word m (out + 6)) 0x0002;
  checki "두 번째 14h: 앞 세그먼트" (peek_word m (out + 8)) psp_seg;
  checki "두 번째 14h: 앞 오프셋" (peek_word m (out + 10)) handler_a;
  (* 새 등록(마스크 06h: 왼쪽 누름·뗌)의 루틴 B 가 불린다 *)
  Dos_machine.set_mouse m ~x:320 ~y:96 ~buttons:1;
  Dos_machine.run m ~max_steps:5000;
  check "누르면 끝난다" (Dos_machine.exited m);
  checki "불린 것은 새 루틴" (Dos_machine.mem_read m (psp + flag)) mark_b

let run_program ?(before = fun _ -> ()) code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (image code);
  before m;
  Dos_machine.run m ~max_steps:5000;
  check "program exited" (Dos_machine.exited m);
  m

let masks_at at = mouse_call 0x27 ^ store_ax at ^ store_bx (at + 2) ^ store_cx (at + 4) ^ store_dx (at + 6)

let test_masks_and_mickeys () =
  let m =
    run_program
      ~before:(fun m -> Dos_machine.set_mouse m ~x:(320 + 8) ~y:(100 + 8) ~buttons:0)
      (String.concat ""
         [ masks_at out; masks_at (out + 8);
           mouse_call 0x0A ~bx:1 ~cx:6 ~dx:7; masks_at (out + 16);
           exit_0 ])
  in
  checki "27h 기본 화면 마스크" (peek_word m out) 0x77FF;
  checki "27h 기본 커서 마스크" (peek_word m (out + 2)) 0x7700;
  checki "27h 가로 미키" (peek_word m (out + 4)) 8;
  checki "27h 세로 미키(8 화소당 16)" (peek_word m (out + 6)) 16;
  checki "27h 가 미키를 가져갔다 x" (peek_word m (out + 12)) 0;
  checki "27h 가 미키를 가져갔다 y" (peek_word m (out + 14)) 0;
  checki "하드웨어 커서: 시작 줄" (peek_word m (out + 16)) 6;
  checki "하드웨어 커서: 끝 줄" (peek_word m (out + 18)) 7

let range_at at = mouse_call 0x31 ^ store_ax at ^ store_bx (at + 2) ^ store_cx (at + 4) ^ store_dx (at + 6)

let test_range () =
  let m =
    run_program
      (String.concat ""
         [ range_at out;
           mouse_call 0x07 ~cx:100 ~dx:10; mouse_call 0x08 ~cx:20 ~dx:50;
           range_at (out + 8);
           exit_0 ])
  in
  checki "31h 부팅: 가로 아래" (peek_word m out) 0;
  checki "31h 부팅: 세로 아래" (peek_word m (out + 2)) 0;
  checki "31h 부팅: 가로 위" (peek_word m (out + 4)) 639;
  checki "31h 부팅: 세로 위" (peek_word m (out + 6)) 199;
  checki "31h: 07h 아래" (peek_word m (out + 8)) 10;
  checki "31h: 08h 아래" (peek_word m (out + 10)) 20;
  checki "31h: 07h 위" (peek_word m (out + 12)) 100;
  checki "31h: 08h 위" (peek_word m (out + 14)) 50

let () =
  test_exchange ();
  test_masks_and_mickeys ();
  test_range ();
  if !failed = 0 then print_endline "dos mouse exchange: all passed"
  else begin
    Printf.eprintf "dos mouse exchange: %d failures\n%!" !failed;
    exit 1
  end
