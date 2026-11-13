(* INT 33h 가상 화면 — 드라이버가 상주할 때, 00h 리셋, INT 10h 모드 세우기가
   범위를 화면 전체로, 커서를 가운데로 둔다. 03h·05h 는 칸 단위로 알린다.
   24h 판·종류, 26h 최대 좌표. 기대값은 CuteMouse ctmouse.asm(setupvideo,
   int10handler, version_24, maxscreen_26)과 HelpPC 2.10 INT 33,0 이다.
   COM 이 부른 뒤 레지스터를 0x300 부터 적고 끝난다. *)

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
let set_mode mode = "\xb8" ^ w16 mode ^ "\xcd\x10"  (* mov ax,mode; int 10h *)
let exit_0 = "\xb8\x00\x4c\xcd\x21"

(* 03h 의 CX·DX 를 [at]·[at+2] 에 *)
let position_at at = mouse_call 0x03 ^ store_cx at ^ store_dx (at + 2)

(* 26h 의 BX·CX·DX 를 [at] 부터 *)
let maximum_at at = mouse_call 0x26 ^ store_bx at ^ store_cx (at + 2) ^ store_dx (at + 4)

(* 하네스가 [ready] 에 1 을 쓸 때까지 돈다 *)
let ready = 0x2F0
let wait_for_host = "\xa0" ^ w16 ready ^ "\x08\xc0\x74\xf9"  (* mov al,[ready]; or al,al; jz *)

let psp = 0x10000
let out = 0x300

let peek_word m off =
  Dos_machine.mem_read m (psp + off) lor (Dos_machine.mem_read m (psp + off + 1) lsl 8)

let run_program ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= ready);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  before m;
  Dos_machine.run m ~max_steps:20_000;
  check "program exited" (Dos_machine.exited m);
  m

let test_boot () =
  let m =
    run_program
      (String.concat ""
         [ position_at out; maximum_at (out + 4);
           mouse_call 0x24; store_ax (out + 10); store_bx (out + 12); store_cx (out + 14);
           exit_0 ])
  in
  checki "부팅: 모드 3 가운데 x" (peek_word m out) 320;
  checki "부팅: 모드 3 가운데 y 100 은 칸으로 96" (peek_word m (out + 2)) 96;
  checki "26h BX: 켜짐" (peek_word m (out + 4)) 0;
  checki "26h CX: 640 - 1" (peek_word m (out + 6)) 639;
  checki "26h DX: 200 - 1" (peek_word m (out + 8)) 199;
  checki "24h AX 그대로" (peek_word m (out + 10)) 0x0024;
  checki "24h BX: 7.05" (peek_word m (out + 12)) 0x0705;
  checki "24h CX: CH=4(PS/2), CL=0" (peek_word m (out + 14)) 0x0400

let test_reset () =
  let m =
    run_program
      ~before:(fun m -> Dos_machine.set_mouse m ~x:8 ~y:8 ~buttons:0)
      (String.concat ""
         [ mouse_call 0x07 ~cx:0 ~dx:100;
           mouse_call 0x00;
           position_at out;
           mouse_call 0x0B; store_cx (out + 4); store_dx (out + 6);
           mouse_call 0x04 ~cx:1000 ~dx:1000;
           position_at (out + 8);
           mouse_call 0x04 ~cx:327 ~dx:103;
           position_at (out + 12);
           mouse_call 0x07 ~cx:0 ~dx:100;
           mouse_call 0x26; store_cx (out + 16);
           exit_0 ])
  in
  checki "00h: 가운데 x" (peek_word m out) 320;
  checki "00h: 가운데 y 100 은 칸으로 96" (peek_word m (out + 2)) 96;
  checki "00h: 이동량 x 0" (peek_word m (out + 4)) 0;
  checki "00h: 이동량 y 0" (peek_word m (out + 6)) 0;
  checki "00h: 범위가 화면 끝까지, 칸은 8" (peek_word m (out + 8)) 632;
  checki "00h: 세로 화면 끝 칸" (peek_word m (out + 10)) 192;
  checki "03h: 칸 왼쪽" (peek_word m (out + 12)) 320;
  checki "03h: 칸 위쪽" (peek_word m (out + 14)) 96;
  checki "26h 는 07h 범위가 아니라 화면 끝" (peek_word m (out + 16)) 639

let test_mode_13h () =
  let masks = 0x280 in
  let m =
    run_program
      (String.concat ""
         [ mouse_call 0x01;
           mouse_call 0x07 ~cx:0 ~dx:100;
           "\x0e\x07";  (* push cs; pop es — 09h 마스크는 ES:DX *)
           mouse_call 0x09 ~bx:3 ~cx:4 ~dx:masks;
           mouse_call 0x04 ~cx:8 ~dx:8;
           set_mode 0x13;
           position_at out;
           mouse_call 0x2A; store_ax (out + 4); store_bx (out + 6); store_cx (out + 8);
           maximum_at (out + 10);
           mouse_call 0x04 ~cx:1000 ~dx:1000;
           position_at (out + 16);
           mouse_call 0x04 ~cx:321 ~dx:101;
           position_at (out + 20);
           exit_0 ])
  in
  checki "13h: 가운데 x" (peek_word m out) 320;
  checki "13h: 가운데 y" (peek_word m (out + 2)) 100;
  checki "모드 세우기가 커서를 숨긴다(깃발 -1)" (peek_word m (out + 4)) 0xFFFF;
  checki "모드 세우기는 핫스폿 x 를 남긴다" (peek_word m (out + 6)) 3;
  checki "모드 세우기는 핫스폿 y 를 남긴다" (peek_word m (out + 8)) 4;
  checki "13h 26h CX" (peek_word m (out + 12)) 639;
  checki "13h 26h DX" (peek_word m (out + 14)) 199;
  checki "13h: 07h 범위가 풀려 칸(2) 끝까지" (peek_word m (out + 16)) 638;
  checki "13h: 세로 끝" (peek_word m (out + 18)) 199;
  checki "13h: x 는 짝수로" (peek_word m (out + 20)) 320;
  checki "13h: y 는 그대로" (peek_word m (out + 22)) 101

let test_other_modes () =
  let m =
    run_program
      (String.concat ""
         [ set_mode 0x12; position_at out; maximum_at (out + 4);
           set_mode 0x01; position_at (out + 10); maximum_at (out + 14);
           mouse_call 0x04 ~cx:31 ~dx:15; position_at (out + 20);
           set_mode 0x03;
           exit_0 ])
  in
  checki "12h: 가운데 x" (peek_word m out) 320;
  checki "12h: 가운데 y" (peek_word m (out + 2)) 240;
  checki "12h 26h DX" (peek_word m (out + 8)) 479;
  checki "40열: 가운데 x" (peek_word m (out + 10)) 320;
  checki "40열: 가운데 y 100 은 칸으로 96" (peek_word m (out + 12)) 96;
  checki "40열 26h CX: 640 - 1" (peek_word m (out + 16)) 639;
  checki "40열: 칸 16" (peek_word m (out + 20)) 16;
  checki "40열: 칸 높이 8" (peek_word m (out + 22)) 8

(* 05h 의 마지막 누름 자리도 칸이다 — 누른 때의 모드로 *)
let test_press_log_in_cells () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    (String.concat ""
       [ set_mode 0x13; wait_for_host;
         mouse_call 0x05 ~bx:0; store_bx out; store_cx (out + 2); store_dx (out + 4);
         exit_0 ]);
  Dos_machine.run m ~max_steps:5000;
  check "누름 전에는 기다린다" (not (Dos_machine.exited m));
  Dos_machine.set_mouse m ~x:321 ~y:101 ~buttons:1;
  Dos_machine.mem_write m (psp + ready) 1;
  Dos_machine.run m ~max_steps:5000;
  check "누름 뒤 끝난다" (Dos_machine.exited m);
  checki "05h: 한 번" (peek_word m out) 1;
  checki "05h: 13h 칸 x" (peek_word m (out + 2)) 320;
  checki "05h: y" (peek_word m (out + 4)) 101

(* 드라이버가 없으면 INT 10h 를 가로채지 않는다 *)
let test_no_driver () =
  let m =
    run_program
      ~before:(fun m ->
        Dos_machine.set_mouse m ~x:8 ~y:8 ~buttons:0;
        m.Dos_state.mouse.Dos_state.mouse_present <- false)
      (String.concat "" [ set_mode 0x13; exit_0 ])
  in
  checki "드라이버 없음: 자리 그대로" m.Dos_state.mouse.Dos_state.mouse_x 8;
  checki "드라이버 없음: 깃발 그대로" m.Dos_state.mouse.Dos_state.mouse_cursor_flag
    Dos_state.cursor_flag_after_reset

let () =
  test_boot ();
  test_reset ();
  test_mode_13h ();
  test_other_modes ();
  test_press_log_in_cells ();
  test_no_driver ();
  if !failed = 0 then print_endline "dos mouse screen: all passed"
  else begin
    Printf.eprintf "dos mouse screen: %d failures\n%!" !failed;
    exit 1
  end
