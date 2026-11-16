(* INT 33h 움직임 설정 — 0Fh 미키/화소 비율, 13h 두 배 속도 문턱, 1Ah·1Bh 감도.
   이 기계는 하네스가 커서 자리를 주므로 미키는 화소 × 비율 / 8 이다(CuteMouse
   updateposition 과 DOSBox-X Mouse_CursorMoved 의 거꾸로). 기본 비율은 가로 8,
   세로 16 이라 세로 미키가 화소의 두 배다(RBIL INT 33/000F). *)

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
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let mouse_call ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x33"
let store_bx a = "\x89\x1e" ^ w16 a
let store_cx a = "\x89\x0e" ^ w16 a
let store_dx a = "\x89\x16" ^ w16 a
let set_mode mode = "\xb8" ^ w16 mode ^ "\xcd\x10"
let exit_0 = "\xb8\x00\x4c\xcd\x21"
let sensitivity_at at = mouse_call 0x1B ^ store_bx at ^ store_cx (at + 2) ^ store_dx (at + 4)
let mickeys_at at = mouse_call 0x0B ^ store_cx at ^ store_dx (at + 2)

(* 하네스가 [flag] 에 0 이 아닌 값을 쓸 때까지 돈다 *)
let wait_for flag = "\xa0" ^ w16 flag ^ "\x08\xc0\x74\xf9"

let psp = 0x10000
let out = 0x300
let flag_a = 0x2F0
let flag_b = 0x2F2

(* 부팅 모드 3 의 가운데 *)
let cx0 = 320
let cy0 = 100

let peek_word m off =
  Dos_machine.mem_read m (psp + off) lor (Dos_machine.mem_read m (psp + off + 1) lsl 8)

let signed16 v = if v >= 0x8000 then v - 0x10000 else v

let boot code =
  assert (0x100 + String.length code <= flag_a);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  m

let run_to_exit m =
  Dos_machine.run m ~max_steps:20_000;
  check "program exited" (Dos_machine.exited m)

let test_conversion () =
  let conv ~per_8 ~carry d = mickeys_of_pixels ~per_8 ~carry d in
  check "비율 8: 화소 그대로" (conv ~per_8:8 ~carry:0 5 = (5, 0));
  check "비율 16: 두 배, 음수도" (conv ~per_8:16 ~carry:0 (-3) = (-6, 0));
  check "비율 4: 1 화소는 반 미키라 나머지로" (conv ~per_8:4 ~carry:0 1 = (0, 4));
  check "비율 4: 나머지가 다음 화소와 합쳐진다" (conv ~per_8:4 ~carry:4 1 = (1, 0));
  check "비율 4: 뒤로 1 화소" (conv ~per_8:4 ~carry:0 (-1) = (-1, 4));
  (* 비율 3: 8 화소를 한 칸씩 가면 3 미키, 되돌아오면 0 *)
  let walk d n (total, carry) =
    let rec go i (t, c) =
      if i = 0 then (t, c)
      else
        let mk, c' = conv ~per_8:3 ~carry:c d in
        go (i - 1) (t + mk, c')
    in
    go n (total, carry)
  in
  let forward = walk 1 8 (0, 0) in
  checki "비율 3: 8 화소 = 3 미키" (fst forward) 3;
  checki "비율 3: 나머지 0" (snd forward) 0;
  let back = walk (-1) 8 forward in
  checki "비율 3: 되돌아오면 0" (fst back) 0;
  checki "나머지는 0..7" (snd (conv ~per_8:3 ~carry:0 (-5))) 1

(* 기본 비율에서 0Bh 가 보는 미키 *)
let test_default_ratio () =
  let m = boot (mickeys_at out ^ exit_0) in
  Dos_machine.set_mouse m ~x:(cx0 + 8) ~y:(cy0 + 8) ~buttons:0;
  run_to_exit m;
  checki "기본: 가로 8 화소 = 8 미키" (peek_word m out) 8;
  checki "기본: 세로 8 화소 = 16 미키" (peek_word m (out + 2)) 16

let test_set_ratio () =
  let m = boot (mouse_call 0x0F ~cx:16 ~dx:4 ^ wait_for flag_a ^ mickeys_at out ^ exit_0) in
  Dos_machine.run m ~max_steps:5000;
  Dos_machine.set_mouse m ~x:(cx0 + 8) ~y:(cy0 - 8) ~buttons:0;
  Dos_machine.mem_write m (psp + flag_a) 1;
  run_to_exit m;
  checki "0Fh 16: 가로 두 배" (peek_word m out) 16;
  checki "0Fh 4: 세로 반, 음수" (signed16 (peek_word m (out + 2))) (-4);
  (* 0 이 하나라도 있으면 무시 *)
  let m = boot (mouse_call 0x0F ~cx:0 ~dx:4 ^ mouse_call 0x0F ~cx:4 ~dx:0 ^ exit_0) in
  run_to_exit m;
  check "0Fh 0 은 무시" (m.mouse.mouse_motion = default_motion)

let test_sensitivity () =
  let m =
    boot
      (String.concat ""
         [ sensitivity_at out;
           mouse_call 0x1A ~bx:30 ~cx:70 ~dx:100; sensitivity_at (out + 6);
           mouse_call 0x1A ~bx:0 ~cx:101 ~dx:0; sensitivity_at (out + 12);
           mouse_call 0x13 ~dx:200; sensitivity_at (out + 18);
           mouse_call 0x13 ~dx:0; sensitivity_at (out + 24);
           mouse_call 0x1A ~bx:1 ~cx:100 ~dx:64; sensitivity_at (out + 30);
           exit_0 ])
  in
  run_to_exit m;
  let at k = peek_word m (out + k) in
  checki "1Bh 기본 가로" (at 0) 50;
  checki "1Bh 기본 세로" (at 2) 50;
  checki "1Bh 기본 문턱" (at 4) 64;
  checki "1Ah 가로" (at 6) 30;
  checki "1Ah 세로" (at 8) 70;
  checki "1Ah 문턱" (at 10) 100;
  checki "1Ah 0 은 무시" (at 12) 30;
  checki "1Ah 101 은 무시" (at 14) 70;
  checki "1Ah 문턱 0 은 64" (at 16) 64;
  checki "13h 문턱" (at 22) 200;
  checki "13h 0 은 64" (at 28) 64;
  checki "13h 는 감도를 건드리지 않는다" (at 24) 30;
  checki "1Ah 1 은 받는다" (at 30) 1;
  checki "1Ah 100 은 받는다" (at 32) 100

(* 00h 는 움직임 설정을 되돌리고, INT 10h 모드 세우기는 남긴다 *)
let test_reset_and_mode_set () =
  let m =
    boot
      (String.concat ""
         [ mouse_call 0x0F ~cx:4 ~dx:4; mouse_call 0x1A ~bx:10 ~cx:10 ~dx:10;
           mouse_call 0x00; sensitivity_at out; exit_0 ])
  in
  run_to_exit m;
  checki "00h 뒤 감도 50" (peek_word m out) 50;
  checki "00h 뒤 문턱 64" (peek_word m (out + 4)) 64;
  check "00h 뒤 비율 8·16" (m.mouse.mouse_motion = default_motion);
  let m = boot (mouse_call 0x0F ~cx:4 ~dx:4 ^ set_mode 0x13 ^ exit_0) in
  run_to_exit m;
  Dos_machine.set_mouse m ~x:(cx0 + 8) ~y:cy0 ~buttons:0;
  checki "모드 세우기 뒤에도 비율 4" m.mouse.mouse_dx 4

(* 04h 로 드라이버가 커서를 옮기면 남은 반 미키를 버린다 *)
let test_set_position_drops_carry () =
  let m =
    boot
      (String.concat ""
         [ mouse_call 0x0F ~cx:4 ~dx:4; wait_for flag_a;
           mouse_call 0x04 ~cx:cx0 ~dx:cy0; wait_for flag_b;
           mickeys_at out; exit_0 ])
  in
  Dos_machine.run m ~max_steps:5000;
  Dos_machine.set_mouse m ~x:(cx0 + 1) ~y:cy0 ~buttons:0;
  checki "반 미키가 남는다" m.mouse.mouse_carry_x 4;
  Dos_machine.mem_write m (psp + flag_a) 1;
  Dos_machine.run m ~max_steps:5000;
  checki "04h 가 나머지를 버린다" m.mouse.mouse_carry_x 0;
  Dos_machine.set_mouse m ~x:(cx0 + 1) ~y:cy0 ~buttons:0;
  Dos_machine.mem_write m (psp + flag_b) 1;
  run_to_exit m;
  checki "두 번의 1 화소가 1 미키로 합쳐지지 않는다" (peek_word m out) 0

(* INT 10h 모드 세우기도 커서를 가운데로 옮기며 나머지를 버린다(CuteMouse
   setupvideo 가 rounderr 를 지운다) *)
let test_mode_set_drops_carry () =
  let m =
    boot
      (String.concat ""
         [ mouse_call 0x0F ~cx:4 ~dx:4; wait_for flag_a;
           set_mode 0x13; wait_for flag_b;
           mickeys_at out; exit_0 ])
  in
  Dos_machine.run m ~max_steps:5000;
  Dos_machine.set_mouse m ~x:(cx0 + 1) ~y:cy0 ~buttons:0;
  Dos_machine.mem_write m (psp + flag_a) 1;
  Dos_machine.run m ~max_steps:20_000;
  checki "모드 세우기가 나머지를 버린다" m.mouse.mouse_carry_x 0;
  (* 13h 가운데도 x 320 이다 *)
  Dos_machine.set_mouse m ~x:(cx0 + 1) ~y:cy0 ~buttons:0;
  Dos_machine.mem_write m (psp + flag_b) 1;
  run_to_exit m;
  checki "모드 세우기 앞뒤 1 화소가 합쳐지지 않는다" (peek_word m out) 0

let save_exn m =
  match Dos_snapshot.save m with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let test_snapshot () =
  let m = boot (mouse_call 0x0F ~cx:3 ~dx:5 ^ mouse_call 0x1A ~bx:20 ~cx:30 ~dx:40 ^ exit_0) in
  run_to_exit m;
  Dos_machine.set_mouse m ~x:(cx0 + 1) ~y:(cy0 + 1) ~buttons:0;
  let saved = save_exn m in
  match Dos_snapshot.restore saved with
  | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
  | Ok r ->
    check "저장 → 복원 → 저장" (save_exn r = saved);
    check "움직임 설정이 돌아온다" (r.mouse.mouse_motion = m.mouse.mouse_motion);
    checki "가로 나머지" r.mouse.mouse_carry_x 3;
    checki "세로 나머지" r.mouse.mouse_carry_y 5;
    check "1Ah 값" (r.mouse.mouse_motion.speed_x = 20 && r.mouse.mouse_motion.double_speed = 40)

let () =
  test_conversion ();
  test_default_ratio ();
  test_set_ratio ();
  test_sensitivity ();
  test_reset_and_mode_set ();
  test_set_position_drops_carry ();
  test_mode_set_drops_carry ();
  test_snapshot ();
  if !failed = 0 then print_endline "dos mouse motion: all passed"
  else begin
    Printf.eprintf "dos mouse motion: %d failures\n%!" !failed;
    exit 1
  end
