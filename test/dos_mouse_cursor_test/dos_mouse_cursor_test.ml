(* INT 33h 커서 상태 — 01h·02h 의 깃발, 09h 그래픽 커서, 0Ah 텍스트 커서,
   2Ah 조회, 00h·21h 리셋. COM 프로그램이 부른 뒤 레지스터를 0x300 부터
   적고 끝난다. 그리는 곳이 없으니(이슈 #77) 모양은 기계 상태로 본다. *)

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
let mouse_call ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x33"
let store_ax a = "\xa3" ^ w16 a
let store_bx a = "\x89\x1e" ^ w16 a
let store_cx a = "\x89\x0e" ^ w16 a
let store_dx a = "\x89\x16" ^ w16 a
let es_from_ax = "\x8e\xc0"
let mov_ax n = "\xb8" ^ w16 n
let exit_0 = "\xb8\x00\x4c\xcd\x21"

(* 2Ah 를 불러 AX·BX·CX·DX 를 [at] 부터 적는다. *)
let hot_spot_at at =
  mouse_call 0x2A ^ store_ax at ^ store_bx (at + 2) ^ store_cx (at + 4) ^ store_dx (at + 6)

(* COM 은 0x1000:0100 에 실린다. 결과 칸은 0x300, 마스크 자료는 0x400 이다. *)
let psp = 0x10000
let out = 0x300
let masks = 0x400

let peek_word m off =
  Dos_machine.mem_read m (psp + off) lor (Dos_machine.mem_read m (psp + off + 1) lsl 8)

let poke_word m addr v =
  Dos_machine.mem_write m addr (v land 0xff);
  Dos_machine.mem_write m (addr + 1) ((v lsr 8) land 0xff)

let run_program ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= out);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  before m;
  Dos_machine.run m ~max_steps:5000;
  check "program exited" (Dos_machine.exited m);
  m

(* 서른두 워드가 모두 다르게: 화면 마스크 줄 k 는 A000h+k, 커서 마스크
   줄 k 는 0B00h+k. *)
let screen_row k = 0xA000 + k
let cursor_row k = 0x0B00 + k

let fill_masks m base ~wrap_at =
  for k = 0 to (2 * cursor_rows) - 1 do
    let v = if k < cursor_rows then screen_row k else cursor_row (k - cursor_rows) in
    poke_word m (wrap_at (base, 2 * k)) v
  done

let expected_cursor ~hot_x ~hot_y =
  Custom_cursor
    { hot_x; hot_y; screen_mask = Array.init cursor_rows screen_row;
      cursor_mask = Array.init cursor_rows cursor_row }

let test_flag () =
  let code =
    String.concat ""
      [ hot_spot_at out;
        mouse_call 0x01; hot_spot_at (out + 8);
        mouse_call 0x01; hot_spot_at (out + 16);
        mouse_call 0x02; mouse_call 0x02; hot_spot_at (out + 24);
        mouse_call 0x01; hot_spot_at (out + 32);
        mouse_call 0x01; hot_spot_at (out + 40);
        exit_0 ]
  in
  let m = run_program code in
  checki "fresh: flag -1" (peek_word m out) 0xFFFF;
  checki "fresh: hot x of the arrow" (peek_word m (out + 2)) 0;
  checki "fresh: hot y of the arrow" (peek_word m (out + 4)) 0;
  checki "fresh: PS/2 mouse type" (peek_word m (out + 6)) 0x04;
  checki "01h: shown" (peek_word m (out + 8)) 0;
  checki "01h again: stays 0" (peek_word m (out + 16)) 0;
  checki "02h twice: -2" (peek_word m (out + 24)) 0xFFFE;
  checki "01h once after two hides: still hidden" (peek_word m (out + 32)) 0xFFFF;
  checki "01h twice after two hides: shown" (peek_word m (out + 40)) 0;
  checki "machine state follows the guest" m.mouse.mouse_cursor_flag 0

let test_graphics_cursor () =
  let code =
    String.concat ""
      [ "\x0e\x07" (* push cs; pop es *);
        mouse_call 0x09 ~bx:0xFFFD ~cx:5 ~dx:masks;
        hot_spot_at out; exit_0 ]
  in
  let m =
    run_program code ~before:(fun m ->
        fill_masks m (psp + masks) ~wrap_at:(fun (base, off) -> base + off))
  in
  checki "2Ah: flag unchanged by 09h" (peek_word m out) 0xFFFF;
  checki "2Ah: hot x = BX" (peek_word m (out + 2)) 0xFFFD;
  checki "2Ah: hot y = CX" (peek_word m (out + 4)) 5;
  check "09h: both masks read from ES:DX"
    (m.mouse.mouse_graphics_cursor = expected_cursor ~hot_x:0xFFFD ~hot_y:5);
  (* ES:FFF0 의 마스크는 여덟 줄 뒤 ES:0000 으로 돈다. *)
  let seg = 0x2000 in
  let code =
    String.concat ""
      [ mov_ax seg; es_from_ax; mouse_call 0x09 ~bx:1 ~cx:2 ~dx:0xFFF0; exit_0 ]
  in
  let m =
    run_program code ~before:(fun m ->
        fill_masks m (seg lsl 4) ~wrap_at:(fun (base, off) -> base + ((0xFFF0 + off) land 0xffff)))
  in
  check "09h: the mask offset wraps inside ES"
    (m.mouse.mouse_graphics_cursor = expected_cursor ~hot_x:1 ~hot_y:2)

let test_text_cursor () =
  let run_0a ~bx ~cx ~dx = run_program (mouse_call 0x0A ~bx ~cx ~dx ^ exit_0) in
  check "fresh: default software text cursor"
    ((run_program exit_0).mouse.mouse_text_cursor = default_text_cursor);
  check "0Ah BX=0: software masks"
    ((run_0a ~bx:0 ~cx:0x1234 ~dx:0x5678).mouse.mouse_text_cursor
     = Software_text { screen_mask = 0x1234; cursor_mask = 0x5678 });
  check "0Ah BX=1: hardware scan lines"
    ((run_0a ~bx:1 ~cx:6 ~dx:7).mouse.mouse_text_cursor
     = Hardware_text { first_line = 6; last_line = 7 });
  check "0Ah BX=2: hardware like DOSBox-X"
    ((run_0a ~bx:2 ~cx:3 ~dx:4).mouse.mouse_text_cursor
     = Hardware_text { first_line = 3; last_line = 4 })

(* 00h·21h 는 깃발을 -1 로, 모양을 기본으로 되돌린다. *)
let test_reset reset =
  let name s = Printf.sprintf "%02Xh: %s" reset s in
  let code =
    String.concat ""
      [ "\x0e\x07"; mouse_call 0x01; mouse_call 0x09 ~bx:3 ~cx:4 ~dx:masks;
        mouse_call 0x0A ~bx:1 ~cx:6 ~dx:7; mouse_call reset; hot_spot_at out; exit_0 ]
  in
  let m =
    run_program code ~before:(fun m ->
        fill_masks m (psp + masks) ~wrap_at:(fun (base, off) -> base + off))
  in
  checki (name "flag back to -1") (peek_word m out) 0xFFFF;
  checki (name "hot x back to the arrow") (peek_word m (out + 2)) 0;
  checki (name "hot y back to the arrow") (peek_word m (out + 4)) 0;
  check (name "graphics cursor back to the arrow")
    (m.mouse.mouse_graphics_cursor = Default_arrow);
  check (name "text cursor back to the default")
    (m.mouse.mouse_text_cursor = default_text_cursor)

let save_exn m =
  match Dos_snapshot.save m with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let restore_exn s =
  match Dos_snapshot.restore s with
  | Ok m -> m
  | Error e -> failwith (Dos_snapshot.error_to_string e)

let test_snapshot () =
  let code =
    String.concat ""
      [ "\x0e\x07"; mouse_call 0x02; mouse_call 0x09 ~bx:7 ~cx:8 ~dx:masks;
        mouse_call 0x0A ~bx:1 ~cx:6 ~dx:7; exit_0 ]
  in
  let m =
    run_program code ~before:(fun m ->
        fill_masks m (psp + masks) ~wrap_at:(fun (base, off) -> base + off))
  in
  let bytes = save_exn m in
  let r = restore_exn bytes in
  checki "snapshot: flag" (r.mouse.mouse_cursor_flag land 0xffff) 0xFFFE;
  check "snapshot: graphics cursor"
    (r.mouse.mouse_graphics_cursor = expected_cursor ~hot_x:7 ~hot_y:8);
  check "snapshot: text cursor"
    (r.mouse.mouse_text_cursor = Hardware_text { first_line = 6; last_line = 7 });
  check "snapshot: same bytes after restore" (save_exn r = bytes);
  (* 소프트웨어 텍스트 커서와 기본 화살표도 그대로 돌아온다. *)
  let m = run_program (mouse_call 0x0A ~bx:0 ~cx:0x0102 ~dx:0x0304 ^ exit_0) in
  let r = restore_exn (save_exn m) in
  check "snapshot: default arrow" (r.mouse.mouse_graphics_cursor = Default_arrow);
  check "snapshot: software text cursor"
    (r.mouse.mouse_text_cursor = Software_text { screen_mask = 0x0102; cursor_mask = 0x0304 })

let () =
  test_flag ();
  test_graphics_cursor ();
  test_text_cursor ();
  test_reset 0x00;
  test_reset 0x21;
  test_snapshot ();
  if !failed = 0 then print_endline "dos mouse cursor: all passed"
  else begin
    Printf.eprintf "dos mouse cursor: %d failures\n%!" !failed;
    exit 1
  end
