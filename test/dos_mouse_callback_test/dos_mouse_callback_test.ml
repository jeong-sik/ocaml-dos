(* INT 33h AX=0x0C — 게스트가 건 마우스 이벤트 핸들러를 set_mouse 가
   far call 로 부른다. 게스트는 마스크와 ES:DX(=CS:0x120)를 등록한 뒤
   플래그 워드(0x140)가 0이 아니게 되기를 기다린다. 핸들러는 받은
   레지스터 여섯 개를 메모리에 적고 RETF 로 돌아온다. *)

let failed = ref 0

let checkb name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got 0x%x want 0x%x\n%!" name got want
  end

(* COM 원점 0x100. 마스크와 대기 뒤 꼬리만 달리한다. [after_register] 는
   등록과 대기 사이에 끼운다. *)
let guest ?(after_register = "") ~mask ~tail () =
  let head =
    String.concat ""
      [ "\xb8\x0c\x00" (* mov ax,0x000C *)
      ; Printf.sprintf "\xb9%c%c" (Char.chr (mask land 0xff))
          (Char.chr ((mask lsr 8) land 0xff)) (* mov cx,mask *)
      ; "\xba\x20\x01" (* mov dx,0x0120 — 핸들러 *)
      ; "\x0e\x07" (* push cs; pop es *)
      ; "\xcd\x33" (* int 0x33 *)
      ; after_register
      ; "\xa0\x40\x01" (* wait: mov al,[0x0140] *)
      ; "\x08\xc0" (* or al,al *)
      ; "\x74\xf9" (* jz wait *)
      ]
  in
  let handler =
    String.concat ""
      [ "\xa3\x40\x01" (* mov [0x0140],ax *)
      ; "\x89\x1e\x42\x01" (* mov [0x0142],bx *)
      ; "\x89\x0e\x44\x01" (* mov [0x0144],cx *)
      ; "\x89\x16\x46\x01" (* mov [0x0146],dx *)
      ; "\x89\x36\x48\x01" (* mov [0x0148],si *)
      ; "\x89\x3e\x4a\x01" (* mov [0x014a],di *)
      ; "\xcb" (* retf *)
      ]
  in
  let pad = 0x20 - (String.length head + String.length tail) in
  assert (pad >= 0);
  head ^ tail ^ String.make pad '\x00' ^ handler

(* 플래그 하위 바이트(받은 AX 하위)를 종료 코드로 끝난다. *)
let tail_events = "\xb4\x4c\xcd\x21"

(* AX=0x0B 를 불러 CX(이동량 x)를 종료 코드로 끝난다. *)
let tail_mickeys = "\xb8\x0b\x00\xcd\x33\x89\xc8\xb4\x4c\xcd\x21"

(* 부팅 모드 3(80x25)의 가상 화면은 640x200 이고 칸은 8x8 이다. 드라이버는
   커서를 가운데에 두고 시작한다. 게임에 알리는 좌표는 칸 단위다. *)
let center_x = 320
let center_y = 100

(* COM 기본 psp_seg 0x1000 → 물리 0x10000 + 오프셋. *)
let phys off = 0x10000 + off

let peek_word m off =
  Dos_machine.mem_read m (phys off)
  lor (Dos_machine.mem_read m (phys off + 1) lsl 8)

let save_exn m =
  match Dos_snapshot.save m with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let restore_exn s =
  match Dos_snapshot.restore s with
  | Ok m -> m
  | Error e ->
    failwith
      (match e with
      | Dos_snapshot.Not_a_snapshot -> "not a snapshot"
      | Dos_snapshot.Wrong_format { saved; supported } ->
        Printf.sprintf "format %d, supported %d" saved supported
      | Dos_snapshot.Corrupt message -> "corrupt: " ^ message)

(* ---------- AX=05h·06h·07h·08h ---------- *)

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let mouse_call ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x33"
let store_ax a = "\xa3" ^ w16 a
let store_bx a = "\x89\x1e" ^ w16 a
let store_cx a = "\x89\x0e" ^ w16 a
let store_dx a = "\x89\x16" ^ w16 a
let exit_0 = "\xb8\x00\x4c\xcd\x21"

(* 결과 칸. 코드는 0x100 에서 0x200 앞에서 끝난다. *)
let out = 0x200

let run_program ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= out);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  before m;
  Dos_machine.run m ~max_steps:5000;
  checkb "program exited" (Dos_machine.exited m) true;
  m

let test_button_logs () =
  let code =
    String.concat ""
      [ mouse_call 0x05 ~bx:0; store_ax out; store_bx (out + 2); store_cx (out + 4);
        store_dx (out + 6);
        mouse_call 0x05 ~bx:0; store_bx (out + 8); store_cx (out + 10); store_dx (out + 12);
        mouse_call 0x06 ~bx:0; store_bx (out + 14); store_cx (out + 16); store_dx (out + 18);
        mouse_call 0x05 ~bx:1; store_bx (out + 20);
        mouse_call 0x05 ~bx:7 ~cx:0xFFFF; store_ax (out + 22); store_bx (out + 24);
        store_cx (out + 26);
        exit_0 ]
  in
  let m =
    run_program code ~before:(fun m ->
        Dos_machine.set_mouse m ~x:16 ~y:24 ~buttons:1;
        Dos_machine.set_mouse m ~x:32 ~y:40 ~buttons:0;
        Dos_machine.set_mouse m ~x:48 ~y:64 ~buttons:1)
  in
  checki "05h: AX = buttons now" (peek_word m out) 1;
  checki "05h: BX = two presses" (peek_word m (out + 2)) 2;
  checki "05h: CX = x of the last press" (peek_word m (out + 4)) 48;
  checki "05h: DX = y of the last press" (peek_word m (out + 6)) 64;
  checki "05h again: the count was read" (peek_word m (out + 8)) 0;
  checki "05h again: CX keeps the last press" (peek_word m (out + 10)) 48;
  checki "05h again: DX keeps the last press" (peek_word m (out + 12)) 64;
  checki "06h: BX = one release" (peek_word m (out + 14)) 1;
  checki "06h: CX = x of the release" (peek_word m (out + 16)) 32;
  checki "06h: DX = y of the release" (peek_word m (out + 18)) 40;
  checki "05h right: never pressed" (peek_word m (out + 20)) 0;
  checki "05h button 7: AX = buttons now" (peek_word m (out + 22)) 1;
  checki "05h button 7: BX unchanged" (peek_word m (out + 24)) 7;
  checki "05h button 7: CX unchanged" (peek_word m (out + 26)) 0xFFFF

(* 07h 는 거꾸로 준 범위를 바꿔 쓰고, 지금 자리(가운데)를 범위로 옮긴다 —
   위 끝으로. 04h 와 하네스가 주는 자리도 범위로 자른다 — 04h 는 아래 끝으로.
   범위 끝은 칸에 맞춰 두었다. *)
let range_code =
  String.concat ""
    [ mouse_call 0x07 ~cx:104 ~dx:24; mouse_call 0x08 ~cx:8 ~dx:48;
      mouse_call 0x03; store_cx out; store_dx (out + 2);
      mouse_call 0x04 ~cx:300 ~dx:0;
      mouse_call 0x03; store_cx (out + 4); store_dx (out + 6);
      exit_0 ]

let test_ranges () =
  let m = run_program range_code in
  checki "07h moves x into 24..104" (peek_word m out) 104;
  checki "08h moves y into 8..48" (peek_word m (out + 2)) 48;
  checki "04h x clamped" (peek_word m (out + 4)) 104;
  checki "04h y clamped" (peek_word m (out + 6)) 8;
  Dos_machine.set_mouse m ~x:500 ~y:500 ~buttons:1;
  let mouse = m.Dos_state.mouse in
  checki "set_mouse x clamped" mouse.Dos_state.mouse_x 104;
  checki "set_mouse y clamped" mouse.Dos_state.mouse_y 48;
  checki "mickeys count the clamped move (x)" mouse.Dos_state.mouse_dx 0;
  checki "mickeys count the clamped move (y)" mouse.Dos_state.mouse_dy 40;
  checki "the press is logged where the cursor is"
    mouse.Dos_state.mouse_presses.(0).Dos_state.last_y 48;
  Dos_machine.set_mouse m ~x:600 ~y:600 ~buttons:1;
  checki "out of range again: no more mickeys" mouse.Dos_state.mouse_dy 40;
  (* the snapshot carries ranges and logs *)
  let bytes = save_exn m in
  let r = restore_exn bytes in
  checkb "snapshot: same bytes after restore" (save_exn r = bytes) true;
  Dos_machine.set_mouse r ~x:0 ~y:0 ~buttons:0;
  checki "snapshot: range kept" r.Dos_state.mouse.Dos_state.mouse_x 24;
  checki "snapshot: press log kept" r.Dos_state.mouse.Dos_state.mouse_presses.(0).Dos_state.times 1

let test_reset_clears () =
  let code =
    String.concat ""
      [ mouse_call 0x07 ~cx:20 ~dx:100; mouse_call 0x08 ~cx:5 ~dx:50;
        mouse_call 0x00;
        mouse_call 0x04 ~cx:304 ~dx:0;
        mouse_call 0x03; store_cx out; store_dx (out + 2);
        mouse_call 0x05 ~bx:0; store_bx (out + 4);
        exit_0 ]
  in
  let m = run_program code ~before:(fun m -> Dos_machine.set_mouse m ~x:1 ~y:1 ~buttons:1) in
  checki "00h drops the x range" (peek_word m out) 304;
  checki "00h drops the y range" (peek_word m (out + 2)) 0;
  checki "00h drops the press count" (peek_word m (out + 4)) 0

(* ---------- 00h·21h 리셋 ---------- *)

(* 핸들러를 건 뒤 [call] 을 부르고 누름을 기다린다. 02h(숨기기)는 핸들러를
   그대로 두므로 테스트가 헛돌지 않는지 보는 짝이다. *)
let test_reset_turns_the_handler_off () =
  List.iter
    (fun (name, call, handler_runs) ->
      let m = Dos_machine.create () in
      let call_code = "\xb8" ^ w16 call ^ "\xcd\x33" in
      Dos_machine.load_com m (guest ~after_register:call_code ~mask:0x0002 ~tail:tail_events ());
      Dos_machine.run m ~max_steps:500;
      Dos_machine.set_mouse m ~x:100 ~y:50 ~buttons:1;
      Dos_machine.run m ~max_steps:2000;
      checkb name (Dos_machine.exited m) handler_runs)
    [ ("02h keeps the handler", 0x02, true);
      ("00h turns the handler off", 0x00, false);
      ("21h turns the handler off", 0x21, false) ]

let test_software_reset () =
  let code =
    String.concat ""
      [ mouse_call 0x07 ~cx:20 ~dx:100;
        mouse_call 0x21; store_ax out; store_bx (out + 2);
        mouse_call 0x04 ~cx:304 ~dx:0;
        mouse_call 0x03; store_cx (out + 4);
        exit_0 ]
  in
  let m = run_program code in
  checki "21h: AX = installed" (peek_word m out) 0xFFFF;
  checki "21h: BX = two buttons" (peek_word m (out + 2)) 2;
  checki "21h drops the x range" (peek_word m (out + 4)) 304;
  let absent =
    run_program
      (String.concat "" [ mouse_call 0x21 ~bx:0x1234; store_ax out; store_bx (out + 2); exit_0 ])
      ~before:(fun m -> m.Dos_state.mouse.Dos_state.mouse_present <- false)
  in
  checki "21h without a driver: AX stays 0021h" (peek_word absent out) 0x0021;
  checki "21h without a driver: BX unchanged" (peek_word absent (out + 2)) 0x1234

let () =
  test_button_logs ();
  test_reset_turns_the_handler_off ();
  test_software_reset ();
  test_ranges ();
  test_reset_clears ();
  (* 왼쪽 누름 등록: 이동+누름이 닿아 핸들러가 돈다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0002 ~tail:tail_events ());
  Dos_machine.run m ~max_steps:500;
  checkb "press: still spinning before the event" (Dos_machine.exited m) false;
  Dos_machine.set_mouse m ~x:104 ~y:48 ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "press: handler ran and guest exited" (Dos_machine.exited m) true;
  checki "press: AX = move|left-press" (Dos_machine.exit_code m) 0x03;
  checki "press: BX = buttons" (peek_word m 0x142) 0x0001;
  checki "press: CX = x" (peek_word m 0x144) 104;
  checki "press: DX = y" (peek_word m 0x146) 48;
  checki "press: SI = dx from the center" (peek_word m 0x148) ((104 - center_x) land 0xffff);
  checki "press: DI = dy from the center" (peek_word m 0x14a) ((48 - center_y) land 0xffff);
  (* 이동량 소비: 호출이 si/di 를 가져가면 뒤이은 AX=0x0B 는 0 이다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0002 ~tail:tail_mickeys ());
  Dos_machine.run m ~max_steps:500;
  Dos_machine.set_mouse m ~x:100 ~y:50 ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "mickeys: guest exited" (Dos_machine.exited m) true;
  checki "mickeys: AX=0x0B CX is 0 after the call" (Dos_machine.exit_code m) 0;
  (* 마스크 빗나감: 이동만 보는 등록에 누름은 닿지 않는다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0001 ~tail:tail_events ());
  Dos_machine.run m ~max_steps:500;
  Dos_machine.set_mouse m ~x:center_x ~y:center_y ~buttons:1;
  Dos_machine.run m ~max_steps:500;
  checkb "mask: press alone does not call" (Dos_machine.exited m) false;
  checki "mask: flag still zero" (peek_word m 0x140) 0;
  Dos_machine.set_mouse m ~x:(center_x + 10) ~y:center_y ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "mask: move then calls" (Dos_machine.exited m) true;
  checki "mask: AX = move" (Dos_machine.exit_code m) 0x01;
  (* 자리는 칸(8)으로 내리고, 이동량은 그대로 센다 *)
  checki "mask: CX = x in cells" (peek_word m 0x144) (center_x + 8);
  checki "mask: DX = y in cells (100 to 96)" (peek_word m 0x146) 96;
  checki "mask: SI = dx since press, not in cells" (peek_word m 0x148) 10;
  (* 등록은 스냅샷을 타고: 저장→복원 뒤 누름이 닿는다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0002 ~tail:tail_events ());
  Dos_machine.run m ~max_steps:500;
  let bytes = save_exn m in
  (match Dos_snapshot.header bytes with
  | Ok h -> checki "snapshot: current format" h.Dos_snapshot.format
      Dos_snapshot.format_version
  | Error _ -> checkb "snapshot: header reads" false true);
  let m = restore_exn bytes in
  checkb "snapshot: restored machine spins" (Dos_machine.exited m) false;
  Dos_machine.set_mouse m ~x:100 ~y:50 ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "snapshot: handler ran after restore" (Dos_machine.exited m) true;
  checki "snapshot: AX = move|left-press" (Dos_machine.exit_code m) 0x03;
  if !failed = 0 then print_endline "dos mouse callback: all passed"
  else begin
    Printf.eprintf "dos mouse callback: %d failures\n%!" !failed;
    exit 1
  end
