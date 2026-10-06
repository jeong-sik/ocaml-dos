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

(* COM 원점 0x100. 마스크와 대기 뒤 꼬리만 달리한다. *)
let guest ~mask ~tail =
  let head =
    String.concat ""
      [ "\xb8\x0c\x00" (* mov ax,0x000C *)
      ; Printf.sprintf "\xb9%c%c" (Char.chr (mask land 0xff))
          (Char.chr ((mask lsr 8) land 0xff)) (* mov cx,mask *)
      ; "\xba\x20\x01" (* mov dx,0x0120 — 핸들러 *)
      ; "\x0e\x07" (* push cs; pop es *)
      ; "\xcd\x33" (* int 0x33 *)
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

let () =
  (* 왼쪽 누름 등록: 이동+누름이 닿아 핸들러가 돈다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0002 ~tail:tail_events);
  Dos_machine.run m ~max_steps:500;
  checkb "press: still spinning before the event" (Dos_machine.exited m) false;
  Dos_machine.set_mouse m ~x:100 ~y:50 ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "press: handler ran and guest exited" (Dos_machine.exited m) true;
  checki "press: AX = move|left-press" (Dos_machine.exit_code m) 0x03;
  checki "press: BX = buttons" (peek_word m 0x142) 0x0001;
  checki "press: CX = x" (peek_word m 0x144) 100;
  checki "press: DX = y" (peek_word m 0x146) 50;
  checki "press: SI = dx" (peek_word m 0x148) 100;
  checki "press: DI = dy" (peek_word m 0x14a) 50;
  (* 이동량 소비: 호출이 si/di 를 가져가면 뒤이은 AX=0x0B 는 0 이다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0002 ~tail:tail_mickeys);
  Dos_machine.run m ~max_steps:500;
  Dos_machine.set_mouse m ~x:100 ~y:50 ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "mickeys: guest exited" (Dos_machine.exited m) true;
  checki "mickeys: AX=0x0B CX is 0 after the call" (Dos_machine.exit_code m) 0;
  (* 마스크 빗나감: 이동만 보는 등록에 누름은 닿지 않는다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0001 ~tail:tail_events);
  Dos_machine.run m ~max_steps:500;
  Dos_machine.set_mouse m ~x:0 ~y:0 ~buttons:1;
  Dos_machine.run m ~max_steps:500;
  checkb "mask: press alone does not call" (Dos_machine.exited m) false;
  checki "mask: flag still zero" (peek_word m 0x140) 0;
  Dos_machine.set_mouse m ~x:10 ~y:0 ~buttons:1;
  Dos_machine.run m ~max_steps:2000;
  checkb "mask: move then calls" (Dos_machine.exited m) true;
  checki "mask: AX = move" (Dos_machine.exit_code m) 0x01;
  checki "mask: CX = x" (peek_word m 0x144) 10;
  checki "mask: SI = dx since press" (peek_word m 0x148) 10;
  (* 등록은 스냅샷을 타고: 저장→복원 뒤 누름이 닿는다. *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m (guest ~mask:0x0002 ~tail:tail_events);
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
