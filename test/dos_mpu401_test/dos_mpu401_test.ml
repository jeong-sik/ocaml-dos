(* The MPU-401 UART-only model, driven the way a game's music driver
   drives the card: poll 331h, write a command, read the answer from 330h,
   then write MIDI bytes. *)

module M = Dos_mpu401

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %02Xh want %02Xh\n%!" name got want
  end

let check_b name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* 331h with a byte waiting in 330h, and with none *)
let byte_waiting = 0x3F
let nothing_waiting = 0xBF
let ack = 0xFE

let status t = Option.get (M.port_in t M.status_port)
let data t = Option.get (M.port_in t M.data_port)
let command t v = check_b "a command is taken" (M.port_out t M.status_port v = Some M.Taken)

(* A detection loop: the answer it reads, if a byte was waiting. *)
let answer t = if status t = byte_waiting then Some (data t) else None

let () =
  let t = M.create () in
  check "power-on status" (status t) nothing_waiting;
  check "an empty 330h reads FEh" (data t) ack;
  check "and stays empty" (status t) nothing_waiting;
  (* the detection a music driver runs: FFh, FEh, 3Fh, FEh *)
  command t 0xFF;
  check_b "FFh answers FEh" (answer t = Some ack);
  check "the answer was taken" (status t) nothing_waiting;
  command t 0x3F;
  check_b "3Fh answers FEh" (answer t = Some ack);
  check_b "MIDI out" (M.port_out t M.data_port 0x90 = Some (M.Midi_out 0x90));
  check_b "MIDI out, a data byte" (M.port_out t M.data_port 0x3C = Some (M.Midi_out 0x3C));
  check "MIDI out leaves nothing to read" (status t) nothing_waiting;
  (* UART mode takes only FFh *)
  command t 0x3F;
  check_b "3Fh again: no answer" (answer t = None);
  command t 0xAC;
  check_b "another command in UART mode: no answer" (answer t = None);
  command t 0xFF;
  check_b "FFh in UART mode: no answer" (answer t = None);
  command t 0x3F;
  check_b "back in command mode, 3Fh answers" (answer t = Some ack);
  (* a program that finds the card in UART mode resets twice *)
  command t 0xFF;
  command t 0xFF;
  check_b "the second FFh answers" (answer t = Some ack);
  check "one answer" (status t) nothing_waiting;
  (* command mode ignores the intelligent-mode commands *)
  List.iter
    (fun c ->
      command t c;
      check_b (Printf.sprintf "%02Xh: no answer" c) (answer t = None))
    [ 0xAC; 0xAD; 0x8A; 0xE0; 0x3E; 0x00 ];
  check_b "MIDI out in command mode too" (M.port_out t M.data_port 0xF8 = Some (M.Midi_out 0xF8));
  (* a reset drops the waiting byte before it answers *)
  command t 0xFF;
  command t 0xFF;
  check "two resets unread: one byte waits" (status t) byte_waiting;
  ignore (data t);
  check "after one read" (status t) nothing_waiting;
  (* 3Fh adds to a waiting FEh; a UART-mode reset drops both *)
  command t 0xFF;
  command t 0x3F;
  ignore (data t);
  check "3Fh's answer still waits" (status t) byte_waiting;
  ignore (data t);
  check "both read" (status t) nothing_waiting;
  command t 0xFF;
  command t 0xFF;
  command t 0x3F;
  command t 0xFF;
  check "a UART-mode reset drops the waiting answers" (status t) nothing_waiting;
  (* other ports *)
  check_b "332h is not the card's" (M.port_in t 0x332 = None);
  check_b "32Fh is not the card's" (M.port_out t 0x32F 0xFF = None);
  check_b "a byte to another port leaves it" (answer t = None)

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

let save t =
  let w = C.writer () in
  M.write_state w t;
  C.contents w

let load s =
  let t = M.create () in
  let r = C.reader s in
  M.read_state r t;
  C.end_of_input r;
  t

let () =
  (* UART mode with two answers waiting *)
  let t = M.create () in
  command t 0xFF;
  command t 0x3F;
  let u = load (save t) in
  check "restored: a byte waits" (status u) byte_waiting;
  ignore (data u);
  ignore (data u);
  check "restored: two bytes" (status u) nothing_waiting;
  command u 0x3F;
  check_b "restored: still UART mode" (answer u = None);
  command u 0xFF;
  command u 0x3F;
  check_b "restored: reset back to command mode" (answer u = Some ack);
  (* counts no card reaches *)
  let crafted uart n =
    let w = C.writer () in
    C.put_bool w uart;
    C.put w ~what:"count" (C.range 0 9) n;
    C.contents w
  in
  let refused what s =
    match load s with
    | _ -> check_b (what ^ ": refused") false
    | exception C.Invalid _ -> ()
  in
  refused "command mode with two bytes" (crafted false 2);
  refused "UART mode with three bytes" (crafted true 3);
  check_b "command mode with one byte" (status (load (crafted false 1)) = byte_waiting);
  check_b "UART mode with two bytes" (status (load (crafted true 2)) = byte_waiting)

(* ---------- in the machine ---------- *)

let ports m = m.Dos_state.ports

(* A guest that finds the card the way a music driver does:
     331h <- FFh; wait 331h bit 7 clear; [r0] <- 330h
     331h <- 3Fh; wait 331h bit 7 clear; [r1] <- 330h
     330h <- 90h (MIDI note on); [r2] <- 331h; int 20h *)
let guest_program =
  "\xBA\x31\x03\xB0\xFF\xEE\xEC\xA8\x80\x75\xFB\x4A\xEC\xA2\x28\x01"
  ^ "\x42\xB0\x3F\xEE\xEC\xA8\x80\x75\xFB\x4A\xEC\xA2\x29\x01\xB0\x90"
  ^ "\xEE\x42\xEC\xA2\x2A\x01\xCD\x20\x00\x00\x00"

let r0 = 0x128
let r1 = 0x129
let r2 = 0x12A
let floating = 0xFF

let () =
  let m = Dos_machine.create () in
  check "no card by default: 331h floats" (Dos_ports.port_in (ports m) M.status_port) floating;
  check "no card by default: 330h floats" (Dos_ports.port_in (ports m) M.data_port) floating;
  Dos_ports.port_out (ports m) M.status_port 0xFF;
  check "no card: a reset gets nothing" (Dos_ports.port_in (ports m) M.status_port) floating;
  let m = Dos_machine.create ~mpu401:true () in
  Dos_machine.load_com m guest_program;
  let base = Cpu86.seg m.Dos_state.cpu 3 lsl 4 in
  Dos_machine.run m ~max_steps:10_000;
  let at off = Char.code (Bytes.get m.Dos_state.mem (base + off)) in
  check "the guest's reset answered" (at r0) ack;
  check "the guest's 3Fh answered" (at r1) ack;
  check "the guest saw nothing waiting after a MIDI byte" (at r2) nothing_waiting;
  (* the guest left the card in UART mode: a snapshot keeps it there *)
  let p = ports m in
  let restored m =
    match Dos_snapshot.save m with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok r -> r
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  let u = ports (restored m) in
  Dos_ports.port_out u M.status_port 0x3F;
  check "restored in UART mode: 3Fh is ignored" (Dos_ports.port_in u M.status_port) nothing_waiting;
  Dos_ports.port_out p M.status_port 0xFF;
  Dos_ports.port_out p M.status_port 0xFF;
  let r = ports (restored m) in
  check "restored with the card: the answer waits" (Dos_ports.port_in r M.status_port) byte_waiting;
  check "restored: FEh" (Dos_ports.port_in r M.data_port) ack;
  Dos_ports.port_out r M.status_port 0x3F;
  check "restored: 3Fh answers in command mode" (Dos_ports.port_in r M.status_port) byte_waiting;
  let r = ports (restored (Dos_machine.create ())) in
  check "restored without the card" (Dos_ports.port_in r M.status_port) floating

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "MPU-401: all passed"
