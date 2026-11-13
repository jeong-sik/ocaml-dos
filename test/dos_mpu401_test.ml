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

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "MPU-401: all passed"
