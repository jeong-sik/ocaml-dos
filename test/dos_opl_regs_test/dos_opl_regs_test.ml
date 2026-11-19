(* RFC 0006 stage 4: the OPL2's voice registers are machine state. Writes
   through 388h/389h (and 228h/229h with a Sound Blaster) land in
   Dos_opl; the timer registers 02h-04h stay with the timers; a snapshot
   carries them. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %02Xh want %02Xh\n%!" name got want
  end

let opl_write m ~base reg v =
  let p = m.Dos_state.ports in
  Dos_ports.port_out p base reg;
  Dos_ports.port_out p (base + 1) v

let regs m = Dos_ports.opl m.Dos_state.ports

(* every voice register a game writes, with a value of its own *)
let voice = [ 0x01; 0x08; 0x20; 0x35; 0x40; 0x55; 0x60; 0x75; 0x80; 0x95; 0xA0; 0xA8; 0xB0; 0xB8; 0xBD; 0xC0; 0xC8; 0xE0; 0xF5 ]
let value reg = (reg * 7) land 0xFF

let test_ports () =
  let m = Dos_machine.create () in
  List.iter (fun reg -> opl_write m ~base:0x388 reg (value reg)) voice;
  List.iter (fun reg -> checki (Printf.sprintf "register %02Xh" reg) (Dos_opl.register (regs m) reg) (value reg)) voice;
  (* timers stay with the timers *)
  opl_write m ~base:0x388 0x02 0x80;
  opl_write m ~base:0x388 0x03 0x40;
  opl_write m ~base:0x388 0x04 0x60;
  List.iter (fun reg -> checki (Printf.sprintf "timer register %02Xh is not a voice" reg) (Dos_opl.register (regs m) reg) 0) [ 0x02; 0x03; 0x04 ];
  (* no register past F5h *)
  opl_write m ~base:0x388 0xF6 0x12;
  opl_write m ~base:0x388 0xFF 0x34;
  checki "F6h is no register" (Dos_opl.register (regs m) 0xF6) 0;
  checki "FFh is no register" (Dos_opl.register (regs m) 0xFF) 0;
  (* the Sound Blaster's alias *)
  let m = Dos_machine.create ~sound_blaster:true () in
  opl_write m ~base:0x228 0xB3 0x2A;
  checki "229h writes the same chip" (Dos_opl.register (regs m) 0xB3) 0x2A;
  let m = Dos_machine.create () in
  opl_write m ~base:0x228 0xB3 0x2A;
  checki "without a card 229h is nothing" (Dos_opl.register (regs m) 0xB3) 0

let test_snapshot () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m "\xcd\x20";
  for reg = 0 to Dos_opl.register_count - 1 do
    match reg with
    | 0x02 | 0x03 | 0x04 -> ()
    | _ -> opl_write m ~base:0x388 reg (value reg)
  done;
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok s -> (
    match Dos_snapshot.restore s with
    | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
    | Ok r ->
      let bad = ref 0 in
      for reg = 0 to Dos_opl.register_count - 1 do
        let want = match reg with 0x02 | 0x03 | 0x04 -> 0 | _ -> value reg in
        if Dos_opl.register (regs r) reg <> want then incr bad
      done;
      checki "every register comes back" !bad 0)

let () =
  test_ports ();
  test_snapshot ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos OPL registers: all passed"
