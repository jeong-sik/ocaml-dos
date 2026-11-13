(* The Sound Blaster 2.0 DSP on its own: ports, answers, timing, and
   blocks pulled through a DMA controller.

   Contract: a reset (1 then 0 on 226h) answers AAh 20µs later. Commands
   and their parameter bytes go to 22Ch; answers come from 22Ah in order.
   A block takes one byte from DMA channel 1 every 256 - TC µs, waits on a
   masked channel without bursting, and raises the IRQ when its last byte
   is taken. An IRQ not yet acknowledged through 22Eh swallows the next. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

let us n = n * Dos_clock.cpu_hz / 1_000_000

type rig = { sb : Dos_sb.t; dma : Dos_dma.t; mem : Bytes.t; mutable now : int }

let rig () =
  { sb = Dos_sb.create (); dma = Dos_dma.create (); mem = Bytes.make 0x100000 '\000'; now = 1_000 }

let reset_port = 0x226
let data_port = 0x22A
let write_port = 0x22C
let status_port = 0x22E

let inp r p =
  match Dos_sb.port_in r.sb p with
  | Some v -> v
  | None -> check (Printf.sprintf "port %X answers" p) false; -1

let out r p v =
  check (Printf.sprintf "port %X takes writes" p) (Dos_sb.port_out r.sb ~now:r.now p v)
let cmd r bytes = List.iter (out r write_port) bytes

(* Runs the DSP to cycle [at]; true when it raised the IRQ on the way. *)
let advance_to r at =
  r.now <- at;
  Dos_sb.advance r.sb ~now:at ~dma:r.dma ~mem:r.mem

let read_answers r =
  let rec go acc =
    if inp r status_port land 0x80 <> 0 then go (inp r data_port :: acc) else List.rev acc
  in
  go []

let check_answers name r want =
  let got = read_answers r in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got [%s] want [%s]\n%!" name
      (String.concat " " (List.map (Printf.sprintf "%02X") got))
      (String.concat " " (List.map (Printf.sprintf "%02X") want))
  end

(* DMA channel 1 set up to read [n] bytes at 1000h:0000h. *)
let dma_out r p v = ignore (Dos_dma.port_out r.dma p v)

let setup_dma ?(mode = 0x49) r n =
  dma_out r 0x0C 0;
  dma_out r 0x0B mode;
  dma_out r 0x02 0x00;
  dma_out r 0x02 0x00;
  dma_out r 0x03 ((n - 1) land 0xFF);
  dma_out r 0x03 ((n - 1) lsr 8);
  dma_out r 0x83 0x01;
  dma_out r 0x0A 0x01

let dma_count r =
  dma_out r 0x0C 0;
  let lo = Option.get (Dos_dma.port_in r.dma 0x03) in
  let hi = Option.get (Dos_dma.port_in r.dma 0x03) in
  (hi lsl 8) lor lo

(* 40h D3h: 45µs a byte. *)
let tc = 0xD3
let byte_time = us (256 - tc)
let set_rate r = cmd r [ 0x40; tc ]

let play_once r n = cmd r [ 0x14; (n - 1) land 0xFF; (n - 1) lsr 8 ]

(* Advances one byte time at a time; the steps at which the IRQ rose. *)
let irq_steps r steps =
  List.filter_map
    (fun i -> if advance_to r (r.now + byte_time) then Some i else None)
    (List.init steps (fun i -> i + 1))

let test_power_on () =
  let r = rig () in
  check_int "read status at power-on" (inp r status_port) 0x2A;
  check_int "write status at power-on" (inp r write_port) 0x2A;
  check_int "read data repeats AAh" (inp r data_port) 0xAA;
  check_int "226h reads FFh" (inp r reset_port) 0xFF;
  check "224h is not the DSP's" (Dos_sb.port_in r.sb 0x224 = None);
  check "228h writes are not the DSP's" (not (Dos_sb.port_out r.sb ~now:0 0x228 0));
  check "nothing scheduled" (Dos_sb.due_at r.sb = max_int)

let test_reset () =
  let r = rig () in
  cmd r [ 0xE1 ];
  out r reset_port 1;
  check_int "busy while held" (inp r write_port) 0xFF;
  check_int "reset drops answers" (inp r status_port) 0x2A;
  out r reset_port 0;
  let ready = r.now + us 20 in
  check_int "AAh is due 20us after release" (Dos_sb.due_at r.sb) ready;
  ignore (advance_to r (ready - 1));
  check_int "not yet" (inp r status_port) 0x2A;
  check_int "still busy" (inp r write_port) 0xFF;
  ignore (advance_to r ready);
  check_int "ready for commands" (inp r write_port) 0x2A;
  check_answers "reset answers AAh" r [ 0xAA ];
  check_int "read data repeats the last" (inp r data_port) 0xAA;
  out r reset_port 0;
  check "0 without a held reset does nothing" (Dos_sb.due_at r.sb = max_int)

(* A reset stops the block and forgets the speaker. *)
let test_reset_stops () =
  let r = rig () in
  setup_dma r 4;
  set_rate r;
  cmd r [ 0xD1 ];
  play_once r 4;
  out r reset_port 1;
  out r reset_port 0;
  check_int "only the reset is due" (Dos_sb.due_at r.sb) (r.now + us 20);
  check "no block after a reset" (irq_steps r 6 = []);
  check_int "DMA untouched" (dma_count r) 3;
  ignore (read_answers r);
  cmd r [ 0xD8 ];
  check_answers "speaker off after a reset" r [ 0x00 ]

let test_answers () =
  let r = rig () in
  cmd r [ 0xE1 ];
  check_int "an answer waits: 22Eh AAh" (inp r status_port) 0xAA;
  check_answers "E1h: DSP 2.01" r [ 0x02; 0x01 ];
  cmd r [ 0xE0; 0x5A ];
  check_answers "E0h inverts" r [ 0xA5 ];
  cmd r [ 0xE4; 0x33; 0xE8 ];
  check_answers "E4h/E8h test register" r [ 0x33 ];
  cmd r [ 0xD8 ];
  check_answers "speaker off after power-on" r [ 0x00 ];
  cmd r [ 0xD1; 0xD8 ];
  check_answers "D1h speaker on" r [ 0xFF ];
  cmd r [ 0xD3; 0xD8 ];
  check_answers "D3h speaker off" r [ 0x00 ];
  cmd r [ 0xF8 ];
  check_answers "F8h" r [ 0x00 ];
  cmd r [ 0x20 ];
  check_answers "20h silent input" r [ 0x7F ];
  cmd r [ 0xE1; 0xE1 ];
  check_answers "E1h flushes what was unread" r [ 0x02; 0x01 ]

(* 20h answers without flushing, so every 20h that runs shows. *)
let test_parameters () =
  let r = rig () in
  cmd r [ 0x10; 0x20 ];
  check_answers "10h swallows one byte" r [];
  cmd r [ 0x20 ];
  check_answers "then commands parse again" r [ 0x7F ];
  cmd r [ 0x74; 0x20; 0x20; 0x20 ];
  check_answers "74h (ADPCM, unmodelled) swallows two" r [ 0x7F ];
  cmd r [ 0x38; 0x20; 0x20 ];
  check_answers "38h (MIDI, unmodelled) swallows one" r [ 0x7F ];
  cmd r [ 0x30; 0x20 ];
  check_answers "30h takes none" r [ 0x7F ]

let test_trigger_irq () =
  let r = rig () in
  cmd r [ 0xF2 ];
  let due = r.now + us 10 in
  check_int "F2h fires 10us later" (Dos_sb.due_at r.sb) due;
  check "not before" (not (advance_to r (due - 1)));
  check "F2h raises the IRQ" (advance_to r due);
  check "once" (not (advance_to r (due + 1)));
  check "the line stays high" (Dos_sb.irq_line r.sb);
  cmd r [ 0xF2 ];
  check "an unacknowledged IRQ swallows the next" (not (advance_to r (r.now + us 10)));
  ignore (inp r status_port);
  check "22Eh drops the line" (not (Dos_sb.irq_line r.sb));
  cmd r [ 0xF2 ];
  check "22Eh acknowledges" (advance_to r (r.now + us 10));
  cmd r [ 0x1C ];
  check "1Ch drops the line" (not (Dos_sb.irq_line r.sb))

let test_single_block () =
  let r = rig () in
  setup_dma r 4;
  set_rate r;
  play_once r 4;
  check_int "first byte one byte time later" (Dos_sb.due_at r.sb) (r.now + byte_time);
  let fired = irq_steps r 3 in
  check "no IRQ inside the block" (fired = []);
  check_int "DMA moved three bytes" (dma_count r) 0;
  check "IRQ on the last byte" (irq_steps r 1 = [ 1 ]);
  check "block over" (Dos_sb.due_at r.sb = max_int);
  check_int "DMA reached TC" (Option.get (Dos_dma.port_in r.dma 0x08)) 0x02

let test_masked_waits () =
  let r = rig () in
  setup_dma r 4;
  dma_out r 0x0A 0x05;
  set_rate r;
  play_once r 4;
  check "masked: no IRQ" (irq_steps r 10 = []);
  check_int "masked: DMA untouched" (dma_count r) 3;
  dma_out r 0x0A 0x01;
  ignore (advance_to r (r.now + 1));
  check_int "unmasked: one byte, no burst" (dma_count r) 2

let test_auto_init () =
  let r = rig () in
  setup_dma ~mode:0x59 r 3;
  set_rate r;
  cmd r [ 0x48; 0x02; 0x00; 0x1C ];
  check "IRQ every three bytes" (irq_steps r 3 = [ 3 ]);
  cmd r [ 0x14; 0x00; 0x00 ];
  check "a new transfer drops the line" (not (Dos_sb.irq_line r.sb));
  cmd r [ 0xDA; 0x48; 0x02; 0x00; 0x1C ];
  ignore (inp r status_port);
  check "and again" (irq_steps r 3 = [ 3 ]);
  check "lost without 22Eh" (irq_steps r 3 = []);
  ignore (inp r status_port);
  cmd r [ 0xDA ];
  check "DAh lets the block finish" (irq_steps r 3 = [ 3 ]);
  check "then stops" (Dos_sb.due_at r.sb = max_int)

let test_auto_to_single () =
  let r = rig () in
  setup_dma ~mode:0x59 r 3;
  set_rate r;
  cmd r [ 0x48; 0x02; 0x00; 0x1C ];
  ignore (irq_steps r 1);
  play_once r 2;
  check "the auto block ends first" (irq_steps r 2 = [ 2 ]);
  ignore (inp r status_port);
  check "then the queued single block" (irq_steps r 2 = [ 2 ]);
  check "then nothing" (Dos_sb.due_at r.sb = max_int)

let test_pause () =
  let r = rig () in
  setup_dma r 4;
  set_rate r;
  play_once r 4;
  ignore (irq_steps r 1);
  cmd r [ 0xD0 ];
  check "D0h pauses" (Dos_sb.due_at r.sb = max_int);
  check_int "paused DMA stays" (dma_count r) 2;
  r.now <- r.now + (100 * byte_time);
  cmd r [ 0xD4 ];
  check "D4h continues" (irq_steps r 3 = [ 3 ])

let test_silence () =
  let r = rig () in
  set_rate r;
  cmd r [ 0x80; 0x09; 0x00 ];
  check_int "80h: IRQ after ten byte times" (Dos_sb.due_at r.sb) (r.now + (10 * byte_time));
  check "80h raises the IRQ" (advance_to r (Dos_sb.due_at r.sb))

let test_time_constant () =
  let r = rig () in
  setup_dma r 2;
  cmd r [ 0x40; 0x9C; 0x14; 0x01; 0x00 ];
  check_int "40h 9Ch: 100us a byte" (Dos_sb.due_at r.sb) (r.now + us 100)

let test_snapshot () =
  let r = rig () in
  setup_dma ~mode:0x59 r 3;
  set_rate r;
  cmd r [ 0x48; 0x02; 0x00; 0x1C ];
  ignore (irq_steps r 1);
  cmd r [ 0xE1; 0x14; 0x01 ];   (* an answer unread, a command half in *)
  let w = Dos_snap_codec.writer () in
  Dos_sb.write_state w r.sb;
  let copy = { r with sb = Dos_sb.create () } in
  Dos_sb.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) copy.sb;
  check_int "restored due time" (Dos_sb.due_at copy.sb) (Dos_sb.due_at r.sb);
  out copy write_port 0x00;    (* the high byte: a two-byte block *)
  let w = Dos_snap_codec.writer () in
  Dos_sb.write_state w copy.sb;
  let again = { copy with sb = Dos_sb.create () } in
  Dos_sb.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) again.sb;
  check "restored auto block ends" (irq_steps again 2 = [ 2 ]);
  ignore (inp again status_port);
  check "restored queued block" (irq_steps again 2 = [ 2 ]);
  check "restored: then nothing" (Dos_sb.due_at again.sb = max_int);
  check_answers "restored answers" again [ 0x02; 0x01 ]

(* ---------- In the machine ---------- *)

let ports m = m.Dos_state.ports

let test_machine_without_card () =
  let m = Dos_machine.create () in
  check "no card by default" (Dos_ports.sound_blaster (ports m) = None);
  check_int "no card: 22Eh floats" (Dos_ports.port_in (ports m) 0x22E) 0xFF;
  check_int "no card: 228h floats" (Dos_ports.port_in (ports m) 0x228) 0xFF

(* The card's 228h/229h reach the OPL2 at 388h/389h: arm timer 1 through
   the card's ports, read it expire through 388h (dos_opl_test's sequence). *)
let test_machine_fm_alias () =
  let m = Dos_machine.create ~sound_blaster:true () in
  let p = ports m in
  let write reg dat = Dos_ports.port_out p 0x228 reg; Dos_ports.port_out p 0x229 dat in
  write 0x04 0x60;
  write 0x04 0x80;
  check_int "228h reads the quiet OPL2" (Dos_ports.port_in p 0x228) 0x00;
  write 0x02 0xFF;
  write 0x04 0x21;
  Dos_ports.set_now p 900_000;
  check_int "388h sees the timer armed through 229h" (Dos_ports.port_in p 0x388 land 0xE0) 0xC0

(* A guest that finds the card the way detection code does, then plays a
   block:
     hook INT 0Fh; unmask IRQ 7; 226h <- 1, 0; wait 22Eh bit 7; [result] <- 22Ah
     wait 22Ch bit 7 clear; F2h; sti; wait for [flag]; [result1] <- [flag]
     [flag] <- 0; DMA 1: 4 bytes at 1000:0000; 40h D3h; 14h 03h 00h
     wait for [flag]; [result2] <- [flag]; int 20h
   handler: in al,22Eh; inc cs:[flag]; EOI; iret *)
let guest_program =
  "\xB8\x0F\x25\xBA\x8C\x01\xCD\x21\xE4\x21\x24\x7F\xE6\x21\xBA\x26"
  ^ "\x02\xB0\x01\xEE\xFE\xC8\xEE\xB2\x2E\xEC\xA8\x80\x74\xFB\xB2\x2A"
  ^ "\xEC\xA2\x9F\x01\xB2\x2C\xEC\xA8\x80\x75\xFB\xB0\xF2\xEE\xFB\xB9"
  ^ "\xFF\xFF\x80\x3E\x9E\x01\x00\x75\x02\xE2\xF7\xA0\x9E\x01\xA2\xA0"
  ^ "\x01\xC6\x06\x9E\x01\x00\xB0\x05\xE6\x0A\xE6\x0C\xB0\x49\xE6\x0B"
  ^ "\xB0\x00\xE6\x02\xE6\x02\xB0\x03\xE6\x03\xB0\x00\xE6\x03\xB0\x01"
  ^ "\xE6\x83\xB0\x01\xE6\x0A\xBA\x2C\x02\xB0\x40\xEE\xB0\xD3\xEE\xB0"
  ^ "\x14\xEE\xB0\x03\xEE\x30\xC0\xEE\xB9\xFF\xFF\x80\x3E\x9E\x01\x00"
  ^ "\x75\x02\xE2\xF7\xA0\x9E\x01\xA2\xA1\x01\xCD\x20\x50\x52\xBA\x2E"
  ^ "\x02\xEC\x2E\xFE\x06\x9E\x01\xB0\x20\xE6\x20\x5A\x58\xCF\x00\x00"
  ^ "\x00\x00"

let flag = 0x19E
let result = 0x19F
let result_irq = 0x1A0
let result_block = 0x1A1

let test_machine_guest () =
  let m = Dos_machine.create ~sound_blaster:true () in
  Dos_machine.load_com m guest_program;
  let base = Cpu86.seg m.Dos_state.cpu 3 lsl 4 in
  Dos_machine.run m ~max_steps:200_000;
  let at off = Char.code (Bytes.get m.Dos_state.mem (base + off)) in
  check_int "guest read AAh after the reset" (at result) 0xAA;
  check_int "F2h reached the guest's IRQ 7 handler once" (at result_irq) 1;
  check_int "the block's end reached it once" (at result_block) 1;
  check_int "the handler acknowledged" (at flag) 1;
  match Dos_ports.sound_blaster (ports m) with
  | None -> check "card present" false
  | Some sb -> check "line low after 22Eh" (not (Dos_sb.irq_line sb))

(* F2h with interrupts off, acknowledged through 22Eh before any STI: the
   PIC request goes away with the line, so no stale IRQ 7 comes later. *)
let test_machine_withdrawn_request () =
  let m = Dos_machine.create ~sound_blaster:true () in
  let p = ports m in
  let pic = Dos_ports.pic p in
  Dos_ports.port_out p 0x22C 0xF2;
  let sb = Option.get (Dos_ports.sound_blaster p) in
  let due = Dos_sb.due_at sb in
  Cpu86.set_flags m.Dos_state.cpu (Cpu86.flags m.Dos_state.cpu land lnot Cpu86.f_interrupt);
  Dos_machine.load_com m "\xFA\xEB\xFE";   (* cli; jmp $ *)
  Dos_machine.run m ~max_steps:2_000;
  check "IRQ 7 asked" (Cpu86.cycles m.Dos_state.cpu >= due && Dos_pic.requested pic Dos_pic.Sound_blaster);
  ignore (Dos_ports.port_in p 0x22E);
  check "22Eh withdrew the request" (not (Dos_pic.requested pic Dos_pic.Sound_blaster))

(* A snapshot keeps the card and its DSP. *)
let test_machine_snapshot () =
  let m = Dos_machine.create ~sound_blaster:true () in
  Dos_ports.port_out (ports m) 0x22C 0xE1;
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok s ->
    (match Dos_snapshot.restore s with
     | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
     | Ok r ->
       check "restored machine has the card" (Dos_ports.sound_blaster (ports r) <> None);
       check_int "restored answer" (Dos_ports.port_in (ports r) 0x22A) 0x02);
    match Dos_snapshot.save (Dos_machine.create ()) with
    | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
       | Ok r -> check "restored machine without the card" (Dos_ports.sound_blaster (ports r) = None))

let () =
  test_power_on ();
  test_reset ();
  test_reset_stops ();
  test_answers ();
  test_parameters ();
  test_trigger_irq ();
  test_single_block ();
  test_masked_waits ();
  test_auto_init ();
  test_auto_to_single ();
  test_pause ();
  test_silence ();
  test_time_constant ();
  test_snapshot ();
  test_machine_without_card ();
  test_machine_fm_alias ();
  test_machine_guest ();
  test_machine_withdrawn_request ();
  test_machine_snapshot ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos sound blaster: all passed\n"
