(* The slave 8259 of a PC/AT: IRQ8-15 on vectors 70h-77h, ports A0h/A1h,
   cascaded into the master's IR2. The master's own commands are checked in
   dos_keyboard_test (test E). *)

open Dos_pic

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

let ocw3_read_isr = 0x0B
let ocw3_read_irr = 0x0A
let eoi = 0x20
let specific_eoi_0 = 0x60
let icw1 = 0x11
let cascade = 0x04

let isr p c = write_command p c ocw3_read_isr; let v = read_command p c in write_command p c ocw3_read_irr; v

(* ---------- the cascade ---------- *)

let test_cascade () =
  checki "IRQ8 is vector 70h" (vector Rtc) 0x70;
  let p = create () in
  raise_irq p Rtc;
  check "a slave line asks" (pending p && next p = Some Rtc);
  checki "the master's IRR shows the cascade" (read_command p Master) cascade;
  checki "the slave's IRR" (read_command p Slave) 0x01;
  acknowledge p Rtc;
  checki "slave in service" (isr p Slave) 0x01;
  checki "the master's IR2 is in service too" (isr p Master) cascade;
  checki "a taken line leaves the IRRs" (read_command p Master lor read_command p Slave) 0;
  (* IR2 in service shadows IRQ3-7 and the slave, not IRQ0-1 *)
  raise_irq p Sound_blaster;
  check "IRQ7 waits behind IR2" (next p = None);
  raise_irq p Rtc;
  check "the slave waits for its own EOI" (next p = None);
  checki "a shadowed slave line does not drive IR2" (read_command p Master land cascade) 0;
  raise_irq p Timer;
  check "the timer preempts a slave handler" (next p = Some Timer);
  lower_irq p Timer;
  write_command p Slave eoi;
  checki "the slave EOI clears the slave" (isr p Slave) 0;
  check "until the master's EOI, IR2 still shadows" (next p = None);
  write_command p Master eoi;
  checki "the master EOI clears IR2" (isr p Master) 0;
  check "IRQ8 ranks above IRQ7" (next p = Some Rtc);
  raise_irq p Keyboard;
  check "IRQ1 ranks above IRQ8" (next p = Some Keyboard)

let test_masks () =
  let p = create () in
  raise_irq p Rtc;
  write_mask p Master cascade;
  check "masking IR2 hides the slave" ((not (pending p)) && next p = None);
  checki "the master's mask reads back" (read_mask p Master) cascade;
  write_mask p Master 0;
  write_mask p Slave 0x01;
  check "masking IRQ8 on the slave" ((not (pending p)) && next p = None);
  checki "the slave's mask reads back" (read_mask p Slave) 0x01;
  checki "a masked slave line does not reach IR2" (read_command p Master) 0;
  write_mask p Slave 0;
  acknowledge p Rtc;
  write_command p Slave specific_eoi_0;
  checki "a specific EOI on the slave's level 0" (isr p Slave) 0;
  checki "it leaves the master's IR2" (isr p Master) cascade;
  write_mask p Slave 0x05;
  write_command p Slave icw1;
  checki "an ICW1 on the slave changes nothing" (read_mask p Slave) 0x05

(* ---------- through the machine ---------- *)

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let psp = 0x10000
let count_at = 0x300
let handler_at = 0x400
let mask_at = 0x302
let budget = 2_000

(* Reads back A1h after writing 0FEh, then waits with interrupts on. *)
let main_program =
  "\xb0\xfe\xe6\xa1"          (* mov al,0FEh; out 0A1h,al *)
  ^ "\xe4\xa1\xa2" ^ w16 mask_at (* in al,0A1h; mov [mask],al *)
  ^ "\xb0\x00\xe6\xa1"        (* mov al,0; out 0A1h,al *)
  ^ "\xfb\xeb\xfe"            (* sti; jmp $ *)

(* INT 70h: count, EOI the slave, then the master. *)
let handler =
  "\x50"
  ^ "\x2e\xfe\x06" ^ w16 count_at  (* inc byte cs:[count] *)
  ^ "\xb0\x20\xe6\xa0\xe6\x20"      (* mov al,20h; out 0A0h,al; out 20h,al *)
  ^ "\x58\xcf"

let test_machine () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m main_program;
  String.iteri (fun i c -> Dos_state.wr8 m (psp + handler_at + i) (Char.code c)) handler;
  Dos_state.wr16 m (vector Rtc * 4) handler_at;
  Dos_state.wr16 m ((vector Rtc * 4) + 2) (psp / 16);
  ignore (Dos_machine.run_until m ~max_steps:budget ~stop:(fun _ -> false));
  checki "A1h reads back the slave's mask" (Dos_state.rd8 m (psp + mask_at)) 0xFE;
  let pic = Dos_ports.pic m.Dos_state.ports in
  raise_irq pic Rtc;
  ignore (Dos_machine.run_until m ~max_steps:budget ~stop:(fun _ -> false));
  checki "IRQ8 reached INT 70h once" (Dos_state.rd8 m (psp + count_at)) 1;
  checki "the handler's EOIs left nothing in service" (isr pic Slave lor isr pic Master) 0;
  raise_irq pic Rtc;
  ignore (Dos_machine.run_until m ~max_steps:budget ~stop:(fun _ -> false));
  checki "and again" (Dos_state.rd8 m (psp + count_at)) 2;
  (* the snapshot carries the slave chip *)
  write_mask pic Slave 0x01;
  raise_irq pic Rtc;
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok s -> (
    match Dos_snapshot.restore s with
    | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
    | Ok r ->
      let rp = Dos_ports.pic r.Dos_state.ports in
      checki "restored slave mask" (read_mask rp Slave) 0x01;
      check "restored slave request" (requested rp Rtc);
      write_mask rp Slave 0;
      ignore (Dos_machine.run_until r ~max_steps:budget ~stop:(fun _ -> false));
      checki "the restored request reaches INT 70h" (Dos_state.rd8 r (psp + count_at)) 3)

let () =
  test_cascade ();
  test_masks ();
  test_machine ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos PIC: all passed"
