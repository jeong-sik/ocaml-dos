(* The keyboard path a PC/AT has: a key's make and break bytes in port
   0x60, IRQ1 for each through the 8259 to INT 9, and the BIOS INT 9
   putting the key's word in the ring. Programs small enough to know
   exactly, assembled by hand; offsets are from 0x100. *)

let failed = ref 0

let check name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine image =
  let m = Dos_machine.create () in
  Dos_machine.load_com m image;
  m

let run m n = ignore (Dos_machine.run_until m ~max_steps:n ~stop:(fun _ -> false))
let run_to m n stop = Dos_machine.run_until m ~max_steps:n ~stop

let code_byte m off = Dos_state.rd8 m ((m.Dos_state.psp_seg lsl 4) + off)

let hold = Dos_clock.cpu_hz / 10
let byte_time = Dos_clock.cpu_hz / 1000

(* Enough steps for a key to go down and up while the program spins on
   "jmp $" (15 cycles a step): 100 ms is about 32,000 of them. *)
let tap_steps = 200_000

(* A: hook INT 9, keep every byte port 0x60 gives in a list, EOI, IRET.
   It never chains: the BIOS never sees a key. *)
let count_off = 0x124
let seen_off = 0x126

let recorder =
  String.concat ""
    [ "\xb8\x09\x25";             (* 0x100 mov ax,2509h *)
      "\xba\x0a\x01";             (* 0x103 mov dx,010Ah *)
      "\xcd\x21";                 (* 0x106 int 21h *)
      "\xeb\xfe";                 (* 0x108 jmp $ *)
      "\x50";                     (* 0x10a push ax *)
      "\x53";                     (* 0x10b push bx *)
      "\xe4\x60";                 (* 0x10c in al,60h *)
      "\x2e\x8b\x1e\x24\x01";     (* 0x10e mov bx,cs:[0124h] *)
      "\x2e\x88\x87\x26\x01";     (* 0x113 mov cs:[bx+0126h],al *)
      "\x2e\xff\x06\x24\x01";     (* 0x118 inc word cs:[0124h] *)
      "\xb0\x20";                 (* 0x11d mov al,20h *)
      "\xe6\x20";                 (* 0x11f out 20h,al *)
      "\x5b";                     (* 0x121 pop bx *)
      "\x58";                     (* 0x122 pop ax *)
      "\xcf";                     (* 0x123 iret *)
      String.make 18 '\000' ]     (* 0x124 count, 0x126 bytes *)

let seen m = List.init (code_byte m count_off) (fun i -> code_byte m (seen_off + i))
let installed m = run m 10

let test_hook_sees_make_then_break () =
  let m = machine recorder in
  installed m;
  let pressed_at = Cpu86.cycles (Dos_machine.cpu_of m) in
  Dos_machine.push_key m 0x1E61;
  let r = run_to m tap_steps (fun m -> code_byte m count_off = 1) in
  check "the make code arrives at once" (r.stop_reason = Dos_machine.Stop_requested);
  check "the make code is the scan code" (seen m = [ 0x1E ]);
  check "the controller handed the key over" (not (Dos_machine.key_unread m));
  let r = run_to m tap_steps (fun m -> code_byte m count_off = 2) in
  let released_at = Cpu86.cycles (Dos_machine.cpu_of m) in
  check "the break code follows" (r.stop_reason = Dos_machine.Stop_requested);
  check "make, then break" (seen m = [ 0x1E; 0x9E ]);
  check "the key stays down 100 ms"
    (released_at - pressed_at >= hold && released_at - pressed_at < hold + byte_time);
  run m tap_steps;
  check "nothing more comes" (seen m = [ 0x1E; 0x9E ]);
  check "a hook that does not chain keeps the key from the BIOS"
    (not (Dos_state.key_pending m))

let test_rollover () =
  let m = machine recorder in
  installed m;
  Dos_machine.push_key m 0x1E61;
  Dos_machine.push_key m 0x3062;
  ignore (run_to m tap_steps (fun m -> code_byte m count_off = 1));
  let first = Cpu86.cycles (Dos_machine.cpu_of m) in
  ignore (run_to m tap_steps (fun m -> code_byte m count_off = 2));
  check "the next byte waits the 1 ms a keyboard takes to send one"
    (Cpu86.cycles (Dos_machine.cpu_of m) - first >= byte_time);
  run m tap_steps;
  check "two keys pressed together: both makes, then both breaks"
    (seen m = [ 0x1E; 0x30; 0x9E; 0xB0 ])

let test_upper_case_is_shifted () =
  let m = machine recorder in
  installed m;
  Dos_machine.push_key m 0x1E41;
  run m tap_steps;
  check "shift down, A down, A up, shift up" (seen m = [ 0x2A; 0x1E; 0x9E; 0xAA ])

(* B: the same hook, but it chains to the vector it found, as most do; the
   program then reads the key with INT 16h AH=00 and exits. *)
let chained_result_off = 0x13f
let chained_count_off = 0x141
let chained_seen_off = 0x143

let chainer =
  String.concat ""
    [ "\xb8\x09\x35";             (* 0x100 mov ax,3509h *)
      "\xcd\x21";                 (* 0x103 int 21h -> es:bx *)
      "\x89\x1e\x3b\x01";         (* 0x105 mov [013Bh],bx *)
      "\x8c\x06\x3d\x01";         (* 0x109 mov [013Dh],es *)
      "\xb8\x09\x25";             (* 0x10d mov ax,2509h *)
      "\xba\x21\x01";             (* 0x110 mov dx,0121h *)
      "\xcd\x21";                 (* 0x113 int 21h *)
      "\xb4\x00";                 (* 0x115 mov ah,0 *)
      "\xcd\x16";                 (* 0x117 int 16h *)
      "\xa3\x3f\x01";             (* 0x119 mov [013Fh],ax *)
      "\xb8\x00\x4c";             (* 0x11c mov ax,4C00h *)
      "\xcd\x21";                 (* 0x11f int 21h *)
      "\x50";                     (* 0x121 push ax *)
      "\x53";                     (* 0x122 push bx *)
      "\xe4\x60";                 (* 0x123 in al,60h *)
      "\x2e\x8b\x1e\x41\x01";     (* 0x125 mov bx,cs:[0141h] *)
      "\x2e\x88\x87\x43\x01";     (* 0x12a mov cs:[bx+0143h],al *)
      "\x2e\xff\x06\x41\x01";     (* 0x12f inc word cs:[0141h] *)
      "\x5b";                     (* 0x134 pop bx *)
      "\x58";                     (* 0x135 pop ax *)
      "\x2e\xff\x2e\x3b\x01";     (* 0x136 jmp far cs:[013Bh] *)
      String.make 24 '\000' ]     (* 0x13b old vector, 0x13f AX, 0x141 count, 0x143 bytes *)

let test_hook_chains_to_bios () =
  let m = machine chainer in
  run m 100;
  check "the program waits for a key" (not (Dos_machine.exited m));
  Dos_machine.push_key m 0x1E61;
  run m tap_steps;
  check "the program read a key and exited" (Dos_machine.exited m);
  check "the hook saw the make code first" (code_byte m chained_seen_off = 0x1E);
  check "INT 16h answers the word the host pressed"
    (code_byte m chained_result_off = 0x61 && code_byte m (chained_result_off + 1) = 0x1E);
  check "the hook ran once per byte it saw" (code_byte m chained_count_off >= 1)

(* C: no hook. The BIOS INT 9 keeps the shift state in 0040:0017. *)
let spin = "\xeb\xfe"
let bda_shift = 0x417
let left_shift = 0x02

let test_bios_shift_state () =
  let m = machine spin in
  Dos_machine.push_key m 0x1E41;
  let r = run_to m tap_steps (fun m -> Dos_state.key_pending m) in
  check "the BIOS put the key in the ring" (r.stop_reason = Dos_machine.Stop_requested);
  check "the ring holds the word pressed" (Dos_state.peek_key m = 0x1E41);
  check "shift is down while A is" (Dos_state.rd8 m bda_shift land left_shift <> 0);
  run m tap_steps;
  check "shift is up after" (Dos_state.rd8 m bda_shift land left_shift = 0);
  check "the key is still in the ring for the program" (Dos_state.peek_key m = 0x1E41)

(* D: IRQ1 masked; the program polls port 0x64 bit 0, then reads 0x60. *)
let polled_off = 0x116

let poller =
  String.concat ""
    [ "\xe4\x21";                 (* 0x100 in al,21h *)
      "\x0c\x02";                 (* 0x102 or al,2 *)
      "\xe6\x21";                 (* 0x104 out 21h,al ; mask IRQ1 *)
      "\xe4\x64";                 (* 0x106 in al,64h *)
      "\xa8\x01";                 (* 0x108 test al,1 *)
      "\x74\xfa";                 (* 0x10a jz 0106h *)
      "\xe4\x60";                 (* 0x10c in al,60h *)
      "\xa2\x16\x01";             (* 0x10e mov [0116h],al *)
      "\xb8\x00\x4c";             (* 0x111 mov ax,4C00h *)
      "\xcd\x21";                 (* 0x114 int 21h *)
      "\x00" ]                    (* 0x116 the byte *)

let test_polling_with_irq1_masked () =
  let m = machine poller in
  run m 100;
  check "the program polls an empty controller" (not (Dos_machine.exited m));
  Dos_machine.push_key m 0x1E61;
  run m tap_steps;
  check "the poll found the make code" (Dos_machine.exited m && code_byte m polled_off = 0x1E);
  check "a masked IRQ1 never reached the BIOS" (not (Dos_state.key_pending m))

(* E: the controller itself. *)
let test_pic () =
  let open Dos_pic in
  let p = create () in
  raise_irq p Keyboard;
  raise_irq p Timer;
  check "the timer comes first" (next p = Some Timer);
  acknowledge p Timer;
  check "a line in service shadows the lines below it" (next p = None);
  write_command p 0x0B;
  check "OCW3 reads ISR" (read_command p = 0x01);
  write_command p 0x0A;
  check "OCW3 reads IRR" (read_command p = 0x02);
  write_command p 0x20;
  check "a non-specific EOI" (next p = Some Keyboard);
  acknowledge p Keyboard;
  raise_irq p Timer;
  check "the timer preempts a keyboard handler" (next p = Some Timer);
  write_mask p 0x01;
  check "the mask hides a line" (next p = None);
  check "the mask reads back" (read_mask p = 0x01);
  write_command p 0x61;
  write_command p 0x0B;
  check "a specific EOI on IRQ1" (read_command p = 0x00);
  write_command p 0x11;
  check "an ICW1 changes nothing" (read_mask p = 0x01 && read_command p = 0x00)

(* F: a snapshot taken while bytes are still in the controller. *)
let save m = match Dos_snapshot.save m with
  | Ok s -> s | Error e -> failwith (Dos_snapshot.save_error_to_string e)
let restore s = match Dos_snapshot.restore s with
  | Ok m -> m | Error e -> failwith (Dos_snapshot.error_to_string e)

let test_snapshot_mid_key () =
  let m = machine spin in
  Dos_machine.push_key m 0x1E61;
  Dos_machine.push_key m 0x3062;
  run m 50;
  check "a key is still in the controller" (Dos_machine.key_unread m);
  let resumed = restore (save m) in
  List.iter (fun current -> run current tap_steps) [ m; resumed ];
  check "both machines got both keys"
    (List.for_all (fun c -> Dos_state.pop_key c = 0x1E61 && Dos_state.pop_key c = 0x3062)
       [ m; resumed ]);
  check "the restored machine ran the same" (save m = save resumed)

let () =
  test_hook_sees_make_then_break ();
  test_rollover ();
  test_upper_case_is_shifted ();
  test_hook_chains_to_bios ();
  test_bios_shift_state ();
  test_polling_with_irq1_masked ();
  test_pic ();
  test_snapshot_mid_key ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos keyboard: all passed"
