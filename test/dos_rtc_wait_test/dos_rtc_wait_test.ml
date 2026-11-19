(* INT 15h AH=83h·86h — waits counted by the RTC's periodic interrupt
   (SeaBIOS src/clock.c). Each IRQ8 at 1024 Hz takes 977 us off the time
   left; with less than 977 us left, INT 70h sets bit 7 of the flag byte and
   ends the wait. So a wait of N us ends at IRQ8 number N / 977 + 1. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04Xh want %04Xh\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let regs ?(bx = 0) ~cx ~dx ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx

let psp = 0x10000
let ax_at = 0x300
let flags_at = 0x302
let ax2_at = 0x304
let flag_byte = 0x310
let ticks_at = 0x312
let quit = "\xcd\x20"

(* mov [at],ax; pushf; pop word [at+2] *)
let save_result at = "\xa3" ^ w16 at ^ "\x9c\x8f\x06" ^ w16 (at + 2)
let int15 = "\xcd\x15"

let wait_us us = regs ~cx:(us lsr 16) ~dx:(us land 0xFFFF) 0x8600 ^ int15
let event us = "\x0e\x07" ^ regs ~bx:flag_byte ~cx:(us lsr 16) ~dx:(us land 0xFFFF) 0x8300 ^ int15
let cancel = regs ~cx:0 ~dx:0 0x8301 ^ int15

(* test byte [flag],80h; jz $-7 *)
let spin_on_flag = "\xf6\x06" ^ w16 flag_byte ^ "\x80\x74\xf9"

let budget = 20_000_000
let in_dos = (0x0070 * 16) + 1   (* SDA 0070:0001, dos_in_dos_test *)
let carry = 0x0001

let pc_hz = Dos_ports.speed_hz Dos_ports.pc_speed
let rtc_hz = 32768
let ticks_1024 = 32
let irq8_end k = ((k * ticks_1024 * pc_hz) + rtc_hz - 1) / rtc_hz
let usec_per_rtc = 977
let ending_irq us = (us / usec_per_rtc) + 1

let machine code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  m

let run m = ignore (Dos_machine.run_until m ~max_steps:budget ~stop:(fun _ -> false))
let word m at = Dos_state.rd16 m (psp + at)
let byte m at = Dos_state.rd8 m (psp + at)
let cycles m = Cpu86.cycles m.Dos_state.cpu

let cmos m r =
  Dos_ports.port_out m.Dos_state.ports 0x70 r;
  Dos_ports.port_in m.Dos_state.ports 0x71

let ended_near name m k =
  let c = cycles m and e = irq8_end k in
  check (Printf.sprintf "%s: ends at IRQ8 %d (%d cycles, end at %d)" name k c e)
    (c >= e && c < e + 3000)

(* ---------- AH=86h ---------- *)

let test_wait () =
  let us = 5000 in
  let m = machine (wait_us us ^ save_result ax_at ^ quit) in
  run m;
  check "86h: the program ends" (Dos_machine.exited m);
  ended_near "86h 5000us" m (ending_irq us);
  check "86h: CF clear" (word m flags_at land carry = 0);
  checki "86h: AH is left as it was (SeaBIOS set_success)" (word m ax_at lsr 8) 0x86;
  checki "86h: no wait pending after" (Dos_state.rd8 m 0x4A0) 0;
  checki "86h: the end flag is 40:A1h bit 7" (Dos_state.rd8 m 0x4A1) 0x80;
  checki "86h: PIE is off again" (cmos m 0x0B land 0x40) 0;
  (* one IRQ8 per 977 us: under 977 ends at the first *)
  let m = machine (wait_us 976 ^ quit) in
  run m;
  ended_near "86h 976us" m 1;
  let m = machine (wait_us 977 ^ quit) in
  run m;
  ended_near "86h 977us" m 2;
  (* a second wait starts from a clear flag: 3 + 3 IRQ8 *)
  let m = machine (wait_us 2000 ^ wait_us 2000 ^ quit) in
  run m;
  ended_near "86h twice 2000us" m (2 * ending_irq 2000)

(* the timer keeps ticking through a wait: INT 1Ch counts *)
let test_irqs_during_wait () =
  let hook_1c =
    "\x31\xc0\x8e\xc0"                                          (* xor ax,ax; mov es,ax *)
    ^ "\xfa\x26\xc7\x06\x70\x00" ^ w16 0x400                     (* cli; mov es:[70h],400h *)
    ^ "\x26\x8c\x0e\x72\x00\xfb"                                 (* mov es:[72h],cs; sti *)
  in
  let code = hook_1c ^ wait_us 120_000 ^ quit in
  let handler = "\x2e\xfe\x06" ^ w16 ticks_at ^ "\xcf" in      (* inc byte cs:[ticks]; iret *)
  let image = code ^ String.make (0x400 - 0x100 - String.length code) '\x00' ^ handler in
  let m = machine image in
  run m;
  check "86h 120ms: ends" (Dos_machine.exited m);
  check (Printf.sprintf "86h 120ms: timer ticks arrived (%d)" (byte m ticks_at)) (byte m ticks_at >= 2)

(* a snapshot taken mid-wait ends the wait at the same cycle *)
let test_snapshot () =
  let us = 8000 in
  let code = wait_us us ^ quit in
  let straight = machine code in
  run straight;
  let m = machine code in
  ignore (Dos_machine.run_until m ~max_steps:budget ~stop:(fun m -> cycles m >= irq8_end 3));
  check "mid-wait: still running" (not (Dos_machine.exited m));
  checki "mid-wait: a BIOS wait is not inside DOS (INDOS)" (Dos_state.rd8 m in_dos) 0;
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok s -> (
    match Dos_snapshot.restore s with
    | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
    | Ok r ->
      run r;
      check "restored wait ends" (Dos_machine.exited r);
      checki "restored wait ends at the same cycle" (cycles r) (cycles straight))

(* INT 70h only counts a period that ended: called by hand before the
   first end, it leaves the time alone. *)
let test_int70_without_pf () =
  let us = 1_000_000 in
  let m = machine (event us ^ "\xcd\x70\xcd\x70" ^ cancel ^ quit) in
  run m;
  check "INT 70h by hand: ran before the first end" (cycles m < irq8_end 1);
  checki "INT 70h by hand: time left unchanged (low)" (Dos_state.rd16 m 0x49C) (us land 0xFFFF);
  checki "INT 70h by hand: time left unchanged (high)" (Dos_state.rd16 m 0x49E) (us lsr 16)

(* With no wait pending, IRQ8 through the BIOS leaves the flags alone. *)
let test_irq8_without_wait () =
  let code =
    wait_us 2000
    ^ "\x31\xc0\x8e\xc0\x26\xc6\x06\xa1\x04\x00"            (* es=0; mov byte es:[4A1h],0 *)
    ^ "\xb0\x0b\xe6\x70\xe4\x71\x0c\x40\xe6\x71"              (* B |= PIE *)
    ^ "\xb9\xff\xff\xe2\xfe"                                    (* mov cx,0FFFFh; loop $ *)
    ^ quit
  in
  let m = machine code in
  run m;
  check "PIE by the program: ends" (Dos_machine.exited m);
  check "PIE by the program: many IRQ8 went by" (cycles m > irq8_end 50);
  checki "PIE by the program: the old flag byte stays clear" (Dos_state.rd8 m 0x4A1) 0

(* ---------- AH=83h ---------- *)

let test_event () =
  let us = 2000 in
  let code =
    "\xc6\x06" ^ w16 flag_byte ^ "\x01"                         (* mov byte [flag],01h *)
    ^ event us ^ save_result ax_at ^ spin_on_flag ^ quit
  in
  let m = machine code in
  run m;
  check "83h: the program ends" (Dos_machine.exited m);
  check "83h: set returns at once with CF clear" (word m flags_at land carry = 0);
  ended_near "83h 2000us" m (ending_irq us);
  checki "83h: bit 7 is ORed into the flag byte" (byte m flag_byte) 0x81;
  checki "83h: no wait pending after" (Dos_state.rd8 m 0x4A0) 0;
  (* a second wait while one is pending *)
  let code =
    event 1_000_000 ^ event 1000 ^ save_result ax_at
    ^ wait_us 1000 ^ save_result ax2_at ^ cancel ^ quit
  in
  let m = machine code in
  run m;
  check "83h twice: CF set" (word m flags_at land carry <> 0);
  checki "83h twice: AH=86h" (word m ax_at lsr 8) 0x86;
  check "86h over 83h: CF set" (word m (ax2_at + 2) land carry <> 0);
  checki "86h over 83h: AH=83h" (word m ax2_at lsr 8) 0x83;
  checki "cancel: no wait pending" (Dos_state.rd8 m 0x4A0) 0;
  checki "cancel: PIE is off" (cmos m 0x0B land 0x40) 0;
  checki "cancel: the flag byte is never set" (byte m flag_byte) 0;
  (* an unknown AL *)
  let m = machine (regs ~cx:0 ~dx:0 0x8302 ^ int15 ^ save_result ax_at ^ quit) in
  run m;
  checki "83h AL=02h: AH=86h, AL one less" (word m ax_at) 0x8601;
  check "83h AL=02h: CF set" (word m flags_at land carry <> 0)

let () =
  test_wait ();
  test_irqs_during_wait ();
  test_snapshot ();
  test_event ();
  test_int70_without_pf ();
  test_irq8_without_wait ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos RTC wait: all passed"
