(* CMOS 시계 — 포트 70h(번지)와 71h(데이터).

   계약: 시각 레지스터는 INT 1Ah 와 같은 기계 시계를 BCD 24시간 꼴로
   읽고, 레지스터 B 가 이진·12시간 꼴을 고른다. UIP 는 초 경계 244µs
   전부터 1984µs 뒤까지 켜진다. UIP 의 오르내림을 기다리는 루프는 끝나고,
   끝난 뒤 읽은 초는 바뀐 초다. 메모리 크기와 체크섬은 IBM AT 배치를
   따른다. 레지스터 A 가 고른 주기가 끝나면 레지스터 C 에 PF 가 서고,
   읽으면 지워진다. PIE 가 켜져 있으면 IRQ8 이 오르고, C 를 읽을 때까지
   다시 오르지 않는다. BIOS 의 INT 70h 는 C 를 읽고 두 칩에 EOI 를 보낸다. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %02Xh want %02Xh\n%!" name got want
  end

let ports m = m.Dos_state.ports

let reg m r =
  Dos_ports.port_out (ports m) 0x70 r;
  Dos_ports.port_in (ports m) 0x71

let set m r v =
  Dos_ports.port_out (ports m) 0x70 r;
  Dos_ports.port_out (ports m) 0x71 v

let machine ?speed ~hour ~minute ~second () =
  let m = Dos_machine.create ?speed () in
  Dos_machine.set_clock m ~year:1999 ~month:12 ~day:31 ~hour ~minute ~second;
  m

let uip m = reg m 0x0A land 0x80 <> 0

let pc_hz = Dos_ports.speed_hz Dos_ports.pc_speed
let cycles_of_us us = us * pc_hz / 1_000_000
let before_boundary = cycles_of_us 244
let after_boundary = cycles_of_us 1984

(* ---------- 시각과 꼴 ---------- *)

let test_time () =
  let m = machine ~hour:23 ~minute:59 ~second:58 () in
  check_int "seconds" (reg m 0x00) 0x58;
  check_int "minutes" (reg m 0x02) 0x59;
  check_int "hours" (reg m 0x04) 0x23;
  check_int "weekday: Friday is 6" (reg m 0x06) 0x06;
  check_int "day" (reg m 0x07) 0x31;
  check_int "month" (reg m 0x08) 0x12;
  check_int "year" (reg m 0x09) 0x99;
  check_int "century" (reg m 0x32) 0x19;
  check_int "register B: 24-hour BCD" (reg m 0x0B) 0x02;
  set m 0x0B 0x06;
  check_int "binary seconds" (reg m 0x00) 58;
  check_int "binary hours" (reg m 0x04) 23;
  check_int "binary year" (reg m 0x09) 99;
  check_int "century stays BCD" (reg m 0x32) 0x19;
  set m 0x0B 0x00;
  check_int "12-hour BCD: 11 PM" (reg m 0x04) 0x91;
  set m 0x0B 0x04;
  check_int "12-hour binary: 11 PM" (reg m 0x04) 0x8B;
  set m 0x0B 0x02;
  set m 0x00 0x00;
  check_int "writing a time register leaves the clock" (reg m 0x00) 0x58;
  let noon = machine ~hour:12 ~minute:0 ~second:0 () in
  set noon 0x0B 0x00;
  check_int "12-hour: noon is 12 PM" (reg noon 0x04) 0x92;
  let midnight = machine ~hour:0 ~minute:0 ~second:0 () in
  set midnight 0x0B 0x00;
  check_int "12-hour: midnight is 12 AM" (reg midnight 0x04) 0x12

(* ---------- 상태 레지스터와 RAM 표 ---------- *)

let test_status () =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  check_int "register A rate bits" (reg m 0x0A land 0x7F) 0x26;
  set m 0x0A 0xA0;
  check_int "register A keeps written rate bits" (reg m 0x0A land 0x7F) 0x20;
  Dos_ports.set_now (ports m) (pc_hz / 2);
  check "UIP is not writable" (not (uip m));
  check_int "register C" (reg m 0x0C) 0x00;
  check_int "register D: valid RAM" (reg m 0x0D) 0x80;
  Dos_ports.port_out (ports m) 0x70 0x8D;
  check_int "NMI bit is not part of the index" (Dos_ports.port_in (ports m) 0x70) 0x0D;
  check_int "NMI-off select reads register D" (Dos_ports.port_in (ports m) 0x71) 0x80;
  check_int "base memory low" (reg m 0x15) 0x80;
  check_int "base memory high" (reg m 0x16) 0x02;
  check "BDA memory size matches CMOS"
    (Bytes.get_uint16_le m.Dos_state.mem 0x413 = reg m 0x15 lor (reg m 0x16 lsl 8));
  (* 확장 메모리 15MB = 15360KB = 3C00h. 체크섬은 10h~2Dh 의 합:
     80h + 02h(기본) + 00h + 3Ch(확장 17h·18h) = BEh. *)
  List.iter
    (fun (r, v) -> check_int (Printf.sprintf "extended memory %02Xh" r) (reg m r) v)
    [ (0x17, 0x00); (0x18, 0x3C); (0x30, 0x00); (0x31, 0x3C) ];
  check_int "checksum high" (reg m 0x2E) 0x00;
  check_int "checksum low" (reg m 0x2F) 0xBE;
  set m 0x15 0x00;
  check_int "RAM table is read-only" (reg m 0x15) 0x80

(* ---------- UIP ---------- *)

let test_uip () =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  let at n = Dos_ports.set_now (ports m) n; uip m in
  check "UIP on at a second boundary" (at 0);
  check "UIP on to the end of the update" (at (after_boundary - 1));
  check "UIP off after the update" (not (at after_boundary));
  check "UIP off mid-second" (not (at (pc_hz / 2)));
  check "UIP off just before the warning" (not (at (pc_hz - before_boundary - 1)));
  check "UIP on 244us before the boundary" (at (pc_hz - before_boundary));
  check "UIP on at the next boundary" (at pc_hz);
  set m 0x0B 0x82;
  check "SET holds UIP off" (not (at 0));
  (* 10 배 기계에서 1984µs 는 사이클이 10/4 배다. *)
  let fast = match Dos_ports.speed_of_mhz 12. with Ok s -> s | Error e -> failwith e in
  let f = machine ~speed:fast ~hour:10 ~minute:0 ~second:0 () in
  Dos_ports.set_now (ports f) after_boundary;
  check "UIP window follows the CPU clock" (uip f)

(* 오르내림을 기다린 뒤 초를 읽는 COM 프로그램. 포트 71h 가 늘 0 이면
   두 번째 루프에서 영원히 돈다. *)
let uip_edge_com =
  let wait_while_set = "\xB0\x0A\xE6\x70\xE4\x71\xA8\x80\x75\xF6" in
  let wait_while_clear = "\xB0\x0A\xE6\x70\xE4\x71\xA8\x80\x74\xF6" in
  wait_while_set ^ wait_while_clear ^ wait_while_set
  ^ "\xB0\x00\xE6\x70\xE4\x71"  (* mov al,0; out 70h,al; in al,71h *)
  ^ "\xA2\x00\x02"              (* mov [0200h],al *)
  ^ "\xCD\x20"

let psp_base = 0x10000
let result_at = psp_base + 0x200
let step_budget = 5_000_000

let test_uip_edge () =
  let m = machine ~hour:23 ~minute:59 ~second:58 () in
  Dos_machine.load_com m uip_edge_com;
  let report = Dos_machine.run_until m ~max_steps:step_budget ~stop:(fun _ -> false) in
  check "UIP edge wait ends" (report.Dos_machine.stop_reason = Dos_machine.Program_exited);
  check_int "seconds after the edge" (Bytes.get_uint8 m.Dos_state.mem result_at) 0x59;
  let cycles = Cpu86.cycles m.Dos_state.cpu in
  check (Printf.sprintf "edge seen right after the update (%d cycles)" cycles)
    (cycles >= pc_hz + after_boundary && cycles < pc_hz + after_boundary + 1000)

(* ---------- 주기 깃발(레지스터 C) ---------- *)

let pf = 0x40
let irqf = 0x80
let rtc_hz = 32768

(* 주기가 [ticks] 틱인 탭의 [k] 번째 끝 — 그 사이클부터 PF 가 보인다. *)
let edge ~ticks k = ((k * ticks * pc_hz) + (rtc_hz - 1)) / rtc_hz

let flags_at m n = Dos_ports.set_now (ports m) n; reg m 0x0C

let test_periodic () =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  let ticks = 32 in   (* 기본 26h: RS 6, 1024Hz *)
  check_int "no period has ended at power on" (flags_at m 0) 0;
  check_int "not before the first end" (flags_at m (edge ~ticks 1 - 1)) 0;
  check_int "PF at the first end" (flags_at m (edge ~ticks 1)) pf;
  check_int "reading C clears PF" (flags_at m (edge ~ticks 1)) 0;
  check_int "PF waits for the next end" (flags_at m (edge ~ticks 2 - 1)) 0;
  check_int "PF stays until read, over several ends" (flags_at m (edge ~ticks 5)) pf;
  set m 0x0B 0x42;
  check_int "PIE adds IRQF" (flags_at m (edge ~ticks 6)) (irqf lor pf);
  check_int "IRQF goes with PF" (flags_at m (edge ~ticks 6)) 0;
  set m 0x0B 0x02;
  (* 1024Hz 의 끝이 지난 뒤 2Hz 로 바꿔도 이미 선 PF 는 남는다. *)
  Dos_ports.set_now (ports m) (edge ~ticks 7);
  set m 0x0A 0x2F;
  check_int "PF of the old rate survives a rate change" (flags_at m (edge ~ticks 7)) pf;
  check_int "2Hz: nothing within the half second" (flags_at m (pc_hz / 2 - 1)) 0;
  check_int "2Hz: PF at the half second" (flags_at m (pc_hz / 2)) pf;
  (* 탭을 바꿔도 사슬의 위상은 그대로다: 512Hz 의 첫 끝은 바꾼 때부터
     한 주기 뒤가 아니라 사이클 0 에서 센 자리다. *)
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  ignore (flags_at m (edge ~ticks 1 + 1));
  set m 0x0A 0x27;
  check_int "512Hz: not before its own end" (flags_at m (edge ~ticks:64 1 - 1)) 0;
  check_int "512Hz: PF on the chain's end" (flags_at m (edge ~ticks:64 1)) pf;
  (* RS 1·2 는 짧은 표가 아니라 256·128Hz 다. *)
  set m 0x0A 0x21;
  ignore (flags_at m (edge ~ticks:128 3));
  check_int "RS 1: 256Hz" (flags_at m (edge ~ticks:128 4 - 1)) 0;
  check_int "RS 1: PF at 1/256 s" (flags_at m (edge ~ticks:128 4)) pf;
  set m 0x0A 0x22;
  check_int "RS 2: 128Hz" (flags_at m (edge ~ticks:256 3 - 1)) 0;
  check_int "RS 2: PF at 1/128 s" (flags_at m (edge ~ticks:256 3)) pf;
  set m 0x0A 0x23;
  ignore (flags_at m (edge ~ticks:4 1000));
  check_int "RS 3: 8192Hz" (flags_at m (edge ~ticks:4 1001)) pf;
  set m 0x0A 0x20;
  check_int "RS 0: no tap" (flags_at m (2 * pc_hz)) 0;
  set m 0x0A 0x66;
  check_int "divider held in reset: no tap" (flags_at m (3 * pc_hz)) 0;
  (* 스냅숏은 아직 읽지 않은 PF 와 세기 시작한 때를 지닌다. *)
  set m 0x0A 0x26;
  Dos_ports.set_now (ports m) (edge ~ticks 20000);
  set m 0x0A 0x2F;
  let round_trip name m k =
    match Dos_snapshot.save m with
    | Error e -> check (name ^ " save: " ^ Dos_snapshot.save_error_to_string e) false
    | Ok bytes -> (
      match Dos_snapshot.restore bytes with
      | Error e -> check (name ^ " restore: " ^ Dos_snapshot.error_to_string e) false
      | Ok r -> k r)
  in
  round_trip "unread PF" m (fun r ->
      check_int "restored PF" (flags_at r (edge ~ticks 20000)) pf);
  (* 5.25 초에 C 를 읽어 지웠다: 2Hz 의 다음 끝은 5.5 초다. 세기 시작점을
     잃으면 사이클 0 부터 세어 5.4 초에 PF 가 보인다. *)
  ignore (flags_at m (pc_hz * 21 / 4));
  round_trip "count start" m (fun r ->
      check_int "restored count start" (flags_at r (pc_hz * 27 / 5)) 0;
      check_int "restored rate" (flags_at r (pc_hz * 11 / 2)) pf)

(* PF 를 기다리는 COM 프로그램: C 를 한 번 읽어 지우고, PF 가 설 때까지
   C 를 읽는다. 포트 71h 가 늘 0 이면 끝나지 않는다. *)
let periodic_wait_com =
  "\xB0\x0C\xE6\x70\xE4\x71"       (* mov al,0Ch; out 70h,al; in al,71h *)
  ^ "\xE4\x71\xA8\x40\x74\xFA"     (* again: in al,71h; test al,40h; jz again *)
  ^ "\xCD\x20"

let test_periodic_wait () =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  Dos_machine.load_com m periodic_wait_com;
  let report = Dos_machine.run_until m ~max_steps:step_budget ~stop:(fun _ -> false) in
  check "PF wait ends" (report.Dos_machine.stop_reason = Dos_machine.Program_exited);
  let cycles = Cpu86.cycles m.Dos_state.cpu in
  check (Printf.sprintf "PF seen at the first 1024Hz end (%d cycles)" cycles)
    (cycles >= edge ~ticks:32 1 && cycles < edge ~ticks:32 1 + 100)

(* ---------- IRQ8 ---------- *)

let pie_on = 0x42
let pie_off = 0x02

let due m n = Dos_ports.set_now (ports m) n; Dos_ports.rtc_irq_due (ports m)
let pic m = Dos_ports.pic (ports m)

let test_irq_line () =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  let e = edge ~ticks:32 in
  check "PIE off: no IRQ8" (not (due m (e 1)));
  ignore (flags_at m (e 1));
  set m 0x0B pie_on;
  check "not before the next end" (not (due m (e 2 - 1)));
  check "IRQ8 rises at the end" (due m (e 2));
  check "it rises once" (not (due m (e 2)));
  (* while the line is up, other registers leave it up *)
  Dos_pic.raise_irq (pic m) Dos_pic.Rtc;
  ignore (reg m 0x0A);
  set m 0x0B (pie_on lor 0x04);
  check "reading A and writing B keep the request" (Dos_pic.requested (pic m) Dos_pic.Rtc);
  Dos_pic.lower_irq (pic m) Dos_pic.Rtc;
  check "an end while the line is up raises nothing" (not (due m (e 4)));
  Dos_pic.raise_irq (pic m) Dos_pic.Rtc;
  check_int "reading C" (flags_at m (e 4)) (irqf lor pf);
  check "reading C withdraws an untaken request" (not (Dos_pic.requested (pic m) Dos_pic.Rtc));
  check "after C the next end counts" (not (due m (e 5 - 1)));
  check "and raises IRQ8" (due m (e 5));
  Dos_pic.raise_irq (pic m) Dos_pic.Rtc;
  Dos_ports.set_now (ports m) (e 5);
  set m 0x0B pie_off;
  check "PIE off withdraws the request" (not (Dos_pic.requested (pic m) Dos_pic.Rtc));
  check "PIE off: nothing later" (not (due m (e 9)));
  (* PF stood before PIE came on: IRQF rises with PIE (86Box
     timer_update_irq on a register B write). *)
  set m 0x0B pie_on;
  check "PIE on over a standing PF rises at once" (due m (e 9));
  (* PF kept across a register A write, then PIE: IRQF rises at once too *)
  ignore (flags_at m (e 9));
  set m 0x0B pie_off;
  Dos_ports.set_now (ports m) (e 10 + 1);
  set m 0x0A 0x26;
  set m 0x0B pie_on;
  check "PIE on over a PF kept by A rises at once" (due m (e 10 + 1));
  ignore (flags_at m (e 10 + 1));
  let e n = e (n + 1) in
  ignore (flags_at m (e 9));
  (* the snapshot keeps the line and the next rise *)
  let mid = (e 9 + e 10) / 2 in
  Dos_ports.set_now (ports m) mid;
  let round_trip name m k =
    match Dos_snapshot.save m with
    | Error e -> check (name ^ " save: " ^ Dos_snapshot.save_error_to_string e) false
    | Ok bytes -> (
      match Dos_snapshot.restore bytes with
      | Error e -> check (name ^ " restore: " ^ Dos_snapshot.error_to_string e) false
      | Ok r -> k r)
  in
  check "the line is down mid-period" (not (due m mid));
  round_trip "next rise" m (fun r ->
      check "restored: not before the next end" (not (due r (e 10 - 1)));
      check "restored: rises at the next end" (due r (e 10)));
  check "the line rises" (due m (e 10));
  round_trip "line up" m (fun r ->
      set r 0x0B pie_on;
      check "restored: the line stays up" (not (due r (e 12)));
      ignore (flags_at r (e 12));
      check "restored: reading C re-arms it" (due r (e 13)))

(* COM 프로그램: INT 70h 를 걸고 PIE 를 켠 뒤 IRQ8 을 기다린다. 처리기는
   센 뒤 [tail] 을 한다 — 스스로 C 를 읽고 EOI 하거나, 옛 벡터(BIOS)로
   간다. *)
let irq8_vector = 0x70
let count_at = 0x300
let old_vector_at = 0x304
let handler_at = 0x400
let wanted = 4

let le16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))

let irq8_program ~handler =
  let hook =
    "\x31\xc0\x8e\xc0"                                   (* xor ax,ax; mov es,ax *)
    ^ "\x26\xa1" ^ le16 (irq8_vector * 4) ^ "\xa3" ^ le16 old_vector_at
    ^ "\x26\xa1" ^ le16 ((irq8_vector * 4) + 2) ^ "\xa3" ^ le16 (old_vector_at + 2)
    ^ "\xfa"                                                (* cli *)
    ^ "\x26\xc7\x06" ^ le16 (irq8_vector * 4) ^ le16 handler_at
    ^ "\x26\x8c\x0e" ^ le16 ((irq8_vector * 4) + 2)        (* mov es:[1C2h],cs *)
    ^ "\xb0\x0b\xe6\x70\xe4\x71\x0c\x40\xe6\x71"         (* B |= PIE *)
    ^ "\xfb"                                                (* sti *)
  in
  let wait =
    "\x80\x3e" ^ le16 count_at ^ String.make 1 (Char.chr wanted)  (* again: cmp byte [count],N *)
    ^ "\x72\xf9"                                           (* jb again *)
  in
  let code = hook ^ wait ^ "\xcd\x20" in
  assert (0x100 + String.length code <= count_at);
  code ^ String.make (handler_at - 0x100 - String.length code) '\x00' ^ handler

let own_handler =
  "\x50"
  ^ "\x2e\xfe\x06" ^ le16 count_at                         (* inc byte cs:[count] *)
  ^ "\xb0\x0c\xe6\x70\xe4\x71"                              (* read C *)
  ^ "\xb0\x20\xe6\xa0\xe6\x20"                              (* EOI both *)
  ^ "\x58\xcf"

let chaining_handler =
  "\x2e\xfe\x06" ^ le16 count_at                           (* inc byte cs:[count] *)
  ^ "\x2e\xff\x2e" ^ le16 old_vector_at                     (* jmp far cs:[old] *)

let run_irq8 name handler =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  Dos_machine.load_com m (irq8_program ~handler);
  let report = Dos_machine.run_until m ~max_steps:step_budget ~stop:(fun _ -> false) in
  check (name ^ ": the program sees four IRQ8") (report.Dos_machine.stop_reason = Dos_machine.Program_exited);
  let cycles = Cpu86.cycles m.Dos_state.cpu in
  let fourth = edge ~ticks:32 wanted in
  check (Printf.sprintf "%s: the fourth at the fourth 1024Hz end (%d, end %d)" name cycles fourth)
    (cycles >= fourth && cycles < fourth + 300);
  let pic = pic m in
  Dos_pic.write_command pic Dos_pic.Slave 0x0B;
  Dos_pic.write_command pic Dos_pic.Master 0x0B;
  check_int (name ^ ": nothing left in service")
    (Dos_pic.read_command pic Dos_pic.Slave lor Dos_pic.read_command pic Dos_pic.Master) 0

(* IRQ9-15 의 BIOS 기본 루틴은 두 칩에 EOI 를 보낸다. 줄을 올릴 장치가
   없어서, 하네스가 두 칩을 서비스 중으로 둔 뒤 프로그램이 INT 를 부른다.
   78h 는 두 번째 칩 범위 밖이라 IRET 뿐이다. *)
let in_service m =
  let pic = pic m in
  Dos_pic.write_command pic Dos_pic.Slave 0x0B;
  Dos_pic.write_command pic Dos_pic.Master 0x0B;
  let v = Dos_pic.read_command pic Dos_pic.Slave lsl 8 lor Dos_pic.read_command pic Dos_pic.Master in
  Dos_pic.write_command pic Dos_pic.Slave 0x0A;
  Dos_pic.write_command pic Dos_pic.Master 0x0A;
  v

let slave_and_cascade = 0x0104

let default_handler name vector want =
  let m = machine ~hour:10 ~minute:0 ~second:0 () in
  Dos_machine.load_com m ("\xcd" ^ String.make 1 (Char.chr vector) ^ "\xcd\x20");
  Dos_pic.raise_irq (pic m) Dos_pic.Rtc;
  Dos_pic.acknowledge (pic m) Dos_pic.Rtc;
  check_int (name ^ ": both chips in service first") (in_service m) slave_and_cascade;
  ignore (Dos_machine.run_until m ~max_steps:step_budget ~stop:(fun _ -> false));
  check (name ^ ": the program ends") (Dos_machine.exited m);
  check_int (name ^ ": in service after") (in_service m) want

let test_irq8_programs () =
  run_irq8 "own handler" own_handler;
  run_irq8 "BIOS handler" chaining_handler;
  default_handler "INT 71h" 0x71 0;
  default_handler "INT 77h" 0x77 0;
  default_handler "INT 78h" 0x78 slave_and_cascade

(* ---------- 자정 넘김과 스냅숏 ---------- *)

let busy_com = "\xEB\xFE"

let test_midnight_and_snapshot () =
  let m = machine ~hour:23 ~minute:59 ~second:58 () in
  Dos_machine.load_com m busy_com;
  ignore
    (Dos_machine.run_until m ~max_steps:max_int ~stop:(fun m ->
         Cpu86.cycles m.Dos_state.cpu >= 2 * pc_hz));
  check_int "midnight seconds" (reg m 0x00) 0x00;
  check_int "midnight hours" (reg m 0x04) 0x00;
  check_int "weekday: Saturday is 7" (reg m 0x06) 0x07;
  check_int "new day" (reg m 0x07) 0x01;
  check_int "new month" (reg m 0x08) 0x01;
  check_int "new year" (reg m 0x09) 0x00;
  check_int "new century" (reg m 0x32) 0x20;
  set m 0x0A 0x2A;
  set m 0x0B 0x06;
  Dos_ports.port_out (ports m) 0x70 0x0B;
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok bytes -> (
    match Dos_snapshot.restore bytes with
    | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
    | Ok r ->
      check_int "restored index" (Dos_ports.port_in (ports r) 0x71) 0x06;
      check_int "restored register A" (reg r 0x0A land 0x7F) 0x2A;
      check_int "restored machine reads its own clock" (reg r 0x07) 1)

let () =
  test_time ();
  test_status ();
  test_uip ();
  test_uip_edge ();
  test_periodic ();
  test_periodic_wait ();
  test_irq_line ();
  test_irq8_programs ();
  test_midnight_and_snapshot ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos CMOS: all passed\n"
