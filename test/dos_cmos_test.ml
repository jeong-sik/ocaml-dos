(* CMOS 시계 — 포트 70h(번지)와 71h(데이터).

   계약: 시각 레지스터는 INT 1Ah 와 같은 기계 시계를 BCD 24시간 꼴로
   읽고, 레지스터 B 가 이진·12시간 꼴을 고른다. UIP 는 초 경계 244µs
   전부터 1984µs 뒤까지 켜진다. UIP 의 오르내림을 기다리는 루프는 끝나고,
   끝난 뒤 읽은 초는 바뀐 초다. 메모리 크기와 체크섬은 IBM AT 배치를
   따른다. *)

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
  List.iter
    (fun r -> check_int (Printf.sprintf "extended memory %02Xh" r) (reg m r) 0x00)
    [ 0x17; 0x18; 0x30; 0x31 ];
  check_int "checksum high" (reg m 0x2E) 0x00;
  check_int "checksum low" (reg m 0x2F) 0x82;
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
  test_midnight_and_snapshot ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos CMOS: all passed\n"
