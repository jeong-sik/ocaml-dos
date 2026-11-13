(* PIT 읽기-되돌림 명령과 상태 바이트 — 포트 0x40·0x42·0x43 으로만 본다.
   기대값은 Intel 82C54 데이터시트(231244)의 Read-Back Command 와
   Figure 10~13, Mode Definitions 에서 나온다. 틱은 CLK 펄스다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let make () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  Dos_ports.create ~mem ~video

let at p tick = Dos_ports.set_now p (tick * Dos_ports.cpu_cycles_per_pit_tick)
let ctrl p v = Dos_ports.port_out p 0x43 v
let rd0 p = Dos_ports.port_in p 0x40
let rd2 p = Dos_ports.port_in p 0x42
let wr2 p v = Dos_ports.port_out p 0x42 v
let wr2_16 p n = wr2 p (n land 0xff); wr2 p (n lsr 8)
let gate p on = Dos_ports.port_out p 0x61 (if on then 0x01 else 0x00)

(* 읽기-되돌림 명령: 11, COUNT, STATUS, CNT2, CNT1, CNT0, 0. 비트가 0 이면 잡는다. *)
let read_back ~count ~status ~ch0 ~ch2 =
  0xC0
  lor (if count then 0 else 0x20)
  lor (if status then 0 else 0x10)
  lor (if ch2 then 0x08 else 0)
  lor (if ch0 then 0x02 else 0)

let status_of p ~ch0 =
  ctrl p (read_back ~count:false ~status:true ~ch0 ~ch2:(not ch0));
  if ch0 then rd0 p else rd2 p

let out_bit = 0x80
let null_bit = 0x40

(* 채널 2 제어 워드: 10, 하위·상위 바이트(11), 모드, 바이너리 *)
let ch2_word mode = 0xB0 lor (mode lsl 1)

let test_status_after_post () =
  let p = make () in
  (* POST: 36h, 65536 을 틱 0 에 실었다. 모드 3 은 앞 32768 펄스가 높다. *)
  at p 100;
  check "높은 반: OUT, 실림, 36h" (status_of p ~ch0:true) (out_bit lor 0x36);
  at p 40000;
  check "낮은 반: OUT 낮음" (status_of p ~ch0:true) 0x36;
  (* 상태를 읽은 뒤에는 카운트가 나온다: 39999 펄스 → 65536 - 2*(39999-32768) *)
  let lo = rd0 p in
  let hi = rd0 p in
  check "상태 다음은 래치 안 한 카운트" (lo lor (hi lsl 8)) (0x10000 - (2 * (39999 - 32768)))

let test_status_as_written () =
  let p = make () in
  at p 0;
  ctrl p 0xBE;
  check "모드 7 은 7 로 남는다" (status_of p ~ch0:false land 0x3F) 0x3E;
  ctrl p 0x9C;
  check "하위 바이트·모드 6" (status_of p ~ch0:false land 0x3F) 0x1C;
  ctrl p 0xB1;
  check "BCD 비트" (status_of p ~ch0:false land 0x3F) 0x31

let null p = status_of p ~ch0:false land null_bit <> 0
let check_null name p want = check name (Bool.to_int (null p)) (Bool.to_int want)

let test_null_count () =
  let p = make () in
  gate p true;
  at p 10;
  ctrl p (ch2_word 0);
  check_null "제어 워드 뒤 1" p true;
  at p 11;
  wr2 p 5;
  check_null "첫 바이트만 쓰면 그대로 1" p true;
  at p 12;
  wr2 p 0;
  check_null "둘째 바이트를 쓴 틱은 1" p true;
  at p 13;
  check_null "다음 펄스에 실려 0" p false;
  at p 20;
  ctrl p (ch2_word 0);
  check_null "실린 뒤 제어 워드를 쓰면 다시 1" p true;
  (* 모드 2: 세는 중에 쓴 카운트는 이번 주기 끝에 실린다 *)
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 2);
  wr2_16 p 10;
  at p 5;
  wr2_16 p 20;
  at p 10;
  check_null "모드 2 주기 끝 앞은 1" p true;
  at p 11;
  check_null "모드 2 주기 끝(1 + 10)에 0" p false;
  (* 모드 3: 이번 반주기 끝에 실린다 *)
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 3);
  wr2_16 p 10;
  at p 3;
  wr2_16 p 6;
  at p 5;
  check_null "모드 3 반주기 끝 앞은 1" p true;
  at p 6;
  check_null "모드 3 높은 반 끝(1 + 5)에 0" p false;
  (* 모드 1: 트리거 다음 펄스에 실린다 *)
  let p = make () in
  gate p false;
  at p 0;
  ctrl p (ch2_word 1);
  wr2_16 p 5;
  at p 50;
  check_null "모드 1 트리거 전 1" p true;
  at p 60;
  gate p true;
  check_null "트리거 틱은 1" p true;
  at p 61;
  check_null "트리거 다음 펄스에 0" p false;
  (* 모드 0 은 GATE 가 낮아도 다음 펄스에 싣는다 *)
  let p = make () in
  gate p false;
  at p 0;
  ctrl p (ch2_word 0);
  wr2_16 p 5;
  at p 1;
  check_null "모드 0 GATE 낮음도 0" p false;
  (* 모드 2: GATE 가 낮은 동안 쓴 카운트는 다음 상승이 싣는다 *)
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 2);
  wr2_16 p 10;
  at p 4;
  gate p false;
  at p 5;
  wr2_16 p 30;
  at p 100;
  check_null "GATE 낮은 동안 1" p true;
  gate p true;
  at p 101;
  check_null "GATE 상승 다음 펄스에 0" p false;
  (* 모드 2: 새 카운트를 쓴 뒤 주기 끝 전에 GATE 가 내려가면 실리지 않는다 *)
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 2);
  wr2_16 p 10;
  at p 3;
  wr2_16 p 40;
  at p 5;
  gate p false;
  at p 50;
  check_null "주기 끝(11)이 지나도 GATE 가 낮으면 1" p true;
  gate p true;
  at p 51;
  check_null "GATE 상승 다음 펄스에 0" p false;
  (* 2바이트 카운트는 둘째 바이트를 쓸 때 1 이 된다(Figure 12 주 2) *)
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 2);
  wr2_16 p 10;
  at p 4;
  wr2 p 20;
  check_null "첫 바이트만 쓰면 앞 카운트가 실린 그대로 0" p false;
  wr2 p 0;
  check_null "둘째 바이트에 1" p true

let rd2_16 p = let lo = rd2 p in let hi = rd2 p in lo lor (hi lsl 8)

let test_status_before_count () =
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 0);
  wr2_16 p 1000;
  at p 101;
  ctrl p (read_back ~count:true ~status:true ~ch0:false ~ch2:true);
  at p 500;
  check "함께 잡으면 상태 먼저" (rd2 p) (ch2_word 0 land 0x3F);
  check "그다음 잡은 카운트" (rd2_16 p) 900;
  check "그 뒤는 지금 카운트" (rd2_16 p) 501;
  (* 카운터 래치를 먼저 하고 상태를 나중에 잡아도 상태가 먼저다 *)
  ctrl p 0x80;
  at p 600;
  ctrl p (read_back ~count:false ~status:true ~ch0:false ~ch2:true);
  check "상태가 먼저" (rd2 p) (ch2_word 0 land 0x3F);
  check "카운트는 래치한 때(틱 500) 것" (rd2_16 p) 501

let test_second_latch_ignored () =
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 0);
  wr2_16 p 5;
  at p 2;
  ctrl p (read_back ~count:false ~status:true ~ch0:false ~ch2:true);
  at p 20;
  ctrl p (read_back ~count:false ~status:true ~ch0:false ~ch2:true);
  check "두 번째 상태 래치는 무시 — OUT 낮던 때" (rd2 p) (ch2_word 0 land 0x3F);
  check "읽은 뒤 다시 잡으면 지금 OUT" (status_of p ~ch0:false) (out_bit lor (ch2_word 0 land 0x3F));
  (* 읽기-되돌림의 카운트 래치도 앞 래치가 있으면 무시한다 *)
  ctrl p (ch2_word 0);
  wr2_16 p 1000;
  at p 30;
  ctrl p 0x80;
  at p 40;
  ctrl p (read_back ~count:true ~status:false ~ch0:false ~ch2:true);
  check "카운트는 처음 래치한 때 것" (rd2_16 p) (1000 - (30 - 21))

let test_two_channels () =
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 0);
  wr2_16 p 100;
  at p 51;
  ctrl p (read_back ~count:true ~status:false ~ch0:true ~ch2:true);
  at p 80;
  check "채널 2 래치" (rd2_16 p) 50;
  let lo = rd0 p in
  let hi = rd0 p in
  (* 채널 0 모드 3, 65536: 50 펄스 → 65536 - 100 *)
  check "채널 0 도 같은 때 래치" (lo lor (hi lsl 8)) (0x10000 - 100)

let test_control_word_drops_status () =
  let p = make () in
  at p 0;
  ctrl p (ch2_word 0);
  ctrl p (read_back ~count:false ~status:true ~ch0:false ~ch2:true);
  ctrl p (ch2_word 0);
  wr2_16 p 0x1234;
  at p 1;
  check "제어 워드가 잡은 상태를 버린다" (rd2_16 p) 0x1234

let test_channel1_ignored () =
  let p = make () in
  gate p true;
  at p 0;
  ctrl p (ch2_word 0);
  wr2_16 p 100;
  (* CNT1 만 고르면 채널 0·2 는 그대로다 *)
  ctrl p 0xC4;
  (* 채널 1 제어 워드(모드 3)도 채널 2 를 건드리지 않는다 *)
  ctrl p 0x76;
  at p 11;
  check "채널 2 는 래치되지 않았다" (rd2_16 p) 90;
  check "채널 2 는 모드 0 그대로" (status_of p ~ch0:false land 0x3F) (ch2_word 0 land 0x3F)

let ports_bytes p =
  let w = Dos_snap_codec.writer () in
  Dos_ports.write_state w p;
  Dos_snap_codec.contents w

let test_snapshot () =
  let p = make () in
  gate p true;
  at p 0;
  ctrl p 0xBE;
  wr2_16 p 10;
  at p 3;
  wr2_16 p 6;
  ctrl p (read_back ~count:true ~status:true ~ch0:false ~ch2:true);
  let saved = ports_bytes p in
  let q = make () in
  Dos_ports.read_state (Dos_snap_codec.reader saved) q;
  check "저장 → 복원 → 저장" (Bool.to_int (ports_bytes q = saved)) 1;
  check "잡은 상태가 돌아온다" (rd2 q) (rd2 p);
  check "잡은 카운트가 돌아온다" (rd2_16 q) (rd2_16 p);
  (* 틱 3 에 쓴 6 은 높은 반 끝(틱 6)에 실린다 — 그 틱에 NULL COUNT 가 0 이 된다 *)
  at p 6;
  at q 6;
  check "실리는 때가 돌아온다" (status_of q ~ch0:false) (status_of p ~ch0:false);
  check "틱 6 에 실렸다" (status_of q ~ch0:false land null_bit) 0;
  check "모드 7 이 7 로 돌아온다" (status_of q ~ch0:false land 0x3F) 0x3E

(* 스냅숏의 제어 워드 bit 5-4 가 0 이면 래치 명령이지 프로그램된 상태가 아니다 *)
let test_snapshot_refuses_latch_word () =
  let w = Dos_snap_codec.writer () in
  Dos_pit.write_state w (Dos_pit.create ());
  let b = Bytes.of_string (Dos_snap_codec.contents w) in
  (* 맨 앞 8바이트(빅엔디언)가 제어 워드 bit 5-0 이다 *)
  let control_low_byte = 7 in
  let access_bits = 0x30 in
  let low = Char.code (Bytes.get b control_low_byte) in
  Bytes.set b control_low_byte (Char.chr (low land lnot access_bits));
  let refused =
    match Dos_pit.read_state (Dos_snap_codec.reader (Bytes.to_string b)) (Dos_pit.create ()) with
    | () -> false
    | exception Dos_snap_codec.Invalid _ -> true
  in
  check "접근 비트 00 은 거부" (Bool.to_int refused) 1

(* 기계에서: 게스트가 채널 0 상태를 읽어 bit 5-0 을 종료 코드로 낸다 *)
let test_guest_reads_status () =
  let prog =
    "\xb0\xe2\xe6\x43"       (* mov al,E2h; out 43h,al — 채널 0 상태 *)
    ^ "\xe4\x40\x24\x3f"     (* in al,40h; and al,3Fh *)
    ^ "\xb4\x4c\xcd\x21"     (* mov ah,4Ch; int 21h *)
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m prog;
  ignore (Dos_machine.run_until m ~max_steps:10_000 ~stop:Dos_machine.exited);
  check "게스트가 읽은 채널 0 모드" (Dos_machine.exit_code m) 0x36

let () =
  test_status_after_post ();
  test_status_as_written ();
  test_null_count ();
  test_status_before_count ();
  test_second_latch_ignored ();
  test_two_channels ();
  test_control_word_drops_status ();
  test_channel1_ignored ();
  test_snapshot ();
  test_snapshot_refuses_latch_word ();
  test_guest_reads_status ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n" !failed;
    exit 1
  end;
  print_endline "dos_pit_read_back_test: ok"
