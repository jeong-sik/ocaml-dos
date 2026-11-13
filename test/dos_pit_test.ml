(* PIT 채널 2 — 포트 0x42·0x43·0x61 로만 본다. 기대값은 Intel 82C54
   데이터시트(231244)의 Mode Definitions 에서 나온다. 틱은 CLK 펄스다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let check_b name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let make () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  Dos_ports.create ~mem ~video

let cycles_per_pit_tick = Dos_ports.cpu_cycles_per_pit_tick

let at p tick = Dos_ports.set_now p (tick * cycles_per_pit_tick)
let ctrl p v = Dos_ports.port_out p 0x43 v
let wr p v = Dos_ports.port_out p 0x42 v
let rd p = Dos_ports.port_in p 0x42
let gate p on = Dos_ports.port_out p 0x61 (if on then 0x01 else 0x00)
let out2 p = Dos_ports.port_in p 0x61 land 0x20 <> 0
let wr16 p n = wr p (n land 0xff); wr p (n lsr 8)
let rd16 p = let lo = rd p in let hi = rd p in lo lor (hi lsl 8)
let latch p = ctrl p 0x80
let latched16 p = latch p; rd16 p

(* 제어 워드: 채널 2(10), 하위·상위 바이트(11), 모드, 바이너리 *)
let ch2_word mode = 0xB0 lor (mode lsl 1)

let outs p ticks = List.map (fun t -> at p t; out2 p) ticks

let check_outs name p ticks want =
  List.iteri
    (fun i (got, w) -> check_b (Printf.sprintf "%s tick %d" name (List.nth ticks i)) got w)
    (List.combine (outs p ticks) want)

(* SeaBIOS src/hw/timer.c tsctimer_setup: GATE 를 열고 모드 0 에 0x800 을
   쓴 뒤 0x61 bit5 를 기다린다. OUT 은 카운트를 쓴 뒤 N+1 펄스에 높아진다. *)
let test_mode0_delay_loop () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 0);
  check_b "제어 워드 직후 OUT 낮음" (out2 p) false;
  wr p 0x00;
  at p 10;
  wr p 0x08;
  check_b "카운트 직후 OUT 낮음" (out2 p) false;
  at p (10 + 0x800);
  check_b "N 펄스 뒤 아직 낮음" (out2 p) false;
  at p (10 + 0x800 + 1);
  check_b "N+1 펄스 뒤 높음" (out2 p) true;
  at p (11 + 5);
  check "다섯 펄스 뒤 카운트" (latched16 p) (0x800 - 5);
  at p (11 + 0x800 + 3);
  check "0 을 지나 FFFFh 로 돈다" (latched16 p) 0xFFFD;
  check_b "도는 동안 OUT 높음" (out2 p) true

let test_mode0_gate_pauses () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 0);
  wr16 p 100;
  at p 41;
  gate p false;
  at p 1000;
  check "GATE 낮은 동안 멈춤" (latched16 p) 60;
  check_b "GATE 는 OUT 을 안 바꿈" (out2 p) false;
  gate p true;
  at p 1059;
  check_b "남은 펄스 전" (out2 p) false;
  at p 1060;
  check_b "남은 60 펄스 뒤 높음" (out2 p) true

let test_mode0_written_with_gate_low () =
  let p = make () in
  at p 0;
  ctrl p (ch2_word 0);
  wr16 p 100;
  at p 500;
  check "GATE 가 낮으면 실리기만 한다" (latched16 p) 100;
  gate p true;
  at p 599;
  check_b "GATE 상승 뒤 N 펄스 전" (out2 p) false;
  at p 600;
  check_b "GATE 상승 뒤 N 펄스에 높음" (out2 p) true

let test_mode0_rewrite_drops_out () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 0);
  wr16 p 4;
  at p 10;
  check_b "만료" (out2 p) true;
  wr p 0x05;
  check_b "첫 바이트에서 OUT 낮음" (out2 p) false;
  at p 12;
  wr p 0x00;
  at p 17;
  check_b "새 카운트 N 펄스 뒤 아직 낮음" (out2 p) false;
  at p 18;
  check_b "새 카운트 N+1 펄스 뒤 높음" (out2 p) true

let test_latch () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 2);
  wr16 p 1000;
  at p 101;
  latch p;
  at p 201;
  latch p;
  at p 301;
  let lo = rd p in
  at p 401;
  let hi = rd p in
  check "처음 래치한 값을 읽는다" (lo lor (hi lsl 8)) 900;
  at p 501;
  check "읽고 나면 래치가 풀린다" (rd16 p) 500

let test_single_byte_access () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p 0x90;
  wr p 0x50;
  at p 17;
  check "LSB 만" (rd p) (0x50 - 16);
  check "LSB 만 — 다시 읽어도 LSB" (rd p) (0x50 - 16);
  ctrl p 0xA0;
  wr p 0x02;
  at p (18 + 0x10);
  check "MSB 만" (rd p) ((0x200 - 0x10) lsr 8)

let test_rate_generator () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 2);
  wr16 p 5;
  let ticks = List.init 11 (fun i -> i + 1) in
  check_outs "모드 2" p ticks
    [ true; true; true; true; false; true; true; true; true; false; true ];
  at p 2;
  check "모드 2 카운트" (latched16 p) 4;
  at p 6;
  check "모드 2 다시 실림" (latched16 p) 5;
  at p 15;
  check_b "낮은 펄스" (out2 p) false;
  gate p false;
  check_b "GATE 하강이 OUT 을 곧바로 높인다" (out2 p) true;
  (* GATE 상승은 멈춘 자리에서 잇지 않고 초기 카운트를 다시 싣는다. 멈춘
     자리(13 펄스, 5 로 나눈 나머지 3)와 다시 실은 자리가 다르게 고른다. *)
  let q = make () in
  at q 0;
  gate q true;
  ctrl q (ch2_word 2);
  wr16 q 5;
  at q 14;
  gate q false;
  at q 50;
  check "GATE 낮은 동안 멈춤" (latched16 q) 2;
  gate q true;
  check_outs "모드 2 트리거 뒤" q [ 51; 54; 55; 56 ] [ true; true; false; true ]

let test_square_wave () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 3);
  wr16 p 5;
  let ticks = List.init 10 (fun i -> i + 1) in
  check_outs "모드 3 홀수" p ticks
    [ true; true; true; false; false; true; true; true; false; false ];
  let counts = List.map (fun t -> at p t; latched16 p) [ 11; 12; 13; 14; 15 ] in
  List.iteri (fun i (got, want) -> check (Printf.sprintf "모드 3 홀수 카운트 %d" i) got want)
    (List.combine counts [ 4; 2; 0; 4; 2 ]);
  let q = make () in
  at q 0;
  gate q true;
  ctrl q (ch2_word 3);
  wr16 q 4;
  check_outs "모드 3 짝수" q [ 1; 2; 3; 4; 5 ] [ true; true; false; false; true ];
  at q 3;
  check "모드 3 짝수 카운트" (latched16 q) 4;
  at q 4;
  check "모드 3 짝수 카운트 2" (latched16 q) 2;
  at q 4;
  gate q false;
  check_b "낮은 반에서 GATE 하강 — 곧바로 높음" (out2 q) true

(* 세는 동안 쓴 카운트는 지금 반주기가 끝날 때 실린다. 높은 반이 끝나면
   새 카운트의 낮은 반부터 간다. *)
let test_square_wave_reload_at_half () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p 0x96;
  wr p 6;
  at p 2;
  wr p 4;
  check_outs "모드 3 새 카운트" p (List.init 9 (fun i -> i + 1))
    [ true; true; true; false; false; true; true; false; false ]

let test_rate_generator_reload_at_period () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p 0x94;
  wr p 5;
  at p 2;
  wr p 3;
  (* 옛 주기는 tick 5 에 끝난다. 그 뒤 3 펄스 주기 *)
  check_outs "모드 2 새 카운트" p [ 5; 6; 7; 8; 9; 10; 11 ]
    [ false; true; true; false; true; true; false ]

let test_one_shot () =
  let p = make () in
  at p 0;
  ctrl p (ch2_word 1);
  wr16 p 10;
  at p 5;
  check_b "무장만 — OUT 높음" (out2 p) true;
  at p 20;
  gate p true;
  check_b "트리거 펄스에는 아직 높음" (out2 p) true;
  check_outs "원숏" p [ 21; 30; 31 ] [ false; false; true ];
  at p 32;
  gate p false;
  at p 33;
  gate p true;
  check_outs "다시 트리거" p [ 34; 43; 44 ] [ false; false; true ];
  at p 45;
  gate p false;
  at p 46;
  gate p true;
  at p 47;
  wr16 p 3;
  check_outs "새 카운트는 다음 트리거부터" p [ 48; 56; 57 ] [ false; false; true ];
  at p 60;
  gate p false;
  at p 61;
  gate p true;
  check_outs "새 카운트로 트리거" p [ 62; 64; 65 ] [ false; false; true ];
  (* 새 제어 워드는 카운트 레지스터를 비운다 — 카운트 없이 트리거하면 무장이
     안 된 상태다 *)
  at p 70;
  ctrl p (ch2_word 1);
  gate p false;
  at p 71;
  gate p true;
  check_outs "카운트 없는 트리거" p [ 72; 73 ] [ true; true ]

let test_strobes () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 4);
  wr16 p 3;
  check_outs "모드 4" p [ 1; 2; 3; 4; 5; 6 ] [ true; true; true; false; true; true ];
  at p 10;
  wr16 p 2;
  check_outs "모드 4 다시 쓰기" p [ 12; 13; 14 ] [ true; false; true ];
  let q = make () in
  at q 0;
  ctrl q (ch2_word 5);
  wr16 q 3;
  check_outs "모드 5 트리거 전" q [ 1; 4; 8 ] [ true; true; true ];
  at q 10;
  gate q true;
  check_outs "모드 5" q [ 11; 13; 14; 15 ] [ true; true; false; true ]

let test_bcd () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p 0xB1;
  wr16 p 0x0010;
  at p 4;
  check "BCD 10-3" (latched16 p) 0x0007;
  at p 12;
  check "BCD 0 아래는 9999" (latched16 p) 0x9999;
  wr16 p 0x1234;
  at p (13 + 35);
  check "BCD 1234-35" (latched16 p) 0x1199;
  ctrl p 0xB1;
  wr16 p 0x0000;
  at p (49 + 1);
  check "BCD 0 은 10000" (latched16 p) 0x9999

let test_control_word_stops () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p (ch2_word 2);
  wr16 p 100;
  at p 31;
  ctrl p (ch2_word 0);
  at p 500;
  check "제어 워드가 카운터를 멈춘다" (rd16 p) 70;
  check_b "모드 0 제어 워드 — OUT 낮음" (out2 p) false;
  ctrl p (ch2_word 3);
  check_b "모드 3 제어 워드 — OUT 높음" (out2 p) true

let test_port61_bits () =
  let p = make () in
  at p 0;
  Dos_ports.port_out p 0x61 0x0F;
  check "bit 0-3 은 쓴 값" (Dos_ports.port_in p 0x61 land 0x2F) 0x0F;
  Dos_ports.port_out p 0x61 0x31;
  check "bit 5 는 쓴 값이 아니라 OUT2" (Dos_ports.port_in p 0x61 land 0x2F) 0x01;
  ctrl p (ch2_word 3);
  check "OUT2 가 높으면 bit 5" (Dos_ports.port_in p 0x61 land 0x20) 0x20

(* ---------- 채널 0 과 IRQ0 ----------

   채널 0 의 OUT 이 오르는 틱마다 [Dos_ports.irq0_due] 가 한 번 참이다.
   틱 [k] 에서 물으려면 사이클을 [k * cycles_per_pit_tick] 로 둔다. *)

let rd0 p = Dos_ports.port_in p 0x40
let wr0 p v = Dos_ports.port_out p 0x40 v
let latched0 p = ctrl p 0x00; let lo = rd0 p in let hi = rd0 p in lo lor (hi lsl 8)
let due p tick = at p tick; Dos_ports.irq0_due p

(* [ticks] 마다 물어 참이 나온 틱 *)
let irq0_ticks p ticks = List.filter (fun tick -> due p tick) ticks

let test_channel0_after_post () =
  let p = make () in
  (* POST 가 모드 3·65536 을 틱 0 에 실었다. 셈은 다음 틱부터다. *)
  check_b "켠 직후는 요청 없음" (due p 0) false;
  check_b "65536 틱 뒤는 아직" (due p 0x10000) false;
  check_b "65537 틱에 첫 상승" (due p 0x10001) true;
  check_b "같은 상승을 두 번 세지 않는다" (due p 0x10001) false;
  check_b "다음 상승 앞" (due p 0x20000) false;
  check_b "한 주기 뒤 다음 상승" (due p 0x20001) true;
  (* 모드 3 은 반주기마다 둘씩 센다: 상승 직후 65536(= 0), 그 뒤 FFFEh *)
  at p 0x20001;
  check "상승 직후 카운트" (latched0 p) 0x0000;
  at p 0x20002;
  check "한 틱 뒤 카운트" (latched0 p) 0xFFFE

let test_channel0_count_load () =
  let p = make () in
  at p 1000;
  (* 모드 2, 하위·상위 바이트, 카운트 100 *)
  ctrl p 0x34;
  wr0 p 100;
  wr0 p 0;
  (* 1101, 1201, ..., 2001 *)
  check "카운트를 실은 때부터 센다"
    (List.length (irq0_ticks p (List.init 1001 (fun i -> 1001 + i))))
    10;
  let p = make () in
  at p 1000;
  ctrl p 0x34;
  wr0 p 100;
  wr0 p 0;
  check_b "상승은 실은 틱 + 1 + 100" (due p 1100) false;
  check_b "1101 틱에 상승" (due p 1101) true;
  at p 1101;
  check "모드 2 상승 직후 카운트" (latched0 p) 100;
  check_b "1200 틱은 아직" (due p 1200) false;
  check_b "다음 상승 1201" (due p 1201) true

(* 제어 워드는 카운터를 멈춘다. 새 카운트를 쓰기 전까지는 상승이 없다. *)
let test_channel0_control_word_stops () =
  let p = make () in
  at p 100;
  ctrl p 0x34;
  check_b "POST 의 상승 시점이 지나도 요청 없음" (due p 0x10001) false;
  check_b "한참 뒤에도 없음" (due p 0x30000) false;
  wr0 p 10;
  wr0 p 0;
  check_b "카운트를 쓰면 다시 센다" (due p (0x30000 + 11)) true

let test_channel0_merges_missed_rises () =
  let p = make () in
  at p 0;
  ctrl p 0x34;
  wr0 p 10;
  wr0 p 0;
  (* 상승 11, 21, 31, 41 을 한 번에 지나친다 — IRR 은 한 비트다 *)
  check_b "늦게 물어도 한 번" (due p 45) true;
  check_b "지나친 상승은 쌓이지 않는다" (due p 50) false;
  check_b "다음 상승은 51" (due p 51) true

let test_channel0_latch_twice () =
  let p = make () in
  at p 10;
  ctrl p 0x00;
  at p 500;
  ctrl p 0x00;
  let lo = rd0 p in
  let hi = rd0 p in
  (* 모드 3, 65536: 틱 10 은 셈 시작 뒤 9 펄스 — 65536 - 18 *)
  check "두 번째 래치는 무시된다" (lo lor (hi lsl 8)) (0x10000 - 18)

let test_channel0_untouched () =
  let p = make () in
  at p 0;
  ctrl p 0x34;
  wr0 p 0x00;
  ctrl p (ch2_word 0);
  wr p 0x34;
  wr0 p 0x10;
  wr p 0x12;
  (* 채널 0 카운트 1000h 를 틱 0 에 실었다 *)
  check_b "채널 2 를 사이에 써도 채널 0 은 1000h" (due p 0x1000) false;
  check_b "1001h 틱에 상승" (due p 0x1001) true

let ports_bytes p =
  let w = Dos_snap_codec.writer () in
  Dos_ports.write_state w p;
  Dos_snap_codec.contents w

let test_snapshot () =
  let p = make () in
  at p 0;
  gate p true;
  ctrl p 0x96;
  wr p 6;
  at p 2;
  wr p 4;
  let saved = ports_bytes p in
  let q = make () in
  Dos_ports.read_state (Dos_snap_codec.reader saved) q;
  check_b "저장 → 복원 → 저장" (ports_bytes q = saved) true;
  let ticks = List.init 12 (fun i -> i + 3) in
  check_b "복원한 카운터가 같은 OUT" (outs q ticks = outs p ticks) true;
  at p 20;
  at q 20;
  check "복원한 카운터가 같은 카운트" (latched16 q) (latched16 p)

let test_snapshot_refuses_bcd_overflow () =
  let pit = Dos_pit.create () in
  Dos_pit.control pit ~tick:0 0xB0;
  Dos_pit.write pit ~tick:0 ~gate:true 0x60;
  Dos_pit.write pit ~tick:0 ~gate:true 0xEA;
  let w = Dos_snap_codec.writer () in
  Dos_pit.write_state w pit;
  let b = Bytes.of_string (Dos_snap_codec.contents w) in
  (* 맨 앞이 제어 워드 bit 5-0 (8바이트 빅엔디언)이고, 그 마지막 바이트의
     bit 0 이 BCD 다 *)
  let control_low_byte = 7 in
  let bcd_bit = 0x01 in
  let low = Char.code (Bytes.get b control_low_byte) in
  check "바이너리로 저장" (low land bcd_bit) 0;
  Bytes.set b control_low_byte (Char.chr (low lor bcd_bit));
  let refused =
    match Dos_pit.read_state (Dos_snap_codec.reader (Bytes.to_string b)) (Dos_pit.create ()) with
    | () -> false
    | exception Dos_snap_codec.Invalid _ -> true
  in
  check_b "BCD 카운트 60000 은 거부" refused true

(* 기계에서: SeaBIOS 의 지연 루프가 끝나야 한다. 0x61 bit 5 가 쓴 값을
   돌려주면 이 루프는 영원히 돈다. *)
let test_delay_loop_on_machine () =
  let prog =
    "\xe4\x61\x24\xfc\x0c\x01\xe6\x61"  (* in al,61h; and al,FCh; or al,1; out 61h,al *)
    ^ "\xb0\xb0\xe6\x43"                (* mov al,B0h; out 43h,al *)
    ^ "\xb0\x00\xe6\x42\xb0\x08\xe6\x42" (* 0800h *)
    ^ "\xe4\x61\xa8\x20\x74\xfa"        (* l: in al,61h; test al,20h; jz l *)
    ^ "\xb8\x00\x4c\xcd\x21"            (* mov ax,4C00h; int 21h *)
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m prog;
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:Dos_machine.exited);
  check_b "지연 루프가 끝난다" (Dos_machine.exited m) true;
  check_b "0x801 펄스 이상 기다렸다"
    (Cpu86.cycles m.Dos_state.cpu >= 0x801 * cycles_per_pit_tick) true

(* ---------- 다음 OUT 상승 ----------

   [Dos_pit.next_rise] 를 [Dos_pit.out] 을 한 틱씩 짚어 찾은 상승과 견준다.
   짚으면 카운터가 그 틱까지 셈을 정리하므로, 저장·복원으로 만든 복제본에서
   짚는다. 시나리오는 모드 0~5 에 카운트 1(데이터시트가 모드 2·3 에서 금하는
   값)부터 8 까지, 셈 도중 새 카운트, 첫 바이트만 쓴 새 카운트, GATE 를 내렸다
   올리기와 내린 채 두기를 섞는다. *)

let clone pit =
  let w = Dos_snap_codec.writer () in
  Dos_pit.write_state w pit;
  let q = Dos_pit.create () in
  Dos_pit.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) q;
  q

let scan_rise pit ~tick ~limit =
  let q = clone pit in
  let rec go prev x =
    if x > tick + limit then None
    else
      let cur = Dos_pit.out q ~tick:x in
      if (not prev) && cur then Some x else go cur (x + 1)
  in
  go (Dos_pit.out q ~tick) (tick + 1)

type event = Control of int | Byte of int | Gate of bool

let play ~gate events =
  let pit = Dos_pit.create () in
  let level = ref gate in
  List.iter
    (fun (tick, e) ->
      match e with
      | Control v -> Dos_pit.control pit ~tick v
      | Byte v -> Dos_pit.write pit ~tick ~gate:!level v
      | Gate g ->
        if g <> !level then begin
          level := g;
          Dos_pit.gate_edge pit ~tick ~high:g
        end)
    events;
  pit

let show_rise = function None -> "None" | Some x -> string_of_int x

let check_rises name ~gate events ~longest =
  let last = List.fold_left (fun acc (tick, _) -> max acc tick) 0 events in
  let limit = (4 * longest) + 8 in
  for tick = last to last + (2 * longest) + 4 do
    let got = Dos_pit.next_rise (clone (play ~gate events)) ~tick in
    let want = scan_rise (play ~gate events) ~tick ~limit in
    if got <> want then begin
      incr failed;
      Printf.eprintf "FAIL next_rise %s at tick %d: got %s want %s\n%!" name tick
        (show_rise got) (show_rise want)
    end
  done

let test_next_rise_matches_out () =
  let counts = [ 1; 2; 3; 5; 8 ] and second_counts = [ 1; 2; 5 ] in
  List.iter
    (fun mode ->
      (* GATE 로 트리거하는 모드 1·5 는 GATE 를 낮게 두고 쓴 뒤 올린다 *)
      let triggered = mode = 1 || mode = 5 in
      let start = if triggered then [ (5, Gate true) ] else [] in
      List.iter
        (fun n ->
          let load = [ (0, Control (0x30 lor (mode lsl 1))); (1, Byte n); (2, Byte 0) ] in
          let base = load @ start in
          let gate = not triggered in
          let name extra = Printf.sprintf "mode %d n %d%s" mode n extra in
          check_rises (name "") ~gate base ~longest:n;
          let after = if triggered then 5 else 2 in
          for d = 1 to n + 2 do
            let at = after + d in
            List.iter
              (fun n2 ->
                check_rises (name (Printf.sprintf " then %d at %d" n2 at)) ~gate
                  (base @ [ (at, Byte n2); (at, Byte 0) ]) ~longest:(max n n2))
              second_counts;
            check_rises (name (Printf.sprintf " then a lone low byte at %d" at)) ~gate
              (base @ [ (at, Byte 3) ]) ~longest:n;
            List.iter
              (fun e ->
                check_rises (name (Printf.sprintf " gate low at %d for %d" at e)) ~gate
                  (base @ [ (at, Gate false); (at + e, Gate true) ]) ~longest:n)
              [ 1; 2; 3 ];
            check_rises (name (Printf.sprintf " gate left low at %d" at)) ~gate
              (base @ [ (at, Gate false) ]) ~longest:n
          done)
        counts)
    [ 0; 1; 2; 3; 4; 5 ]

(* 기계에서: 게스트가 BIOS 틱(0040:006C) 하나를 기다린 뒤 INT 8 을 걸고 채널
   0 을 모드 2·1000(3E8h)으로 바꾼다. 핸들러는 들어오자마자 채널 0 을 래치해
   읽는다. IRQ0 이 그 카운트를 실은 때부터 센 OUT 상승이면 막 다시 실린
   카운트(1000 에서 몇 틱 뺀 값)를 읽는다. 셈을 켠 때부터 세면 첫 BIOS 틱
   뒤라서 상관없는 값이 나온다. *)
let test_irq0_handler_sees_reload () =
  let count_at = 0x2F0 and results = 0x300 and n = 1000 in
  let prog =
    "\x31\xc0\x8e\xc0\x26\xa1\x6c\x04"     (* 0100: xor ax,ax; mov es,ax; mov ax,[es:046Ch] *)
    ^ "\xfb\x26\x3b\x06\x6c\x04\x74\xf9"   (* 0108: sti; l: cmp ax,[es:046Ch]; je l *)
    ^ "\xfa\xb8\x08\x25\xba\x33\x01\xcd\x21" (* 0110: cli; mov ax,2508h; mov dx,0133h; int 21h *)
    ^ "\xb0\x34\xe6\x43"                   (* 0119: mov al,34h; out 43h,al *)
    ^ "\xb0\xe8\xe6\x40\xb0\x03\xe6\x40"   (* 011D: 카운트 03E8h *)
    ^ "\xfb"                               (* 0125: sti *)
    ^ "\x80\x3e\xf0\x02\x03\x72\xf9"       (* 0126: cmp byte [02F0h],3; jb 0126h *)
    ^ "\xfa\xb8\x00\x4c\xcd\x21"           (* 012D: cli; mov ax,4C00h; int 21h *)
    ^ "\x50\x53"                           (* 0133: push ax; push bx *)
    ^ "\xb0\x00\xe6\x43"                   (* 채널 0 래치 *)
    ^ "\xe4\x40\x88\xc4\xe4\x40\x86\xc4"   (* in al,40h; mov ah,al; in al,40h; xchg al,ah *)
    ^ "\x8a\x1e\xf0\x02\x30\xff\xd1\xe3"   (* mov bl,[02F0h]; xor bh,bh; shl bx,1 *)
    ^ "\x89\x87\x00\x03"                   (* mov [bx+0300h],ax *)
    ^ "\xfe\x06\xf0\x02"                   (* inc byte [02F0h] *)
    ^ "\xb0\x20\xe6\x20\x5b\x58\xcf"       (* EOI; pop bx; pop ax; iret *)
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m prog;
  ignore (Dos_machine.run_until m ~max_steps:500_000 ~stop:Dos_machine.exited);
  check_b "IRQ0 세 번 뒤 끝난다" (Dos_machine.exited m) true;
  let psp = 0x10000 in
  check "핸들러가 세 번 돌았다" (Dos_machine.mem_read m (psp + count_at)) 3;
  for i = 0 to 2 do
    let a = psp + results + (2 * i) in
    let v = Dos_machine.mem_read m a lor (Dos_machine.mem_read m (a + 1) lsl 8) in
    (* 상승에서 래치까지 열 명령 안팎 — 64 틱보다 짧다 *)
    check_b (Printf.sprintf "IRQ0 %d: 래치한 카운트 %d 가 막 다시 실린 값" i v)
      (v <= n && v > n - 64) true
  done;
  (* 첫 BIOS 틱(65537) 뒤 카운트를 싣고, 그 뒤 세 주기 *)
  let first_tick = 0x10001 in
  let ticks = Cpu86.cycles m.Dos_state.cpu / cycles_per_pit_tick in
  check_b "세 주기가 흘렀다" (ticks >= first_tick + (3 * n)) true;
  check_b "세 주기 남짓만 흘렀다" (ticks < first_tick + (3 * n) + 512) true

let () =
  test_mode0_delay_loop ();
  test_mode0_gate_pauses ();
  test_mode0_written_with_gate_low ();
  test_mode0_rewrite_drops_out ();
  test_latch ();
  test_single_byte_access ();
  test_rate_generator ();
  test_square_wave ();
  test_square_wave_reload_at_half ();
  test_rate_generator_reload_at_period ();
  test_one_shot ();
  test_strobes ();
  test_bcd ();
  test_control_word_stops ();
  test_port61_bits ();
  test_channel0_after_post ();
  test_channel0_count_load ();
  test_channel0_control_word_stops ();
  test_channel0_merges_missed_rises ();
  test_channel0_latch_twice ();
  test_channel0_untouched ();
  test_snapshot ();
  test_snapshot_refuses_bcd_overflow ();
  test_delay_loop_on_machine ();
  test_next_rise_matches_out ();
  test_irq0_handler_sees_reload ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n" !failed;
    exit 1
  end;
  print_endline "dos_pit_test: ok"
