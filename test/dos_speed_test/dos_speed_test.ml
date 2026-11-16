(* CPU 속도 — PIT 한 틱에 CPU 사이클 몇 개가 도는가.

   계약: 속도가 바뀌면 게스트가 시간을 재는 길이 모두 같은 비율로
   늘어난다. IRQ0 간격, PIT 카운터, 포트 0x61 리프레시 비트, 0x3DA 수직
   귀선 비트, OPL 타이머, 시각(초)이 그렇다. 실제 시간은 그대로고 그 사이에
   도는 사이클만 많아진다. 스냅숏은 속도를 같이 싣는다. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let pc = Dos_ports.pc_speed

(* 12MHz 는 PIT 의 10 배(11.93MHz)로 맞춰진다. *)
let fast =
  match Dos_ports.speed_of_mhz 12. with
  | Ok s -> s
  | Error e -> failwith e

let fast_factor = 10
let pc_factor = 4
let default_divisor = 0x10000

(* ---------- MHz → 속도 ---------- *)

let test_speed_of_mhz () =
  check_int "PC speed is four PIT ticks" (Dos_ports.speed_hz pc) (pc_factor * 1_193_182);
  check_int "12 MHz rounds to ten PIT ticks" (Dos_ports.speed_hz fast)
    (fast_factor * 1_193_182);
  (match Dos_ports.speed_of_mhz 4.77 with
   | Ok s -> check_int "4.77 MHz is the PC speed" (Dos_ports.speed_hz s) (Dos_ports.speed_hz pc)
   | Error e -> check ("4.77 MHz accepted: " ^ e) false);
  (match Dos_ports.speed_of_mhz 1. with
   | Ok s -> check_int "1 MHz rounds up to one PIT tick" (Dos_ports.speed_hz s) 1_193_182
   | Error e -> check ("1 MHz accepted: " ^ e) false);
  (match Dos_ports.speed_of_mhz 1193.182 with
   | Ok s -> check_int "top speed is a thousand PIT ticks" (Dos_ports.speed_hz s) 1_193_182_000
   | Error e -> check ("1193.182 MHz accepted: " ^ e) false);
  List.iter
    (fun mhz ->
      match Dos_ports.speed_of_mhz mhz with
      | Ok _ -> check (Printf.sprintf "%g MHz refused" mhz) false
      | Error _ -> ())
    [ 0.5; 0.; -5.; 1200.; Float.nan; Float.infinity; Float.neg_infinity ]

(* ---------- 포트 ---------- *)

let ports speed =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  Dos_ports.create ~speed ~mpu401:false ~mem ~video

let pit_count t =
  let lo = Dos_ports.port_in t 0x40 in
  let hi = Dos_ports.port_in t 0x40 in
  lo lor (hi lsl 8)

let refresh_bit t = Dos_ports.port_in t 0x61 land 0x10 <> 0
let vsync_bit t = Dos_ports.port_in t 0x3DA land 0x08 <> 0

let test_ports () =
  let p = ports pc and f = ports fast in
  check_int "PC IRQ0 interval" (Dos_ports.cycles_per_tick p) (default_divisor * pc_factor);
  check_int "fast IRQ0 interval" (Dos_ports.cycles_per_tick f) (default_divisor * fast_factor);
  (* 1000 사이클 뒤: PC 는 PIT 250 틱, 빠른 기계는 100 틱이 지났다. *)
  Dos_ports.set_now p 1000;
  Dos_ports.set_now f 1000;
  check_int "PC PIT count" (pit_count p) (default_divisor - 250);
  check_int "fast PIT count" (pit_count f) (default_divisor - 100);
  (* 리프레시 비트는 PIT 채널 1 의 18 틱마다 뒤집힌다: PC 에서 72 사이클,
     10 배 기계에서 180 사이클. *)
  Dos_ports.set_now p 100;
  Dos_ports.set_now f 100;
  check "PC refresh flipped by 100 cycles" (refresh_bit p);
  check "fast refresh not yet flipped at 100 cycles" (not (refresh_bit f));
  Dos_ports.set_now f 200;
  check "fast refresh flipped by 200 cycles" (refresh_bit f);
  (* 수직 귀선은 PC 에서 70000 사이클, 10 배 기계에서 175000 사이클 칸의
     첫 칸에 켜진다. *)
  Dos_ports.set_now p 100_000;
  Dos_ports.set_now f 100_000;
  check "PC vsync over by 100000 cycles" (not (vsync_bit p));
  check "fast vsync still on at 100000 cycles" (vsync_bit f);
  Dos_ports.set_now f 200_000;
  check "fast vsync over by 200000 cycles" (not (vsync_bit f))

(* OPL 타이머는 레지스터 값에서 256 까지 센다. 값 00h 는 칸 256 개다.
   타이머 1 은 80µs 칸: PC 는 382×256 = 97792 사이클, 10 배 기계는
   955×256 = 244480 사이클에 넘친다. 타이머 2 는 320µs 칸: PC 는
   1527×256 = 390912, 10 배 기계는 3817×256 = 977152 사이클이다. *)
let opl_t1_status = 0xC0                  (* IRQ + 타이머 1 *)
let opl_t2_status = 0xA0                  (* IRQ + 타이머 2 *)
let opl_start_t1 = 0x21                   (* 타이머 2 마스크, 타이머 1 시작 *)
let opl_start_t2 = 0x42                   (* 타이머 1 마스크, 타이머 2 시작 *)

let test_opl () =
  let arm t ~count_reg ~start =
    Dos_ports.set_now t 0;
    List.iter
      (fun (reg, v) -> Dos_ports.port_out t 0x388 reg; Dos_ports.port_out t 0x389 v)
      [ (0x04, 0x60); (0x04, 0x80); (count_reg, 0x00); (0x04, start) ]
  in
  let raised t ~now bits =
    Dos_ports.set_now t now;
    Dos_ports.port_in t 0x388 land bits = bits
  in
  List.iter
    (fun (name, speed, count_reg, start, bits, due) ->
      let t = ports speed in
      arm t ~count_reg ~start;
      check (name ^ " quiet one cycle before") (not (raised t ~now:(due - 1) bits));
      check (name ^ " raised on time") (raised t ~now:due bits))
    [ ("PC OPL timer 1", pc, 0x02, opl_start_t1, opl_t1_status, 382 * 256);
      ("fast OPL timer 1", fast, 0x02, opl_start_t1, opl_t1_status, 955 * 256);
      ("PC OPL timer 2", pc, 0x03, opl_start_t2, opl_t2_status, 1527 * 256);
      ("fast OPL timer 2", fast, 0x03, opl_start_t2, opl_t2_status, 3817 * 256) ]

(* ---------- 기계 ---------- *)

let busy_com = "\xFB\xEB\xFE"  (* sti; jmp $ *)

(* 1 초 남짓 = 10 배 기계에서 1200 만 사이클. PC 라면 2.5 초다. *)
let run_cycles = 12_000_000

let run_fast_machine () =
  let m = Dos_machine.create ~speed:fast () in
  Dos_machine.set_clock m ~year:2000 ~month:1 ~day:1 ~hour:0 ~minute:0 ~second:0;
  Dos_machine.load_com m busy_com;
  let report =
    Dos_machine.run_until m ~max_steps:max_int ~stop:(fun m ->
        Cpu86.cycles m.Dos_state.cpu >= run_cycles)
  in
  check "busy loop ran to the cycle budget" (report.Dos_machine.stop_reason = Dos_machine.Stop_requested);
  m

(* PIT 채널 2 도 PIT 틱으로 센다. 모드 0 에 N 을 쓰면 OUT(포트 0x61 bit5)이
   N+1 틱 뒤에 높아진다. 10 배 기계에서는 그 틱이 10 배 사이클이다. *)
let pit2_mode0 = 0xB0                    (* 채널 2, 하위·상위 바이트, 모드 0 *)
let pit2_count = 0x800

let test_pit2 () =
  let t = ports fast in
  Dos_ports.set_now t 0;
  Dos_ports.port_out t 0x61 0x01;        (* GATE 2 열기 *)
  Dos_ports.port_out t 0x43 pit2_mode0;
  Dos_ports.port_out t 0x42 (pit2_count land 0xFF);
  Dos_ports.port_out t 0x42 (pit2_count lsr 8);
  let out2 now = Dos_ports.set_now t now; Dos_ports.port_in t 0x61 land 0x20 <> 0 in
  let due = (pit2_count + 1) * fast_factor in
  check "fast PIT channel 2 low one cycle before" (not (out2 (due - 1)));
  check "fast PIT channel 2 high after N+1 ticks" (out2 due)

let test_machine m =
  (* IRQ0 은 655360 사이클마다: 1200 만 사이클이면 18 번. 틱을 세는 기준은
     틱이 들어간 명령의 끝이라 한 틱이 덜 셀 수 있다. *)
  let ticks = Dos_machine.tick_count m in
  check (Printf.sprintf "18 ticks or one fewer at ten cycles per PIT tick (got %d)" ticks)
    (ticks = 18 || ticks = 17);
  let _, _, _, h, mi, s = Dos_state.now_fields m in
  check_int "one second passed" ((h * 3600) + (mi * 60) + s) 1

let test_snapshot m =
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok bytes -> (
    match Dos_snapshot.restore bytes with
    | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
    | Ok r ->
      check_int "restored machine keeps its clock" (Dos_ports.cpu_hz r.Dos_state.ports)
        (Dos_ports.speed_hz fast);
      check_int "restored machine keeps its IRQ0 interval"
        (Dos_ports.cycles_per_tick r.Dos_state.ports)
        (default_divisor * fast_factor))

let () =
  test_speed_of_mhz ();
  test_ports ();
  test_opl ();
  test_pit2 ();
  let m = run_fast_machine () in
  test_machine m;
  test_snapshot m;
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos speed: all passed\n"
