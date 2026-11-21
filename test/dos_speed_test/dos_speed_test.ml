(* CPU 속도 — PIT 한 틱에 CPU 사이클 몇 개가 도는가.

   계약: 속도가 바뀌면 게스트가 시간을 재는 길이 모두 같은 비율로
   늘어난다. IRQ0 간격, PIT 카운터, 포트 0x61 리프레시 비트, 0x3DA 수직
   귀선 비트, OPL 타이머, 키보드 바이트 간격, 시각(초)이 그렇다. 실제
   시간은 그대로고 그 사이에 도는 사이클만 많아진다. 스냅숏은 속도를 같이
   싣는다. *)

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

(* 채널 0 은 POST 가 틱 0 에 실은 모드 3·65536 으로 켜진다. 셈은 다음
   틱부터라, 첫 OUT 상승(IRQ0)은 65537 틱이고 그 뒤로 65536 틱마다다.
   모드 3 은 반주기마다 한 틱에 둘씩 준다. *)
let first_irq0_tick = default_divisor + 1
let mode3_count_after ticks = (default_divisor - (2 * (ticks - 1))) land 0xffff

(* [ports] 를 [from] 사이클부터 PIT 한 틱([factor] 사이클)씩 옮기며 IRQ0 이
   온 사이클을 [n] 개까지 모은다. 네 주기 안에 못 모으면 모은 것만 준다. *)
let irq0_cycles ports ~factor ~from ~n =
  let limit = from + (4 * default_divisor * factor) in
  let rec go now acc count =
    if count = n || now > limit then List.rev acc
    else begin
      Dos_ports.set_now ports now;
      if Dos_ports.irq0_due ports then go (now + factor) (now :: acc) (count + 1)
      else go (now + factor) acc count
    end
  in
  go from [] 0

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
  Dos_ports.create ~speed ~mpu401:false ~mem ~video ~sound_blaster:false
    ~cmos:(Dos_cmos.create ~clock:(fun () -> invalid_arg "no clock in this test"))
    ~a20:(Dos_a20.create Dos_a20.One_megabyte)

let pit_count t =
  let lo = Dos_ports.port_in t 0x40 in
  let hi = Dos_ports.port_in t 0x40 in
  lo lor (hi lsl 8)

let refresh_bit t = Dos_ports.port_in t 0x61 land 0x10 <> 0
let vsync_bit t = Dos_ports.port_in t 0x3DA land 0x08 <> 0

let test_ports () =
  let p = ports pc and f = ports fast in
  (* 1000 사이클 뒤: PC 는 PIT 250 틱, 빠른 기계는 100 틱이 지났다. *)
  Dos_ports.set_now p 1000;
  Dos_ports.set_now f 1000;
  check_int "PC PIT count" (pit_count p) (mode3_count_after 250);
  check_int "fast PIT count" (pit_count f) (mode3_count_after 100);
  (* 리프레시 비트는 PIT 채널 1 의 18 틱마다 뒤집힌다: PC 에서 72 사이클,
     10 배 기계에서 180 사이클. *)
  Dos_ports.set_now p 100;
  Dos_ports.set_now f 100;
  check "PC refresh flipped by 100 cycles" (refresh_bit p);
  check "fast refresh not yet flipped at 100 cycles" (not (refresh_bit f));
  Dos_ports.set_now f 200;
  check "fast refresh flipped by 200 cycles" (refresh_bit f);
  (* 수직 귀선은 412 번째 줄(13.09ms)에서 켜진다. PC 에서는 62486 사이클,
     10 배 기계에서는 156215 사이클이다. *)
  Dos_ports.set_now p 62_600;
  Dos_ports.set_now f 62_600;
  check "PC vsync on at 62600 cycles" (vsync_bit p);
  check "fast vsync not yet on at 62600 cycles" (not (vsync_bit f));
  Dos_ports.set_now f 156_500;
  check "fast vsync on at 156500 cycles" (vsync_bit f);
  (* 첫 IRQ0 은 65537 틱: PC 는 262148 사이클, 10 배 기계는 655370 사이클 *)
  List.iter
    (fun (name, ports, factor) ->
      let at = first_irq0_tick * factor in
      Dos_ports.set_now ports (at - 1);
      check (name ^ " IRQ0 not yet one cycle early") (not (Dos_ports.irq0_due ports));
      Dos_ports.set_now ports at;
      check (name ^ " IRQ0 at 65537 PIT ticks") (Dos_ports.irq0_due ports))
    [ ("PC", p, pc_factor); ("fast", f, fast_factor) ]

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

(* 키보드도 실제 시간으로 잰다: 바이트 사이 1ms, 키 누름 100ms. 대문자는
   Shift make 다음에 글자 make 가 나간다. *)
let shifted_a = 0x1E41

let test_keyboard () =
  let hz = Dos_ports.speed_hz fast in
  let k = Dos_kbc.create () in
  Dos_kbc.press k ~hz ~now:0 shifted_a;
  check "fast keyboard: Shift make at once" (Dos_kbc.latch k ~hz ~now:0);
  ignore (Dos_kbc.read_data k ~hz ~now:0);
  check_int "fast keyboard: next byte 1 ms later" (Dos_kbc.ready_at k) (hz / 1000);
  check "fast keyboard: A make then" (Dos_kbc.latch k ~hz ~now:(hz / 1000));
  ignore (Dos_kbc.read_data k ~hz ~now:(hz / 1000));
  check_int "fast keyboard: break 100 ms after the press" (Dos_kbc.ready_at k) (hz / 10)

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
      (* 셋째와 둘째의 차이를 본다 — 첫째는 저장 전에 지난 상승일 수 있다 *)
      match
        irq0_cycles r.Dos_state.ports ~factor:fast_factor
          ~from:(Cpu86.cycles r.Dos_state.cpu) ~n:3
      with
      | [ _; second; third ] ->
        check_int "restored machine keeps its IRQ0 interval" (third - second)
          (default_divisor * fast_factor)
      | got -> check_int "restored machine raises IRQ0 three times" (List.length got) 3)

let () =
  test_speed_of_mhz ();
  test_ports ();
  test_opl ();
  test_pit2 ();
  test_keyboard ();
  let m = run_fast_machine () in
  test_machine m;
  test_snapshot m;
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos speed: all passed\n"
