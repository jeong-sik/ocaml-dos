(* DOS 시각·날짜 — MS-DOS 4.0 src/BIOS/MSCLOCK.ASM(TIM$READ,
   TIME_TO_TICKS), src/DOS/TIME.ASM, src/BIOS/READCLOC.INC. 기대값은
   그 식으로 손 계산했다. 요일은 0 이 일요일. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end
let check_true name b = if not b then begin incr failed; Printf.eprintf "FAIL %s\n%!" name end

let hms (t : Dos_clock.time) = (t.hour, t.minute, t.second, t.hundredths)
let check_time name got (h, m, s, c) =
  let gh, gm, gs, gc = hms got in
  check (name ^ ": hour") gh h; check (name ^ ": minute") gm m;
  check (name ^ ": second") gs s; check (name ^ ": hundredths") gc c

let date y m d =
  match Dos_clock.date_of_fields ~year:y ~month:m ~day:d with
  | Some v -> v
  | None -> failwith (Printf.sprintf "test date %d-%d-%d" y m d)

let time h m s c =
  match Dos_clock.time_of_fields ~hour:h ~minute:m ~second:s ~hundredths:c with
  | Some v -> v
  | None -> failwith "test time"

(* ---------- 틱 ↔ 시각 ---------- *)

let () =
  check_time "tick 0" (Dos_clock.time_of_ticks 0) (0, 0, 0, 0);
  check_time "one tick is 5 hundredths" (Dos_clock.time_of_ticks 1) (0, 0, 0, 5);
  check_time "boot ticks" (Dos_clock.time_of_ticks 524348) (8, 0, 0, 7);
  check_time "last tick of a day" (Dos_clock.time_of_ticks 0x1800AF) (23, 59, 59, 94);
  check "08:00:00.00 to ticks" (Dos_clock.ticks_of_time (time 8 0 0 0)) 524346;
  check "23:59:59.99 to ticks" (Dos_clock.ticks_of_time (time 23 59 59 99)) 1573039;
  (* 되돌리면 한 틱 모자랄 수 있다 — MS-DOS 와 같다 *)
  check "round trip loses a tick"
    (Dos_clock.ticks_of_time (Dos_clock.time_of_ticks 524348)) 524347

(* ---------- 검사 ---------- *)

let () =
  let t h m s c = Dos_clock.time_of_fields ~hour:h ~minute:m ~second:s ~hundredths:c <> None in
  check_true "23:59:59.99 ok" (t 23 59 59 99);
  check_true "hour 24" (not (t 24 0 0 0));
  check_true "minute 60" (not (t 0 60 0 0));
  check_true "second 60" (not (t 0 0 60 0));
  check_true "hundredths 100" (not (t 0 0 0 100));
  check_true "negative" (not (t (-1) 0 0 0));
  let d y m dd = Dos_clock.date_of_fields ~year:y ~month:m ~day:dd <> None in
  check_true "1980-01-01 ok" (d 1980 1 1);
  check_true "1979-12-31" (not (d 1979 12 31));
  check_true "2099-12-31 ok" (d 2099 12 31);
  check_true "2100-01-01" (not (d 2100 1 1));
  check_true "month 0" (not (d 1990 0 1));
  check_true "month 13" (not (d 1990 13 1));
  check_true "day 0" (not (d 1990 1 0));
  check_true "April 31" (not (d 1990 4 31));
  check_true "1992-02-29 ok" (d 1992 2 29);
  check_true "1990-02-29" (not (d 1990 2 29));
  check_true "2000-02-29 ok" (d 2000 2 29)

(* ---------- 날 수 ---------- *)

let () =
  List.iter
    (fun ((y, m, dd), days, wd) ->
      let name = Printf.sprintf "%04d-%02d-%02d" y m dd in
      check (name ^ ": days") (Dos_clock.days_of_date (date y m dd)) days;
      check (name ^ ": weekday") (Dos_clock.weekday days) wd)
    [ ((1980, 1, 1), 0, 2); ((1980, 3, 1), 60, 6); ((1981, 1, 1), 366, 4);
      ((1990, 1, 1), 3653, 1); ((1992, 2, 29), 4442, 6); ((1992, 3, 4), 4446, 3);
      ((2000, 1, 1), 7305, 6); ((2099, 12, 31), 43829, 4) ];
  (* 날 수 → 날짜 → 날 수 는 모든 날에서 제자리 *)
  let bad = ref 0 in
  for n = 0 to 43829 do
    if Dos_clock.days_of_date (Dos_clock.date_of_days n) <> n then incr bad
  done;
  check "every day round trips" !bad 0;
  check "next day" (Dos_clock.next_day 43828) 43829;
  check "no day after 2099-12-31" (Dos_clock.next_day 43829) 43829

(* ---------- 기계 위 ---------- *)

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let out = 0x400

(* AX·CX·DX 를 넣고 INT 21h, 돌려받은 AX·CX·DX 를 out+6k 에 둔다 *)
let call k ~ax ?(cx = 0) ?(dx = 0) () =
  let at = out + (6 * k) in
  "\xb8" ^ w ax ^ "\xb9" ^ w cx ^ "\xba" ^ w dx ^ "\xcd\x21"
  ^ "\xa3" ^ w at ^ "\x89\x0e" ^ w (at + 2) ^ "\x89\x16" ^ w (at + 4)

let wait_midnight =
  "\xfb\xb8\x40\x00\x8e\xc0"             (* sti; mov ax,40h; mov es,ax *)
  ^ "\x26\x80\x3e\x70\x00\x00\x74\xf8"   (* wait: cmp byte es:[70h],0; je wait *)

let quit = "\xb8\x00\x4c\xcd\x21"

let write_ticks m v =
  List.iteri (fun i b -> Dos_machine.mem_write m (0x46C + i) ((v lsr (8 * b)) land 0xff))
    [ 0; 1; 2; 3 ]

let run ?(setup = fun _ -> ()) code =
  let m = Dos_machine.create () in
  setup m;
  Dos_machine.load_com m (code ^ quit);
  let base = Dos_machine.psp_seg_of m * 16 in
  Dos_machine.run m ~max_steps:3_000_000;
  check_true "program exited" (Dos_machine.exited m);
  let word a = Dos_machine.mem_read m (base + a) lor (Dos_machine.mem_read m (base + a + 1) lsl 8) in
  let reg k i = word (out + (6 * k) + (2 * i)) in
  (m, reg)

let lo v = v land 0xff
let hi v = v lsr 8

let () =
  (* 2Ch: 부팅 틱 524348 → 08:00:00.07, AL=0 *)
  let _, reg = run (call 0 ~ax:0x2C55 ()) in
  check "2Ch AL" (lo (reg 0 0)) 0;
  check "2Ch hour" (hi (reg 0 1)) 8;
  check "2Ch minute" (lo (reg 0 1)) 0;
  check "2Ch second" (hi (reg 0 2)) 0;
  check "2Ch hundredths" (lo (reg 0 2)) 7;
  ()

(* 시각은 틱에서 온다: 틱만 정오(786522)로 바꾸면 2Ch 는 정오이고,
   RTC(INT 1Ah AH=02h)는 기준시각 08 시 그대로다 *)
let () =
  let code =
    "\xb8" ^ w 0x2C00 ^ "\xcd\x21" ^ "\x89\x0e" ^ w out
    ^ "\xb4\x02\xcd\x1a" ^ "\x89\x0e" ^ w (out + 2)
  in
  let _, reg = run ~setup:(fun m -> write_ticks m 786522) code in
  check "2Ch reads the ticks" (hi (reg 0 0)) 12;
  check "RTC keeps its own time (BCD 08h)" (hi (reg 0 1)) 0x08

let () =
  (* 2Dh: 범위 밖이면 AL=FFh 이고 틱은 그대로 *)
  let m, reg = run (call 0 ~ax:0x2D00 ~cx:0x1800 ~dx:0 ()) in
  check "2Dh hour 24: AL" (lo (reg 0 0)) 0xFF;
  check_true "2Dh hour 24: ticks kept" (Dos_machine.tick_count m >= 524348);
  let _, reg = run (call 0 ~ax:0x2D00 ~cx:0x0000 ~dx:0x0064 ()) in
  check "2Dh hundredths 100: AL" (lo (reg 0 0)) 0xFF;
  (* 12:34:56.78 → 틱 824695 → 다시 읽으면 12:34:56.77 *)
  let m, reg =
    run (call 0 ~ax:0x2D00 ~cx:0x0C22 ~dx:0x384E () ^ call 1 ~ax:0x2C00 ())
  in
  check "2Dh AL" (lo (reg 0 0)) 0;
  check "2Dh ticks" (Dos_machine.tick_count m) 824695;
  check "2Ch after 2Dh: CX" (reg 1 1) 0x0C22;
  check "2Ch after 2Dh: DX" (reg 1 2) 0x384D

let set_date y mo d m = Dos_machine.set_clock m ~year:y ~month:mo ~day:d ~hour:8 ~minute:0 ~second:0

let () =
  (* 자정을 넘기면 날짜가 하루 간다: 1992-02-28 → 02-29(토) *)
  let _, reg =
    run ~setup:(fun m -> set_date 1992 2 28 m; write_ticks m 0x1800AF)
      (wait_midnight ^ call 0 ~ax:0x2A00 () ^ call 1 ~ax:0x2C00 ())
  in
  check "midnight: year" (reg 0 1) 1992;
  check "midnight: month/day" (reg 0 2) 0x021D;
  check "midnight: weekday" (lo (reg 0 0)) 6;
  check "midnight: time CX" (reg 1 1) 0;
  check "midnight: time DH" (hi (reg 1 2)) 0;
  (* 표시가 2 여도 하루다 *)
  let _, reg =
    run ~setup:(fun m -> set_date 1992 2 28 m; Dos_machine.mem_write m 0x470 2)
      (call 0 ~ax:0x2A00 ())
  in
  check "flag 2 is one day" (reg 0 2) 0x021D;
  (* 읽으면 표시를 지운다: 두 번 읽어도 하루, 그 뒤 INT 1Ah AH=00h 는 AL=0 *)
  let code =
    call 0 ~ax:0x2A00 () ^ call 1 ~ax:0x2A00 ()
    ^ "\xb4\x00\xcd\x1a" ^ "\xa2" ^ w (out + 12)
  in
  let _, reg = run ~setup:(fun m -> set_date 1992 2 28 m; Dos_machine.mem_write m 0x470 1) code in
  check "first read" (reg 0 2) 0x021D;
  check "second read" (reg 1 2) 0x021D;
  check "INT 1Ah after the DOS read: AL" (lo (reg 2 0)) 0;
  (* 2Dh 는 먼저 읽어 넘긴 자정을 날짜에 반영한다 *)
  (* 그래서 2Dh 뒤 INT 1Ah AH=00h 는 AL=0 이고, 날짜는 이미 넘어가 있다 *)
  let code =
    call 0 ~ax:0x2D00 ~cx:0x0A00 ~dx:0 ()
    ^ "\xb4\x00\xcd\x1a" ^ "\xa2" ^ w (out + 6)
    ^ call 2 ~ax:0x2A00 ()
  in
  let _, reg =
    run ~setup:(fun m -> set_date 1992 2 28 m; Dos_machine.mem_write m 0x470 1) code
  in
  check "2Dh clears the midnight flag" (lo (reg 1 0)) 0;
  check "2Dh applies the pending midnight" (reg 2 2) 0x021D;
  (* 2099-12-31 다음 자정에도 날짜는 그대로 *)
  let _, reg =
    run ~setup:(fun m -> set_date 2099 12 31 m; write_ticks m 0x1800AF)
      (wait_midnight ^ call 0 ~ax:0x2A00 ())
  in
  check "after 2099-12-31: year" (reg 0 1) 2099;
  check "after 2099-12-31: month/day" (reg 0 2) 0x0C1F

let () =
  (* 2Bh: 범위 밖이면 AL=FFh, 날짜 그대로 *)
  let _, reg = run (call 0 ~ax:0x2B00 ~cx:1990 ~dx:0x021E () ^ call 1 ~ax:0x2A00 ()) in
  check "2Bh Feb 30: AL" (lo (reg 0 0)) 0xFF;
  check "2Bh Feb 30: date kept" (reg 1 2) 0x0101;
  let _, reg = run (call 0 ~ax:0x2B00 ~cx:1979 ~dx:0x0C1F ()) in
  check "2Bh 1979: AL" (lo (reg 0 0)) 0xFF;
  let _, reg = run (call 0 ~ax:0x2B00 ~cx:2100 ~dx:0x0101 ()) in
  check "2Bh 2100: AL" (lo (reg 0 0)) 0xFF;
  (* 2000-01-01 은 토요일. DODATE 가 시각을 읽어 다시 쓰므로 틱
     524348 은 524347 이 된다. *)
  let m, reg = run (call 0 ~ax:0x2B00 ~cx:2000 ~dx:0x0101 () ^ call 1 ~ax:0x2A00 ()) in
  check "2Bh AL" (lo (reg 0 0)) 0;
  check "2Ah after 2Bh: year" (reg 1 1) 2000;
  check "2Ah after 2Bh: month/day" (reg 1 2) 0x0101;
  check "2Ah after 2Bh: weekday" (lo (reg 1 0)) 6;
  check "2Bh rewrites the ticks" (Dos_machine.tick_count m) 524347

let () =
  (* 날짜는 스냅숏에 실린다 *)
  let m, _ = run (call 0 ~ax:0x2B00 ~cx:2000 ~dx:0x0101 ()) in
  (match Dos_snapshot.save m with
   | Error e -> incr failed; prerr_endline ("FAIL save: " ^ Dos_snapshot.save_error_to_string e)
   | Ok s ->
     (match Dos_snapshot.restore s with
      | Error e -> incr failed; prerr_endline ("FAIL restore: " ^ Dos_snapshot.error_to_string e)
      | Ok r ->
        check "snapshot keeps the DOS days" r.Dos_state.dos_days 7305));
  (* 범위 밖 날짜로 set_clock 하면 Invalid_argument *)
  let m = Dos_machine.create () in
  check_true "set_clock 1979 refused"
    (match set_date 1979 12 31 m with
     | () -> false
     | exception Invalid_argument _ -> true)

let () =
  if !failed = 0 then print_endline "dos clock: all passed"
  else begin
    Printf.eprintf "dos clock: %d failures\n%!" !failed;
    exit 1
  end
