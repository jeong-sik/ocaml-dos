(* BIOS 틱(0x40:0x6C)과 자정 넘김(0x40:0x70).
   SeaBIOS src/clock.c clock_setup·clock_update, src/hw/timer.c
   ticks_from_ms, src/std/bda.h TICKS_PER_DAY. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let midnight_flag = 0x470
let tick_lo = 0x46C
let tick_hi = 0x46E
let ticks_per_day = 0x1800B0

let set_clock m ~hour ~minute ~second =
  Dos_machine.set_clock m ~year:1992 ~month:3 ~day:4 ~hour ~minute ~second

(* ---------- 부팅 값 ---------- *)

let () =
  (* ceil(ceil(ms × 3579545 / 65536) / 3000) — 손으로 푼 값 *)
  check "boot 08:00:00" (Dos_machine.tick_count (Dos_machine.create ())) 524348;
  let m = Dos_machine.create () in
  set_clock m ~hour:0 ~minute:0 ~second:0;
  check "midnight" (Dos_machine.tick_count m) 0;
  (* SeaBIOS 의 첫 올림이 값을 바꾸는 시각: 내림이면 27419 *)
  set_clock m ~hour:0 ~minute:25 ~second:6;
  check "first rounding up" (Dos_machine.tick_count m) 27420;
  set_clock m ~hour:12 ~minute:0 ~second:0;
  check "noon" (Dos_machine.tick_count m) 786522;
  set_clock m ~hour:23 ~minute:59 ~second:59;
  check "last second" (Dos_machine.tick_count m) 1573025;
  (* 기준시각을 다시 정하면 자정 넘김 표시도 지운다 *)
  Dos_machine.mem_write m midnight_flag 5;
  set_clock m ~hour:1 ~minute:0 ~second:0;
  check "set_clock clears the midnight flag" (Dos_machine.mem_read m midnight_flag) 0

(* ---------- IRQ0 한 번 ---------- *)

let spin = "\xfb\xeb\xfe"                 (* sti; jmp $ *)

let write_ticks m v =
  let w a x =
    Dos_machine.mem_write m a (x land 0xFF);
    Dos_machine.mem_write m (a + 1) ((x lsr 8) land 0xFF)
  in
  w tick_lo (v land 0xFFFF);
  w tick_hi (v lsr 16)

let irq0_routine_steps = 100

let spinning () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m spin;
  m

(* 틱을 [start] 로 두고 IRQ0 한 번이 지나갈 때까지 돌린다. *)
let after_one_tick ?(m = spinning ()) start =
  write_ticks m start;
  let flag = Dos_machine.mem_read m midnight_flag in
  ignore
    (Dos_machine.run_until m ~max_steps:3_000_000 ~stop:(fun m ->
         Dos_machine.tick_count m <> start || Dos_machine.mem_read m midnight_flag <> flag));
  (* 멈춘 자리는 ROM 루틴 한가운데일 수 있다(저워드만 쓴 순간). 루틴을
     끝까지 돌린다 — 다음 틱은 수십만 사이클 뒤다. *)
  ignore (Dos_machine.run_until m ~max_steps:irq0_routine_steps ~stop:(fun _ -> false));
  m

let tick_case name start ~ticks ~flag =
  let m = after_one_tick start in
  check (name ^ ": ticks") (Dos_machine.tick_count m) ticks;
  check (name ^ ": midnight flag") (Dos_machine.mem_read m midnight_flag) flag

let () =
  tick_case "plain" 524348 ~ticks:524349 ~flag:0;
  (* 저워드에서 고워드로 자리올림 — 0x180000 은 아직 하루가 안 된다 *)
  tick_case "carry into the high word" 0x17FFFF ~ticks:0x180000 ~flag:0;
  tick_case "one before a day" (ticks_per_day - 2) ~ticks:(ticks_per_day - 1) ~flag:0;
  tick_case "reaches a day" (ticks_per_day - 1) ~ticks:0 ~flag:1;
  (* 게스트가 AH=01h 로 하루보다 큰 값을 넣어도 다음 틱에 0 으로 *)
  tick_case "past a day" 0x190000 ~ticks:0 ~flag:1;
  tick_case "low word past B0h" (ticks_per_day + 0x10) ~ticks:0 ~flag:1;
  (* 두 번 넘기면 표시는 2 다(SeaBIOS 는 더한다) *)
  let m = after_one_tick (ticks_per_day - 1) in
  let m = after_one_tick ~m (ticks_per_day - 1) in
  check "two midnights" (Dos_machine.mem_read m midnight_flag) 2

(* ---------- 게스트가 INT 1Ah AH=00h 로 읽는다 ---------- *)

(* 하루 두 틱 전에서 시작해 보통 틱 하나와 자정 틱 하나를 지난다. 그
   뒤 AH=00h 를 두 번 부른다. 첫 번째는 AL=1 을 받고 표시를 지운다. *)
let () =
  let out = 0x200 in
  let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff)) in
  let code =
    "\xfb"                                (* sti *)
    ^ "\xb8\x40\x00\x8e\xc0"              (* mov ax,40h; mov es,ax *)
    ^ "\x26\x80\x3e\x70\x00\x00"          (* wait: cmp byte es:[70h],0 *)
    ^ "\x74\xf8"                          (* je wait *)
    ^ "\xb4\x00\xcd\x1a"                  (* mov ah,0; int 1Ah *)
    ^ "\xa2" ^ w out                      (* mov [out],al *)
    ^ "\x89\x0e" ^ w (out + 2)            (* mov [out+2],cx *)
    ^ "\x89\x16" ^ w (out + 4)            (* mov [out+4],dx *)
    ^ "\xb4\x00\xcd\x1a"                  (* mov ah,0; int 1Ah *)
    ^ "\xa2" ^ w (out + 6)                (* mov [out+6],al *)
    ^ "\xb8\x00\x4c\xcd\x21"
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  let base = Dos_machine.psp_seg_of m * 16 in
  write_ticks m (ticks_per_day - 2);
  Dos_machine.run m ~max_steps:3_000_000;
  let byte a = Dos_machine.mem_read m (base + a) in
  let word a = byte a lor (byte (a + 1) lsl 8) in
  check "guest: exited" (if Dos_machine.exited m then 1 else 0) 1;
  check "guest: AL after midnight" (byte out) 1;
  check "guest: CX after midnight" (word (out + 2)) 0;
  check "guest: DX after midnight" (word (out + 4)) 0;
  check "guest: AL read again" (byte (out + 6)) 0

let () =
  if !failed = 0 then print_endline "dos bios tick: all passed"
  else begin
    Printf.eprintf "dos bios tick: %d failures\n%!" !failed;
    exit 1
  end
