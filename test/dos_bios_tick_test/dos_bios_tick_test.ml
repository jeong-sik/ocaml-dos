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

(* ---------- 인터럽트를 끈 채 틱을 기다리는 지연 루프 ----------
   SeaBIOS handle_1a00 은 틱을 읽기 전에 yield() 로 보류된 IRQ 를 받게 한다.
   cli 로 시작해 INT 1Ah AH=0 만 되풀이하는 프로그램도 틱이 오르는 것을 본다
   (Arcy 2 의 지연 루프). *)
let () =
  let out = 0x200 in
  let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff)) in
  let code =
    "\xfa"                                (* cli *)
    ^ "\xb4\x00\xcd\x1a"                  (* mov ah,0; int 1Ah *)
    ^ "\x89\xd6"                          (* mov si,dx *)
    ^ "\xb4\x00\xcd\x1a"                  (* poll: mov ah,0; int 1Ah *)
    ^ "\x39\xf2"                          (* cmp dx,si *)
    ^ "\x74\xf8"                          (* je poll *)
    ^ "\x89\x16" ^ w out                  (* mov [out],dx *)
    ^ "\x89\x36" ^ w (out + 2)            (* mov [out+2],si *)
    ^ "\xb8\x00\x4c\xcd\x21"
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  let base = Dos_machine.psp_seg_of m * 16 in
  write_ticks m 1000;
  Dos_machine.run m ~max_steps:3_000_000;
  let byte a = Dos_machine.mem_read m (base + a) in
  let word a = byte a lor (byte (a + 1) lsl 8) in
  check "delay loop with IF=0: exited" (if Dos_machine.exited m then 1 else 0) 1;
  check "delay loop with IF=0: first reading" (word (out + 2)) 1000;
  check "delay loop with IF=0: the count grew by one" (word out) 1001

(* ---------- INT 1Ch 훅이 DX 를 지워도 프로그램의 DX 는 그대로 ----------
   Castle Adventure 의 INT 1Ch 훅은 DX 를 저장하지 않고 `xor dx,dx` 로 지운다.
   메인 루프는 DX=1234h 를 두고 훅이 한 번 돌 때까지 기다린 뒤 DX 를 본다.
   1234h 면 [out]=1, 0 이면 [out]=2. *)
let () =
  let counter = 0x200 and out = 0x202 in
  let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff)) in
  let main handler =
    "\x31\xc0\x8e\xc0"                      (* xor ax,ax; mov es,ax *)
    ^ "\x26\xc7\x06\x70\x00" ^ w handler     (* mov word es:[70h],handler (INT 1Ch) *)
    ^ "\x8c\xc8\x26\xa3\x72\x00"            (* mov ax,cs; mov es:[72h],ax *)
    ^ "\xfb"                                  (* sti *)
    ^ "\xba\x34\x12"                          (* mov dx,1234h *)
    ^ "\x80\x3e" ^ w counter ^ "\x01"          (* L: cmp byte [counter],1 *)
    ^ "\x72\xf9"                              (* jb L — 훅이 돌 때까지 *)
    ^ "\x81\xfa\x34\x12"                      (* cmp dx,1234h *)
    ^ "\x75\x07"                              (* jne bad *)
    ^ "\xc6\x06" ^ w out ^ "\x01"              (* mov byte [out],1 *)
    ^ "\xeb\x05"                              (* jmp exit *)
    ^ "\xc6\x06" ^ w out ^ "\x02"              (* bad: mov byte [out],2 *)
    ^ "\xb8\x00\x4c\xcd\x21"                  (* exit: mov ax,4C00h; int 21h *)
  in
  let handler =
    "\x2e\xfe\x06" ^ w counter               (* inc byte cs:[counter] *)
    ^ "\x31\xd2"                              (* xor dx,dx *)
    ^ "\xcf"                                  (* iret *)
  in
  let handler_off = 0x100 + String.length (main 0) in
  let m = Dos_machine.create () in
  Dos_machine.load_com m (main handler_off ^ handler);
  let base = Dos_machine.psp_seg_of m * 16 in
  Dos_machine.run m ~max_steps:3_000_000;
  let byte a = Dos_machine.mem_read m (base + a) in
  check "INT 1Ch hook clobbers DX: exited" (if Dos_machine.exited m then 1 else 0) 1;
  check "INT 1Ch hook clobbers DX: hook ran" (if byte counter >= 1 then 1 else 0) 1;
  check "INT 1Ch hook clobbers DX: DX kept" (byte out) 1

let () =
  if !failed = 0 then print_endline "dos bios tick: all passed"
  else begin
    Printf.eprintf "dos bios tick: %d failures\n%!" !failed;
    exit 1
  end
