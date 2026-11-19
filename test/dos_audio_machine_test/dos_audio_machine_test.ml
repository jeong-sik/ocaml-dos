(* RFC 0006 stage 2: the PC speaker through the machine's ports. A COM
   program plays a tone; the samples are checked for the tone, for being
   the same however often the host takes them, and for not changing the
   machine's path. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))

let tone_count = 1193      (* 1193182 / 1193 = 1000.15 Hz *)
let second_count = 597     (* 1998.6 Hz *)

(* Channel 2 in mode 3 at [tone_count], speaker on, a wait, a new count
   while counting, a wait that reads [poll] (a port, or none), a control
   word that stops the counter (OUT high) for a while, the first count
   again, speaker off, a wait, exit. *)
let set_count n = "\xb8" ^ w16 n ^ "\xe6\x42\x88\xe0\xe6\x42"
let short_wait = "\xb9" ^ w16 0x2000 ^ "\xe2\xfe"
let port_b v = "\xb0" ^ String.make 1 (Char.chr v) ^ "\xe6\x61"   (* mov al,v; out 61h,al *)

let program ~poll =
  let wait_poll =
    match poll with
    | Some port -> "\xb9" ^ w16 0x4000 ^ "\xe4" ^ String.make 1 (Char.chr port) ^ "\xe2\xfc"
    | None -> "\xb9" ^ w16 0x4000 ^ "\xe2\xfe"
  in
  "\xb0\xb6\xe6\x43"                       (* mov al,0B6h; out 43h,al *)
  ^ set_count tone_count
  ^ "\xe4\x61\x0c\x03\xe6\x61"             (* in al,61h; or al,3; out 61h,al *)
  ^ "\xb9\xff\xff\xe2\xfe"                 (* mov cx,0FFFFh; loop $ *)
  ^ set_count second_count
  ^ wait_poll
  ^ "\xb0\xb6\xe6\x43" ^ short_wait
  ^ set_count tone_count ^ short_wait
  ^ "\xe4\x61\x24\xfc\xe6\x61"             (* speaker off *)
  ^ short_wait
  ^ "\xcd\x20"

(* Mode 0 restarted over and over, as a PWM player does: each LSB write
   pulls OUT low, the count of 40 ticks lets it rise before the next
   write. *)
let pwm =
  "\xb0\xb0\xe6\x43"                       (* ch2, LSB then MSB, mode 0 *)
  ^ port_b 0x03
  ^ "\xb9" ^ w16 2000                        (* mov cx,2000 *)
  ^ "\xb0\x28\xe6\x42\xb0\x00\xe6\x42"       (* count 40 *)
  ^ "\xbb" ^ w16 30 ^ "\x4b\x75\xfd"            (* mov bx,30; dec bx; jnz $-1 *)
  ^ "\xe2\xf0"                               (* loop to the count, 16 bytes back *)
  ^ port_b 0x00 ^ "\xcd\x20"

(* Gate without data, then data without gate, on a running tone. *)
let gate_and_data =
  "\xb0\xb6\xe6\x43" ^ set_count tone_count
  ^ port_b 0x01 ^ short_wait               (* gate only: silent *)
  ^ port_b 0x02 ^ short_wait               (* data only: OUT held high *)
  ^ port_b 0x00 ^ short_wait ^ "\xcd\x20"

let rate = Dos_audio.opl2_rate
let budget = 2_000_000

let machine code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  m

(* Runs to the end, taking samples every [every] steps (0: only at the
   end). *)
let cpu_hz = Dos_ports.speed_hz Dos_ports.pc_speed

(* Samples finished by [cycles] when sound started at [start]. *)
let finished_by ~start cycles =
  let n = ref 0 in
  while start + ((!n + 1) * cpu_hz / rate) <= cycles do incr n done;
  !n

let play ?(every = 0) ?(audio = true) code =
  let m = machine code in
  if audio then Dos_machine.start_audio m ~rate;
  let buf = Buffer.create 65536 in
  let take () =
    Option.iter (Buffer.add_bytes buf) (Dos_machine.take_audio m);
    if audio then
      checki "every sample up to the CPU's cycle" (Buffer.length buf / 2)
        (finished_by ~start:0 (Cpu86.cycles (Dos_machine.cpu_of m)))
  in
  let n = ref 0 in
  while (not (Dos_machine.exited m)) && !n < budget do
    ignore (Dos_machine.step m);
    incr n;
    if every > 0 && !n mod every = 0 then take ()
  done;
  take ();
  (m, Buffer.to_bytes buf)

let samples b = Array.init (Bytes.length b / 2) (fun i -> Bytes.get_int16_le b (2 * i))

let crossings s ~from ~len =
  let mid = Dos_audio.speaker_high / 2 in
  let c = ref 0 in
  for i = from + 1 to min (Array.length s - 1) (from + len - 1) do
    if s.(i - 1) > mid <> (s.(i) > mid) then incr c
  done;
  !c

let first_high s =
  let mid = Dos_audio.speaker_high / 2 in
  let rec go i = if i >= Array.length s || s.(i) > mid then i else go (i + 1) in
  go 0

let test_tone () =
  let m, b = play (program ~poll:None) in
  check "the program ends" (Dos_machine.exited m);
  let s = samples b in
  let start = first_high s in
  check "the speaker sounds" (start < Array.length s);
  (* the first wait is 65535 loops: 0.069 s at this machine's 5 cycles a
     LOOP, 0.233 s at the 8086's 17 (#181). 0.05 s is inside either. *)
  let window = rate / 20 in
  let c = crossings s ~from:start ~len:window in
  check (Printf.sprintf "1kHz for 0.05s: %d edges" c) (c >= 99 && c <= 101);
  checki "the end is silent" s.(Array.length s - 1) 0;
  checki "nothing clipped" (Option.value ~default:(-1) (Dos_machine.audio_clipped m)) 0;
  let cpu_hz = Dos_ports.speed_hz Dos_ports.pc_speed in
  let cycles = Cpu86.cycles (Dos_machine.cpu_of m) in
  let finished = ref 0 in
  while (!finished + 1) * cpu_hz / rate <= cycles do incr finished done;
  checki "one sample per 1/49716 s up to the last cycle" (Array.length s) !finished

(* Taking the samples after every step renders before every instruction;
   taking them once leaves it to the port writes and reads. They agree
   only if every read of channel 2 renders first: the new count loads at
   the end of a half period, and a read after that would drop the old
   one. *)
let test_split () =
  List.iter
    (fun (name, code) ->
      let _, once = play code in
      let _, eager = play ~every:1 code in
      let _, some = play ~every:777 code in
      check (name ^ ": taking once = taking every step") (Bytes.equal once eager);
      check (name ^ ": taking every 777 steps") (Bytes.equal once some))
    [ ("no reads", program ~poll:None); ("polling 61h", program ~poll:(Some 0x61));
      ("polling 42h", program ~poll:(Some 0x42)); ("mode 0 PWM", pwm);
      ("gate and data", gate_and_data) ]

(* A PWM player's level moves between the two; gate without data is
   silence and data without gate holds the cone out. *)
let test_bits () =
  let _, b = play pwm in
  let s = samples b in
  let c = crossings s ~from:0 ~len:(Array.length s) in
  check (Printf.sprintf "PWM moves (%d crossings)" c) (c > 100);
  check "PWM: never above the speaker" (Array.for_all (fun v -> v >= 0 && v <= Dos_audio.speaker_high) s);
  let _, b = play gate_and_data in
  let s = samples b in
  checki "gate then data: one rise, one fall" (crossings s ~from:0 ~len:(Array.length s)) 2;
  check "data alone holds the cone out" (Array.exists (fun v -> v = Dos_audio.speaker_high) s)

(* Sound is an observer: the machine takes the same path with it off. *)
let test_observer () =
  let code = program ~poll:(Some 0x61) in
  let on, _ = play code and off, none = play ~audio:false code in
  checki "same cycles" (Cpu86.cycles (Dos_machine.cpu_of on)) (Cpu86.cycles (Dos_machine.cpu_of off));
  check "same memory" (Dos_machine.frame_digest on = Dos_machine.frame_digest off);
  checki "no samples when off" (Bytes.length none) 0;
  check "take_audio is None when off" (Dos_machine.take_audio off = None)

(* Turned on later, the samples start then. *)
let test_late_start () =
  let m = machine (program ~poll:None) in
  for _ = 1 to 5000 do ignore (Dos_machine.step m) done;
  let start = Cpu86.cycles (Dos_machine.cpu_of m) in
  Dos_machine.start_audio m ~rate;
  while not (Dos_machine.exited m) do ignore (Dos_machine.step m) done;
  let b = Option.value ~default:Bytes.empty (Dos_machine.take_audio m) in
  let cpu_hz = Dos_ports.speed_hz Dos_ports.pc_speed in
  let stop = Cpu86.cycles (Dos_machine.cpu_of m) in
  let finished = ref 0 in
  while start + ((!finished + 1) * cpu_hz / rate) <= stop do incr finished done;
  checki "samples from the late start" (Bytes.length b / 2) !finished

let () =
  test_tone ();
  test_split ();
  test_bits ();
  test_observer ();
  test_late_start ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos audio machine: all passed"
