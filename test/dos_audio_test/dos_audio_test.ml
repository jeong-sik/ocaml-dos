(* RFC 0006 stage 1: samples as box averages, and the PC speaker as a
   source. The PIT side checks Dos_pit.next_change against a tick-by-tick
   scan of Dos_pit.out; the speaker side checks rendered samples against
   samples summed cycle by cycle. *)

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

let samples b = Array.init (Bytes.length b / 2) (fun i -> Bytes.get_int16_le b (2 * i))

(* A source from a list of (first cycle, level), sorted, the first at 0. *)
let steps l c =
  let rec go = function
    | (_, v) :: ((s, _) :: _ as rest) -> if c < s then (v, s) else go rest
    | [ (_, v) ] -> (v, max_int)
    | [] -> (0, max_int)
  in
  go l

(* ---------- box averages ---------- *)

let test_box () =
  let a = Dos_audio.create ~cpu_hz:10 ~rate:1 ~start:0 in
  Dos_audio.render a ~until:30 (steps [ (0, 100); (5, 0); (20, -7) ]);
  let s = samples (Dos_audio.take a) in
  checki "three samples" (Array.length s) 3;
  checki "half a sample at 100" s.(0) 50;
  checki "a sample at 0" s.(1) 0;
  checki "a sample at -7" s.(2) (-7);
  checki "take empties" (Bytes.length (Dos_audio.take a)) 0;
  checki "position" (Dos_audio.position a) 30;
  (* rounding: half away from zero *)
  let a = Dos_audio.create ~cpu_hz:10 ~rate:1 ~start:0 in
  Dos_audio.render a ~until:20 (steps [ (0, 1); (5, 0); (10, -1); (15, 0) ]);
  let s = samples (Dos_audio.take a) in
  checki "+0.5 rounds to 1" s.(0) 1;
  checki "-0.5 rounds to -1" s.(1) (-1);
  (* an unfinished sample stays inside *)
  let a = Dos_audio.create ~cpu_hz:10 ~rate:1 ~start:0 in
  Dos_audio.render a ~until:15 (steps [ (0, 10) ]);
  checki "one finished" (Bytes.length (Dos_audio.take a)) 2;
  Dos_audio.render a ~until:12 (steps [ (0, 999) ]);
  checki "a render that goes back does nothing" (Dos_audio.position a) 15;
  Dos_audio.render a ~until:20 (steps [ (0, 30) ]);
  checki "the rest of the sample at another level" (samples (Dos_audio.take a)).(0) 20;
  (* uneven boundaries: 7 cycles at rate 3 are samples of 2, 2, 3 *)
  let a = Dos_audio.create ~cpu_hz:7 ~rate:3 ~start:0 in
  Dos_audio.render a ~until:7 (steps [ (0, 6); (4, 0) ]);
  let s = samples (Dos_audio.take a) in
  check "boundaries i*7/3" (s = [| 6; 6; 0 |]);
  (* clipping *)
  let a = Dos_audio.create ~cpu_hz:2 ~rate:1 ~start:0 in
  Dos_audio.render a ~until:6 (steps [ (0, 40000); (2, -40000); (4, 32767) ]);
  let s = samples (Dos_audio.take a) in
  check "clipped to the 16-bit range" (s = [| 32767; -32768; 32767 |]);
  checki "two clipped" (Dos_audio.clipped a) 2;
  (* a source that does not move on *)
  let a = Dos_audio.create ~cpu_hz:2 ~rate:1 ~start:0 in
  check "a stuck source is an error"
    (match Dos_audio.render a ~until:4 (fun c -> (0, c)) with
     | () -> false
     | exception Invalid_argument _ -> true);
  check "a rate above the CPU clock is an error"
    (match Dos_audio.create ~cpu_hz:2 ~rate:3 ~start:0 with
     | _ -> false
     | exception Invalid_argument _ -> true)

(* Sound turned on at cycle 1003: samples count from there, and a render
   up to an earlier cycle does nothing. *)
let test_start () =
  let a = Dos_audio.create ~cpu_hz:10 ~rate:1 ~start:1003 in
  checki "position starts at the start" (Dos_audio.position a) 1003;
  Dos_audio.render a ~until:500 (steps [ (0, 1) ]);
  checki "nothing before the start" (Bytes.length (Dos_audio.take a)) 0;
  Dos_audio.render a ~until:1023 (steps [ (0, 0); (1008, 100) ]);
  let s = samples (Dos_audio.take a) in
  check "samples from cycle 1003" (s = [| 50; 100 |])

(* RIFF WAVE header: 49716 Hz mono 16-bit, 1000 bytes of samples. *)
let test_wav_header () =
  let le32 n = String.init 4 (fun i -> Char.chr ((n lsr (8 * i)) land 0xff)) in
  let le16 n = String.init 2 (fun i -> Char.chr ((n lsr (8 * i)) land 0xff)) in
  let want =
    "RIFF" ^ le32 1036 ^ "WAVE" ^ "fmt " ^ le32 16 ^ le16 1 ^ le16 1 ^ le32 49716
    ^ le32 99432 ^ le16 2 ^ le16 16 ^ "data" ^ le32 1000
  in
  check "WAV header" (Bytes.to_string (Dos_audio.wav_header ~rate:49716 ~data_bytes:1000) = want)

(* Splitting a render anywhere gives the same samples. *)
let test_split () =
  let rng = Random.State.make [| 6 |] in
  for _ = 1 to 200 do
    let points = List.init 30 (fun _ -> Random.State.int rng 5000) |> List.sort_uniq compare in
    let l = (0, 0) :: List.map (fun p -> (p + 1, Random.State.int rng 2000 - 1000)) points in
    let whole = Dos_audio.create ~cpu_hz:1000 ~rate:37 ~start:0 in
    Dos_audio.render whole ~until:6000 (steps l);
    let parts = Dos_audio.create ~cpu_hz:1000 ~rate:37 ~start:0 in
    let cuts = List.init 10 (fun _ -> Random.State.int rng 6000) |> List.sort compare in
    List.iter (fun u -> Dos_audio.render parts ~until:u (steps l)) (cuts @ [ 6000 ]);
    check "split renders" (Bytes.equal (Dos_audio.take whole) (Dos_audio.take parts))
  done

(* ---------- Dos_pit.next_change ---------- *)

(* From [from] to [until], every tick's OUT and next change agree with the
   OUT of the ticks after it. Calls stay in tick order. *)
let walk name pit ~from ~until =
  let t = ref from in
  while !t < until do
    let v = Dos_pit.out pit ~tick:!t in
    let next = Dos_pit.next_change pit ~tick:!t in
    let stop = match next with Some c -> c | None -> max_int in
    if stop <= !t then (check (Printf.sprintf "%s: next %d at %d" name stop !t) false; t := until)
    else begin
      let last = min stop until in
      let u = ref (!t + 1) in
      while !u < last do
        if Dos_pit.out pit ~tick:!u <> v then begin
          check (Printf.sprintf "%s: OUT changed at %d before %d" name !u stop) false;
          u := last
        end
        else if Dos_pit.next_change pit ~tick:!u <> next then begin
          check (Printf.sprintf "%s: next change from %d" name !u) false;
          u := last
        end
        else incr u
      done;
      if stop < until then
        check (Printf.sprintf "%s: OUT changes at %d" name stop) (Dos_pit.out pit ~tick:stop <> v);
      t := last
    end
  done

type event = Control of int | Write of int | Gate of bool

let random_event rng gate =
  match Random.State.int rng 10 with
  | 0 | 1 ->
    (* channel bits are not the PIT's business here: bit 7-6 = 0 *)
    let access = 1 + Random.State.int rng 3 and mode = Random.State.int rng 8 in
    let bcd = if Random.State.int rng 6 = 0 then 1 else 0 in
    Control ((access lsl 4) lor (mode lsl 1) lor bcd)
  | 2 -> Control 0x00  (* latch *)
  | 3 | 4 -> Gate (not gate)
  | _ ->
    (* mostly short counts, so OUT moves a lot *)
    Write (if Random.State.int rng 4 = 0 then Random.State.int rng 256 else Random.State.int rng 12)

let test_next_change () =
  for seed = 1 to 40 do
    let rng = Random.State.make [| seed |] in
    let pit = Dos_pit.create () in
    let gate = ref true in
    let tick = ref 0 in
    for _ = 1 to 400 do
      let at = !tick + 1 + Random.State.int rng 40 in
      walk (Printf.sprintf "seed %d" seed) pit ~from:!tick ~until:at;
      (match random_event rng !gate with
       | Control v -> Dos_pit.control pit ~tick:at v
       | Write v -> Dos_pit.write pit ~tick:at ~gate:!gate v
       | Gate high -> gate := high; Dos_pit.gate_edge pit ~tick:at ~high);
      tick := at
    done
  done

(* ---------- the PC speaker ---------- *)

let cpu_hz = Dos_ports.speed_hz Dos_ports.pc_speed
let speed = cpu_hz / Dos_ports.pit_hz  (* CPU cycles per PIT tick *)
let rate = Dos_audio.opl2_rate

let square_control = 0x36  (* LSB then MSB, mode 3, binary *)

let program pit ~tick ~gate n =
  Dos_pit.control pit ~tick square_control;
  Dos_pit.write pit ~tick ~gate (n land 0xff);
  Dos_pit.write pit ~tick ~gate (n lsr 8)

type speaker_event = Count of int | Gate_to of bool | Data of bool

(* The same events, rendered through the source and summed cycle by cycle
   on a second PIT. *)
let speaker_case name ~ticks events =
  let pit = Dos_pit.create () and ref_pit = Dos_pit.create () in
  program pit ~tick:0 ~gate:true 1193;
  program ref_pit ~tick:0 ~gate:true 1193;
  let gate = ref true and data = ref true in
  let rgate = ref true and rdata = ref true in
  let a = Dos_audio.create ~cpu_hz ~rate ~start:0 in
  let apply pit gate data tick = function
    | Count n ->
      Dos_pit.write pit ~tick ~gate:!gate (n land 0xff);
      Dos_pit.write pit ~tick ~gate:!gate (n lsr 8)
    | Gate_to h -> gate := h; Dos_pit.gate_edge pit ~tick ~high:h
    | Data d -> data := d
  in
  List.iter
    (fun (tick, e) ->
      Dos_audio.render a ~until:(tick * speed) (Dos_audio.speaker pit ~data:!data ~speed);
      apply pit gate data tick e)
    events;
  Dos_audio.render a ~until:(ticks * speed) (Dos_audio.speaker pit ~data:!data ~speed);
  let got = samples (Dos_audio.take a) in
  (* reference *)
  let boundary i = i * cpu_hz / rate in
  let n = Array.length got in
  let sums = Array.make (n + 1) 0 in
  let i = ref 0 in
  let pending = ref events in
  for k = 0 to ticks - 1 do
    let rec due () =
      match !pending with
      | (t, e) :: rest when t = k -> apply ref_pit rgate rdata k e; pending := rest; due ()
      | _ -> ()
    in
    due ();
    let level = if !rdata && Dos_pit.out ref_pit ~tick:k then Dos_audio.speaker_high else 0 in
    for c = k * speed to ((k + 1) * speed) - 1 do
      while c >= boundary (!i + 1) do incr i done;
      if !i <= n then sums.(!i) <- sums.(!i) + level
    done
  done;
  let want =
    Array.init n (fun j ->
        let len = boundary (j + 1) - boundary j in
        (sums.(j) + (len / 2)) / len)
  in
  let finished = ref 0 in
  while boundary (!finished + 1) <= ticks * speed do incr finished done;
  checki (name ^ ": sample count") n !finished;
  check (name ^ ": samples match the cycle-by-cycle sum") (got = want);
  got

let crossings s =
  let mid = Dos_audio.speaker_high / 2 in
  let c = ref 0 in
  for i = 1 to Array.length s - 1 do
    if s.(i - 1) > mid <> (s.(i) > mid) then incr c
  done;
  !c

let test_speaker () =
  (* 1193182 / 1193 = 1000.15 Hz: about 1000 edges in half a second *)
  let half_second = Dos_ports.pit_hz / 2 in
  let s = speaker_case "1kHz" ~ticks:half_second [] in
  let c = crossings s in
  check (Printf.sprintf "1kHz: %d edges in 0.5s" c) (c >= 999 && c <= 1002);
  (* a new count waits for the half period; gate low holds OUT high; data
     off is silence; data on again follows OUT *)
  let s =
    speaker_case "events" ~ticks:40_000
      [ (3_000, Count 300); (9_001, Gate_to false); (12_000, Data false); (15_000, Gate_to true);
        (20_000, Data true); (30_000, Count 2) ]
  in
  let at tick = s.(tick * speed * rate / cpu_hz) in
  checki "gate low: OUT high" (at 10_500) Dos_audio.speaker_high;
  checki "data off: silence" (at 18_000) 0;
  (* a sample is 95 or 96 cycles, 24 ticks give or take one: the mean over
     many samples is half within 1% *)
  let first = 35_000 * speed * rate / cpu_hz and count = 150 in
  let total = ref 0 in
  for i = first to first + count - 1 do total := !total + s.(i) done;
  let mean = !total / count and half = Dos_audio.speaker_high / 2 in
  check (Printf.sprintf "count 2 at 596kHz: mean %d near half" mean) (abs (mean - half) * 100 <= half);
  let s = speaker_case "data off from the start" ~ticks:5_000 [ (0, Data false) ] in
  check "data off: all zero" (Array.for_all (( = ) 0) s)

let () =
  test_box ();
  test_split ();
  test_start ();
  test_wav_header ();
  test_next_change ();
  test_speaker ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos audio: all passed"
