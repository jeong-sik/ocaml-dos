(* Box averages of a piecewise constant level. See dos_audio.mli. *)

let opl2_rate = 49716

let sample_max = 32767
let sample_min = -32768

type t = {
  cpu_hz : int;
  rate : int;
  mutable index : int;  (* the sample being filled *)
  mutable start : int;  (* its first cycle, [boundary index] *)
  mutable stop : int;   (* its end, [boundary (index + 1)] *)
  mutable sum : int;    (* level × cycles over [start, at) *)
  mutable at : int;     (* the first cycle not covered *)
  out : Buffer.t;
  mutable clipped : int;
}

(* i × cpu_hz stays below 2^62 for days of samples at any CPU speed this
   machine has. *)
let boundary t i = i * t.cpu_hz / t.rate

let create ~cpu_hz ~rate =
  (* rate <= cpu_hz keeps every sample at least one cycle long *)
  if rate <= 0 || rate > cpu_hz then invalid_arg "Dos_audio.create: need 0 < rate <= cpu_hz";
  let t =
    { cpu_hz; rate; index = 0; start = 0; stop = 0; sum = 0; at = 0;
      out = Buffer.create 4096; clipped = 0 }
  in
  t.stop <- boundary t 1;
  t

let position t = t.at
let clipped t = t.clipped

let take t =
  let b = Buffer.to_bytes t.out in
  Buffer.clear t.out;
  b

(* Rounds half away from zero, so a level and its negation round alike. *)
let div_round a b = if a >= 0 then (a + (b / 2)) / b else -((-a + (b / 2)) / b)

let finish t =
  let v = div_round t.sum (t.stop - t.start) in
  let v =
    if v > sample_max then (t.clipped <- t.clipped + 1; sample_max)
    else if v < sample_min then (t.clipped <- t.clipped + 1; sample_min)
    else v
  in
  Buffer.add_int16_le t.out v;
  t.index <- t.index + 1;
  t.start <- t.stop;
  t.stop <- boundary t (t.index + 1);
  t.sum <- 0

let fill t level until =
  while t.at < until do
    let e = min t.stop until in
    t.sum <- t.sum + (level * (e - t.at));
    t.at <- e;
    if e = t.stop then finish t
  done

let render t ~until source =
  while t.at < until do
    let level, next = source t.at in
    if next <= t.at then invalid_arg "Dos_audio.render: the next change is not after the cycle";
    fill t level (min next until)
  done

let speaker_high = 8192

let speaker pit ~data ~speed c =
  let tick = c / speed in
  if not data then (0, max_int)
  else
    let level = if Dos_pit.out pit ~tick then speaker_high else 0 in
    match Dos_pit.next_change pit ~tick with
    | Some k -> (level, k * speed)
    | None -> (level, max_int)
