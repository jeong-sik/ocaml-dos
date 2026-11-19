(* Box averages of a piecewise constant level. See dos_audio.mli. *)

let opl2_rate = 49716

let sample_max = 32767
let sample_min = -32768

type t = {
  cpu_hz : int;
  rate : int;
  origin : int;         (* the first cycle of sample 0 *)
  mutable index : int;  (* the sample being filled *)
  mutable first : int;  (* its first cycle, [boundary index] *)
  mutable stop : int;   (* its end, [boundary (index + 1)] *)
  mutable sum : int;    (* level × cycles over [first, at) *)
  mutable at : int;     (* the first cycle not covered *)
  out : Buffer.t;
  mutable clipped : int;
}

(* i × cpu_hz stays below 2^62 for days of samples at any CPU speed this
   machine has. *)
let boundary t i = t.origin + (i * t.cpu_hz / t.rate)

let create ~cpu_hz ~rate ~start =
  (* rate <= cpu_hz keeps every sample at least one cycle long *)
  if rate <= 0 || rate > cpu_hz then invalid_arg "Dos_audio.create: need 0 < rate <= cpu_hz";
  let t =
    { cpu_hz; rate; origin = start; index = 0; first = start; stop = start; sum = 0; at = start;
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
  let v = div_round t.sum (t.stop - t.first) in
  let v =
    if v > sample_max then (t.clipped <- t.clipped + 1; sample_max)
    else if v < sample_min then (t.clipped <- t.clipped + 1; sample_min)
    else v
  in
  Buffer.add_int16_le t.out v;
  t.index <- t.index + 1;
  t.first <- t.stop;
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

(* The canonical RIFF WAVE layout: a "fmt " chunk of 16 bytes for PCM
   (format 1), then the "data" chunk. *)
let wav_header_bytes = 44
let riff_size_after_id = 36
let fmt_chunk_bytes = 16
let pcm_format = 1
let mono = 1
let bits_per_sample = 16
let bytes_per_sample = bits_per_sample / 8

let wav_header ~rate ~data_bytes =
  let b = Bytes.create wav_header_bytes in
  Bytes.blit_string "RIFF" 0 b 0 4;
  Bytes.set_int32_le b 4 (Int32.of_int (riff_size_after_id + data_bytes));
  Bytes.blit_string "WAVEfmt " 0 b 8 8;
  Bytes.set_int32_le b 16 (Int32.of_int fmt_chunk_bytes);
  Bytes.set_int16_le b 20 pcm_format;
  Bytes.set_int16_le b 22 mono;
  Bytes.set_int32_le b 24 (Int32.of_int rate);
  Bytes.set_int32_le b 28 (Int32.of_int (rate * mono * bytes_per_sample));
  Bytes.set_int16_le b 32 (mono * bytes_per_sample);
  Bytes.set_int16_le b 34 bits_per_sample;
  Bytes.blit_string "data" 0 b 36 4;
  Bytes.set_int32_le b 40 (Int32.of_int data_bytes);
  b
