(* A Sound Blaster 2.0 DSP, as a DOS game probes and drives it. See
   dos_sb.mli. *)

let reset_port = 0x226
let read_data_port = 0x22A
let write_port = 0x22C
let read_status_port = 0x22E

let cycles_of_us us = us * Dos_clock.cpu_hz / 1_000_000

(* DOSBox dsp_do_reset: AAh 20µs after the reset bit drops. *)
let reset_delay_us = 20
let ready_byte = 0xAA

(* DOSBox command 0F2h: the IRQ follows 10µs later. *)
let trigger_delay_us = 10

(* DSP 2.01-3.xx status bytes (86Box sb_read): bit 7 is the flag, the
   other bits read 0101010b. A DSP still in reset reads FFh on 22Ch. *)
let status_clear = 0x2A
let status_set = 0xAA
let status_in_reset = 0xFF

(* 226h is write-only (DOSBox read_sb). *)
let reset_port_read = 0xFF

(* After a reset: DOSBox plays at 22050 Hz, the nearest time constant is
   D3h (45µs a byte); 86Box sets the auto-init length to 7FFFh + 1. *)
let default_time_constant = 0xD3
let default_block = 0x8000

(* E1h: DSP 2.01. *)
let version_major = 0x02
let version_minor = 0x01

(* 20h: DOSBox answers 7Fh, "silent input". *)
let adc_silence = 0x7F
let speaker_on_answer = 0xFF
let speaker_off_answer = 0x00
let undocumented_answer = 0x00

let length_of lo hi = 1 + lo + (hi lsl 8)

(* What one DMA byte holds: one 8-bit sample, or the ADPCM codes of 2, 3
   or 4 samples. DOSBox takes ADPCM bytes at the sample rate over that
   many (dma.mul); the reference byte of 75h, 77h, 17h and the auto-init
   forms is the block's first byte and takes its turn like the rest. *)
type format = Pcm8 | Adpcm4 | Adpcm26 | Adpcm2

let formats = [| Pcm8; Adpcm4; Adpcm26; Adpcm2 |]
let format_index = function Pcm8 -> 0 | Adpcm4 -> 1 | Adpcm26 -> 2 | Adpcm2 -> 3
let samples_per_byte = function Pcm8 -> 1 | Adpcm4 -> 2 | Adpcm26 -> 3 | Adpcm2 -> 4

(* What a command asks of the DSP once its parameter bytes are in. *)
type request =
  | Direct_dac            (* 10h: the sample goes nowhere *)
  | Dma_once of format * int  (* 14h, 74h-77h, 16h, 17h: a block of this many bytes *)
  | High_speed_once       (* 91h: one block of the 48h length *)
  | Dma_auto of format    (* 1Ch, 90h, 7Dh, 7Fh, 1Fh: blocks of the 48h length *)
  | Midi_uart             (* 34h, 35h: every later byte is MIDI out, until a reset *)
  | Direct_adc            (* 20h *)
  | Time_constant of int  (* 40h *)
  | Block_size of int     (* 48h *)
  | Silence of int        (* 80h: an IRQ after this many byte times *)
  | Halt                  (* D0h *)
  | Speaker_on            (* D1h *)
  | Speaker_off           (* D3h *)
  | Continue              (* D4h *)
  | Speaker_status        (* D8h *)
  | Exit_auto             (* DAh *)
  | Invert of int         (* E0h *)
  | Version               (* E1h *)
  | Test_write of int     (* E4h *)
  | Test_read             (* E8h *)
  | Trigger_irq           (* F2h *)
  | Undocumented_zero     (* F8h *)
  | Unmodelled            (* swallowed with its parameter bytes *)

(* A command byte and the parameter bytes it still wants, by DOSBox's
   dsp_cmd_len_sb. *)
type decoder =
  | Ready of request
  | Needs_one of (int -> request)
  | Needs_two of (int -> int -> request)

let decode b =
  match b with
  | 0x10 -> Needs_one (fun _ -> Direct_dac)
  (* 15h: DOSBox plays it as 14h ("Wari hack"). *)
  | 0x14 | 0x15 -> Needs_two (fun lo hi -> Dma_once (Pcm8, length_of lo hi))
  | 0x74 | 0x75 -> Needs_two (fun lo hi -> Dma_once (Adpcm4, length_of lo hi))
  | 0x76 | 0x77 -> Needs_two (fun lo hi -> Dma_once (Adpcm26, length_of lo hi))
  | 0x16 | 0x17 -> Needs_two (fun lo hi -> Dma_once (Adpcm2, length_of lo hi))
  | 0x91 -> Ready High_speed_once
  | 0x1C | 0x90 -> Ready (Dma_auto Pcm8)
  | 0x7D -> Ready (Dma_auto Adpcm4)
  | 0x7F -> Ready (Dma_auto Adpcm26)
  | 0x1F -> Ready (Dma_auto Adpcm2)
  (* 86Box: 34h (poll) and 35h (IRQ) enter UART mode; 36h and 37h, the
     time-stamp forms, do nothing there. *)
  | 0x34 | 0x35 -> Ready Midi_uart
  | 0x20 -> Ready Direct_adc
  | 0x40 -> Needs_one (fun tc -> Time_constant tc)
  | 0x48 -> Needs_two (fun lo hi -> Block_size (length_of lo hi))
  | 0x80 -> Needs_two (fun lo hi -> Silence (length_of lo hi))
  | 0xD0 -> Ready Halt
  | 0xD1 -> Ready Speaker_on
  | 0xD3 -> Ready Speaker_off
  | 0xD4 -> Ready Continue
  | 0xD8 -> Ready Speaker_status
  | 0xDA -> Ready Exit_auto
  | 0xE0 -> Needs_one (fun v -> Invert v)
  | 0xE1 -> Ready Version
  | 0xE4 -> Needs_one (fun v -> Test_write v)
  | 0xE8 -> Ready Test_read
  | 0xF2 -> Ready Trigger_irq
  | 0xF8 -> Ready Undocumented_zero
  | 0x24 | 0x41 | 0x42 -> Needs_two (fun _ _ -> Unmodelled)
  | 0x38 | 0xE2 -> Needs_one (fun _ -> Unmodelled)
  | _ -> Ready Unmodelled

(* A command still taking parameter bytes. [code] and [first] are what a
   snapshot keeps; [k] is rebuilt from them. *)
type pending =
  | No_command
  | One_left of { code : int; first : int option; k : int -> request }
  | Two_left of { code : int; k : int -> int -> request }

type reset = Running | Held | Releasing of int  (* AAh at this cycle *)

type block = {
  left : int;                     (* bytes still to take in this block *)
  format : format;
  auto : bool;                    (* another block follows this one *)
  queued : (format * int) option; (* a single block waiting behind this one *)
}

type playback = Idle | Playing of block * int (* next byte at this cycle *) | Paused of block

type t = {
  mutable reset : reset;
  mutable answers : int list;          (* oldest first *)
  mutable last_answer : int;
  mutable pending : pending;
  mutable playback : playback;
  mutable timers : int list;           (* IRQ cycles, earliest first *)
  mutable irq_high : bool;             (* raised, not yet acknowledged through 22Eh *)
  mutable time_constant : int;
  mutable block_length : int;
  mutable speaker : bool;
  mutable test_register : int;
  mutable uart : bool;                 (* MIDI UART: 22Ch bytes are MIDI out *)
}

let create () =
  { reset = Running; answers = []; last_answer = ready_byte; pending = No_command;
    playback = Idle; timers = []; irq_high = false; time_constant = default_time_constant;
    block_length = default_block; speaker = false; test_register = 0;
    uart = false }

let sample_us t = 256 - t.time_constant
let byte_cycles t format = cycles_of_us (sample_us t * samples_per_byte format)

(* DOSBox DspBufSize: a full buffer drops the answer. *)
let answer_capacity = 64

let answer t b =
  if List.length t.answers < answer_capacity then t.answers <- t.answers @ [ b land 0xFF ]
let answer_alone t b = t.answers <- [ b land 0xFF ]

let add_timer t at = t.timers <- List.merge compare [ at ] t.timers

(* DOSBox dsp_reset: the DSP stops and forgets; the test register stays. *)
let reset_dsp t =
  t.answers <- [];
  t.pending <- No_command;
  t.playback <- Idle;
  t.timers <- [];
  t.irq_high <- false;
  t.time_constant <- default_time_constant;
  t.block_length <- default_block;
  t.speaker <- false;
  t.uart <- false

(* A new transfer drops an unacknowledged IRQ (DOSBox dsp_do_dma_transfer). *)
let start_single t ~now format length =
  t.irq_high <- false;
  let queued = Some (format, length) in
  match t.playback with
  | Playing (({ auto = true; _ } as b), next) ->
    t.playback <- Playing ({ b with auto = false; queued }, next)
  | Paused ({ auto = true; _ } as b) -> t.playback <- Paused { b with auto = false; queued }
  | Idle | Playing ({ auto = false; _ }, _) | Paused { auto = false; _ } ->
    t.playback <-
      Playing ({ left = length; format; auto = false; queued = None }, now + byte_cycles t format)

let start_auto t ~now format =
  t.irq_high <- false;
  t.playback <-
    Playing
      ( { left = t.block_length; format; auto = true; queued = None },
        now + byte_cycles t format )

let run t ~now req =
  match req with
  | Direct_dac | Unmodelled -> ()
  | Dma_once (format, length) -> start_single t ~now format length
  | High_speed_once -> start_single t ~now Pcm8 t.block_length
  | Dma_auto format -> start_auto t ~now format
  | Midi_uart -> t.uart <- true
  | Direct_adc -> answer t adc_silence
  | Time_constant tc -> t.time_constant <- tc
  | Block_size length -> t.block_length <- length
  | Silence length -> add_timer t (now + (length * byte_cycles t Pcm8))
  | Halt ->
    (match t.playback with
     | Playing (b, _) -> t.playback <- Paused b
     | Idle | Paused _ -> ())
  | Continue ->
    (match t.playback with
     | Paused b -> t.playback <- Playing (b, now + byte_cycles t b.format)
     | Idle | Playing _ -> ())
  | Speaker_on -> t.speaker <- true
  | Speaker_off -> t.speaker <- false
  | Speaker_status -> answer_alone t (if t.speaker then speaker_on_answer else speaker_off_answer)
  | Exit_auto ->
    (match t.playback with
     | Playing (b, next) -> t.playback <- Playing ({ b with auto = false }, next)
     | Paused b -> t.playback <- Paused { b with auto = false }
     | Idle -> ())
  | Invert v -> answer_alone t (lnot v)
  | Version ->
    answer_alone t version_major;
    answer t version_minor
  | Test_write v -> t.test_register <- v
  | Test_read -> answer_alone t t.test_register
  | Trigger_irq -> add_timer t (now + cycles_of_us trigger_delay_us)
  | Undocumented_zero -> answer_alone t undocumented_answer

(* In UART mode the byte goes to MIDI out, which has no device here. *)
let write t ~now b =
  if t.uart then ()
  else
  match t.pending with
  | No_command ->
    (match decode b with
     | Ready req -> run t ~now req
     | Needs_one k -> t.pending <- One_left { code = b; first = None; k }
     | Needs_two k -> t.pending <- Two_left { code = b; k })
  | One_left { k; _ } ->
    t.pending <- No_command;
    run t ~now (k b)
  | Two_left { code; k } -> t.pending <- One_left { code; first = Some b; k = k b }

let reset_write t ~now v =
  let held = v land 1 <> 0 in
  match t.reset, held with
  | (Running | Releasing _), true -> reset_dsp t; t.reset <- Held
  | Held, false -> t.reset <- Releasing (now + cycles_of_us reset_delay_us)
  | Held, true | (Running | Releasing _), false -> ()

let port_out t ~now p v =
  let v = v land 0xFF in
  if p = reset_port then begin reset_write t ~now v; true end
  else if p = write_port then begin write t ~now v; true end
  else false

let read_data t =
  match t.answers with
  | b :: rest ->
    t.answers <- rest;
    t.last_answer <- b;
    b
  | [] -> t.last_answer

let write_status t =
  match t.reset with
  | Running -> status_clear
  | Held | Releasing _ -> status_in_reset

let read_status t =
  t.irq_high <- false;
  if t.answers = [] then status_clear else status_set

let port_in t p =
  if p = reset_port then Some reset_port_read
  else if p = read_data_port then Some (read_data t)
  else if p = write_port then Some (write_status t)
  else if p = read_status_port then Some (read_status t)
  else None

let irq_line t = t.irq_high

let due_at t =
  let reset = match t.reset with Releasing at -> at | Running | Held -> max_int in
  let timer = match t.timers with at :: _ -> at | [] -> max_int in
  let byte = match t.playback with Playing (_, next) -> next | Idle | Paused _ -> max_int in
  min reset (min timer byte)

(* What plays after a block: the single block queued behind it, another
   auto-init block, or nothing. *)
let next_block t b =
  match b.queued, b.auto with
  | Some (format, length), _ -> Some { left = length; format; auto = false; queued = None }
  | None, true -> Some { b with left = t.block_length }
  | None, false -> None

let advance t ~now ~dma ~mem =
  let fired = ref false in
  let raise_irq () =
    if not t.irq_high then begin
      t.irq_high <- true;
      fired := true
    end
  in
  (match t.reset with
   | Releasing at when at <= now ->
     t.reset <- Running;
     answer_alone t ready_byte
   | Releasing _ | Running | Held -> ());
  let due, later = List.partition (fun at -> at <= now) t.timers in
  t.timers <- later;
  if due <> [] then raise_irq ();
  let rec take () =
    match t.playback with
    | Playing (b, next) when next <= now ->
      (match Dos_dma.pull dma ~mem Dos_dma.Channel1 with
       | Dos_dma.Masked ->
         (* The DSP holds its request until the channel opens; the next
            byte goes the moment it does, not in a burst for the wait. *)
         t.playback <- Playing (b, now)
       | Dos_dma.Byte _ | Dos_dma.No_data _ ->
         (* The next byte's time is its own block's byte time. *)
         let left = b.left - 1 in
         if left > 0 then t.playback <- Playing ({ b with left }, next + byte_cycles t b.format)
         else begin
           raise_irq ();
           t.playback <-
             (match next_block t b with
              | Some nb -> Playing (nb, next + byte_cycles t nb.format)
              | None -> Idle)
         end;
         take ())
    | Playing _ | Idle | Paused _ -> ()
  in
  take ();
  !fired

module C = Dos_snap_codec

let block_r = C.range 1 0x10000
let cycle = C.count

let format_r = C.range 0 (Array.length formats - 1)
let put_format w f = C.put w ~what:"SB format" format_r (format_index f)
let get_format r = formats.(C.get r ~what:"SB format" format_r)

let put_block w { left; format; auto; queued } =
  C.put w ~what:"SB block left" block_r left;
  put_format w format;
  C.put_bool w auto;
  match queued with
  | None -> C.put_bool w false
  | Some (f, n) -> C.put_bool w true; put_format w f; C.put w ~what:"SB queued block" block_r n

let get_block r =
  let left = C.get r ~what:"SB block left" block_r in
  let format = get_format r in
  let auto = C.get_bool r in
  let queued =
    if C.get_bool r then begin
      let f = get_format r in
      Some (f, C.get r ~what:"SB queued block" block_r)
    end
    else None
  in
  { left; format; auto; queued }

let put_list w ~what r l =
  C.put w ~what:(what ^ " count") C.count (List.length l);
  List.iter (C.put w ~what r) l

let get_list r ~what rng =
  let n = C.get r ~what:(what ^ " count") C.count in
  List.init n (fun _ -> C.get r ~what rng)

let reset_tag = C.range 0 2
let pending_tag = C.range 0 2

let put_pending w p =
  let tag n = C.put w ~what:"SB pending" pending_tag n in
  let code c = C.put w ~what:"SB command" C.byte c in
  match p with
  | No_command -> tag 0
  | One_left { code = c; first; k = _ } ->
    tag 1;
    code c;
    (match first with
     | None -> C.put_bool w false
     | Some f -> C.put_bool w true; C.put w ~what:"SB first parameter" C.byte f)
  | Two_left { code = c; k = _ } -> tag 2; code c

(* The decoder is rebuilt from the command byte; a byte that does not take
   the parameters the snapshot says it took is a bad snapshot. *)
let get_pending r =
  let bad () = C.fail "SB pending command does not match its parameters" in
  match C.get r ~what:"SB pending" pending_tag with
  | 0 -> No_command
  | 1 ->
    let code = C.get r ~what:"SB command" C.byte in
    let first = if C.get_bool r then Some (C.get r ~what:"SB first parameter" C.byte) else None in
    (match decode code, first with
     | Needs_one k, None -> One_left { code; first; k }
     | Needs_two k, Some f -> One_left { code; first; k = k f }
     | (Ready _ | Needs_two _), None | (Ready _ | Needs_one _), Some _ -> bad ())
  | _ ->
    let code = C.get r ~what:"SB command" C.byte in
    (match decode code with
     | Needs_two k -> Two_left { code; k }
     | Ready _ | Needs_one _ -> bad ())
let playback_tag = C.range 0 2

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. *)
let write_state w
    { reset; answers; last_answer; pending; playback; timers; irq_high; time_constant;
      block_length; speaker; test_register; uart } =
  (match reset with
   | Running -> C.put w ~what:"SB reset" reset_tag 0
   | Held -> C.put w ~what:"SB reset" reset_tag 1
   | Releasing at -> C.put w ~what:"SB reset" reset_tag 2; C.put w ~what:"SB reset at" cycle at);
  put_list w ~what:"SB answer" C.byte answers;
  C.put w ~what:"SB last answer" C.byte last_answer;
  put_pending w pending;
  (match playback with
   | Idle -> C.put w ~what:"SB playback" playback_tag 0
   | Playing (b, next) ->
     C.put w ~what:"SB playback" playback_tag 1;
     put_block w b;
     C.put w ~what:"SB next byte" cycle next
   | Paused b -> C.put w ~what:"SB playback" playback_tag 2; put_block w b);
  put_list w ~what:"SB timer" cycle timers;
  C.put_bool w irq_high;
  C.put w ~what:"SB time constant" C.byte time_constant;
  C.put w ~what:"SB block length" block_r block_length;
  C.put_bool w speaker;
  C.put w ~what:"SB test register" C.byte test_register;
  C.put_bool w uart

let read_state r t =
  t.reset <-
    (match C.get r ~what:"SB reset" reset_tag with
     | 0 -> Running
     | 1 -> Held
     | _ -> Releasing (C.get r ~what:"SB reset at" cycle));
  t.answers <- get_list r ~what:"SB answer" C.byte;
  t.last_answer <- C.get r ~what:"SB last answer" C.byte;
  t.pending <- get_pending r;
  t.playback <-
    (match C.get r ~what:"SB playback" playback_tag with
     | 0 -> Idle
     | 1 ->
       let b = get_block r in
       Playing (b, C.get r ~what:"SB next byte" cycle)
     | _ -> Paused (get_block r));
  t.timers <- get_list r ~what:"SB timer" cycle;
  t.irq_high <- C.get_bool r;
  t.time_constant <- C.get r ~what:"SB time constant" C.byte;
  t.block_length <- C.get r ~what:"SB block length" block_r;
  t.speaker <- C.get_bool r;
  t.test_register <- C.get r ~what:"SB test register" C.byte;
  t.uart <- C.get_bool r
