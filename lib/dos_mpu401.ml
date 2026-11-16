(* Roland MPU-401, UART-only kind (Sound Blaster 16/AWE32). See the .mli
   for the behaviour and the sources. *)

let data_port = 0x330
let status_port = 0x331

let ack = 0xFE
let reset_command = 0xFF
let uart_command = 0x3F

(* 331h: bits 0-5 read 1; bit 7 set = nothing to read in 330h. Bit 6 (the
   interface cannot take a byte) is never set here. *)
let status_low_bits = 0x3F
let nothing_to_read = 0x80

type mode =
  | Command                  (* takes 3Fh and FFh, answers each with FEh *)
  | Uart                     (* takes only FFh, and does not answer it *)

(* Every byte the card answers is FEh, so a count is the whole queue. A
   reset empties it before answering, and a UART-mode reset empties it
   without answering: command mode holds at most one FEh, and 3Fh adds one
   on the way into UART mode. *)
let max_waiting = function
  | Command -> 1
  | Uart -> 2

type t = { mutable mode : mode; mutable waiting : int }

let create () = { mode = Command; waiting = 0 }

let status t = status_low_bits lor (if t.waiting = 0 then nothing_to_read else 0)

let read_data t =
  if t.waiting > 0 then t.waiting <- t.waiting - 1;
  ack

let port_in t p =
  if p = data_port then Some (read_data t)
  else if p = status_port then Some (status t)
  else None

type write =
  | Taken
  | Midi_out of int

let command t v =
  match t.mode with
  | Command ->
    if v = reset_command then t.waiting <- 1
    else if v = uart_command then begin
      t.mode <- Uart;
      t.waiting <- t.waiting + 1
    end
  | Uart ->
    if v = reset_command then begin
      t.mode <- Command;
      t.waiting <- 0
    end

let port_out t p v =
  if p = data_port then Some (Midi_out v)
  else if p = status_port then begin
    command t v;
    Some Taken
  end
  else None

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

(* The mode comes first: it bounds the count. *)
let waiting_r mode = C.range 0 (max_waiting mode)

let write_state w { mode; waiting } =
  C.put_bool w (match mode with Uart -> true | Command -> false);
  C.put w ~what:"MPU-401 waiting bytes" (waiting_r mode) waiting

let read_state r t =
  let mode = if C.get_bool r then Uart else Command in
  let waiting = C.get r ~what:"MPU-401 waiting bytes" (waiting_r mode) in
  t.mode <- mode;
  t.waiting <- waiting
