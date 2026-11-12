(* The 8042 keyboard controller, from the program's side. See dos_kbc.mli. *)

(* In CPU cycles. A keyboard sends a byte as an 11-bit frame at 10-16 kHz,
   about 1 ms; a person holds a tapped key about 100 ms. *)
let byte_gap = Dos_clock.cpu_hz / 1000
let key_hold = Dos_clock.cpu_hz / 10

let break_bit = 0x80
let left_shift_make = 0x2A
let left_shift_break = left_shift_make lor break_bit
let back_tab = 0x0F00

(* Bit 2 is the system flag a POST sets; bit 4 says the keyboard is not
   inhibited. Bit 0 is the output buffer. *)
let status_idle = 0x14
let output_full = 0x01

type byte_out = {
  code : int;
  word : int option;  (* the BIOS word a make code stands for *)
  due : int;          (* the cycle it is sent at, at the earliest *)
}

type t = {
  mutable queue : byte_out list;  (* by [due]; equal ones in press order *)
  mutable data : int;
  mutable word : int option;
  mutable full : bool;
  mutable last_read : int option; (* None: nothing was ever read *)
  mutable ready_at : int;
      (* Derived from the fields above: the first cycle the head can
         arrive at, [never] while nothing can. Not in the snapshot. *)
}

let never = max_int

let update_ready t =
  t.ready_at <-
    (match t.queue with
     | [] -> never
     | next :: _ ->
       if t.full then never
       else
         match t.last_read with
         | None -> next.due
         | Some r -> max next.due (r + byte_gap))

let create () =
  { queue = []; data = 0; word = None; full = false; last_read = None; ready_at = never }

let shifted word =
  let ascii = word land 0xff in
  (ascii >= Char.code 'A' && ascii <= Char.code 'Z') || word = back_tab

(* Insert after every byte due no later: two keys pressed together keep
   their order, and a key pressed while another is held sends its make
   before the held key's break, as a keyboard with rollover does. *)
let rec insert b = function
  | x :: rest when x.due <= b.due -> x :: insert b rest
  | rest -> b :: rest

let press t ~now word =
  let scan = (word lsr 8) land 0xff in
  let down = { code = scan; word = Some word; due = now }
  and up = { code = scan lor break_bit; word = None; due = now + key_hold } in
  let bytes =
    if shifted word then
      [ { code = left_shift_make; word = None; due = now }; down; up;
        { code = left_shift_break; word = None; due = now + key_hold } ]
    else [ down; up ]
  in
  t.queue <- List.fold_left (fun q b -> insert b q) t.queue bytes;
  update_ready t

let ready_at t = t.ready_at

let latch t ~now =
  now >= t.ready_at
  && (match t.queue with
      | [] -> false
      | next :: rest ->
        t.queue <- rest;
        t.data <- next.code;
        t.word <- next.word;
        t.full <- true;
        update_ready t;
        true)

let read_data t ~now =
  if t.full then begin
    t.full <- false;
    t.last_read <- Some now;
    update_ready t
  end;
  t.data

let status t = if t.full then status_idle lor output_full else status_idle
let data_word t = t.word

let holds_key t =
  (t.full && Option.is_some t.word)
  || List.exists (fun (b : byte_out) -> Option.is_some b.word) t.queue

module C = Dos_snap_codec

let put_word_option w = function
  | None -> C.put_bool w false
  | Some v -> C.put_bool w true; C.put w ~what:"key word" C.word v

let get_word_option r =
  if C.get_bool r then Some (C.get r ~what:"key word" C.word) else None

let write_state w { queue; data; word; full; last_read; ready_at = _ (* derived *) } =
  C.put w ~what:"keyboard queue length" C.count (List.length queue);
  List.iter
    (fun { code; word; due } ->
      C.put w ~what:"keyboard byte" C.byte code;
      put_word_option w word;
      C.put w ~what:"keyboard due" C.count due)
    queue;
  C.put w ~what:"keyboard data" C.byte data;
  put_word_option w word;
  C.put_bool w full;
  match last_read with
  | None -> C.put_bool w false
  | Some r -> C.put_bool w true; C.put w ~what:"keyboard last read" C.count r

let read_state r t =
  (* The reads are in the order the bytes were written; a loop keeps them
     there whatever order a list builder calls its function in. *)
  let rec bytes n acc =
    if n = 0 then List.rev acc
    else begin
      let code = C.get r ~what:"keyboard byte" C.byte in
      let word = get_word_option r in
      let due = C.get r ~what:"keyboard due" C.count in
      bytes (n - 1) ({ code; word; due } :: acc)
    end
  in
  t.queue <- bytes (C.get r ~what:"keyboard queue length" C.count) [];
  t.data <- C.get r ~what:"keyboard data" C.byte;
  t.word <- get_word_option r;
  t.full <- C.get_bool r;
  t.last_read <-
    (if C.get_bool r then Some (C.get r ~what:"keyboard last read" C.count) else None);
  update_ready t
