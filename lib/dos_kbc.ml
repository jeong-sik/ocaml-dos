(* The 8042 keyboard controller, from the program's side. See dos_kbc.mli.

   The controller and keyboard commands follow QEMU (hw/input/pckbd.c,
   hw/input/ps2.c, master 29760efaf96b): replies come before key bytes, a
   keyboard reset drops the keys it had not sent, and a command the keyboard
   does not know draws a resend. *)

(* In CPU cycles. A keyboard sends a byte as an 11-bit frame at 10-16 kHz,
   about 1 ms; a person holds a tapped key about 100 ms. *)
let byte_gap = Dos_clock.cpu_hz / 1000
let key_hold = Dos_clock.cpu_hz / 10

let break_bit = 0x80

(* Bit 2 is the system flag a POST sets; bit 4 says the keyboard is not
   inhibited. Bit 0 is the output buffer. *)
let status_idle = 0x14
let output_full = 0x01

(* The command byte (QEMU's "mode"), read with 20h and written with 60h.
   After POST it is what SeaBIOS leaves (src/hw/ps2port.c, Ps2ctr, commit
   28aa46349b97): keyboard interrupt on, aux port off, translation on. *)
let cb_keyboard_irq = 0x01
let cb_keyboard_disabled = 0x10
let cb_aux_disabled = 0x20
let cb_translate = 0x40
let command_byte_after_post = 0x61

(* The output port (D0h/D1h). Bit 0 high keeps the CPU running, bit 1 is
   the A20 gate, and bits 2-3 and 6-7 read 1 (QEMU KBD_OUT_ONES). This
   board's A20 line stays low: memory wraps at 1 MB, so the bit reads 0
   whatever a program writes. *)
let out_a20 = 0x02
let output_port_after_post = 0xCD

(* QEMU's answer to C0h: bit 7, the keyboard is not locked. *)
let input_port = 0x80

let reply_ack = 0xFA
let reply_resend = 0xFE
let reply_reset_ok = 0xAA
let reply_echo = 0xEE
let reply_id = 0xAB
let self_test_ok = 0x55
let interface_ok = 0x00

(* The keyboard's ID after 0xAB, and the number a scan code set reports as,
   each as the controller's translation passes it on or as the keyboard
   sends it (QEMU ps2.c, KBD_CMD_GET_ID and translate_table). *)
let id_translated = 0x41
let id_raw = 0x83
let default_set = 2
let set_translated = function 1 -> 0x43 | 2 -> 0x41 | _ -> 0x3F

(* LEDs: scroll, num and caps lock in bits 0-2. *)
let led_bits = 0x07

(* What the next byte written to port 0x60 is for, after a command took
   one. *)
type controller_arg = Command_byte | Output_port | Keyboard_buffer | Aux_buffer | Aux_device
type keyboard_arg = Leds | Scan_set | Typematic_rate | Key_mode

type controller_command =
  | Read_command_byte
  | Take of controller_arg
  | Aux_disabled of bool
  | Test_aux
  | Self_test
  | Test_keyboard
  | Keyboard_disabled of bool
  | Read_input_port
  | Read_output_port
  | Gate_a20 of bool
  | Reset_cpu
  | Ignored  (* a pulse that leaves bit 0 alone, FFh, or a command QEMU ignores *)

let controller_command = function
  | 0x20 -> Read_command_byte
  | 0x60 -> Take Command_byte
  | 0xA7 -> Aux_disabled true
  | 0xA8 -> Aux_disabled false
  | 0xA9 -> Test_aux
  | 0xAA -> Self_test
  | 0xAB -> Test_keyboard
  | 0xAD -> Keyboard_disabled true
  | 0xAE -> Keyboard_disabled false
  | 0xC0 -> Read_input_port
  | 0xD0 -> Read_output_port
  | 0xD1 -> Take Output_port
  | 0xD2 -> Take Keyboard_buffer
  | 0xD3 -> Take Aux_buffer
  | 0xD4 -> Take Aux_device
  | 0xDD -> Gate_a20 false
  | 0xDF -> Gate_a20 true
  | v when v land 0xF0 = 0xF0 -> if v land 1 = 0 then Reset_cpu else Ignored
  | _ -> Ignored

type keyboard_command =
  | Takes of keyboard_arg
  | Echo
  | Identify
  | Enable
  | Reset_disable
  | Reset_enable
  | Reset
  | Acknowledge  (* FAh: all keys typematic and make/break, nothing to keep *)
  | Unknown

let keyboard_command = function
  | 0xED -> Takes Leds
  | 0xEE -> Echo
  | 0xF0 -> Takes Scan_set
  | 0xF2 -> Identify
  | 0xF3 -> Takes Typematic_rate
  | 0xF4 -> Enable
  | 0xF5 -> Reset_disable
  | 0xF6 -> Reset_enable
  | 0xFA -> Acknowledge
  | 0xFC -> Takes Key_mode
  | 0xFF -> Reset
  | _ -> Unknown

type byte_out = {
  code : int;
  word : int option;  (* the BIOS word a make code stands for *)
  due : int;          (* the cycle it is sent at, at the earliest *)
}

type t = {
  mutable queue : byte_out list;  (* key bytes, by [due]; equal ones in press order *)
  mutable replies : byte_out list;  (* the keyboard's answers, before any key byte *)
  mutable controller_reply : int option;  (* the controller's own, before both *)
  mutable data : int;
  mutable word : int option;
  mutable full : bool;
  mutable last_read : int option; (* None: nothing was ever read *)
  mutable command_byte : int;
  mutable output_port : int;
  mutable controller_wants : controller_arg option;
  mutable keyboard_wants : keyboard_arg option;
  mutable scanning : bool;
  mutable leds : int;
  mutable scan_set : int;
  mutable ready_at : int;
      (* Derived from the fields above: the first cycle the next byte can
         arrive at, [never] while nothing can. Not in the snapshot. *)
}

let never = max_int

let after_gap t due =
  match t.last_read with None -> due | Some r -> max due (r + byte_gap)

let keyboard_enabled t = t.command_byte land cb_keyboard_disabled = 0

let update_ready t =
  t.ready_at <-
    (if t.full then never
     else
       match t.controller_reply, t.replies, t.queue with
       | Some _, _, _ -> 0 (* the controller answers at once *)
       | None, next :: _, _ -> after_gap t next.due
       | None, [], next :: _ -> if keyboard_enabled t then after_gap t next.due else never
       | None, [], [] -> never)

let create () =
  { queue = []; replies = []; controller_reply = None; data = 0; word = None; full = false;
    last_read = None; command_byte = command_byte_after_post;
    output_port = output_port_after_post; controller_wants = None; keyboard_wants = None;
    scanning = true; leds = 0; scan_set = default_set; ready_at = never }

(* Insert after every byte due no later: two keys pressed together keep
   their order, and a key pressed while another is held sends its make
   before the held key's break, as a keyboard with rollover does. *)
let rec insert b = function
  | x :: rest when x.due <= b.due -> x :: insert b rest
  | rest -> b :: rest

(* The key the word stands for, and the modifier held around it. A word
   no key gives keeps the old contract: its high byte is the make code. *)
let press t ~now word =
  if t.scanning then begin
    let scan, held =
      match Dos_keymap.of_word word with
      | Some { Dos_keymap.scan; held } -> (scan, held)
      | None -> ((word lsr 8) land 0xff, None)
    in
    let down = { code = scan; word = Some word; due = now }
    and up = { code = scan lor break_bit; word = None; due = now + key_hold } in
    let bytes =
      match held with
      | None -> [ down; up ]
      | Some m ->
        let make = Dos_keymap.make_code m in
        [ { code = make; word = None; due = now }; down; up;
          { code = make lor break_bit; word = None; due = now + key_hold } ]
    in
    t.queue <- List.fold_left (fun q b -> insert b q) t.queue bytes;
    update_ready t
  end

let ready_at t = t.ready_at

let take t code word =
  t.data <- code;
  t.word <- word;
  t.full <- true

let latch t ~now =
  now >= t.ready_at
  && begin
    let arrived =
      match t.controller_reply, t.replies, t.queue with
      | Some code, _, _ -> t.controller_reply <- None; take t code None; true
      | None, next :: rest, _ -> t.replies <- rest; take t next.code None; true
      | None, [], next :: rest -> t.queue <- rest; take t next.code next.word; true
      | None, [], [] -> false
    in
    update_ready t;
    arrived && t.command_byte land cb_keyboard_irq <> 0
  end

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

(* ---------- commands ---------- *)

let controller_answers t code = t.controller_reply <- Some code

(* A new command drops the answers to the last one the program has not
   read (QEMU ps2_cqueue_reset). *)
let keyboard_answers t ~now codes =
  t.replies <- List.map (fun code -> { code; word = None; due = now }) codes

(* A reset drops the keys the keyboard had not sent. *)
let reset_keyboard t ~scanning =
  t.queue <- [];
  t.scanning <- scanning;
  t.leds <- 0;
  t.scan_set <- default_set

let translating t = t.command_byte land cb_translate <> 0

let keyboard_write t ~now v =
  match t.keyboard_wants with
  | Some arg ->
    t.keyboard_wants <- None;
    (match arg with
     | Leds -> t.leds <- v land led_bits; keyboard_answers t ~now [ reply_ack ]
     | Scan_set ->
       if v = 0 then
         keyboard_answers t ~now
           [ reply_ack; (if translating t then set_translated t.scan_set else t.scan_set) ]
       else if v >= 1 && v <= 3 then begin
         t.scan_set <- v;
         keyboard_answers t ~now [ reply_ack ]
       end
       else keyboard_answers t ~now [ reply_resend ]
     | Typematic_rate | Key_mode -> keyboard_answers t ~now [ reply_ack ])
  | None ->
    (match keyboard_command v with
     | Takes arg -> t.keyboard_wants <- Some arg; keyboard_answers t ~now [ reply_ack ]
     | Echo -> keyboard_answers t ~now [ reply_echo ]
     | Identify ->
       keyboard_answers t ~now
         [ reply_ack; reply_id; (if translating t then id_translated else id_raw) ]
     | Enable -> t.scanning <- true; keyboard_answers t ~now [ reply_ack ]
     | Reset_disable -> reset_keyboard t ~scanning:false; keyboard_answers t ~now [ reply_ack ]
     | Reset_enable -> reset_keyboard t ~scanning:true; keyboard_answers t ~now [ reply_ack ]
     | Reset -> reset_keyboard t ~scanning:true; keyboard_answers t ~now [ reply_ack; reply_reset_ok ]
     | Acknowledge -> keyboard_answers t ~now [ reply_ack ]
     | Unknown -> keyboard_answers t ~now [ reply_resend ])

let write_data t ~now v =
  let v = v land 0xff in
  (match t.controller_wants with
   | Some arg ->
     t.controller_wants <- None;
     (match arg with
      | Command_byte -> t.command_byte <- v
      | Output_port -> t.output_port <- v land lnot out_a20
      | Keyboard_buffer -> controller_answers t v
      | Aux_buffer | Aux_device -> () (* no aux device behind this controller *))
   | None ->
     (* Writing to the keyboard enables its interface again (QEMU
        kbd_write_data). *)
     t.command_byte <- t.command_byte land lnot cb_keyboard_disabled;
     keyboard_write t ~now v);
  update_ready t

let write_command t v =
  (match controller_command (v land 0xff) with
   | Read_command_byte -> controller_answers t t.command_byte
   | Take arg -> t.controller_wants <- Some arg
   | Aux_disabled true -> t.command_byte <- t.command_byte lor cb_aux_disabled
   | Aux_disabled false -> t.command_byte <- t.command_byte land lnot cb_aux_disabled
   | Gate_a20 _ (* the A20 line stays low *) | Reset_cpu (* not carried out *) | Ignored -> ()
   | Test_aux | Test_keyboard -> controller_answers t interface_ok
   | Self_test -> controller_answers t self_test_ok
   | Keyboard_disabled true -> t.command_byte <- t.command_byte lor cb_keyboard_disabled
   | Keyboard_disabled false -> t.command_byte <- t.command_byte land lnot cb_keyboard_disabled
   | Read_input_port -> controller_answers t input_port
   | Read_output_port -> controller_answers t t.output_port);
  update_ready t

let command_byte t = t.command_byte
let leds t = t.leds

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

let put_word_option w = function
  | None -> C.put_bool w false
  | Some v -> C.put_bool w true; C.put w ~what:"key word" C.word v

let get_word_option r =
  if C.get_bool r then Some (C.get r ~what:"key word" C.word) else None

let put_bytes_out w what list =
  C.put w ~what:(what ^ " length") C.count (List.length list);
  List.iter
    (fun { code; word; due } ->
      C.put w ~what C.byte code;
      put_word_option w word;
      C.put w ~what:(what ^ " due") C.count due)
    list

(* The reads are in the order the bytes were written; a loop keeps them
   there whatever order a list builder calls its function in. *)
let get_bytes_out r what =
  let rec bytes n acc =
    if n = 0 then List.rev acc
    else begin
      let code = C.get r ~what C.byte in
      let word = get_word_option r in
      let due = C.get r ~what:(what ^ " due") C.count in
      bytes (n - 1) ({ code; word; due } :: acc)
    end
  in
  bytes (C.get r ~what:(what ^ " length") C.count) []

let controller_arg_tag = function
  | Command_byte -> 0 | Output_port -> 1 | Keyboard_buffer -> 2 | Aux_buffer -> 3 | Aux_device -> 4

let controller_arg_of_tag = function
  | 0 -> Command_byte | 1 -> Output_port | 2 -> Keyboard_buffer | 3 -> Aux_buffer
  | 4 -> Aux_device
  | n -> C.fail (Printf.sprintf "controller argument tag %d" n)

let keyboard_arg_tag = function Leds -> 0 | Scan_set -> 1 | Typematic_rate -> 2 | Key_mode -> 3

let keyboard_arg_of_tag = function
  | 0 -> Leds | 1 -> Scan_set | 2 -> Typematic_rate | 3 -> Key_mode
  | n -> C.fail (Printf.sprintf "keyboard argument tag %d" n)

let arg_tags = C.range 0 4

let put_tag_option w what tag = function
  | None -> C.put_bool w false
  | Some a -> C.put_bool w true; C.put w ~what arg_tags (tag a)

let get_tag_option r what of_tag =
  if C.get_bool r then Some (of_tag (C.get r ~what arg_tags)) else None

let scan_sets = C.range 1 3

let write_state w
    { queue; replies; controller_reply; data; word; full; last_read; command_byte;
      output_port; controller_wants; keyboard_wants; scanning; leds; scan_set;
      ready_at = _ (* derived *) } =
  put_bytes_out w "keyboard byte" queue;
  put_bytes_out w "keyboard reply" replies;
  (match controller_reply with
   | None -> C.put_bool w false
   | Some b -> C.put_bool w true; C.put w ~what:"controller reply" C.byte b);
  C.put w ~what:"keyboard data" C.byte data;
  put_word_option w word;
  C.put_bool w full;
  (match last_read with
   | None -> C.put_bool w false
   | Some r -> C.put_bool w true; C.put w ~what:"keyboard last read" C.count r);
  C.put w ~what:"command byte" C.byte command_byte;
  C.put w ~what:"output port" C.byte output_port;
  put_tag_option w "controller argument" controller_arg_tag controller_wants;
  put_tag_option w "keyboard argument" keyboard_arg_tag keyboard_wants;
  C.put_bool w scanning;
  C.put w ~what:"keyboard LEDs" (C.range 0 led_bits) leds;
  C.put w ~what:"scan code set" scan_sets scan_set

let read_state r t =
  t.queue <- get_bytes_out r "keyboard byte";
  t.replies <- get_bytes_out r "keyboard reply";
  t.controller_reply <-
    (if C.get_bool r then Some (C.get r ~what:"controller reply" C.byte) else None);
  t.data <- C.get r ~what:"keyboard data" C.byte;
  t.word <- get_word_option r;
  t.full <- C.get_bool r;
  t.last_read <-
    (if C.get_bool r then Some (C.get r ~what:"keyboard last read" C.count) else None);
  t.command_byte <- C.get r ~what:"command byte" C.byte;
  t.output_port <- C.get r ~what:"output port" C.byte;
  t.controller_wants <- get_tag_option r "controller argument" controller_arg_of_tag;
  t.keyboard_wants <- get_tag_option r "keyboard argument" keyboard_arg_of_tag;
  t.scanning <- C.get_bool r;
  t.leds <- C.get r ~what:"keyboard LEDs" (C.range 0 led_bits);
  t.scan_set <- C.get r ~what:"scan code set" scan_sets;
  update_ready t
