(* The first 8237A, as a DOS program and a device drive it. See dos_dma.mli. *)

type channel = Channel0 | Channel1 | Channel2 | Channel3

let numbered = [| Channel0; Channel1; Channel2; Channel3 |]
let index = function Channel0 -> 0 | Channel1 -> 1 | Channel2 -> 2 | Channel3 -> 3

(* Mode register bits 3-2, in the data sheet's order: 00 verify, 01 write
   (device to memory), 10 read (memory to device), 11 illegal. *)
type transfer = Verify | Device_to_memory | Memory_to_device | Illegal

let transfers = [| Verify; Device_to_memory; Memory_to_device; Illegal |]

let transfer_index = function
  | Verify -> 0 | Device_to_memory -> 1 | Memory_to_device -> 2 | Illegal -> 3

type state = {
  mutable base_address : int;  (* the value last written; auto-init reloads it *)
  mutable address : int;       (* the current address, 16 bits *)
  mutable base_count : int;
  mutable count : int;         (* the current count: bytes left, minus one *)
  mutable page : int;          (* address bits 16-23 *)
  mutable masked : bool;
  mutable terminal : bool;     (* TC reached since the last status read *)
  mutable auto_init : bool;
  mutable decrement : bool;
  mutable transfer : transfer;
}

type t = {
  channels : state array;
  mutable high_next : bool;  (* the byte pointer flip-flop: the next access is the high byte *)
}

let fresh () =
  { base_address = 0; address = 0; base_count = 0; count = 0; page = 0; masked = true;
    terminal = false; auto_init = false; decrement = false; transfer = Verify }

let create () = { channels = Array.init (Array.length numbered) (fun _ -> fresh ()); high_next = false }

let chan t ch = t.channels.(index ch)

(* Ports 00h-0Fh by their low four bits: the even ports below 8 are the
   channels' addresses and the odd ones their counts; 08h-0Fh are the
   controller's own registers. *)
type register =
  | Address of channel
  | Count of channel
  | Status_command          (* 08h: read status, write command *)
  | Request                 (* 09h *)
  | Single_mask             (* 0Ah *)
  | Mode                    (* 0Bh *)
  | Clear_flip_flop         (* 0Ch *)
  | Temporary_master_clear  (* 0Dh: read temporary, write master clear *)
  | Clear_masks             (* 0Eh *)
  | All_masks               (* 0Fh *)

let controller_ports = 0x10
let channel_registers = 8

let control_registers =
  [| Status_command; Request; Single_mask; Mode; Clear_flip_flop; Temporary_master_clear;
     Clear_masks; All_masks |]

let register_of_port p =
  if p < channel_registers then
    let ch = numbered.(p lsr 1) in
    if p land 1 = 0 then Address ch else Count ch
  else control_registers.(p - channel_registers)

(* The page registers sit where the IBM AT put them. *)
let page_channel p =
  match p with
  | 0x87 -> Some Channel0
  | 0x83 -> Some Channel1
  | 0x81 -> Some Channel2
  | 0x82 -> Some Channel3
  | _ -> None

(* Bits of the bytes written to 0Ah, 0Bh and 0Fh. *)
let channel_bits = 0x03
let mask_set = 0x04
let transfer_shift = 2
let mode_auto_init = 0x10
let mode_decrement = 0x20

(* Reading a write-only register floats the bus. *)
let write_only_read = 0xFF

(* The temporary register holds a byte only during a memory-to-memory
   transfer, which the model does not run. *)
let temporary_register = 0x00

let byte_pointer t =
  let high = t.high_next in
  t.high_next <- not high;
  high

let half ~high v = if high then (v lsr 8) land 0xFF else v land 0xFF
let set_half ~high old v = if high then (old land 0x00FF) lor (v lsl 8) else (old land 0xFF00) lor v

(* The low nibble of the status is TC per channel, cleared by this read.
   The high nibble is the channels whose device asks (DREQ); no device of
   this machine asks yet. *)
let read_status t =
  let tc = ref 0 in
  Array.iteri
    (fun i c ->
      if c.terminal then tc := !tc lor (1 lsl i);
      c.terminal <- false)
    t.channels;
  !tc

(* Master clear resets the controller, not the channels' programming: the
   flip-flop, the status and the masks (all set). Mode, address and count
   survive, as on the discrete 8237 (86Box dma_xt8237_master_clear). *)
let master_clear t =
  t.high_next <- false;
  Array.iter (fun c -> c.terminal <- false; c.masked <- true) t.channels

let controller_in t p =
  match register_of_port p with
  | Address ch -> half ~high:(byte_pointer t) (chan t ch).address
  | Count ch -> half ~high:(byte_pointer t) (chan t ch).count
  | Status_command -> read_status t
  | Temporary_master_clear -> temporary_register
  | Request | Single_mask | Mode | Clear_flip_flop | Clear_masks | All_masks -> write_only_read

let controller_out t p v =
  let selected () = t.channels.(v land channel_bits) in
  match register_of_port p with
  | Address ch ->
    let c = chan t ch and high = byte_pointer t in
    c.base_address <- set_half ~high c.base_address v;
    c.address <- set_half ~high c.address v
  | Count ch ->
    let c = chan t ch and high = byte_pointer t in
    c.base_count <- set_half ~high c.base_count v;
    c.count <- set_half ~high c.count v
  | Status_command -> ()  (* the command register is outside the model *)
  | Request -> ()         (* so is the software request *)
  | Single_mask -> (selected ()).masked <- v land mask_set <> 0
  | Mode ->
    let c = selected () in
    c.transfer <- transfers.((v lsr transfer_shift) land channel_bits);
    c.auto_init <- v land mode_auto_init <> 0;
    c.decrement <- v land mode_decrement <> 0
  | Clear_flip_flop -> t.high_next <- false
  | Temporary_master_clear -> master_clear t
  | Clear_masks -> Array.iter (fun c -> c.masked <- false) t.channels
  | All_masks -> Array.iteri (fun i c -> c.masked <- v land (1 lsl i) <> 0) t.channels

let port_in t p =
  let p = p land 0xffff in
  if p < controller_ports then Some (controller_in t p)
  else Option.map (fun ch -> (chan t ch).page) (page_channel p)

let port_out t p v =
  let p = p land 0xffff and v = v land 0xff in
  if p < controller_ports then begin controller_out t p v; true end
  else
    match page_channel p with
    | Some ch -> (chan t ch).page <- v; true
    | None -> false

type pull =
  | Masked
  | Byte of { value : int; terminal : bool }
  | No_data of { terminal : bool }

(* Address and count are 16 bits. The address moves inside its 64KB page:
   the page register does not carry. *)
let word_mask = 0xFFFF

let pull t ~mem ch =
  let c = chan t ch in
  if c.masked then Masked
  else begin
    let physical = ((c.page lsl 16) lor c.address) mod Bytes.length mem in
    let value =
      match c.transfer with
      | Memory_to_device -> Some (Char.code (Bytes.get mem physical))
      | Verify | Device_to_memory | Illegal -> None
    in
    c.address <- (if c.decrement then c.address - 1 else c.address + 1) land word_mask;
    let terminal = c.count = 0 in
    c.count <- (c.count - 1) land word_mask;
    if terminal then begin
      c.terminal <- true;
      if c.auto_init then begin
        c.address <- c.base_address;
        c.count <- c.base_count
      end
      else c.masked <- true
    end;
    match value with
    | Some value -> Byte { value; terminal }
    | None -> No_data { terminal }
  end

module C = Dos_snap_codec

let transfer_r = C.range 0 (Array.length transfers - 1)

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. *)
let write_channel w
    { base_address; address; base_count; count; page; masked; terminal; auto_init; decrement;
      transfer } =
  let put what r v = C.put w ~what r v in
  put "DMA base address" C.word base_address;
  put "DMA address" C.word address;
  put "DMA base count" C.word base_count;
  put "DMA count" C.word count;
  put "DMA page" C.byte page;
  C.put_bool w masked;
  C.put_bool w terminal;
  C.put_bool w auto_init;
  C.put_bool w decrement;
  put "DMA transfer" transfer_r (transfer_index transfer)

let read_channel r c =
  let get what rng = C.get r ~what rng in
  c.base_address <- get "DMA base address" C.word;
  c.address <- get "DMA address" C.word;
  c.base_count <- get "DMA base count" C.word;
  c.count <- get "DMA count" C.word;
  c.page <- get "DMA page" C.byte;
  c.masked <- C.get_bool r;
  c.terminal <- C.get_bool r;
  c.auto_init <- C.get_bool r;
  c.decrement <- C.get_bool r;
  c.transfer <- transfers.(get "DMA transfer" transfer_r)

let write_state w { channels; high_next } =
  Array.iter (write_channel w) channels;
  C.put_bool w high_next

let read_state r t =
  Array.iter (read_channel r) t.channels;
  t.high_next <- C.get_bool r
