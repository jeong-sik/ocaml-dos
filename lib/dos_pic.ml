(* The two 8259As of a PC/AT, as far as a real-mode DOS game drives them.
   See dos_pic.mli. *)

type line = Timer | Keyboard | Rtc | Sound_blaster
type controller = Master | Slave

type chip = {
  mutable imr : int;       (* mask port: a set bit masks that input *)
  mutable irr : int;       (* asking, not yet taken *)
  mutable isr : int;       (* taken, no EOI yet *)
  mutable read_isr : bool; (* OCW3: a read of the command port returns ISR, not IRR *)
}

type t = { master : chip; slave : chip }

(* SeaBIOS src/hw/pic.h BIOS_HWIRQ0_VECTOR, BIOS_HWIRQ8_VECTOR *)
let master_base = 0x08
let slave_base = 0x70

(* The slave's INT output is the master's IR2 (SeaBIOS pic_reset: ICW3 04h
   on the master, 02h on the slave). *)
let cascade_input = 2
let cascade_bit = 1 lsl cascade_input

(* Priority order: the first line is the highest. The slave's lines rank
   where the cascade sits on the master, between IR1 and IR3. *)
let lines = [ Timer; Keyboard; Rtc; Sound_blaster ]

let controller = function
  | Timer | Keyboard | Sound_blaster -> Master
  | Rtc -> Slave

let number = function Timer -> 0 | Keyboard -> 1 | Sound_blaster -> 7 | Rtc -> 0
let bit l = 1 lsl number l

let vector l =
  (match controller l with Master -> master_base | Slave -> slave_base) + number l

let chip t = function Master -> t.master | Slave -> t.slave
let chip_of t l = chip t (controller l)

let new_chip () = { imr = 0; irr = 0; isr = 0; read_isr = false }
let create () = { master = new_chip (); slave = new_chip () }

let raise_irq t l = let c = chip_of t l in c.irr <- c.irr lor bit l
let requested t l = (chip_of t l).irr land bit l <> 0
let lower_irq t l = let c = chip_of t l in c.irr <- c.irr land lnot (bit l)

(* An input is shadowed by any input in service at its own priority or
   above: the ISR bits from 0 up to its own. *)
let at_or_above b = (b lsl 1) - 1

let admits c b = c.imr land b = 0 && c.isr land at_or_above b = 0

(* The slave drives IR2 while one of its inputs asks, is unmasked and is
   not shadowed on the slave. *)
let slave_asks t =
  let s = t.slave in
  let open_bits = s.irr land lnot s.imr in
  open_bits <> 0
  && (let lowest = open_bits land (-open_bits) in
      s.isr land at_or_above lowest = 0)

let deliverable t l =
  let b = bit l in
  match controller l with
  | Master -> t.master.irr land b <> 0 && admits t.master b
  | Slave ->
    t.slave.irr land b <> 0 && admits t.slave b && admits t.master cascade_bit

(* The machine asks before every instruction, and almost always nothing
   is asking at all: both IRRs are 0, which the first test settles. *)
let[@inline] pending t =
  let m = t.master and s = t.slave in
  m.irr lor s.irr <> 0
  && (m.irr land lnot m.imr <> 0
      || (s.irr land lnot s.imr <> 0 && m.imr land cascade_bit = 0))

let next t = if pending t then List.find_opt (deliverable t) lines else None

(* Taking a slave input puts the master's IR2 in service as well: the
   master shadows its own IR2-IR7 until the handler's EOI to port 20h
   (DOSBox-X src/hardware/pic.cpp slave_startIRQ). *)
let acknowledge t l =
  let c = chip_of t l and b = bit l in
  c.irr <- c.irr land lnot b;
  c.isr <- c.isr lor b;
  match controller l with
  | Master -> ()
  | Slave -> t.master.isr <- t.master.isr lor cascade_bit

(* Non-specific EOI clears the highest-priority bit in service, the lowest
   set bit of ISR. *)
let lowest_bit v = v land (-v)

(* Command port writes, by the bits the data sheet decodes them with: bit
   4 set is ICW1, else bit 3 set is OCW3, else OCW2, whose bits 7-5 name
   the command (001 non-specific EOI, 011 specific EOI on the level in bits
   2-0). *)
let icw1_bit = 0x10
let ocw3_bit = 0x08
let ocw3_read_register = 0x02
let ocw3_choose_isr = 0x01
let ocw2_non_specific_eoi = 0b001
let ocw2_specific_eoi = 0b011
let ocw2_level_mask = 0x07

let eoi c = c.isr <- c.isr land lnot (lowest_bit c.isr)
let end_of_interrupt t = eoi t.master

let write_command t ctl v =
  let c = chip t ctl in
  if v land icw1_bit <> 0 then ()
  else if v land ocw3_bit <> 0 then begin
    if v land ocw3_read_register <> 0 then c.read_isr <- v land ocw3_choose_isr <> 0
  end
  else
    let op = v lsr 5 in
    if op = ocw2_non_specific_eoi then eoi c
    else if op = ocw2_specific_eoi then c.isr <- c.isr land lnot (1 lsl (v land ocw2_level_mask))

(* The master's IR2 is the slave's output, so its IRR bit shows while the
   slave asks. *)
let read_command t ctl =
  let c = chip t ctl in
  if c.read_isr then c.isr
  else
    match ctl with
    | Master -> c.irr lor if slave_asks t then cascade_bit else 0
    | Slave -> c.irr

let read_mask t ctl = (chip t ctl).imr
let write_mask t ctl v = (chip t ctl).imr <- v land 0xff

module C = Dos_snap_codec

let write_chip w name { imr; irr; isr; read_isr } =
  C.put w ~what:(name ^ " mask") C.byte imr;
  C.put w ~what:(name ^ " requests") C.byte irr;
  C.put w ~what:(name ^ " in service") C.byte isr;
  C.put_bool w read_isr

let read_chip r name c =
  c.imr <- C.get r ~what:(name ^ " mask") C.byte;
  c.irr <- C.get r ~what:(name ^ " requests") C.byte;
  c.isr <- C.get r ~what:(name ^ " in service") C.byte;
  c.read_isr <- C.get_bool r

let master_name = "PIC"
let slave_name = "slave PIC"

let write_state w t =
  write_chip w master_name t.master;
  write_chip w slave_name t.slave

let read_state r t =
  read_chip r master_name t.master;
  read_chip r slave_name t.slave
