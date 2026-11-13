(* The 8259A, as far as a real-mode DOS game drives it. See dos_pic.mli. *)

type line = Timer | Keyboard | Sound_blaster

type t = {
  mutable imr : int;       (* port 0x21: a set bit masks that IRQ *)
  mutable irr : int;       (* asking, not yet taken *)
  mutable isr : int;       (* taken, no EOI yet *)
  mutable read_isr : bool; (* OCW3: a read of 0x20 returns ISR, not IRR *)
}

let base_vector = 0x08

(* Priority order: the first line is the highest. *)
let lines = [ Timer; Keyboard; Sound_blaster ]

let number = function Timer -> 0 | Keyboard -> 1 | Sound_blaster -> 7
let bit l = 1 lsl number l
let vector l = base_vector + number l

let create () = { imr = 0; irr = 0; isr = 0; read_isr = false }

let raise_irq t l = t.irr <- t.irr lor bit l
let requested t l = t.irr land bit l <> 0
let lower_irq t l = t.irr <- t.irr land lnot (bit l)

(* A line is shadowed by any line in service at its own priority or above:
   the ISR bits from 0 up to its own. The machine asks before every
   instruction, and almost always nothing unmasked is asking. *)
let deliverable t l =
  t.irr land bit l <> 0 && t.imr land bit l = 0 && t.isr land ((bit l lsl 1) - 1) = 0

let pending t = t.irr land lnot t.imr <> 0

let next t = if pending t then List.find_opt (deliverable t) lines else None

let acknowledge t l =
  t.irr <- t.irr land lnot (bit l);
  t.isr <- t.isr lor bit l

(* Non-specific EOI clears the highest-priority bit in service, the lowest
   set bit of ISR. *)
let lowest_bit v = v land (-v)

(* Port 0x20 commands, by the bits the data sheet decodes them with:
   bit 4 set is ICW1, else bit 3 set is OCW3, else OCW2, whose bits 7-5
   name the command (001 non-specific EOI, 011 specific EOI on the level in
   bits 2-0). *)
let icw1_bit = 0x10
let ocw3_bit = 0x08
let ocw3_read_register = 0x02
let ocw3_choose_isr = 0x01
let ocw2_non_specific_eoi = 0b001
let ocw2_specific_eoi = 0b011
let ocw2_level_mask = 0x07

let end_of_interrupt t = t.isr <- t.isr land lnot (lowest_bit t.isr)

let write_command t v =
  if v land icw1_bit <> 0 then ()
  else if v land ocw3_bit <> 0 then begin
    if v land ocw3_read_register <> 0 then t.read_isr <- v land ocw3_choose_isr <> 0
  end
  else
    let op = v lsr 5 in
    if op = ocw2_non_specific_eoi then end_of_interrupt t
    else if op = ocw2_specific_eoi then
      t.isr <- t.isr land lnot (1 lsl (v land ocw2_level_mask))

let read_command t = if t.read_isr then t.isr else t.irr
let read_mask t = t.imr
let write_mask t v = t.imr <- v land 0xff

module C = Dos_snap_codec

let write_state w { imr; irr; isr; read_isr } =
  C.put w ~what:"PIC mask" C.byte imr;
  C.put w ~what:"PIC requests" C.byte irr;
  C.put w ~what:"PIC in service" C.byte isr;
  C.put_bool w read_isr

let read_state r t =
  t.imr <- C.get r ~what:"PIC mask" C.byte;
  t.irr <- C.get r ~what:"PIC requests" C.byte;
  t.isr <- C.get r ~what:"PIC in service" C.byte;
  t.read_isr <- C.get_bool r
