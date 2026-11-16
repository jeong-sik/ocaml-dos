(** The 8259A interrupt controller of a PC/XT: one chip, IRQ0-7 on vectors
    08h-0Fh, fixed priority with IRQ0 highest (Intel 8259A data sheet,
    "fully nested mode").

    A line asks with {!raise_irq}. The CPU takes the highest-priority line
    that is asking, not masked, and not shadowed by a line of the same or
    higher priority still in service. Taking it moves its bit from the
    request register (IRR) to the in-service register (ISR); an
    end-of-interrupt command written to port 0x20 clears it. A handler
    that never sends one blocks its own line and every line below it, as
    on the chip.

    Of the commands on port 0x20, the model runs the two EOIs and the OCW3
    choice of which register a read of 0x20 returns. Initialization (ICW1)
    and the rotation and priority commands are outside it: a write of one
    changes nothing. *)

type t

(** The lines a device of this machine raises. A closed set: an IRQ no
    device raises cannot be asked for. *)
type line = Timer  (** IRQ0, the PIT channel 0 *) | Keyboard  (** IRQ1 *)

val vector : line -> int
(** 08h for [Timer], 09h for [Keyboard]. *)

val create : unit -> t
(** Nothing asked, nothing in service, nothing masked. *)

val raise_irq : t -> line -> unit
(** The line asks. Asking again before the CPU takes it is the same ask. *)

val requested : t -> line -> bool

val pending : t -> bool
(** A line asks and is not masked. Cheaper than {!next}, for the check
    before every instruction. *)

val next : t -> line option
(** The line the CPU takes at the next instruction boundary with IF set. *)

val acknowledge : t -> line -> unit
(** The CPU took [line]: it stops asking and is in service. *)

val end_of_interrupt : t -> unit
(** A non-specific EOI, as a handler's [mov al,20h; out 20h,al]: the line
    in service with the highest priority is done. *)

(** {1 Ports} 0x20 is the command port, 0x21 the mask. *)

val read_command : t -> int
(** IRR, or ISR after an OCW3 chose it. *)

val write_command : t -> int -> unit
val read_mask : t -> int
val write_mask : t -> int -> unit

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
