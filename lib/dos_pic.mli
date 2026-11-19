(** The two 8259A interrupt controllers of a PC/AT. The master takes IRQ0-7
    on vectors 08h-0Fh at ports 20h/21h; the slave takes IRQ8-15 on vectors
    70h-77h at ports A0h/A1h, and its output is the master's IR2 (SeaBIOS
    src/hw/pic.c pic_reset). Each chip runs fixed priority with input 0
    highest (Intel 8259A data sheet, "fully nested mode"), so the slave's
    lines rank between IRQ1 and IRQ3.

    A line asks with {!raise_irq}. The CPU takes the highest-priority line
    that is asking, not masked, and not shadowed by a line of the same or
    higher priority still in service. Taking it moves its bit from the
    request register (IRR) to the in-service register (ISR); an
    end-of-interrupt command written to the command port clears it. A
    handler that never sends one blocks its own line and every line below
    it, as on the chip.

    Taking a slave line also puts the master's IR2 in service, as the
    master acknowledges the cascade (DOSBox-X src/hardware/pic.cpp
    slave_startIRQ). A slave handler sends EOI to both chips, the slave
    first (SeaBIOS pic_eoi2). Until the master's EOI, IR2 shadows every
    slave line and IRQ3-7. The master's IR2 mask masks the whole slave.

    Of the commands on a command port, the model runs the two EOIs and the
    OCW3 choice of which register a read returns. Initialization (ICW1)
    and the rotation and priority commands are outside it: a write of one
    changes nothing, so the vectors stay 08h and 70h. Both chips start with
    nothing masked. *)

type t

(** The lines a device of this machine raises. A closed set: an IRQ no
    device raises cannot be asked for. *)
type line =
  | Timer  (** IRQ0, the PIT channel 0 *)
  | Keyboard  (** IRQ1 *)
  | Rtc  (** IRQ8, the RTC's interrupt output (slave input 0) *)
  | Sound_blaster  (** IRQ7, the card's DOSBox default ([irq=7]) *)

type controller = Master | Slave

val vector : line -> int
(** 08h for [Timer], 09h for [Keyboard], 70h for [Rtc], 0Fh for
    [Sound_blaster]. *)

val create : unit -> t
(** Nothing asked, nothing in service, nothing masked. *)

val raise_irq : t -> line -> unit
(** The line asks. Asking again before the CPU takes it is the same ask. *)

val requested : t -> line -> bool

val lower_irq : t -> line -> unit
(** The line falls before the CPU took it: the request goes away (DOSBox
    [PIC_DeActivateIRQ]). A line already in service stays there until
    its EOI. *)

val pending : t -> bool
(** A line asks and is not masked. Cheaper than {!next}, for the check
    before every instruction. *)

val next : t -> line option
(** The line the CPU takes at the next instruction boundary with IF set. *)

val acknowledge : t -> line -> unit
(** The CPU took [line]: it stops asking and is in service, and for a
    slave line so is the master's IR2. *)

val end_of_interrupt : t -> unit
(** A non-specific EOI to the master, as a handler's
    [mov al,20h; out 20h,al]: the master's line in service with the
    highest priority is done. *)

(** {1 Ports} [Master] is 20h (command) and 21h (mask), [Slave] is A0h and
    A1h. *)

val read_command : t -> controller -> int
(** IRR, or ISR after an OCW3 chose it. The master's IRR bit 2 is set
    while the slave asks. *)

val write_command : t -> controller -> int -> unit
val read_mask : t -> controller -> int
val write_mask : t -> controller -> int -> unit

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
