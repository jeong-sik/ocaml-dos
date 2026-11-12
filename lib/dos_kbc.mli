(** The keyboard as the 8042 controller shows it: one byte at a time in
    port 0x60, a full flag in bit 0 of port 0x64, and an IRQ1 each time a
    new byte arrives.

    A key goes down and comes up. The make code (the BIOS scan code) comes
    first and the break code (make + 80h) after it. A word the key gives
    with shift, ctrl or alt ({!Dos_keymap}) is wrapped in that modifier's
    make and break: 'A' is 2Ah 1Eh 9Eh AAh, alt-X is 38h 2Dh ADh B8h. The
    make is sent when the key is pressed and the break 100 ms later, so a
    game that samples which keys are down sees the key down. Bytes go out
    in the order they are due; a key pressed while another is held sends
    its make before that key's break. A byte arrives only after the program
    read the last one, and 1 ms after that read at the earliest — the time
    a keyboard takes to send a byte.

    A make code carries the BIOS word it stands for. The BIOS keyboard
    interrupt puts that word in the ring, so INT 16h answers the word the
    host pressed, and a game that hooks INT 9 and reads port 0x60 itself
    sees the scan codes. *)

type t

val create : unit -> t

val press : t -> now:int -> int -> unit
(** One key down at cycle [now] and up 100 ms later, for the BIOS word
    [scan lsl 8 lor ascii]. The key and its modifier come from
    {!Dos_keymap.of_word}; for a word no key gives, the make code is the
    word's high byte and nothing is held. *)

val latch : t -> now:int -> bool
(** Moves the next byte into port 0x60 if it is due, the last one was
    read, and 1 ms passed since. [true] when a byte arrived: the controller
    raises IRQ1.
    The machine calls it before each instruction. *)

val ready_at : t -> int
(** The first cycle {!latch} can move a byte at; [max_int] while it
    cannot. The machine looks at it before each instruction so that
    nothing else runs while the keyboard is idle. *)

val read_data : t -> now:int -> int
(** A read of port 0x60. The first read of a byte empties the buffer;
    reading again before the next byte arrives gives the same byte, as a
    hook that read it and then chains to the BIOS needs. *)

val status : t -> int
(** A read of port 0x64. Bit 0 is set while a byte waits in port 0x60. *)

val data_word : t -> int option
(** The BIOS word the byte in port 0x60 stands for: [Some] for a make code
    from {!press}, [None] for a break code or a modifier byte. *)

val holds_key : t -> bool
(** A make code the program has not read yet, queued or waiting in port
    0x60. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
