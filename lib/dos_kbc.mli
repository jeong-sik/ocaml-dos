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
    sees the scan codes.

    A program can also talk to both chips, as QEMU's 8042 and PS/2
    keyboard answer (hw/input/pckbd.c, hw/input/ps2.c):
    - A byte written to port 0x64 is a controller command: 20h/60h read and
      write the command byte, AAh answers 55h, ABh and A9h answer 00h,
      ADh/AEh disable and enable the keyboard, A7h/A8h the aux port, C0h
      and D0h read the input and output ports, D1h writes the output port,
      D2h puts the next byte in port 0x60 as if the keyboard sent it.
    - A byte written to port 0x60 goes to the keyboard unless a controller
      command is waiting for it. EDh (LEDs), F0h (scan code set), F3h
      (rate) and FCh take one more byte; each byte draws FAh. EEh echoes,
      F2h answers FAh ABh 41h, F4h enables scanning, F5h and F6h reset it
      off and on, FFh answers FAh AAh. Any other byte draws FEh.
    - The controller's answer comes before the keyboard's, and both before
      any key. A reset drops the keys not yet sent; while scanning is off a
      pressed key sends nothing. While the keyboard is disabled its keys
      wait, and a write to port 0x60 enables it again.

    What this board does not do: the A20 line stays low (memory wraps at
    1 MB) so bit 1 of the output port reads 0; a CPU reset (FEh, or bit 0
    cleared) is not carried out; there is no aux device, so D3h and D4h
    take their byte and nothing answers; keys keep coming in the set the
    BIOS expects, whichever set or translation a program picks. *)

type t

val create : unit -> t

val press : t -> now:int -> int -> unit
(** One key down at cycle [now] and up 100 ms later, for the BIOS word
    [scan lsl 8 lor ascii]. The key and its modifier come from
    {!Dos_keymap.of_word}; for a word no key gives, the make code is the
    word's high byte and nothing is held. *)

val latch : t -> now:int -> bool
(** Moves the next byte into port 0x60 if it is due and the last one was
    read. A controller answer comes at once; a byte from the keyboard 1 ms
    after the last read at the earliest. [true] when a byte arrived and bit
    0 of the command byte asks for IRQ1.
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

val write_data : t -> now:int -> int -> unit
(** A write to port 0x60 at cycle [now]. *)

val write_command : t -> int -> unit
(** A write to port 0x64. *)

val command_byte : t -> int
(** The controller's command byte, 61h after POST. *)

val leds : t -> int
(** The LEDs the program last set: scroll, num and caps lock in bits 0-2. *)

val data_word : t -> int option
(** The BIOS word the byte in port 0x60 stands for: [Some] for a make code
    from {!press}, [None] for a break code or a modifier byte. *)

val holds_key : t -> bool
(** A make code the program has not read yet, queued or waiting in port
    0x60. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
