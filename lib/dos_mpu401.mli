(** A Roland MPU-401 MIDI interface of the UART-only kind, at ports 330h
    (data) and 331h (status and command), without sound. The Sound Blaster
    16 and AWE32 carry this kind: of the MPU-401's commands they take only
    3Fh (enter UART mode) and FFh (reset). A music driver for a Roland
    MT-32 or a General MIDI module resets the card and reads the FEh
    answer, sends 3Fh and reads FEh again, then writes MIDI bytes to
    330h.

    {b Ports.} 331h reads as status: bits 0-5 read 1, bit 6 clear means
    the interface takes a byte now (always here), bit 7 clear means a byte
    waits in 330h. Reading 330h takes that byte; with none waiting it
    reads FEh (DOSBox and 86Box). Every byte written to 330h is MIDI out,
    in either mode (86Box's UART-only variant).

    {b Commands} (writes to 331h).
    - In command mode, the mode after a reset and at power-on: FFh resets
      (the waiting bytes are dropped) and answers FEh. 3Fh enters UART mode
      and answers FEh. The card ignores every other command and does not
      answer it.
    - In UART mode: FFh resets and returns to command mode without an
      answer. The card ignores every other command.

    A program that finds the card still in UART mode, left there by the
    program before it, gets no answer to its first FFh and an FEh for its
    second.

    {b Sources.} 86Box [src/sound/snd_mpu401.c] (the UART-only variant
    "seen on the Sound Blaster 16/AWE32": the two commands, the reset to
    command mode at power-on, the cleared queue on reset, the silent reset
    in UART mode) and DOSBox-staging [src/hardware/audio/mpu401.cpp] (the
    same status bits, FEh from an empty 330h, no answer to a reset in UART
    mode).

    {b Outside the model.} The intelligent-mode commands of a full MPU-401
    (tracks, timers, tempo). MIDI input. The reset's busy time (86Box
    27 x 33 us, DOSBox 14 ms): a command written then sets bit 6 until
    the reset ends, and runs at once (86Box) or after the reset (DOSBox).
    Either way the answers come out in the order they do here. The IRQ:
    86Box raises it while a byte waits, DOSBox does not raise it for this
    kind; whoever wires the card in decides. *)

type t

val data_port : int
(** 330h. *)

val status_port : int
(** 331h. *)

val create : unit -> t
(** Power-on: command mode, nothing waiting. *)

val port_in : t -> int -> int option
(** A read of 330h or 331h; [None] for any other port. *)

(** What a write did. *)
type write =
  | Taken                    (** a command, handled or ignored *)
  | Midi_out of int          (** a byte for the MIDI device *)

val port_out : t -> int -> int -> write option
(** [port_out t port byte]: a write to 330h or 331h; [None] for any other
    port. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
