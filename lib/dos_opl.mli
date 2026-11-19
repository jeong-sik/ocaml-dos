(** The YM3812 (OPL2) FM chip's voice registers (RFC 0006 stage 4): 01h
    (waveform select enable), 08h (CSM, note select), 20h-35h, 40h-55h,
    60h-75h, 80h-95h (operators), A0h-A8h, B0h-B8h, C0h-C8h (channels),
    BDh (depth, rhythm) and E0h-F5h (waveforms), as Jeffrey S. Lee's
    "Programming the AdLib/Sound Blaster FM Music Chips" lays them out.

    The timer registers 02h-04h are {!Dos_ports}'s: they never reach this
    module. Numbers from F6h up name no register and are dropped. The gaps
    inside the range (36h-3Fh, A9h-AFh, ...) hold what was written and
    play no part.

    The registers are machine state: a game sets a voice up once and
    plays notes on it later, so a snapshot carries them whether or not
    sound is on. Nothing here makes sound yet (stage 5). *)

type t

val register_count : int
(** F6h: registers 00h-F5h. *)

val create : unit -> t
(** Power-on: every register 0. The chip has no software reset of these
    registers; programs write them all at start. *)

val write : t -> int -> int -> unit
(** [write t reg v]: the low 8 bits of [v] into register [reg]. A [reg] of
    F6h or more is dropped. *)

val register : t -> int -> int
(** The value last written; 0 for a [reg] of F6h or more. *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
