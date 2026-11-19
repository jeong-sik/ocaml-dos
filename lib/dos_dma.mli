(** The first 8237A DMA controller of a PC: channels 0-3 on ports
    00h-0Fh, and the page registers that hold bits 16-23 of each channel's
    address (87h channel 0, 83h channel 1, 81h channel 2, 82h channel 3).

    A program sets a channel up through the ports: address and count, low
    byte then high byte through one shared flip-flop; the mode; the mask.
    A device then moves bytes through it one at a time with {!pull}. Each
    byte advances the address by one (down, if the mode says so) inside its
    64KB page and takes one from the count. The byte that takes the count
    below zero is the terminal count (TC): the status register (08h) shows
    it until a read of the status clears it. At TC an auto-initialize
    channel reloads its address and count from the values last written;
    any other channel masks itself and its count reads FFFFh.

    The behaviour follows DOSBox-staging [src/hardware/dma.cpp] and 86Box
    [src/dma.c]. Outside the model: the command register (08h write), the
    software request register (09h), memory-to-memory transfers, the
    demand/single/block/cascade distinction (a device pulls one byte at a
    time), and the second controller (channels 4-7). A read of a
    write-only register (09h-0Ch, 0Eh, 0Fh) returns FFh, as in DOSBox;
    86Box returns the last value written there. *)

type t

val create : unit -> t
(** Power-on: every channel masked, every register zero, flip-flop low. *)

(** A channel of this controller. *)
type channel = Channel0 | Channel1 | Channel2 | Channel3

val port_in : t -> int -> int option
(** A read of port [p]: [Some] for 00h-0Fh and the four page registers,
    [None] for a port this controller does not answer. *)

val port_out : t -> int -> int -> bool
(** A write of a byte to port [p]; [false] for a port this controller does
    not answer. *)

(** {1 Devices} *)

(** What one DMA cycle on a channel gave the device. *)
type pull =
  | Masked  (** The channel is masked: nothing moved. *)
  | Byte of { value : int; terminal : bool }
      (** A memory-to-device ("read") transfer moved the byte at the
          channel's address. [terminal] is true on the TC byte. *)
  | No_data of { terminal : bool }
      (** A verify, device-to-memory ("write") or illegal transfer type:
          the address and count moved but memory was not read. The model
          never writes memory for a device that has nothing to give. *)

val pull : t -> mem:Bytes.t -> channel -> pull
(** One cycle on the channel. The physical address is the page register
    above the 16-bit address, taken modulo the size of [mem]. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
