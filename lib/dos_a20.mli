(** The memory board's address lines and the A20 gate (RFC 0005 stage 2).

    An 8086, 186 or 286 machine here has 1 MB of RAM and 20 address lines:
    every physical address wraps at 1 MB and no gate exists. A 386 machine
    has 16 MB and 24 address lines (a 386SX board): the bus keeps the low
    24 bits of the CPU's address, so 1000000h reads what 0 reads, and the
    gate sits on bit 20.

    With the gate closed bit 20 is forced to 0: 100000h-1FFFFFh read what
    0-0FFFFFh read, and 300000h-3FFFFFh read what 200000h-2FFFFFh read.
    That is the wrap a real-mode program sees at FFFF:0010. With the gate
    open the bit passes. A machine starts closed, as a BIOS leaves it.

    One line drives the gate for both ways to open it: the 8042's output
    port (D1h, DDh, DFh) and port 92h. Whichever wrote last decides. *)

type board =
  | One_megabyte  (** 20 address lines, 1 MB of RAM, no gate *)
  | Sixteen_megabytes  (** 24 address lines, 16 MB of RAM, a gate on bit 20 *)

type t = private {
  board : board;
  mutable enabled : bool;
  mutable mask : int;
      (** What the bus ANDs into every physical address. Read it per access;
          only {!set} writes it. *)
}

val create : board -> t
(** Closed. *)

val memory_bytes : board -> int
(** The size of the RAM a board has: 1 MB or 16 MB. *)

val enabled : t -> bool
(** Always [false] on a [One_megabyte] board. *)

val set : t -> bool -> unit
(** Opens or closes the gate. A [One_megabyte] board has no gate, so the
    call changes nothing and the line stays low. *)
