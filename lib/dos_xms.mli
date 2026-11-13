(** XMS 3.0 driver (the HIMEM.SYS role), without the HMA or the A20 gate.

    A program asks INT 2Fh AX=4300h whether a driver is there (AL=80h) and
    AX=4310h for its control function (ES:BX). It far-calls that function
    with AH = the function number; AX=1 is success, AX=0 failure with the
    error code in BL (XMS 3.0 specification, Microsoft 1991).

    {b Extended memory.} [capacity_kb] (15 MB, the extended memory of
    DOSBox's default 16 MB machine) starts at physical address 100000h.
    Blocks are placed first fit in kilobyte units, so freeing a block in
    the middle leaves a hole and the largest free block can be smaller
    than the total. A lock returns the block's physical address. The CPU
    never reaches these addresses: only moves (0Bh) read and write them.
    New memory reads 0.

    {b Handles.} [handle_count] (32, the MS-DOS 6.22 default that DOSBox
    names), numbered 1 to 32. A block may be 0 KB.

    {b Moves.} Handle 0 means a real-mode segment:offset in the first
    megabyte; a range that runs past 100000h is refused with A7h, because
    without an HMA no memory lies there. An odd length is A7h (the
    specification and HimemX; DOSBox answers A9h). An offset past the end
    of a block is A4h or A6h, and a length that runs past it A7h (DOSBox).
    When the source starts below the destination and the two overlap, the
    move is refused with A8h, as HimemX does; every other move copies
    byte by byte from the start, through the [read] and [write] the caller
    gives for the first megabyte.

    {b Resizing.} A block keeps its place when the new size fits there
    with its own space counted as free; otherwise it moves to the first
    hole that fits. Its bytes survive up to the smaller size. A locked
    block does not resize (ABh).

    {b HMA and A20.} There is no HMA (00h reports DX=0; 01h and 02h answer
    90h) and the A20 line stays off: 03h and 05h answer 82h, 06h answers
    82h as HIMEM does with no local enable outstanding, 04h succeeds and
    07h reports 0.

    {b Outside the model.} Upper memory blocks (10h-12h) and the 32-bit
    functions (88h-8Fh, which a driver on a 286 refuses) answer 80h, as
    does every other function number. INT 15h AH=87h/88h are not hooked. *)

type error =
  | Not_implemented        (** 80h *)
  | A20_error              (** 82h *)
  | Hma_missing            (** 90h *)
  | Out_of_memory          (** A0h *)
  | Out_of_handles         (** A1h *)
  | Invalid_handle         (** A2h *)
  | Invalid_source_handle  (** A3h *)
  | Invalid_source_offset  (** A4h *)
  | Invalid_dest_handle    (** A5h *)
  | Invalid_dest_offset    (** A6h *)
  | Invalid_length         (** A7h *)
  | Invalid_overlap        (** A8h *)
  | Not_locked             (** AAh *)
  | Locked                 (** ABh *)
  | Lock_overflow          (** ACh *)

val error_code : error -> int

val version : int
(** 0300h: XMS 3.00, as DOSBox reports. *)

val revision : int
(** 0301h, the driver revision DOSBox reports. *)

val capacity_kb : int
val handle_count : int

val extended_base : int
(** 100000h, where the first block can start. *)

val conventional_limit : int
(** 100000h: a handle-0 range must end at or below it. *)

type t

val create : unit -> t

type free = { largest_kb : int; total_kb : int }

val query_free : t -> (free, error) result
(** [Out_of_memory] when nothing is free. *)

val allocate : t -> kb:int -> (int, error) result
(** The new handle. A free handle is looked for before free memory. *)

val release : t -> int -> (unit, error) result
val lock : t -> int -> (int, error) result
(** The block's physical address. A block locks at most 255 times. *)

val unlock : t -> int -> (unit, error) result

type info = { lock_count : int; free_handles : int; size_kb : int }

val info : t -> int -> (info, error) result
val resize : t -> int -> kb:int -> (unit, error) result

type endpoint =
  | Conventional of int  (** a physical address below 100000h *)
  | Block of { handle : int; offset : int }

type move = { length : int; source : endpoint; dest : endpoint }

val move :
  t -> read:(int -> int) -> write:(int -> int -> unit) -> move ->
  (unit, error) result
(** Checks the length's parity, then the source, then the destination,
    then the overlap; copies only when all pass. *)

val control :
  t -> Cpu86.t -> read:(int -> int) -> write:(int -> int -> unit) -> unit
(** The control function: reads AH and the function's registers, answers
    in AX, BX and DX. Success leaves BL = 0, except 0Bh (BL untouched, as
    in MS HIMEM per HimemX) and 0Eh (BL = free handles). The move
    structure at DS:SI is read through [read]. *)

val multiplex : Cpu86.t -> entry:int * int -> unit
(** INT 2Fh: AX=4300h sets AL=80h, AX=4310h sets ES:BX to [entry]
    (segment, offset). Any other AX leaves every register as it was. *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
(** Refuses blocks that overlap, run past [capacity_kb], or whose bytes
    are not their size. *)
