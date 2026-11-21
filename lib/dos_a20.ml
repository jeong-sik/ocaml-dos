(* The memory board's address lines and the A20 gate. See dos_a20.mli. *)

type board = One_megabyte | Sixteen_megabytes

type t = { board : board; mutable enabled : bool; mutable mask : int }

let one_megabyte = 0x100000
let sixteen_megabytes = 0x1000000
let a20_bit = 0x100000

let memory_bytes = function
  | One_megabyte -> one_megabyte
  | Sixteen_megabytes -> sixteen_megabytes

(* The bytes the bus keeps of a physical address. *)
let mask_of board ~enabled =
  match board with
  | One_megabyte -> one_megabyte - 1
  | Sixteen_megabytes ->
    if enabled then sixteen_megabytes - 1 else (sixteen_megabytes - 1) land lnot a20_bit

let create board = { board; enabled = false; mask = mask_of board ~enabled:false }
let enabled t = t.enabled

let set t enabled =
  match t.board with
  | One_megabyte -> ()
  | Sixteen_megabytes ->
    t.enabled <- enabled;
    t.mask <- mask_of t.board ~enabled
