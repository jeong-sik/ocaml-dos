(** Versioned binary primitives for {!Dos_snapshot}. Every value is an int,
    a bool or a byte string, read back with its range checked. *)

exception Invalid of string

type writer
type reader

val writer : unit -> writer
val contents : writer -> string
val reader : string -> reader
val remaining : reader -> int
val end_of_input : reader -> unit
val fail : string -> 'a

val put_int : writer -> int -> unit
val put_bool : writer -> bool -> unit
val put_string : writer -> string -> unit
val put_bytes : writer -> Bytes.t -> unit
val put_int_array : writer -> int array -> unit

val get_int : reader -> min:int -> max:int -> int
val get_bool : reader -> bool
val get_string : reader -> string
val get_bytes : reader -> Bytes.t
val fill_int_array : reader -> min:int -> max:int -> int array -> unit
val fill_bytes : reader -> Bytes.t -> unit
