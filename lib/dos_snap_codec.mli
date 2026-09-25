(** Versioned binary primitives for {!Dos_snapshot}. Every value is an int,
    a bool or a byte string.

    Every int is written and read against one {!range}, and the same range
    value is named on both sides. A value outside its range is refused where
    it is written ({!Unsaveable}), so [save] never writes a snapshot that
    [restore] would refuse, and refused where it is read ({!Invalid}), so a
    crafted snapshot never builds a machine. *)

exception Invalid of string
(** Reading: the bytes do not hold a machine. *)

exception Unsaveable of string
(** Writing: the machine holds a value its snapshot cannot carry. *)

type range = private { lo : int; hi : int }

val range : int -> int -> range
val byte : range
val word : range

val physical : range
(** A 20-bit real-mode address. *)

val count : range
(** Any non-negative int. *)

val any : range

type writer
type reader

val writer : unit -> writer
val contents : writer -> string
val reader : string -> reader
val remaining : reader -> int
val end_of_input : reader -> unit

val fail : string -> 'a
(** Raises {!Invalid}. *)

val put : writer -> what:string -> range -> int -> unit
val put_bool : writer -> bool -> unit
val put_string : writer -> string -> unit
val put_bytes : writer -> Bytes.t -> unit
val put_int_array : writer -> what:string -> range -> int array -> unit

val get : reader -> what:string -> range -> int
val get_bool : reader -> bool
val get_string : reader -> string
val get_bytes : reader -> Bytes.t
val fill_int_array : reader -> what:string -> range -> int array -> unit
(** The array's length is part of the machine's shape: a different length
    is {!Invalid}. *)

val fill_bytes : reader -> Bytes.t -> unit
