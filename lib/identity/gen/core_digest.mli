(** Content digest of the core's sources, as {!Dos_core_identity} reports it. *)

val of_files : (string * string) list -> string
(** [of_files [(name, contents); ...]] is the lowercase hex MD5 of every
    file's name, length and bytes, taken in name order. The input order does
    not matter. *)

val of_paths : string list -> string
(** {!of_files} over the files at these paths, each named by its base name. *)
