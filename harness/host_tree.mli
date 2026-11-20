(** A game folder on the host, as the files a machine mounts.

    Not part of the core: it reads the host disk. *)

val max_depth : int
(** How many folders may nest below the one given. DOS paths hold at most 64
    characters, so a folder below 32 levels cannot be named by a guest; the
    limit also stops a symbolic link that points back at its own parent. *)

val files : string -> ((string * string) list, string) result
(** [files dir] is every file under [dir], each as [(name, host path)].
    [name] is relative to [dir] with [/] between folders ([DATA/ITEMS.DAT]),
    the form [Dos_machine.mount_file] takes. The list is sorted by name at
    each level, so the same folder always mounts in the same order. A name
    that is neither a file nor a folder (a link to nothing) is left out.
    [Error] names the folder that nests deeper than {!max_depth}. *)
