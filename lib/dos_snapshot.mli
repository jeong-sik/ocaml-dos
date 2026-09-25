(** Whole-machine snapshot: the CPU, 1MB of RAM, the video planes and
    registers, the device ports, the DOS process and file state (mounted
    files, open handles with their positions, EXEC frames, memory blocks),
    EMS pages and the mouse — as bytes a fresh process can resume from.

    The format is explicit, not [Marshal]: the machine holds closures (the
    CPU's memory and port callbacks, the interrupt hook), and those are
    rebuilt by {!Dos_machine.create}. Only plain data goes into the bytes.

    Layout: [magic] ["OCAML-DOS-SNAPSHOT\000"], the format version (int64
    big-endian), the writing core's {!Dos_core_identity.source_digest} (32
    hex characters), the MD5 of the payload (16 bytes), then the payload.

    Only the format version decides whether a snapshot is read. It is bumped
    by hand when what {!save} writes changes shape or meaning; any other
    version is refused, and nothing reads an old one. The core digest is
    there to be shown — a core that changed a comment still reads the
    snapshot — and is never compared.

    The same machine always gives the same bytes: every table is written in
    key order. A snapshot of a restored machine equals the snapshot it was
    restored from. *)

type header = {
  format : int;
  core : string;  (** the digest of the core that wrote it; for display *)
}

type error =
  | Not_a_snapshot  (** no magic, or shorter than a header *)
  | Wrong_format of { saved : int; supported : int }
  | Corrupt of string
      (** The checksum, a value's range, or the consistency between values
          (EMS pages and mappings) does not hold. *)

val error_to_string : error -> string

val format_version : int
(** The one format {!restore} reads and {!save} writes. *)

type save_error =
  | Unsaveable of string
      (** The machine holds a value outside what a snapshot carries, or two
          fields that disagree (an EMS frame mapping a page that does not
          exist). [save] refuses exactly what {!restore} would refuse, so
          it never writes a snapshot that cannot be read back. *)

val save_error_to_string : save_error -> string

val save : Dos_machine.t -> (string, save_error) result
(** Does not advance or change the machine. *)

val header : string -> (header, error) result
(** Reads the header alone, without checking the payload. *)

val restore : string -> (Dos_machine.t, error) result
(** A fresh machine carrying the saved state, or an error and no machine. *)
