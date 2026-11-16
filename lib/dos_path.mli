(** A file or directory name on the machine's one drive, C:, parsed the way
    MS-DOS 5 reads a name passed to INT 21h.

    - [\] and [/] both separate. A leading separator starts at the root;
      otherwise the name starts at the current directory. A [C:] in front
      is allowed; any other drive is not.
    - [.] stays and [..] goes up. [..] at the root is an error.
    - A component is upper-cased and cut to 8.3 as DOS cuts it:
      [LONGFILENAME.TEXT] is [LONGFILE.TEX]. A second dot, an empty
      component, or a character DOS does not allow in a name (a double
      quote, any of [* + , / : ; < = > ? |], a bracket, a backslash, a
      space or a control character) is an error.

    A path is always absolute and in that canonical form, so two names
    for one file give the same path. *)

type t = private string list
(** Components from the root, each a canonical [NAME] or [NAME.EXT]. *)

type error =
  | Bad_drive   (** a drive other than C: *)
  | Bad_name    (** not a name DOS can hold *)
  | Above_root  (** [..] past the root *)

val root : t

val parse : cwd:t -> string -> (t, error) result

val parse_pattern : cwd:t -> string -> (t * string, error) result
(** A find pattern: the directory it searches, and its last component
    upper-cased with [?] and [*] kept. The directory part follows
    {!parse}. *)

val key : t -> string
(** The components joined by [\], without a leading one: [DATA\X.DAT];
    the root is [""]. The machine's file and directory tables use it. *)

val of_key : string -> t option
(** The inverse of {!key}. [None] for a string {!key} cannot give. *)

val parent : t -> t option
(** [None] for the root. *)

val base : t -> string option
(** The last component; [None] for the root. *)

val is_within : dir:t -> t -> bool
(** [dir] itself, or anything under it. *)
