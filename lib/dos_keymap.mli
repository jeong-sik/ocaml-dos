(** Which key, and which modifier held with it, a BIOS keystroke word
    stands for — the inverse of the table a PC/AT BIOS keeps to turn a
    scan code into a word.

    A word such as 1E41h ('A') is the A key with shift; 2D00h is X with
    alt; 7800h is 1 with alt, although its high byte is not a key. The
    table is SeaBIOS's copy of the AT layout (src/kbd.c). Where one key
    gives the same word with several modifiers, the word stands for the
    key alone, then with shift, then ctrl, then alt. *)

type modifier =
  | Shift  (** left shift, make 2Ah *)
  | Ctrl   (** left ctrl, make 1Dh *)
  | Alt    (** left alt, make 38h *)

type edge = Down | Up

type key = {
  scan : int;               (** the key's make code *)
  held : modifier option;   (** the modifier down while it is *)
}

val of_word : int -> key option
(** [None] for a word no key in the table gives, such as the gray
    cursor keys' E0h words. *)

val make_code : modifier -> int
(** The break code is this plus 80h. *)

val modifier_of_code : int -> (modifier * edge) option
(** What a byte from port 0x60 says about a modifier: [Some] for the make
    or break code of left shift, ctrl or alt. *)
