(** The value of a [--cpu] flag, shared by the command-line tools. *)

val parse : string -> (Cpu86.model, string) result
(** Exactly ["286"] or ["386"]. Any other text, including ["386 "] and
    ["I80386"], is an [Error] that names the flag and the two values. *)
