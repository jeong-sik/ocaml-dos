(* See cpu_flag.mli. *)

let parse = function
  | "286" -> Ok Cpu86.I80286
  | "386" -> Ok Cpu86.I80386
  | s -> Error (Printf.sprintf "bad --cpu %s (want 286 or 386)" s)
