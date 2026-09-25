(* Versioned binary primitives for a machine snapshot. Nothing here decodes
   a closure or a runtime value: every field is an int, a bool or bytes, read
   back with its range checked. Malformed input raises [Invalid] and never
   publishes a half-built machine. *)

exception Invalid of string

let fail message = raise (Invalid message)

type writer = Buffer.t
type reader = { input : string; mutable pos : int }

let writer () = Buffer.create 65536
let contents = Buffer.contents

let put_int w n =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int n);
  Buffer.add_bytes w b

let put_bool w b = Buffer.add_char w (if b then '\001' else '\000')
let put_string w s = put_int w (String.length s); Buffer.add_string w s
let put_bytes w b = put_string w (Bytes.unsafe_to_string b)
let put_int_array w a = put_int w (Array.length a); Array.iter (put_int w) a

let reader input = { input; pos = 0 }
let remaining r = String.length r.input - r.pos

let take r n =
  if n < 0 || n > remaining r then fail "truncated snapshot";
  let p = r.pos in
  r.pos <- p + n;
  p

let get_int r ~min ~max =
  let n = String.get_int64_be r.input (take r 8) in
  if n < Int64.of_int min || n > Int64.of_int max then
    fail "snapshot integer out of range";
  Int64.to_int n

let get_bool r =
  match r.input.[take r 1] with
  | '\000' -> false
  | '\001' -> true
  | _ -> fail "invalid snapshot boolean"

let get_string r =
  let n = get_int r ~min:0 ~max:(remaining r) in
  String.sub r.input (take r n) n

let get_bytes r = Bytes.of_string (get_string r)

(* Fixed-size arrays: the length is part of the machine's shape, so a
   different length is a different machine, not a value to adapt. *)
let fill_int_array r ~min ~max a =
  let n = get_int r ~min:0 ~max:max_int in
  if n <> Array.length a then fail "snapshot array length differs";
  Array.iteri (fun i _ -> a.(i) <- get_int r ~min ~max) a

let fill_bytes r dst =
  let s = get_string r in
  if String.length s <> Bytes.length dst then fail "snapshot buffer length differs";
  Bytes.blit_string s 0 dst 0 (String.length s)

let end_of_input r = if remaining r <> 0 then fail "trailing data in snapshot"
