(* Versioned binary primitives for a machine snapshot. See
   dos_snap_codec.mli. Nothing here decodes a closure or a runtime value. *)

exception Invalid of string
exception Unsaveable of string

let fail message = raise (Invalid message)

type range = { lo : int; hi : int }

let range lo hi =
  if lo > hi then invalid_arg "Dos_snap_codec.range: lo > hi";
  { lo; hi }

let byte = { lo = 0; hi = 0xff }
let word = { lo = 0; hi = 0xffff }
let physical = { lo = 0; hi = 0xfffff }
let count = { lo = 0; hi = max_int }
let any = { lo = min_int; hi = max_int }

let within { lo; hi } n = n >= lo && n <= hi

type writer = Buffer.t
type reader = { input : string; mutable pos : int }

let writer () = Buffer.create 65536
let contents = Buffer.contents

let put w ~what r n =
  if not (within r n) then
    raise (Unsaveable (Printf.sprintf "%s = %d is outside %d..%d" what n r.lo r.hi));
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int n);
  Buffer.add_bytes w b

let put_bool w b = Buffer.add_char w (if b then '\001' else '\000')
let put_string w s = put w ~what:"string length" count (String.length s); Buffer.add_string w s
let put_bytes w b = put_string w (Bytes.unsafe_to_string b)

let put_int_array w ~what r a =
  put w ~what:(what ^ " length") count (Array.length a);
  Array.iter (put w ~what r) a

let reader input = { input; pos = 0 }
let remaining r = String.length r.input - r.pos

let take r n =
  if n < 0 || n > remaining r then fail "truncated snapshot";
  let p = r.pos in
  r.pos <- p + n;
  p

(* An int64 that does not fit an OCaml int is out of every range: compare
   in Int64 before converting. *)
let get r ~what { lo; hi } =
  let n = String.get_int64_be r.input (take r 8) in
  if Int64.compare n (Int64.of_int lo) < 0 || Int64.compare n (Int64.of_int hi) > 0 then
    fail (Printf.sprintf "%s = %Ld is outside %d..%d" what n lo hi);
  Int64.to_int n

let get_bool r =
  match r.input.[take r 1] with
  | '\000' -> false
  | '\001' -> true
  | _ -> fail "invalid snapshot boolean"

let get_string r =
  let n = get r ~what:"string length" { lo = 0; hi = remaining r } in
  String.sub r.input (take r n) n

let get_bytes r = Bytes.of_string (get_string r)

let fill_int_array r ~what rng a =
  let n = get r ~what:(what ^ " length") count in
  if n <> Array.length a then fail (what ^ ": array length differs");
  Array.iteri (fun i _ -> a.(i) <- get r ~what rng) a

let fill_bytes r dst =
  let s = get_string r in
  if String.length s <> Bytes.length dst then fail "snapshot buffer length differs";
  Bytes.blit_string s 0 dst 0 (String.length s)

let end_of_input r = if remaining r <> 0 then fail "trailing data in snapshot"
