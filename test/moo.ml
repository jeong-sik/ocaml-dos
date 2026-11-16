(* A reader for MOO, the chunked binary format SingleStepTests publishes
   its 286 and 386 suites in (github.com/dbalsom/moo, doc/moo_format_v1.md,
   version 1.1). Only what a state-based test needs: each test's name,
   initial and final registers and RAM, and the masks for what the CPU
   leaves undefined, and where an exception pushed the flags. Cycles,
   queue and hashes are skipped.

   Every chunk is a four-byte ASCII id, a little-endian uint32 length and
   that many bytes. A reader moves by the length, so an unknown or longer
   chunk is skipped, as the format asks. *)

type reg =
  | Ax | Bx | Cx | Dx | Cs | Ss | Ds | Es | Sp | Bp | Si | Di | Ip | Flags

(* REGS and RMSK name registers by bit, in this order. *)
let reg_bits = [| Ax; Bx; Cx; Dx; Cs; Ss; Ds; Es; Sp; Bp; Si; Di; Ip; Flags |]

type state = {
  regs : (reg * int) list;
  ram : (int * int) list;   (* address, byte *)
}

type test = {
  name : string;
  initial : state;
  final : state;
  masks : (reg * int) list; (* per register, the bits that are defined *)
  exception_taken : (int * int) option;
      (* EXCP: the vector the instruction raised, and the stack address of
         the flags it pushed. Those flags may hold undefined bits too. *)
}

type file = { tests : test list; file_masks : (reg * int) list }

exception Bad of string

let u8 s o = Char.code s.[o]
let u16 s o = u8 s o lor (u8 s (o + 1) lsl 8)
let u32 s o = u16 s o lor (u16 s (o + 2) lsl 16)

(* The chunks in [s] from [o] to [stop], as (id, payload offset, length). *)
let chunks s o stop =
  let rec go o acc =
    if o >= stop then List.rev acc
    else if o + 8 > stop then raise (Bad "chunk header past the end")
    else
      let id = String.sub s o 4 and len = u32 s (o + 4) in
      if o + 8 + len > stop then raise (Bad ("chunk " ^ id ^ " past the end"))
      else go (o + 8 + len) ((id, o + 8, len) :: acc)
  in
  go o []

let regs s o =
  let present = u16 s o in
  let rec go bit at acc =
    if bit = Array.length reg_bits then List.rev acc
    else if present land (1 lsl bit) <> 0 then
      go (bit + 1) (at + 2) ((reg_bits.(bit), u16 s at) :: acc)
    else go (bit + 1) at acc
  in
  go 0 (o + 2) []

let ram s o =
  let n = u32 s o in
  List.init n (fun i -> let e = o + 4 + (i * 5) in (u32 s e, u8 s (e + 4)))

let find id cs = List.find_opt (fun (i, _, _) -> i = id) cs

let state s o len =
  let cs = chunks s o (o + len) in
  { regs = (match find "REGS" cs with Some (_, p, _) -> regs s p | None -> []);
    ram = (match find "RAM " cs with Some (_, p, _) -> ram s p | None -> []) }

let masks_in s o len =
  match find "RMSK" (chunks s o (o + len)) with Some (_, p, _) -> regs s p | None -> []

let test s o len =
  (* The payload starts with the test's uint32 index. *)
  let cs = chunks s (o + 4) (o + len) in
  let need id = match find id cs with Some c -> c | None -> raise (Bad ("test without " ^ id)) in
  let _, np, _ = need "NAME" in
  let _, ip, il = need "INIT" and _, fp, fl = need "FINA" in
  { name = String.sub s (np + 4) (u32 s np);
    initial = state s ip il;
    final = state s fp fl;
    masks = masks_in s fp fl;
    exception_taken =
      (match find "EXCP" cs with Some (_, p, _) -> Some (u8 s p, u32 s (p + 1)) | None -> None) }

let parse s =
  match chunks s 0 (String.length s) with
  | ("MOO ", _, _) :: rest ->
    let file_masks =
      match find "RMSK" rest with Some (_, p, _) -> regs s p | None -> []
    in
    { tests = List.filter_map (fun (id, p, l) -> if id = "TEST" then Some (test s p l) else None) rest;
      file_masks }
  | _ -> raise (Bad "not a MOO file")
