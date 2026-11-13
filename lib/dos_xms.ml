(* XMS 3.0 driver. See dos_xms.mli. *)

module C = Dos_snap_codec

type error =
  | Not_implemented
  | A20_error
  | Hma_missing
  | Out_of_memory
  | Out_of_handles
  | Invalid_handle
  | Invalid_source_handle
  | Invalid_source_offset
  | Invalid_dest_handle
  | Invalid_dest_offset
  | Invalid_length
  | Invalid_overlap
  | Not_locked
  | Locked
  | Lock_overflow

(* XMS 3.0 specification, "ERROR CODE INDEX". *)
let error_code = function
  | Not_implemented -> 0x80
  | A20_error -> 0x82
  | Hma_missing -> 0x90
  | Out_of_memory -> 0xA0
  | Out_of_handles -> 0xA1
  | Invalid_handle -> 0xA2
  | Invalid_source_handle -> 0xA3
  | Invalid_source_offset -> 0xA4
  | Invalid_dest_handle -> 0xA5
  | Invalid_dest_offset -> 0xA6
  | Invalid_length -> 0xA7
  | Invalid_overlap -> 0xA8
  | Not_locked -> 0xAA
  | Locked -> 0xAB
  | Lock_overflow -> 0xAC

let version = 0x0300
let revision = 0x0301
let capacity_kb = 15 * 1024
let handle_count = 32
let extended_base = 0x100000
let conventional_limit = 0x100000
let kb_bytes = 1024
let max_locks = 0xFF

type block = { base_kb : int; size_kb : int; locks : int; data : Bytes.t }

(* Slot i holds handle i + 1. *)
type t = { slots : block option array }

let create () = { slots = Array.make handle_count None }

let block t h =
  if h >= 1 && h <= handle_count then t.slots.(h - 1) else None

let set t h b = t.slots.(h - 1) <- b

(* The holes between blocks, as (start, length) in KB, lowest first.
   [except] counts that handle's own space as free. *)
let holes t ~except =
  let used =
    Array.to_list t.slots
    |> List.mapi (fun i b -> (i + 1, b))
    |> List.filter_map (fun (h, b) ->
           match b with
           | Some b when h <> except && b.size_kb > 0 -> Some (b.base_kb, b.size_kb)
           | Some _ | None -> None)
    |> List.sort compare
  in
  let rec walk at = function
    | [] -> if at < capacity_kb then [ (at, capacity_kb - at) ] else []
    | (base, size) :: rest ->
      let tail = walk (base + size) rest in
      if base > at then (at, base - at) :: tail else tail
  in
  walk 0 used

let first_fit t ~except kb =
  match List.find_opt (fun (_, len) -> len >= kb) (holes t ~except) with
  | Some (start, _) -> Some start
  | None -> if kb = 0 then Some 0 else None

let no_handle = 0

type free = { largest_kb : int; total_kb : int }

let query_free t =
  let hs = holes t ~except:no_handle in
  let total_kb = List.fold_left (fun acc (_, len) -> acc + len) 0 hs in
  let largest_kb = List.fold_left (fun acc (_, len) -> max acc len) 0 hs in
  if total_kb = 0 then Error Out_of_memory else Ok { largest_kb; total_kb }

let fresh kb = Bytes.make (kb * kb_bytes) '\000'

let allocate t ~kb =
  let rec free_slot i =
    if i >= handle_count then None
    else match t.slots.(i) with None -> Some (i + 1) | Some _ -> free_slot (i + 1)
  in
  match free_slot 0 with
  | None -> Error Out_of_handles
  | Some h ->
    (match first_fit t ~except:no_handle kb with
     | None -> Error Out_of_memory
     | Some base_kb ->
       set t h (Some { base_kb; size_kb = kb; locks = 0; data = fresh kb });
       Ok h)

let release t h =
  match block t h with
  | None -> Error Invalid_handle
  | Some b when b.locks > 0 -> Error Locked
  | Some _ -> set t h None; Ok ()

let address b = extended_base + (b.base_kb * kb_bytes)

let lock t h =
  match block t h with
  | None -> Error Invalid_handle
  | Some b when b.locks >= max_locks -> Error Lock_overflow
  | Some b -> set t h (Some { b with locks = b.locks + 1 }); Ok (address b)

let unlock t h =
  match block t h with
  | None -> Error Invalid_handle
  | Some b when b.locks = 0 -> Error Not_locked
  | Some b -> set t h (Some { b with locks = b.locks - 1 }); Ok ()

type info = { lock_count : int; free_handles : int; size_kb : int }

let info t h =
  match block t h with
  | None -> Error Invalid_handle
  | Some b ->
    let free_handles =
      Array.fold_left (fun n s -> match s with None -> n + 1 | Some _ -> n) 0 t.slots
    in
    Ok { lock_count = b.locks; free_handles; size_kb = b.size_kb }

let resize t h ~kb =
  match block t h with
  | None -> Error Invalid_handle
  | Some b when b.locks > 0 -> Error Locked
  | Some b ->
    let in_place =
      List.exists
        (fun (start, len) -> start <= b.base_kb && b.base_kb + kb <= start + len)
        (holes t ~except:h)
    in
    (match if in_place then Some b.base_kb else first_fit t ~except:h kb with
     | None -> Error Out_of_memory
     | Some base_kb ->
       let data = fresh kb in
       Bytes.blit b.data 0 data 0 (min (Bytes.length b.data) (Bytes.length data));
       set t h (Some { b with base_kb; size_kb = kb; data });
       Ok ())

type endpoint = Conventional of int | Block of { handle : int; offset : int }
type move = { length : int; source : endpoint; dest : endpoint }

(* Where one end of a move lies: its physical address and how to touch
   byte i of it. *)
type side = { linear : int; get : int -> int; put : int -> int -> unit }

let resolve t ~read ~write ~length ~bad_handle ~bad_offset = function
  | Conventional a ->
    if a + length > conventional_limit then Error Invalid_length
    else Ok { linear = a; get = (fun i -> read (a + i)); put = (fun i v -> write (a + i) v) }
  | Block { handle; offset } ->
    (match block t handle with
     | None -> Error bad_handle
     | Some b ->
       let size = b.size_kb * kb_bytes in
       if offset >= size then Error bad_offset
       else if length > size - offset then Error Invalid_length
       else
         Ok { linear = address b + offset;
              get = (fun i -> Char.code (Bytes.get b.data (offset + i)));
              put = (fun i v -> Bytes.set b.data (offset + i) (Char.chr (v land 0xff))) })

let ( let* ) = Result.bind

let move t ~read ~write m =
  let length = m.length in
  let* () = if length land 1 = 1 then Error Invalid_length else Ok () in
  let* src =
    resolve t ~read ~write ~length ~bad_handle:Invalid_source_handle
      ~bad_offset:Invalid_source_offset m.source
  in
  let* dst =
    resolve t ~read ~write ~length ~bad_handle:Invalid_dest_handle
      ~bad_offset:Invalid_dest_offset m.dest
  in
  if src.linear < dst.linear && src.linear + length > dst.linear then Error Invalid_overlap
  else begin
    for i = 0 to length - 1 do dst.put i (src.get i) done;
    Ok ()
  end

(* ---------- the control function ---------- *)

(* Register numbers in the 8086 encoding (Cpu86). *)
let r_ax = 0
let r_dx = 2
let r_bx = 3
let r_si = 6
let r_al = 0
let r_bl = 3
let r_ah = 4
let r_bh = 7
let s_es = 0
let s_ds = 3

let succeed cpu = Cpu86.set_reg16 cpu r_ax 1; Cpu86.set_reg8 cpu r_bl 0
let fail cpu e = Cpu86.set_reg16 cpu r_ax 0; Cpu86.set_reg8 cpu r_bl (error_code e)

let answer cpu = function Ok () -> succeed cpu | Error e -> fail cpu e

let word_mask = 0xFFFF

(* The extended memory move structure (XMS 3.0, function 0Bh): length
   dword, source handle word, source offset dword, destination handle
   word, destination offset dword. With handle 0 the offset dword is
   offset (low word) then segment (high word). *)
let move_length = 0
let move_source_handle = 4
let move_source_offset = 6
let move_dest_handle = 10
let move_dest_offset = 12

let read_move ~read at =
  let w a = read a lor (read (a + 1) lsl 8) in
  let d a = w a lor (w (a + 2) lsl 16) in
  let endpoint ~handle ~offset =
    let h = w (at + handle) in
    if h = 0 then Conventional ((w (at + offset + 2) lsl 4) + w (at + offset))
    else Block { handle = h; offset = d (at + offset) }
  in
  { length = d (at + move_length);
    source = endpoint ~handle:move_source_handle ~offset:move_source_offset;
    dest = endpoint ~handle:move_dest_handle ~offset:move_dest_offset }

let control t cpu ~read ~write =
  let dx = Cpu86.reg16 cpu r_dx in
  match Cpu86.reg8 cpu r_ah with
  | 0x00 ->
    Cpu86.set_reg16 cpu r_ax version;
    Cpu86.set_reg16 cpu r_bx revision;
    Cpu86.set_reg16 cpu r_dx 0
  | 0x01 | 0x02 -> fail cpu Hma_missing
  | 0x03 | 0x05 | 0x06 -> fail cpu A20_error
  | 0x04 -> succeed cpu
  | 0x07 -> Cpu86.set_reg16 cpu r_ax 0; Cpu86.set_reg8 cpu r_bl 0
  | 0x08 ->
    (match query_free t with
     | Ok { largest_kb; total_kb } ->
       Cpu86.set_reg16 cpu r_ax largest_kb;
       Cpu86.set_reg16 cpu r_dx total_kb;
       Cpu86.set_reg8 cpu r_bl 0
     | Error e ->
       Cpu86.set_reg16 cpu r_ax 0;
       Cpu86.set_reg16 cpu r_dx 0;
       Cpu86.set_reg8 cpu r_bl (error_code e))
  | 0x09 ->
    (match allocate t ~kb:dx with
     | Ok h -> succeed cpu; Cpu86.set_reg16 cpu r_dx h
     | Error e -> fail cpu e; Cpu86.set_reg16 cpu r_dx 0)
  | 0x0A -> answer cpu (release t dx)
  | 0x0B ->
    let at = (Cpu86.seg cpu s_ds lsl 4) + Cpu86.reg16 cpu r_si in
    (match move t ~read ~write (read_move ~read at) with
     | Ok () -> Cpu86.set_reg16 cpu r_ax 1
     | Error e -> fail cpu e)
  | 0x0C ->
    (match lock t dx with
     | Ok a ->
       succeed cpu;
       Cpu86.set_reg16 cpu r_bx (a land word_mask);
       Cpu86.set_reg16 cpu r_dx (a lsr 16)
     | Error e -> fail cpu e)
  | 0x0D -> answer cpu (unlock t dx)
  | 0x0E ->
    (match info t dx with
     | Ok { lock_count; free_handles; size_kb } ->
       Cpu86.set_reg16 cpu r_ax 1;
       Cpu86.set_reg8 cpu r_bh lock_count;
       Cpu86.set_reg8 cpu r_bl free_handles;
       Cpu86.set_reg16 cpu r_dx size_kb
     | Error e -> fail cpu e)
  | 0x0F -> answer cpu (resize t dx ~kb:(Cpu86.reg16 cpu r_bx))
  | _ -> fail cpu Not_implemented

let installed = 0x80
let multiplex_install_check = 0x4300
let multiplex_entry_point = 0x4310

let multiplex cpu ~entry:(segment, offset) =
  match Cpu86.reg16 cpu r_ax with
  | ax when ax = multiplex_install_check -> Cpu86.set_reg8 cpu r_al installed
  | ax when ax = multiplex_entry_point ->
    Cpu86.set_seg cpu s_es segment;
    Cpu86.set_reg16 cpu r_bx offset
  | _ -> ()

(* ---------- snapshot ---------- *)

let kb_range = C.range 0 capacity_kb
let lock_range = C.range 0 max_locks

let write_state w t =
  Array.iter
    (function
      | None -> C.put_bool w false
      | Some (b : block) ->
        C.put_bool w true;
        C.put w ~what:"XMS block base" kb_range b.base_kb;
        C.put w ~what:"XMS block size" kb_range b.size_kb;
        C.put w ~what:"XMS lock count" lock_range b.locks;
        C.put_bytes w b.data)
    t.slots

let read_state r t =
  Array.iteri
    (fun i _ ->
      t.slots.(i) <-
        (if C.get_bool r then begin
           let base_kb = C.get r ~what:"XMS block base" kb_range in
           let size_kb = C.get r ~what:"XMS block size" kb_range in
           let locks = C.get r ~what:"XMS lock count" lock_range in
           let data = C.get_bytes r in
           if base_kb + size_kb > capacity_kb then C.fail "XMS block runs past the end";
           if Bytes.length data <> size_kb * kb_bytes then C.fail "XMS block bytes";
           Some { base_kb; size_kb; locks; data }
         end
         else None))
    t.slots;
  let spans =
    Array.to_list t.slots
    |> List.filter_map (function
         | Some (b : block) when b.size_kb > 0 -> Some (b.base_kb, b.size_kb)
         | Some _ | None -> None)
    |> List.sort compare
  in
  let rec disjoint = function
    | (b1, s1) :: ((b2, _) :: _ as rest) -> b1 + s1 <= b2 && disjoint rest
    | [ _ ] | [] -> true
  in
  if not (disjoint spans) then C.fail "XMS blocks overlap"
