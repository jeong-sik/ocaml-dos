(* Whole-machine snapshot. See dos_snapshot.mli. *)

open Dos_state
module C = Dos_snap_codec

let magic = "OCAML-DOS-SNAPSHOT\000"

(* Bump by hand whenever what [save] writes changes shape or meaning. A
   snapshot of any other version is refused; nothing reads an old one. *)
let format_version = 1

let digest_hex_len = 32
let checksum_len = 16
let version_len = 8
let header_len = String.length magic + version_len + digest_hex_len + checksum_len

type header = { format : int; core : string }

type error =
  | Not_a_snapshot
  | Wrong_format of { saved : int; supported : int }
  | Corrupt of string

let error_to_string = function
  | Not_a_snapshot -> "not an ocaml-dos snapshot"
  | Wrong_format { saved; supported } ->
    Printf.sprintf "snapshot format %d; this core reads only format %d" saved supported
  | Corrupt message -> "corrupt snapshot: " ^ message

(* ---------- writing ---------- *)

let sorted_bindings tbl =
  List.sort (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

let put_pairs w l =
  C.put_int w (List.length l);
  List.iter (fun (a, b) -> C.put_int w a; C.put_int w b) l

let put_list w put l = C.put_int w (List.length l); List.iter put l

(* Handle records are shared: dup (AH=45h/46h) files one record under two
   numbers, and an EXEC frame keeps the parent's records, so a read or write
   through either number moves the one position. The snapshot keeps the
   sharing: each record goes into a pool once, by physical identity, and
   every table names a pool index. Pool order is first appearance in the
   sorted live table, then in the frames, so the bytes are deterministic. *)
let handle_pool t =
  let pool = ref [] and count = ref 0 in
  let index h =
    match List.find_opt (fun (x, _) -> x == h) !pool with
    | Some (_, i) -> i
    | None ->
      let i = !count in
      pool := (h, i) :: !pool;
      incr count;
      i
  in
  let live = List.map (fun (n, h) -> (n, index h)) (sorted_bindings t.handles) in
  let frames =
    List.map (fun f -> List.map (fun (n, h) -> (n, index h)) f.parent_handles)
      t.exec_frames
  in
  (List.rev_map fst !pool, live, frames)

let put_handle w h =
  let { hname; data; pos } = h in
  C.put_string w hname; C.put_bytes w data; C.put_int w pos

let put_frame w f handles =
  let { parent; parent_psp; parent_dta; parent_free_base; parent_free_top;
        parent_blocks; parent_handles = _ (* as pool indexes: [handles] *) } = f
  in
  let { snap_regs; snap_segs; snap_ip; snap_flags } = parent in
  C.put_int_array w snap_regs;
  C.put_int_array w snap_segs;
  C.put_int w snap_ip;
  C.put_int w snap_flags;
  C.put_int w parent_psp;
  C.put_int w parent_dta;
  C.put_int w parent_free_base;
  C.put_int w parent_free_top;
  put_pairs w parent_blocks;
  put_pairs w handles

(* Every [Dos_state.t] field is named: a field added to the machine fails the
   build here until the snapshot carries it or says why not. [read_payload]
   reads in exactly this order. *)
let write_payload w t =
  let { mem; cpu; ports; exited; exit_code; video; host_files;
        handles = _; next_handle; fcbs; dta; psp_seg; kbd_wait;
        ext_scan_pending; kbd_requests; last_tick; pending_irq0; free_base;
        free_top; blocks; find_queue; stubs;
        exec_frames = _ (* with [handles], through [handle_pool] *);
        last_child_code; epoch_year; epoch_month; epoch_day; epoch_hour;
        epoch_min; epoch_sec; ems_next_handle; ems_pages; ems_mapped;
        mouse } = t
  in
  let { Cpu86.saved_regs; saved_segs; saved_ip; saved_flags; saved_halted;
        saved_cycles } = Cpu86.save_state cpu
  in
  C.put_int_array w saved_regs;
  C.put_int_array w saved_segs;
  C.put_int w saved_ip;
  C.put_int w saved_flags;
  C.put_bool w saved_halted;
  C.put_int w saved_cycles;
  C.put_bytes w mem;
  Dos_video.write_state w video;
  Dos_ports.write_state w ports;
  C.put_bool w exited;
  C.put_int w exit_code;
  put_list w (fun (k, v) -> C.put_string w k; C.put_bytes w v) (sorted_bindings host_files);
  let pool, live, frames = handle_pool t in
  put_list w (put_handle w) pool;
  put_pairs w live;
  C.put_int w next_handle;
  put_list w (fun (k, (b, n)) -> C.put_int w k; C.put_bytes w b; C.put_int w n)
    (sorted_bindings fcbs);
  C.put_int w dta;
  C.put_int w psp_seg;
  C.put_bool w kbd_wait;
  C.put_int w ext_scan_pending;
  C.put_int w kbd_requests;
  C.put_int w last_tick;
  C.put_bool w pending_irq0;
  C.put_int w free_base;
  C.put_int w free_top;
  put_pairs w blocks;
  put_list w (C.put_string w) find_queue;
  put_pairs w stubs;
  C.put_int w (List.length frames);
  List.iter2 (put_frame w) t.exec_frames frames;
  C.put_int w last_child_code;
  List.iter (C.put_int w)
    [ epoch_year; epoch_month; epoch_day; epoch_hour; epoch_min; epoch_sec ];
  C.put_int w ems_next_handle;
  put_list w
    (fun (k, pages) -> C.put_int w k; put_list w (C.put_bytes w) (Array.to_list pages))
    (sorted_bindings ems_pages);
  put_pairs w (Array.to_list ems_mapped);
  let { mouse_present; mouse_x; mouse_y; mouse_buttons; mouse_visible;
        mouse_dx; mouse_dy } = mouse
  in
  C.put_bool w mouse_present;
  C.put_int w mouse_x;
  C.put_int w mouse_y;
  C.put_int w mouse_buttons;
  C.put_bool w mouse_visible;
  C.put_int w mouse_dx;
  C.put_int w mouse_dy

let save t =
  let w = C.writer () in
  write_payload w t;
  let payload = C.contents w in
  let version = Bytes.create version_len in
  Bytes.set_int64_be version 0 (Int64.of_int format_version);
  String.concat ""
    [ magic; Bytes.to_string version; Dos_core_identity.source_digest;
      Digest.string payload; payload ]

(* ---------- reading ---------- *)

let byte r = C.get_int r ~min:0 ~max:0xff
let word r = C.get_int r ~min:0 ~max:0xffff
let physical r = C.get_int r ~min:0 ~max:0xfffff
let count r = C.get_int r ~min:0 ~max:max_int
let any r = C.get_int r ~min:min_int ~max:max_int

(* Every element takes at least one byte, so a count above what is left is
   a lie the reader refuses before allocating for it. *)
let get_list r get = List.init (C.get_int r ~min:0 ~max:(C.remaining r)) (fun _ -> get ())

let get_pairs r ~a ~b = get_list r (fun () -> let x = a r in let y = b r in (x, y))

let fill_table tbl bindings =
  Hashtbl.reset tbl;
  List.iter
    (fun (k, v) ->
      if Hashtbl.mem tbl k then C.fail "a key appears twice";
      Hashtbl.replace tbl k v)
    bindings

let get_int_array r ~len ~max =
  let a = Array.make len 0 in
  C.fill_int_array r ~min:0 ~max a;
  a

let read_payload r =
  (* A fresh machine brings the closures (memory, ports, the interrupt hook)
     and the containers; everything else is overwritten from the bytes. *)
  let t = Dos_machine.create () in
  let saved_regs = get_int_array r ~len:8 ~max:0xffff in
  let saved_segs = get_int_array r ~len:4 ~max:0xffff in
  let saved_ip = word r in
  let saved_flags = word r in
  let saved_halted = C.get_bool r in
  let saved_cycles = count r in
  Cpu86.load_state t.cpu
    { Cpu86.saved_regs; saved_segs; saved_ip; saved_flags; saved_halted; saved_cycles };
  C.fill_bytes r t.mem;
  Dos_video.read_state r t.video;
  Dos_ports.read_state r t.ports;
  t.exited <- C.get_bool r;
  t.exit_code <- byte r;
  fill_table t.host_files
    (get_list r (fun () -> let k = C.get_string r in (k, C.get_bytes r)));
  let pool =
    Array.of_list
      (get_list r (fun () ->
           let hname = C.get_string r in
           let data = C.get_bytes r in
           { hname; data; pos = count r }))
  in
  let resolve = List.map (fun (n, i) ->
      if i >= Array.length pool then C.fail "a handle names no record" else (n, pool.(i)))
  in
  fill_table t.handles (resolve (get_pairs r ~a:word ~b:count));
  t.next_handle <- word r;
  fill_table t.fcbs
    (get_list r (fun () ->
         let k = physical r in
         let b = C.get_bytes r in
         (k, (b, count r))));
  t.dta <- physical r;
  t.psp_seg <- word r;
  t.kbd_wait <- C.get_bool r;
  t.ext_scan_pending <- byte r;
  t.kbd_requests <- count r;
  t.last_tick <- count r;
  t.pending_irq0 <- C.get_bool r;
  t.free_base <- word r;
  t.free_top <- word r;
  t.blocks <- get_pairs r ~a:word ~b:word;
  t.find_queue <- get_list r (fun () -> C.get_string r);
  t.stubs <- get_pairs r ~a:byte ~b:word;
  t.exec_frames <-
    get_list r (fun () ->
        let snap_regs = get_int_array r ~len:8 ~max:0xffff in
        let snap_segs = get_int_array r ~len:4 ~max:0xffff in
        let snap_ip = word r in
        let snap_flags = word r in
        let parent_psp = word r in
        let parent_dta = physical r in
        let parent_free_base = word r in
        let parent_free_top = word r in
        let parent_blocks = get_pairs r ~a:word ~b:word in
        let parent_handles = resolve (get_pairs r ~a:word ~b:count) in
        { parent = { snap_regs; snap_segs; snap_ip; snap_flags };
          parent_psp; parent_dta; parent_free_base; parent_free_top;
          parent_blocks; parent_handles });
  t.last_child_code <- byte r;
  (* [Dos_machine.set_clock] takes any ints; a snapshot must not refuse one. *)
  t.epoch_year <- any r;
  t.epoch_month <- any r;
  t.epoch_day <- any r;
  t.epoch_hour <- any r;
  t.epoch_min <- any r;
  t.epoch_sec <- any r;
  t.ems_next_handle <- word r;
  fill_table t.ems_pages
    (get_list r (fun () ->
         let k = word r in
         (k, Array.of_list (get_list r (fun () -> C.get_bytes r)))));
  let mapped = get_pairs r ~a:word ~b:word in
  if List.length mapped <> Array.length t.ems_mapped then C.fail "EMS frame size differs";
  List.iteri (fun i p -> t.ems_mapped.(i) <- p) mapped;
  let m = t.mouse in
  m.mouse_present <- C.get_bool r;
  m.mouse_x <- any r;
  m.mouse_y <- any r;
  m.mouse_buttons <- any r;
  m.mouse_visible <- C.get_bool r;
  m.mouse_dx <- any r;
  m.mouse_dy <- any r;
  C.end_of_input r;
  t

let header s =
  let ml = String.length magic in
  if String.length s < header_len || not (String.equal (String.sub s 0 ml) magic) then
    Error Not_a_snapshot
  else
    let format = Int64.to_int (String.get_int64_be s ml) in
    let core = String.sub s (ml + version_len) digest_hex_len in
    Ok { format; core }

let restore s =
  match header s with
  | Error e -> Error e
  | Ok { format; core = _ (* shown, never compared: the format decides *) } ->
    if format <> format_version then
      Error (Wrong_format { saved = format; supported = format_version })
    else
      let sum_at = header_len - checksum_len in
      let sum = String.sub s sum_at checksum_len in
      let payload = String.sub s header_len (String.length s - header_len) in
      if not (String.equal sum (Digest.string payload)) then
        Error (Corrupt "checksum mismatch")
      else (
        match read_payload (C.reader payload) with
        | t -> Ok t
        | exception C.Invalid message -> Error (Corrupt message)
        | exception Invalid_argument message -> Error (Corrupt message))
