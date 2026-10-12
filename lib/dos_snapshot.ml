(* Whole-machine snapshot. See dos_snapshot.mli. *)

open Dos_state
module C = Dos_snap_codec

let magic = "OCAML-DOS-SNAPSHOT\000"

(* Bump by hand whenever what [save] writes changes shape or meaning. A
   snapshot of any other version is refused; nothing reads an old one.
   2: no next-handle counters (DOS and EMS handles reuse the lowest free
   number), and every int is range-checked on write as well as read.
   3: a handle's [data] is a pool index into a separate buffer pool
   (#35), not inline bytes -- two handles opened separately on one name
   now share a buffer, and the pool keeps that sharing across a
   save/restore round trip the way it already kept [dup]'s whole-record
   sharing.
   4: the mouse carries its INT 33h AX=0x0C registration (mask, segment,
   offset) after dx/dy, so a restored machine keeps calling the handler
   the guest registered. Snapshots of format 3 are refused; only the
   current format is read.
   5: suspended BIOS input services carry their return boundary, and both
   input and EXEC carry their owned ROM service return frame.
   6: DOS byte/line continuations, partial echo widths, and remaining cooked
   console bytes are carried explicitly. *)
let format_version = 6

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

type save_error = Unsaveable of string

let save_error_to_string (Unsaveable message) = "machine cannot be saved: " ^ message

(* ---------- ranges ----------

   Each int field has one range, named by the writer and the reader alike,
   so [save] refuses exactly the values [restore] would. *)

let reg = C.word
let handle_number = C.range 0 (max_handles - 1)
let ems_handle_key = C.range Dos_ems.first_handle Dos_ems.max_handle
(* [ems_mapped] uses handle 0 for "nothing mapped". *)
let ems_mapped_handle = C.range 0 Dos_ems.max_handle
let file_pos = C.count
let vector = C.byte

(* ---------- consistency ----------

   What a range cannot say because it relates two fields. [save] checks it
   before writing and [restore] after reading, so both refuse the same
   machines, and a crafted snapshot is refused at restore instead of
   raising later inside [step] (a mapped page that does not exist, a page
   the EMS blit cannot copy). A DOS handle's position may lie past the end
   of its file: AH=42h can seek there and AH=40h then fills the gap. *)
let check_consistency t =
  let page_ok p = let n = Bytes.length p in n = 0 || n = Dos_ems.page_bytes in
  let bad_page =
    Hashtbl.fold (fun h pages acc ->
        if acc = None && not (Array.for_all page_ok pages) then Some h else acc)
      t.ems_pages None
  in
  let mapped_ok (h, p) =
    (h = 0 && p = 0)
    || (match Hashtbl.find_opt t.ems_pages h with
        | Some pages -> p < Array.length pages
        | None -> false)
  in
  let return_ok = function
    | Direct_return -> true
    | Interrupt_return { status_mask; _ } ->
      status_mask land lnot (Cpu86.f_carry lor Cpu86.f_zero) = 0
  in
  let input_ok wait =
    let line_ok capacity line =
      String.length line.text < capacity
      && List.length line.echo_widths = String.length line.text
      && List.for_all (fun width -> width >= 1 && width <= 8) line.echo_widths
    in
    let request_ok = match wait.request with
      | Bios_key | Dos_input (Read_character _) -> true
      | Dos_input (Read_buffered_line { capacity; line; _ }) -> line_ok capacity line
      | Dos_input (Read_console_line { line; _ }) -> line_ok console_line_capacity line
    in
    wait.return_control_flags land lnot (Cpu86.f_interrupt lor Cpu86.f_trap) = 0
    && return_ok wait.service_return
    && request_ok
  in
  match bad_page with
  | Some h -> Error (Printf.sprintf "EMS handle %d has a page that is not 0 or %d bytes" h
                       Dos_ems.page_bytes)
  | None ->
    if Array.length t.ems_mapped <> Dos_ems.frame_pages then Error "EMS frame size differs"
    else if not (Array.for_all mapped_ok t.ems_mapped) then
      Error "an EMS frame page maps a page that does not exist"
    else if not (List.for_all input_ok t.input_continuations) then
      Error "an input continuation has inconsistent state"
    else if not (List.for_all (fun frame -> return_ok frame.parent_service_return) t.exec_frames) then
      Error "an EXEC return has an invalid flag mask"
    else if String.length t.console_pending > console_line_capacity + 1 then
      Error "pending console bytes exceed the cooked line buffer"
    else Ok ()

(* ---------- writing ---------- *)

let sorted_bindings tbl =
  List.sort (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

let put_list w ~what put l =
  C.put w ~what:(what ^ " count") C.count (List.length l);
  List.iter put l

let put_pairs w ~what ~a ~b l =
  put_list w ~what (fun (x, y) -> C.put w ~what a x; C.put w ~what b y) l

let put_service_return w = function
  | Direct_return -> C.put_bool w false
  | Interrupt_return { frame_ss; frame_sp; status_mask } ->
    C.put_bool w true;
    List.iter (C.put w ~what:"service return frame" C.word) [frame_ss; frame_sp; status_mask]

let put_line_input w { text; echo_widths } =
  C.put_string w text;
  put_list w ~what:"input echo widths"
    (C.put w ~what:"input echo width" (C.range 1 8)) echo_widths

let put_input_request w request =
  let tag n = C.put w ~what:"input request" (C.range 0 3) n in
  match request with
  | Bios_key -> tag 0
  | Dos_input (Read_character { echo }) -> tag 1; C.put_bool w echo
  | Dos_input (Read_buffered_line { base; capacity; line }) ->
    tag 2;
    C.put w ~what:"input buffer address" C.physical base;
    C.put w ~what:"input buffer capacity" (C.range 1 255) capacity;
    put_line_input w line
  | Dos_input (Read_console_line { destination; length; line }) ->
    tag 3;
    C.put w ~what:"input buffer address" C.physical destination;
    C.put w ~what:"console read length" (C.range 1 65535) length;
    put_line_input w line

let put_input_continuation w wait =
  let { request; return_cs; return_ip; return_ss; return_sp;
        return_control_flags; service_return } = wait in
  put_input_request w request;
  List.iter (C.put w ~what:"input return boundary" C.word)
    [return_cs; return_ip; return_ss; return_sp];
  C.put w ~what:"input return control flags" C.word return_control_flags;
  put_service_return w service_return

(* Handle records are shared two ways. Dup (AH=45h/46h) files one whole
   record under two numbers, position included, and an EXEC frame keeps
   the parent's records dormant while a child runs -- either way, a read
   or write through any of those numbers moves the one record. Two names
   opened separately (AH=3Dh twice, Dos_dos.open_handle) instead share
   only the underlying bytes: each keeps its own position, but a write
   through one is a read through the other because [handle.data] is the
   same [Bytes.t ref].

   The snapshot keeps both. Handle records pool by physical identity of
   the record, as before. Inside that, buffers pool separately by
   physical identity of the [Bytes.t ref] a record's [data] names, so two
   records that shared a buffer before saving still share the one
   restored ref, not two copies of the same bytes. Pool order is first
   appearance in the sorted live table, then in the frames, so the bytes
   are deterministic. *)
let handle_pool t =
  let buffers = ref [] and buffer_count = ref 0 in
  let buffer_index buf =
    match List.find_opt (fun (b, _) -> b == buf) !buffers with
    | Some (_, i) -> i
    | None ->
      let i = !buffer_count in
      buffers := (buf, i) :: !buffers;
      incr buffer_count;
      i
  in
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
  let handles = List.rev_map fst !pool in
  let handles_with_buffers = List.map (fun h -> (h, buffer_index h.data)) handles in
  (handles_with_buffers, List.rev_map fst !buffers, live, frames)

let put_buffer w buf = C.put_bytes w !buf

let put_handle w (h, buffer_idx) =
  let { hname; data = _; pos } = h in
  C.put_string w hname; C.put w ~what:"handle buffer" C.count buffer_idx;
  C.put w ~what:"handle position" file_pos pos

let put_frame w f handles =
  let { parent; parent_service_return; parent_psp; parent_dta; parent_free_base; parent_free_top;
        parent_blocks; parent_handles = _ (* as pool indexes: [handles] *) } = f
  in
  let { snap_regs; snap_segs; snap_ip; snap_flags } = parent in
  C.put_int_array w ~what:"frame registers" reg snap_regs;
  C.put_int_array w ~what:"frame segments" reg snap_segs;
  C.put w ~what:"frame ip" reg snap_ip;
  C.put w ~what:"frame flags" reg snap_flags;
  put_service_return w parent_service_return;
  C.put w ~what:"parent psp" C.word parent_psp;
  C.put w ~what:"parent dta" C.physical parent_dta;
  C.put w ~what:"parent free base" C.word parent_free_base;
  C.put w ~what:"parent free top" C.word parent_free_top;
  put_pairs w ~what:"parent block" ~a:C.word ~b:C.word parent_blocks;
  put_pairs w ~what:"parent handle" ~a:handle_number ~b:C.count handles

(* Every [Dos_state.t] field is named: a field added to the machine fails the
   build here until the snapshot carries it or says why not. [read_payload]
   reads in exactly this order with the same ranges. *)
let write_payload w t =
  let { mem; cpu; ports; exited; exit_code; video; host_files;
        handles = _; fcbs; dta; psp_seg; kbd_wait;
        ext_scan_pending; input_continuations; console_pending; kbd_requests; last_tick; pending_irq0; free_base;
        free_top; blocks; find_queue; stubs;
        exec_frames = _ (* with [handles], through [handle_pool] *);
        open_files = _ (* derived from [handles]/[exec_frames]; rebuilt on
                           read, nothing of its own to write *);
        last_child_code; epoch_year; epoch_month; epoch_day; epoch_hour;
        epoch_min; epoch_sec; ems_pages; ems_mapped;
        mouse } = t
  in
  let put what r v = C.put w ~what r v in
  let { Cpu86.saved_regs; saved_segs; saved_ip; saved_flags; saved_halted;
        saved_cycles } = Cpu86.save_state cpu
  in
  C.put_int_array w ~what:"registers" reg saved_regs;
  C.put_int_array w ~what:"segments" reg saved_segs;
  put "ip" reg saved_ip;
  put "flags" reg saved_flags;
  C.put_bool w saved_halted;
  put "cycles" C.count saved_cycles;
  C.put_bytes w mem;
  Dos_video.write_state w video;
  Dos_ports.write_state w ports;
  C.put_bool w exited;
  put "exit code" C.byte exit_code;
  put_list w ~what:"host file" (fun (k, v) -> C.put_string w k; C.put_bytes w v)
    (sorted_bindings host_files);
  let pool, buffers, live, frames = handle_pool t in
  put_list w ~what:"handle buffer" (put_buffer w) buffers;
  put_list w ~what:"handle record" (put_handle w) pool;
  put_pairs w ~what:"handle" ~a:handle_number ~b:C.count live;
  put_list w ~what:"FCB"
    (fun (k, (b, n)) -> put "FCB address" C.physical k; C.put_bytes w b;
      put "FCB position" file_pos n)
    (sorted_bindings fcbs);
  put "dta" C.physical dta;
  put "psp" C.word psp_seg;
  C.put_bool w kbd_wait;
  put "pending extended scan" C.byte ext_scan_pending;
  put_list w ~what:"input continuation" (put_input_continuation w) input_continuations;
  C.put_string w console_pending;
  put "keyboard requests" C.count kbd_requests;
  put "last tick" C.count last_tick;
  C.put_bool w pending_irq0;
  put "free base" C.word free_base;
  put "free top" C.word free_top;
  put_pairs w ~what:"block" ~a:C.word ~b:C.word blocks;
  put_list w ~what:"find queue" (C.put_string w) find_queue;
  put_pairs w ~what:"stub" ~a:vector ~b:C.word stubs;
  put "EXEC frame count" C.count (List.length frames);
  List.iter2 (put_frame w) t.exec_frames frames;
  put "last child code" C.byte last_child_code;
  (* [Dos_machine.set_clock] takes any ints; a snapshot carries any. *)
  List.iter (put "clock" C.any)
    [ epoch_year; epoch_month; epoch_day; epoch_hour; epoch_min; epoch_sec ];
  put_list w ~what:"EMS handle"
    (fun (k, pages) ->
      put "EMS handle" ems_handle_key k;
      put_list w ~what:"EMS page" (C.put_bytes w) (Array.to_list pages))
    (sorted_bindings ems_pages);
  put_pairs w ~what:"EMS mapping" ~a:ems_mapped_handle ~b:C.word (Array.to_list ems_mapped);
  let { mouse_present; mouse_x; mouse_y; mouse_buttons; mouse_visible;
        mouse_dx; mouse_dy; mouse_handler_mask; mouse_handler_seg;
        mouse_handler_off } = mouse
  in
  C.put_bool w mouse_present;
  put "mouse x" C.any mouse_x;
  put "mouse y" C.any mouse_y;
  put "mouse buttons" C.any mouse_buttons;
  C.put_bool w mouse_visible;
  put "mouse dx" C.any mouse_dx;
  put "mouse dy" C.any mouse_dy;
  put "mouse handler mask" C.word mouse_handler_mask;
  put "mouse handler seg" C.word mouse_handler_seg;
  put "mouse handler off" C.word mouse_handler_off

let save t =
  match check_consistency t with
  | Error message -> Error (Unsaveable message)
  | Ok () ->
    let w = C.writer () in
    match write_payload w t with
    | exception C.Unsaveable message -> Error (Unsaveable message)
    | () ->
      let payload = C.contents w in
      let version = Bytes.create version_len in
      Bytes.set_int64_be version 0 (Int64.of_int format_version);
      Ok
        (String.concat ""
           [ magic; Bytes.to_string version; Dos_core_identity.source_digest;
             Digest.string payload; payload ])

(* ---------- reading ---------- *)

(* Every element takes at least one byte, so a count above what is left is
   a lie the reader refuses before allocating for it. *)
let get_list r ~what get =
  List.init (C.get r ~what:(what ^ " count") (C.range 0 (C.remaining r))) (fun _ -> get ())

let get_pairs r ~what ~a ~b =
  get_list r ~what (fun () -> let x = C.get r ~what a in let y = C.get r ~what b in (x, y))

let fill_table tbl bindings =
  Hashtbl.reset tbl;
  List.iter
    (fun (k, v) ->
      if Hashtbl.mem tbl k then C.fail "a key appears twice";
      Hashtbl.replace tbl k v)
    bindings

let get_int_array r ~what ~len rng =
  let a = Array.make len 0 in
  C.fill_int_array r ~what rng a;
  a

let get_service_return r =
  if not (C.get_bool r) then Direct_return
  else
    let get () = C.get r ~what:"service return frame" C.word in
    let frame_ss = get () in
    let frame_sp = get () in
    let status_mask = get () in
    Interrupt_return { frame_ss; frame_sp; status_mask }

let get_line_input r =
  let text = C.get_string r in
  let echo_widths = get_list r ~what:"input echo widths"
      (fun () -> C.get r ~what:"input echo width" (C.range 1 8)) in
  { text; echo_widths }

let get_input_request r =
  match C.get r ~what:"input request" (C.range 0 3) with
  | 0 -> Bios_key
  | 1 -> Dos_input (Read_character { echo = C.get_bool r })
  | 2 ->
    let base = C.get r ~what:"input buffer address" C.physical in
    let capacity = C.get r ~what:"input buffer capacity" (C.range 1 255) in
    let line = get_line_input r in
    Dos_input (Read_buffered_line { base; capacity; line })
  | 3 ->
    let destination = C.get r ~what:"input buffer address" C.physical in
    let length = C.get r ~what:"console read length" (C.range 1 65535) in
    let line = get_line_input r in
    Dos_input (Read_console_line { destination; length; line })
  | _ -> C.fail "invalid input request"

let read_payload r =
  (* A fresh machine brings the closures (memory, ports, the interrupt hook)
     and the containers; everything else is overwritten from the bytes. *)
  let t = Dos_machine.create () in
  let get what rng = C.get r ~what rng in
  let saved_regs = get_int_array r ~what:"registers" ~len:8 reg in
  let saved_segs = get_int_array r ~what:"segments" ~len:4 reg in
  let saved_ip = get "ip" reg in
  let saved_flags = get "flags" reg in
  let saved_halted = C.get_bool r in
  let saved_cycles = get "cycles" C.count in
  Cpu86.load_state t.cpu
    { Cpu86.saved_regs; saved_segs; saved_ip; saved_flags; saved_halted; saved_cycles };
  C.fill_bytes r t.mem;
  Dos_video.read_state r t.video;
  Dos_ports.read_state r t.ports;
  t.exited <- C.get_bool r;
  t.exit_code <- get "exit code" C.byte;
  fill_table t.host_files
    (get_list r ~what:"host file" (fun () -> let k = C.get_string r in (k, C.get_bytes r)));
  let buffers =
    Array.of_list (get_list r ~what:"handle buffer" (fun () -> ref (C.get_bytes r)))
  in
  let pool =
    Array.of_list
      (get_list r ~what:"handle record" (fun () ->
           let hname = C.get_string r in
           let buffer_idx = get "handle buffer" C.count in
           if buffer_idx >= Array.length buffers then C.fail "a handle names no buffer"
           else
             { hname; data = buffers.(buffer_idx); pos = get "handle position" file_pos }))
  in
  let resolve = List.map (fun (n, i) ->
      if i >= Array.length pool then C.fail "a handle names no record" else (n, pool.(i)))
  in
  (* [open_files] is a lookup accelerator, not authority -- every name it
     needs to know is already in [pool], each with the one buffer every
     record sharing that name points at (by construction: [handle_pool]
     never puts two different buffers under the same live name). Rebuild
     it here so the next [Dos_dos.open_handle] on a name already open
     joins the restored buffer instead of starting a fresh, disconnected
     one. *)
  Hashtbl.reset t.open_files;
  Array.iter
    (fun h -> if h.hname <> "" then Hashtbl.replace t.open_files h.hname h.data)
    pool;
  fill_table t.handles (resolve (get_pairs r ~what:"handle" ~a:handle_number ~b:C.count));
  fill_table t.fcbs
    (get_list r ~what:"FCB" (fun () ->
         let k = get "FCB address" C.physical in
         let b = C.get_bytes r in
         (k, (b, get "FCB position" file_pos))));
  t.dta <- get "dta" C.physical;
  t.psp_seg <- get "psp" C.word;
  t.kbd_wait <- C.get_bool r;
  t.ext_scan_pending <- get "pending extended scan" C.byte;
  t.input_continuations <-
    get_list r ~what:"input continuation" (fun () ->
        let request = get_input_request r in
        let return_cs = get "input return boundary" C.word in
        let return_ip = get "input return boundary" C.word in
        let return_ss = get "input return boundary" C.word in
        let return_sp = get "input return boundary" C.word in
        let return_control_flags = get "input return control flags" C.word in
        let service_return = get_service_return r in
        { request; return_cs; return_ip; return_ss; return_sp;
          return_control_flags; service_return });
  t.console_pending <- C.get_string r;
  t.kbd_requests <- get "keyboard requests" C.count;
  t.last_tick <- get "last tick" C.count;
  t.pending_irq0 <- C.get_bool r;
  t.free_base <- get "free base" C.word;
  t.free_top <- get "free top" C.word;
  t.blocks <- get_pairs r ~what:"block" ~a:C.word ~b:C.word;
  t.find_queue <- get_list r ~what:"find queue" (fun () -> C.get_string r);
  t.stubs <- get_pairs r ~what:"stub" ~a:vector ~b:C.word;
  let frame_count = get "EXEC frame count" (C.range 0 (C.remaining r)) in
  t.exec_frames <-
    List.init frame_count (fun _ ->
        let snap_regs = get_int_array r ~what:"frame registers" ~len:8 reg in
        let snap_segs = get_int_array r ~what:"frame segments" ~len:4 reg in
        let snap_ip = get "frame ip" reg in
        let snap_flags = get "frame flags" reg in
        let parent_service_return = get_service_return r in
        let parent_psp = get "parent psp" C.word in
        let parent_dta = get "parent dta" C.physical in
        let parent_free_base = get "parent free base" C.word in
        let parent_free_top = get "parent free top" C.word in
        let parent_blocks = get_pairs r ~what:"parent block" ~a:C.word ~b:C.word in
        let parent_handles =
          resolve (get_pairs r ~what:"parent handle" ~a:handle_number ~b:C.count)
        in
        { parent = { snap_regs; snap_segs; snap_ip; snap_flags };
          parent_service_return;
          parent_psp; parent_dta; parent_free_base; parent_free_top;
          parent_blocks; parent_handles });
  t.last_child_code <- get "last child code" C.byte;
  t.epoch_year <- get "clock" C.any;
  t.epoch_month <- get "clock" C.any;
  t.epoch_day <- get "clock" C.any;
  t.epoch_hour <- get "clock" C.any;
  t.epoch_min <- get "clock" C.any;
  t.epoch_sec <- get "clock" C.any;
  fill_table t.ems_pages
    (get_list r ~what:"EMS handle" (fun () ->
         let k = get "EMS handle" ems_handle_key in
         (k, Array.of_list (get_list r ~what:"EMS page" (fun () -> C.get_bytes r)))));
  let mapped = get_pairs r ~what:"EMS mapping" ~a:ems_mapped_handle ~b:C.word in
  if List.length mapped <> Array.length t.ems_mapped then C.fail "EMS frame size differs";
  List.iteri (fun i p -> t.ems_mapped.(i) <- p) mapped;
  let m = t.mouse in
  m.mouse_present <- C.get_bool r;
  m.mouse_x <- get "mouse x" C.any;
  m.mouse_y <- get "mouse y" C.any;
  m.mouse_buttons <- get "mouse buttons" C.any;
  m.mouse_visible <- C.get_bool r;
  m.mouse_dx <- get "mouse dx" C.any;
  m.mouse_dy <- get "mouse dy" C.any;
  m.mouse_handler_mask <- get "mouse handler mask" C.word;
  m.mouse_handler_seg <- get "mouse handler seg" C.word;
  m.mouse_handler_off <- get "mouse handler off" C.word;
  C.end_of_input r;
  (match check_consistency t with
   | Ok () -> ()
   | Error message -> C.fail message);
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
        (* Only the decoder's own refusal is [Corrupt]. Any other exception
           is a bug in this module or the machine and propagates. *)
        match read_payload (C.reader payload) with
        | t -> Ok t
        | exception C.Invalid message -> Error (Corrupt message))
