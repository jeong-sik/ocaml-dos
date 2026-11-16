(* LIM EMS 4.0 handle functions -- including attributes and names (52h-54h)
   -- and 57h which moves bytes in and out of handles, called the way
   INT 67h calls them:
   registers in, [Dos_ems.service], registers out. The guest path through
   the ROM stub is dos_ems_test's. Expected values are the spec's
   (LIM EMS 4.0, 1987, by Function number). *)

open Dos_state

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

let check_b name got = check name (Bool.to_int got) 1

let check_l name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got [%s]\n%!" name (String.concat " " (List.map (Printf.sprintf "%X") got))
  end

(* Cpu86 register numbers and the spec's status codes. *)
let r_ax = 0
let r_dx = 2
let r_bx = 3
let r_si = 6
let r_di = 7
let s_es = 0
let s_ds = 3
let ok = 0x00
let invalid_handle = 0x83
let undefined_function = 0x84
let map_still_saved = 0x86
let more_than_total = 0x87
let more_than_free = 0x88
let zero_pages = 0x89
let logical_out_of_range = 0x8A
let physical_out_of_range = 0x8B
let map_already_saved = 0x8D
let no_saved_map = 0x8E
let invalid_subfunction = 0x8F
let moved_over_source = 0x92
let past_handle_pages = 0x93
let conventional_overlaps_expanded = 0x94
let offset_past_page = 0x95
let longer_than_1mb = 0x96
let exchange_overlap = 0x97
let undefined_memory_type = 0x98
let wraps_1mb = 0xA2
let undefined_attribute = 0x90
let not_supported = 0x91
let name_not_found = 0xA0
let name_taken_or_null = 0xA1
let unmap = 0xFFFF
let total = 2048

let call t ?(al = 0) ?(bx = 0) ?(dx = 0) ?(es = 0) ?(di = 0) ?(ds = 0) ?(si = 0) ah =
  let cpu = t.cpu in
  Cpu86.set_reg16 cpu r_ax ((ah lsl 8) lor al);
  Cpu86.set_reg16 cpu r_bx bx;
  Cpu86.set_reg16 cpu r_dx dx;
  Cpu86.set_seg cpu s_es es;
  Cpu86.set_reg16 cpu r_di di;
  Cpu86.set_seg cpu s_ds ds;
  Cpu86.set_reg16 cpu r_si si;
  Dos_ems.service t;
  Cpu86.reg16 cpu r_ax lsr 8

let bx t = Cpu86.reg16 t.cpu r_bx
let dx t = Cpu86.reg16 t.cpu r_dx
let frame p = (Dos_ems.page_frame * 16) + (p * Dos_ems.page_bytes)
let peek t a = Dos_machine.mem_read t a
let poke t a v = wr8 t a v

let allocate t n =
  check (Printf.sprintf "allocate %d" n) (call t 0x43 ~bx:n) ok;
  dx t

let map t ~phys ~handle ~log =
  check (Printf.sprintf "map %d:%d at %d" handle log phys) (call t 0x44 ~al:phys ~bx:log ~dx:handle) ok

(* Function 3 and 4: BX is the free count and DX the total; a request
   above the total is 87h, above what is free 88h, zero pages 89h. *)
let () =
  let t = Dos_machine.create () in
  check "42h" (call t 0x42) ok;
  check "42h BX = free" (bx t) total;
  check "42h DX = total" (dx t) total;
  let h = allocate t 3 in
  check "the first handle is 1" h 1;
  ignore (call t 0x42);
  check "42h BX after 3 pages" (bx t) (total - 3);
  check "42h DX after 3 pages" (dx t) total;
  check "43h above the total" (call t 0x43 ~bx:(total + 1)) more_than_total;
  check "43h above what is free" (call t 0x43 ~bx:(total - 2)) more_than_free;
  check "43h zero pages" (call t 0x43 ~bx:0) zero_pages;
  check "43h all that is free" (call t 0x43 ~bx:(total - 3)) ok

(* Function 5: logical page FFFFh empties the physical page, whatever DX
   holds. A write to an empty page reaches no logical page. *)
let () =
  let t = Dos_machine.create () in
  let h = allocate t 2 in
  map t ~phys:0 ~handle:h ~log:0;
  poke t (frame 0) 0xA5;
  check "unmap" (call t 0x44 ~al:0 ~bx:unmap ~dx:0x99) ok;
  check "the frame page is empty" (Bool.to_int (t.ems_mapped.(0) = None)) 1;
  poke t (frame 0) 0x11;
  map t ~phys:0 ~handle:h ~log:0;
  check "the page kept its byte, not the empty page's" (peek t (frame 0)) 0xA5;
  check "unmap past the frame" (call t 0x44 ~al:4 ~bx:unmap) physical_out_of_range;
  check "map an unknown handle" (call t 0x44 ~al:0 ~bx:0 ~dx:0x99) invalid_handle;
  check "map past the handle's pages" (call t 0x44 ~al:0 ~bx:2 ~dx:h) logical_out_of_range

(* One logical page in two windows: the window the guest wrote is the
   newest copy, so mapping the page again takes its bytes. *)
let () =
  let t = Dos_machine.create () in
  let h = allocate t 1 in
  map t ~phys:0 ~handle:h ~log:0;
  poke t (frame 0) 0xA5;
  map t ~phys:1 ~handle:h ~log:0;
  check "the second window shows the first window's write" (peek t (frame 1)) 0xA5

(* Functions 12-14 count the OS handle 0, which is always open. *)
let () =
  let t = Dos_machine.create () in
  check "4Bh at boot" (call t 0x4B) ok;
  check "4Bh: the OS handle" (bx t) 1;
  let h = allocate t 5 in
  ignore (call t 0x4B);
  check "4Bh after one allocation" (bx t) 2;
  check "4Ch" (call t 0x4C ~dx:h) ok;
  check "4Ch pages" (bx t) 5;
  check "4Ch on the OS handle" (call t 0x4C ~dx:Dos_ems.os_handle) ok;
  check "4Ch: the OS handle has no pages" (bx t) 0;
  check "4Ch on an unknown handle" (call t 0x4C ~dx:0x77) invalid_handle;
  let seg = 0x2000 and off = 0x10 in
  let word i = peek t ((seg * 16) + off + i) lor (peek t ((seg * 16) + off + i + 1) lsl 8) in
  check "4Dh" (call t 0x4D ~es:seg ~di:off) ok;
  check "4Dh count" (bx t) 2;
  check "4Dh first handle" (word 0) Dos_ems.os_handle;
  check "4Dh first pages" (word 2) 0;
  check "4Dh second handle" (word 4) h;
  check "4Dh second pages" (word 6) 5;
  (* Function 4 Note: deallocating the OS handle leaves it open. *)
  check "45h on the OS handle" (call t 0x45 ~dx:Dos_ems.os_handle) ok;
  ignore (call t 0x4B);
  check "the OS handle is still open" (bx t) 2

(* Functions 8 and 9: one saved frame map per handle. *)
let () =
  let t = Dos_machine.create () in
  let h = allocate t 2 in
  let isr = allocate t 1 in
  map t ~phys:0 ~handle:h ~log:0;
  poke t (frame 0) 0xA5;
  check "47h" (call t 0x47 ~dx:isr) ok;
  check "47h twice" (call t 0x47 ~dx:isr) map_already_saved;
  check "47h on an unknown handle" (call t 0x47 ~dx:0x77) invalid_handle;
  map t ~phys:0 ~handle:h ~log:1;
  map t ~phys:1 ~handle:isr ~log:0;
  check "45h while a map is saved" (call t 0x45 ~dx:isr) map_still_saved;
  (* the saved map survives a snapshot round trip *)
  let t =
    match Dos_snapshot.save t with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok t -> t
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  check "48h" (call t 0x48 ~dx:isr) ok;
  check "48h put page 0 back" (peek t (frame 0)) 0xA5;
  check "48h emptied physical page 1"
    (Bool.to_int (t.ems_mapped = [| Some (h, 0); None; None; None |])) 1;
  check "48h again" (call t 0x48 ~dx:isr) no_saved_map;
  check "48h on an unknown handle" (call t 0x48 ~dx:0x77) invalid_handle;
  check "45h after the restore" (call t 0x45 ~dx:isr) ok

(* A released page leaves the saved maps: a reused handle number must not
   bring back somebody else's page. *)
let () =
  let t = Dos_machine.create () in
  let h = allocate t 1 in
  let isr = allocate t 1 in
  map t ~phys:2 ~handle:h ~log:0;
  check "47h" (call t 0x47 ~dx:isr) ok;
  check "45h on the page's handle" (call t 0x45 ~dx:h) ok;
  check "the frame forgot it" (Bool.to_int (t.ems_mapped.(2) = None)) 1;
  check "the handle number comes back" (allocate t 1) h;
  check "48h" (call t 0x48 ~dx:isr) ok;
  check "48h left physical page 2 empty" (Bool.to_int (t.ems_mapped.(2) = None)) 1

(* Function 18: grow at the end, shrink from the end; BX is the count after,
   or the count before when it fails. *)
let () =
  let t = Dos_machine.create () in
  let h = allocate t 2 in
  map t ~phys:0 ~handle:h ~log:0;
  poke t (frame 0) 0xA5;
  (* empty the frame page so only the stored page holds the byte *)
  check "unmap" (call t 0x44 ~al:0 ~bx:unmap ~dx:h) ok;
  check "51h grow" (call t 0x51 ~dx:h ~bx:4) ok;
  check "51h grow BX" (bx t) 4;
  map t ~phys:0 ~handle:h ~log:0;
  check "growing kept page 0" (peek t (frame 0)) 0xA5;
  map t ~phys:1 ~handle:h ~log:1;
  map t ~phys:2 ~handle:h ~log:3;
  check "51h shrink" (call t 0x51 ~dx:h ~bx:1) ok;
  check "51h shrink BX" (bx t) 1;
  check "the dropped pages left the frame"
    (Bool.to_int (t.ems_mapped = [| Some (h, 0); None; None; None |])) 1;
  check "page 0 kept its byte" (peek t (frame 0)) 0xA5;
  check "page 1 is gone" (call t 0x44 ~al:1 ~bx:1 ~dx:h) logical_out_of_range;
  check "51h above the total" (call t 0x51 ~dx:h ~bx:(total + 1)) more_than_total;
  check "51h above the total keeps BX" (bx t) 1;
  ignore (allocate t (total - 2));
  check "51h above what is free" (call t 0x51 ~dx:h ~bx:3) more_than_free;
  check "51h above what is free keeps BX" (bx t) 1;
  check "51h to what is free" (call t 0x51 ~dx:h ~bx:2) ok;
  check "51h to zero" (call t 0x51 ~dx:h ~bx:0) ok;
  check "51h to zero leaves the handle open" (call t 0x4C ~dx:h) ok;
  check "51h on an unknown handle" (call t 0x51 ~dx:0x77 ~bx:1) invalid_handle;
  check "51h on an unknown handle leaves BX" (bx t) 1

(* Function 27 takes zero pages; 43h does not. *)
let () =
  let t = Dos_machine.create () in
  check "5A00h zero pages" (call t 0x5A ~al:0 ~bx:0) ok;
  check "5A00h handle" (dx t) 1;
  check "5A01h raw pages" (call t 0x5A ~al:1 ~bx:2) ok;
  check "5A01h handle" (dx t) 2;
  check "5A01h pages" (Array.length (Hashtbl.find t.ems_pages 2)) 2;
  check "5A02h" (call t 0x5A ~al:2 ~bx:1) invalid_subfunction;
  check "5A00h above what is free" (call t 0x5A ~al:0 ~bx:(total - 1)) more_than_free;
  check "55h is not served" (call t 0x55) undefined_function

(* ---------- Function 24: move (5700h) and exchange (5701h) ----------

   The region structure at DS:SI: length (dword), then source and
   destination, each a type byte (0 conventional, 1 expanded), a handle,
   an offset, and a segment or first logical page. *)

let buf = 0x2000                                 (* guest segment for data *)
let at off = (buf * 16) + off
let conv = 0
let ems = 1
let region_at = 0x10

let region t ~length (st, sh, so, ssp) (dt, dh, doff, dsp) =
  let a = at region_at in
  wr16 t a (length land 0xffff);
  wr16 t (a + 2) (length lsr 16);
  wr8 t (a + 4) st; wr16 t (a + 5) sh; wr16 t (a + 7) so; wr16 t (a + 9) ssp;
  wr8 t (a + 11) dt; wr16 t (a + 12) dh; wr16 t (a + 14) doff; wr16 t (a + 16) dsp

let move t ?(al = 0) ~length source dest =
  region t ~length source dest;
  call t 0x57 ~al ~ds:buf ~si:region_at

let bytes_at t a n = List.init n (fun i -> peek t (a + i))
let put t a l = List.iteri (fun i v -> poke t (a + i) v) l

let () =
  let t = Dos_machine.create () in
  let h = allocate t 4 in
  (* conventional to expanded, across a page boundary, then look *)
  put t (at 0x500) [ 0x41; 0x42; 0x43 ];
  check "5700h to expanded" (move t ~length:3 (conv, 0, 0x500, buf) (ems, h, 0x3FFE, 1)) ok;
  map t ~phys:0 ~handle:h ~log:1;
  map t ~phys:1 ~handle:h ~log:2;
  check_l "the bytes span pages 1 and 2"
    (bytes_at t (frame 0 + 0x3FFE) 2 @ bytes_at t (frame 1) 1) [ 0x41; 0x42; 0x43 ];
  (* a shown page is read from the frame and written to the frame *)
  map t ~phys:2 ~handle:h ~log:0;
  poke t (frame 2 + 0x10) 0x5A;
  check "5700h from a shown page" (move t ~length:1 (ems, h, 0x10, 0) (conv, 0, 0x600, buf)) ok;
  check "the frame's byte came out" (peek t (at 0x600)) 0x5A;
  map t ~phys:3 ~handle:h ~log:3;
  check "5700h into a shown page" (move t ~length:1 (conv, 0, 0x500, buf) (ems, h, 0, 3)) ok;
  check "the byte is in the frame" (peek t (frame 3)) 0x41;
  check_b "the mapping did not change"
    (t.ems_mapped = [| Some (h, 1); Some (h, 2); Some (h, 0); Some (h, 3) |]);
  (* a page shown in two windows: the move writes both *)
  map t ~phys:3 ~handle:h ~log:0;
  check "5700h into a page shown twice" (move t ~length:1 (conv, 0, 0x502, buf) (ems, h, 0x20, 0)) ok;
  check "window 2 has it" (peek t (frame 2 + 0x20)) 0x43;
  check "window 3 has it" (peek t (frame 3 + 0x20)) 0x43;
  map t ~phys:3 ~handle:h ~log:3;
  (* one handle, overlapping: the destination gets an intact copy, 92h *)
  put t (frame 2 + 0x100) [ 1; 2; 3; 4 ];
  check "5700h overlapping forward" (move t ~length:4 (ems, h, 0x100, 0) (ems, h, 0x101, 0))
    moved_over_source;
  check_l "an intact copy one byte on" (bytes_at t (frame 2 + 0x101) 4) [ 1; 2; 3; 4 ];
  check "5700h overlapping back" (move t ~length:4 (ems, h, 0x101, 0) (ems, h, 0x100, 0))
    moved_over_source;
  check_l "an intact copy one byte back" (bytes_at t (frame 2 + 0x100) 4) [ 1; 2; 3; 4 ];
  check "5700h one handle, apart" (move t ~length:2 (ems, h, 0x100, 0) (ems, h, 0x200, 0)) ok;
  (* two handles never overlap, even at the same place in each *)
  let h2 = allocate t 1 in
  check "5700h two handles, same place" (move t ~length:4 (ems, h, 0x100, 0) (ems, h2, 0x100, 0)) ok;
  check "5701h two handles, same place" (move t ~al:1 ~length:4 (ems, h2, 0x100, 0) (ems, h, 0x100, 0)) ok;
  (* conventional to conventional, overlapping: an intact copy, status 0 *)
  put t (at 0x700) [ 1; 2; 3; 4 ];
  check "5700h conventional overlap" (move t ~length:4 (conv, 0, 0x700, buf) (conv, 0, 0x702, buf)) ok;
  check_l "conventional intact copy" (bytes_at t (at 0x702) 4) [ 1; 2; 3; 4 ];
  (* exchange *)
  put t (at 0x800) [ 0x11; 0x22 ];
  put t (frame 2 + 0x300) [ 0x33; 0x44 ];
  check "5701h" (move t ~al:1 ~length:2 (conv, 0, 0x800, buf) (ems, h, 0x300, 0)) ok;
  check_l "5701h conventional side" (bytes_at t (at 0x800) 2) [ 0x33; 0x44 ];
  check_l "5701h expanded side" (bytes_at t (frame 2 + 0x300) 2) [ 0x11; 0x22 ];
  check "5701h one handle, overlapping" (move t ~al:1 ~length:2 (ems, h, 0x300, 0) (ems, h, 0x301, 0))
    exchange_overlap;
  check_l "5701h refused, nothing moved" (bytes_at t (frame 2 + 0x300) 2) [ 0x11; 0x22 ];
  check "5701h conventional overlap" (move t ~al:1 ~length:2 (conv, 0, 0x800, buf) (conv, 0, 0x801, buf))
    exchange_overlap;
  check "5701h one handle, apart" (move t ~al:1 ~length:2 (ems, h, 0x300, 0) (ems, h, 0x400, 0)) ok;
  (* the conventional side lying on the frame where the expanded side is shown *)
  check "5700h conventional on the shown page"
    (move t ~length:1 (conv, 0, 0x10, Dos_ems.page_frame) (ems, h, 0x10, 1)) conventional_overlaps_expanded;
  check "5700h conventional on the shown page, other bytes"
    (move t ~length:1 (conv, 0, 0x0F, Dos_ems.page_frame) (ems, h, 0x10, 1)) ok;
  (* window 2 shows page 0 of h, not of h2 *)
  check "5700h conventional on another handle's shown page"
    (move t ~length:1 (conv, 0, 0x10, Dos_ems.page_frame + (2 * 0x400)) (ems, h2, 0x10, 0)) ok;
  check "5700h zero bytes" (move t ~length:0 (conv, 0, 0x10, Dos_ems.page_frame) (ems, h, 0x10, 1)) ok;
  (* refusals, in the order this driver checks *)
  check "5702h" (move t ~al:2 ~length:1 (conv, 0, 0, buf) (conv, 0, 0, buf)) invalid_subfunction;
  check "5700h longer than 1MB" (move t ~length:0x100001 (2, 0, 0, buf) (conv, 0, 0, buf)) longer_than_1mb;
  check "5700h type 2" (move t ~length:1 (conv, 0, 0, buf) (2, 0x77, 0, 0)) undefined_memory_type;
  check "5700h type 2 before the other side's handle"
    (move t ~length:1 (ems, 0x77, 0, 0) (2, 0, 0, 0)) undefined_memory_type;
  check "5700h unknown handle" (move t ~length:1 (ems, 0x77, 0, 0) (conv, 0, 0, buf)) invalid_handle;
  check "5700h offset 4000h" (move t ~length:1 (conv, 0, 0, buf) (ems, h, 0x4000, 0)) offset_past_page;
  check "5700h first page past the handle" (move t ~length:1 (conv, 0, 0, buf) (ems, h, 0, 4))
    logical_out_of_range;
  check "5700h end past the handle" (move t ~length:2 (ems, h, 0x3FFF, 3) (conv, 0, 0, buf))
    past_handle_pages;
  check "5700h to the handle's last byte" (move t ~length:1 (ems, h, 0x3FFF, 3) (conv, 0, 0, buf)) ok;
  check "5700h past 1MB" (move t ~length:1 (conv, 0, 0x10, 0xFFFF) (ems, h, 0, 0)) wraps_1mb;
  check "5700h to the last byte under 1MB" (move t ~length:1 (conv, 0, 0x0F, 0xFFFF) (ems, h, 0, 0)) ok

(* In mode 0Dh the A0000h window is the EGA planes: a move to it goes
   through the graphics controller, and a move from it reads plane 0. *)
let () =
  let t = Dos_machine.create () in
  let h = allocate t 1 in
  Dos_video.set_mode t.video 0x0D ~clear:true;
  let window = 0xA000 * 16 in
  put t (at 0x500) [ 0xC3 ];
  check "into expanded" (move t ~length:1 (conv, 0, 0x500, buf) (ems, h, 0, 0)) ok;
  check "5700h to A000:0000" (move t ~length:1 (ems, h, 0, 0) (conv, 0, 0, 0xA000)) ok;
  Array.iteri
    (fun i plane -> check (Printf.sprintf "plane %d took the byte" i) (Char.code (Bytes.get plane 0)) 0xC3)
    (Dos_video.planes t.video);
  check "the RAM under the window did not" (peek t window) 0;
  Bytes.set (Dos_video.planes t.video).(0) 1 '\x5D';
  check "5700h from A000:0001" (move t ~length:1 (conv, 0, 1, 0xA000) (ems, h, 1, 0)) ok;
  map t ~phys:0 ~handle:h ~log:0;
  check "plane 0's byte reached the page" (peek t (frame 0 + 1)) 0x5D

(* ---------- Functions 19-21: attributes and names ---------- *)

let name_at = 0x900
let dir_at = 0xA00

let set_name t h raw =
  put t (at name_at) (List.init 8 (fun i -> if i < String.length raw then Char.code raw.[i] else 0));
  call t 0x53 ~al:1 ~dx:h ~ds:buf ~si:name_at

let find t raw =
  put t (at name_at) (List.init 8 (fun i -> if i < String.length raw then Char.code raw.[i] else 0));
  call t 0x54 ~al:1 ~ds:buf ~si:name_at

let name_bytes_of t a = String.init 8 (fun i -> Char.chr (peek t (a + i)))
let al t = Cpu86.reg8 t.cpu r_ax

let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %S want %S\n%!" name got want
  end

let () =
  let t = Dos_machine.create () in
  let h1 = allocate t 1 in
  let h2 = allocate t 1 in
  (* Function 19: only volatile handles *)
  check "5200h" (call t 0x52 ~al:0 ~dx:h1) ok;
  check "5200h volatile" (al t) 0;
  check "5200h unknown handle" (call t 0x52 ~al:0 ~dx:0x77) invalid_handle;
  check "5201h volatile" (call t 0x52 ~al:1 ~dx:h1 ~bx:0) ok;
  check "5201h non-volatile" (call t 0x52 ~al:1 ~dx:h1 ~bx:1) not_supported;
  check "5201h attribute 2" (call t 0x52 ~al:1 ~dx:h1 ~bx:2) undefined_attribute;
  check "5201h unknown handle" (call t 0x52 ~al:1 ~dx:0x77 ~bx:0) invalid_handle;
  check "5202h" (call t 0x52 ~al:2) ok;
  check "5202h volatile only" (al t) 0;
  check "5203h" (call t 0x52 ~al:3) invalid_subfunction;
  (* Function 20 *)
  check "5301h" (set_name t h1 "SOUND") ok;
  check "5300h" (call t 0x53 ~al:0 ~dx:h1 ~es:buf ~di:0x980) ok;
  check_s "5300h name" (name_bytes_of t (at 0x980)) "SOUND\000\000\000";
  check "5301h the same eight bytes on another handle" (set_name t h2 "SOUND") name_taken_or_null;
  check "5301h its own name again" (set_name t h1 "SOUND") ok;
  (* every byte is part of the name, NULs too (00h-FFh) *)
  check "5301h bytes after a NUL make another name" (set_name t h2 "SOUND\000xy") ok;
  check "5300h the bytes after the NUL stay" (call t 0x53 ~al:0 ~dx:h2 ~es:buf ~di:0x980) ok;
  check_s "5300h as given" (name_bytes_of t (at 0x980)) "SOUND\000xy";
  check "5301h a leading NUL is still a name" (set_name t h2 "\000MUSIC") ok;
  check "5401h a name with a leading NUL" (find t "\000MUSIC") ok;
  check "5401h a name with a leading NUL: the handle" (dx t) h2;
  check "5301h another name" (set_name t h2 "MUSIC") ok;
  check "5301h eight bytes" (set_name t Dos_ems.os_handle "OSHANDLE") ok;
  check "5300h unknown handle" (call t 0x53 ~al:0 ~dx:0x77 ~es:buf ~di:0x980) invalid_handle;
  check "5301h unknown handle" (set_name t 0x77 "X") invalid_handle;
  check "5302h" (call t 0x53 ~al:2) invalid_subfunction;
  (* Function 21 *)
  check "5400h" (call t 0x54 ~al:0 ~es:buf ~di:dir_at) ok;
  check "5400h entries" (al t) 3;
  let entry i = (peek t (at (dir_at + (10 * i))) lor (peek t (at (dir_at + (10 * i) + 1)) lsl 8),
                 name_bytes_of t (at (dir_at + (10 * i) + 2))) in
  check_b "5400h directory"
    ([ entry 0; entry 1; entry 2 ]
     = [ (0, "OSHANDLE"); (h1, "SOUND\000\000\000"); (h2, "MUSIC\000\000\000") ]);
  check "5401h" (find t "MUSIC") ok;
  check "5401h handle" (dx t) h2;
  check "5401h bytes after the NUL count" (find t "MUSIC\000zz") name_not_found;
  check "5401h not there" (find t "NOPE") name_not_found;
  check "5401h no name" (find t "") name_taken_or_null;
  check "5402h" (call t 0x54 ~al:2) ok;
  check "5402h handles with the OS handle" (bx t) 255;
  check "5403h" (call t 0x54 ~al:3) invalid_subfunction;
  (* an all-NUL name takes the name off *)
  check "5301h no name" (set_name t h2 "") ok;
  check "5301h no name on a second handle" (set_name t (allocate t 1) "") ok;
  check "5401h the taken-off name" (find t "MUSIC") name_not_found;
  check "5301h a taken-off name can be given again" (set_name t h2 "MUSIC") ok;
  (* a snapshot keeps the names *)
  let t =
    match Dos_snapshot.save t with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok t -> t
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  check "5401h after a snapshot" (find t "SOUND") ok;
  check "5401h after a snapshot: the handle" (dx t) h1;
  (* releasing a handle takes its name off (Function 6); the OS handle,
     reallocated to zero pages instead, keeps it *)
  check "45h" (call t 0x45 ~dx:h1) ok;
  check "5401h a released handle's name" (find t "SOUND") name_not_found;
  check "the number comes back" (allocate t 1) h1;
  check "5300h the new handle" (call t 0x53 ~al:0 ~dx:h1 ~es:buf ~di:0x980) ok;
  check_s "5300h no name" (name_bytes_of t (at 0x980)) (String.make 8 '\000');
  check "45h on the OS handle" (call t 0x45 ~dx:Dos_ems.os_handle) ok;
  check "5401h the OS handle's name" (find t "OSHANDLE") ok;
  (* names a snapshot refuses *)
  let refused what =
    match Dos_snapshot.save t with
    | Ok _ -> check (what ^ ": save refuses it") 0 1
    | Error _ -> ()
  in
  let names = Hashtbl.copy t.ems_names in
  let restore () = Hashtbl.reset t.ems_names; Hashtbl.iter (Hashtbl.replace t.ems_names) names in
  Hashtbl.replace t.ems_names h1 "MUSIC\000\000\000";
  refused "two handles with one name";
  restore ();
  Hashtbl.replace t.ems_names h1 "NINEBYTES";
  refused "a name longer than eight bytes";
  restore ();
  Hashtbl.replace t.ems_names h1 (String.make 8 '\000');
  refused "a stored empty name";
  restore ();
  Hashtbl.replace t.ems_names 0x77 "GHOST\000\000\000";
  refused "a name on a handle that is not open";
  restore ();
  Hashtbl.replace t.ems_names h1 "SHORT";
  refused "a name shorter than eight bytes";
  restore ();
  check_b "the names as they were" (match Dos_snapshot.save t with Ok _ -> true | Error _ -> false)

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "EMS handle functions: all passed\n"
