(* LIM EMS 4.0 functions that pass arrays in guest memory: 4Eh (page map),
   4Fh (partial page map), 50h (map several pages), 59h (hardware
   information). Registers are set the way INT 67h leaves them and
   [Dos_ems.service] is called; the guest path through the ROM stub is
   dos_ems_test's. Expected values are the spec's (LIM EMS 4.0, 1987, by
   Function number). *)

open Dos_state

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

let check_b name got = check name (Bool.to_int got) 1

(* Cpu86 register numbers and the spec's status codes. *)
let r_ax = 0
let r_cx = 1
let r_dx = 2
let r_bx = 3
let r_si = 6
let r_di = 7
let s_es = 0
let s_ds = 3
let ok = 0x00
let invalid_handle = 0x83
let logical_out_of_range = 0x8A
let physical_out_of_range = 0x8B
let invalid_subfunction = 0x8F
let corrupt_map = 0xA3
let empty = 0xFFFF
let total = 2048

(* This driver's map array: per physical page, handle and logical page. *)
let map_bytes = 16

let buf = 0x2000                                 (* guest segment for arrays *)

let call t ?(al = 0) ?(bx = 0) ?(cx = 0) ?(dx = 0) ?(si = 0) ?(di = 0) ah =
  let cpu = t.cpu in
  Cpu86.set_reg16 cpu r_ax ((ah lsl 8) lor al);
  Cpu86.set_reg16 cpu r_bx bx;
  Cpu86.set_reg16 cpu r_cx cx;
  Cpu86.set_reg16 cpu r_dx dx;
  Cpu86.set_seg cpu s_ds buf;
  Cpu86.set_reg16 cpu r_si si;
  Cpu86.set_seg cpu s_es buf;
  Cpu86.set_reg16 cpu r_di di;
  Dos_ems.service t;
  Cpu86.reg16 cpu r_ax lsr 8

let al t = Cpu86.reg8 t.cpu r_ax
let bx t = Cpu86.reg16 t.cpu r_bx
let dx t = Cpu86.reg16 t.cpu r_dx
let frame p = (Dos_ems.page_frame * 16) + (p * Dos_ems.page_bytes)
let at off = (buf * 16) + off
let word t off = Dos_machine.mem_read t (at off) lor (Dos_machine.mem_read t (at off + 1) lsl 8)
let words t off n = List.init n (fun i -> word t (off + (2 * i)))
let set_words t off ws = List.iteri (fun i w -> wr16 t (at (off + (2 * i))) w) ws

let fresh pages =
  let t = Dos_machine.create () in
  check "allocate" (call t 0x43 ~bx:pages) ok;
  (t, dx t)

let map t ~phys ~handle ~log =
  check (Printf.sprintf "map %d:%d at %d" handle log phys) (call t 0x44 ~al:phys ~bx:log ~dx:handle) ok

let mapped t = Array.copy t.ems_mapped

(* Function 15: get, set, get and set, size. *)
let () =
  let t, h = fresh 3 in
  check "4E03h" (call t 0x4E ~al:3) ok;
  check "4E03h size" (al t) map_bytes;
  map t ~phys:0 ~handle:h ~log:0;
  map t ~phys:3 ~handle:h ~log:2;
  wr8 t (frame 0) 0xA5;
  check "4E00h" (call t 0x4E ~al:0 ~di:0x100) ok;
  check_b "4E00h array"
    (words t 0x100 8 = [ h; 0; empty; empty; empty; empty; h; 2 ]);
  map t ~phys:0 ~handle:h ~log:1;
  check "unmap 3" (call t 0x44 ~al:3 ~bx:empty ~dx:h) ok;
  check "4E01h" (call t 0x4E ~al:1 ~si:0x100) ok;
  check_b "4E01h put the map back" (t.ems_mapped = [| Some (h, 0); None; None; Some (h, 2) |]);
  check "4E01h put the page's bytes back" (Dos_machine.mem_read t (frame 0)) 0xA5;
  (* get and set at once: the old map goes out, the new one comes in *)
  set_words t 0x200 [ h; 1; h; 2; empty; empty; empty; empty ];
  check "4E02h" (call t 0x4E ~al:2 ~si:0x200 ~di:0x300) ok;
  check_b "4E02h wrote the old map" (words t 0x300 8 = [ h; 0; empty; empty; empty; empty; h; 2 ]);
  check_b "4E02h set the new map" (t.ems_mapped = [| Some (h, 1); Some (h, 2); None; None |]);
  check "4E04h" (call t 0x4E ~al:4) invalid_subfunction;
  (* an array that runs past offset FFFFh wraps inside its segment *)
  check "4E00h at DI=FFFCh" (call t 0x4E ~al:0 ~di:0xFFFC) ok;
  check_b "4E00h wrapped" (words t 0xFFFC 2 @ words t 0 6 = [ h; 1; h; 2; empty; empty; empty; empty ])

(* A3h: an array naming a handle or page that is not there changes nothing,
   even when the bad entry comes last. *)
let () =
  let t, h = fresh 2 in
  map t ~phys:0 ~handle:h ~log:0;
  let before = mapped t in
  let refused name ws =
    set_words t 0x100 ws;
    check name (call t 0x4E ~al:1 ~si:0x100) corrupt_map;
    check_b (name ^ ": nothing changed") (t.ems_mapped = before)
  in
  refused "4E01h unknown handle last" [ h; 1; empty; empty; empty; empty; 0x77; 0 ];
  refused "4E01h page past the handle" [ h; 9; empty; empty; empty; empty; empty; empty ];
  refused "4E01h half-empty entry" [ h; empty; empty; empty; empty; empty; empty; empty ];
  refused "4E01h empty handle, real page" [ empty; 0; empty; empty; empty; empty; empty; empty ]

(* Function 16: the partial array names mappable segments. *)
let () =
  let t, h = fresh 3 in
  check "4F02h" (call t 0x4F ~al:2 ~bx:2) ok;
  check "4F02h size" (al t) (2 + (2 * 6));
  check "4F02h past the frame" (call t 0x4F ~al:2 ~bx:5) physical_out_of_range;
  map t ~phys:1 ~handle:h ~log:1;
  map t ~phys:3 ~handle:h ~log:2;
  map t ~phys:0 ~handle:h ~log:0;
  set_words t 0x100 [ 2; 0xD400; 0xDC00 ];
  check "4F00h" (call t 0x4F ~al:0 ~si:0x100 ~di:0x200) ok;
  check_b "4F00h array" (words t 0x200 7 = [ 2; 1; h; 1; 3; h; 2 ]);
  check "unmap 1" (call t 0x44 ~al:1 ~bx:empty ~dx:h) ok;
  map t ~phys:3 ~handle:h ~log:0;
  map t ~phys:0 ~handle:h ~log:2;
  check "4F01h" (call t 0x4F ~al:1 ~si:0x200) ok;
  check_b "4F01h restored only pages 1 and 3"
    (t.ems_mapped = [| Some (h, 2); Some (h, 1); None; Some (h, 2) |]);
  (* an empty page in a partial array restores as empty *)
  set_words t 0x100 [ 1; 0xD800 ];
  check "4F00h an empty page" (call t 0x4F ~al:0 ~si:0x100 ~di:0x300) ok;
  map t ~phys:2 ~handle:h ~log:0;
  check "4F01h an empty page" (call t 0x4F ~al:1 ~si:0x300) ok;
  check_b "4F01h emptied page 2" (t.ems_mapped.(2) = None);
  set_words t 0x100 [ 1; 0xD200 ];
  check "4F00h a segment between pages" (call t 0x4F ~al:0 ~si:0x100 ~di:0x300) physical_out_of_range;
  set_words t 0x100 [ 1; 0xE000 ];
  check "4F00h the segment after the frame" (call t 0x4F ~al:0 ~si:0x100 ~di:0x300)
    physical_out_of_range;
  set_words t 0x100 [ 5; 0xD000; 0xD400; 0xD800; 0xDC00; 0xD000 ];
  check "4F00h more segments than pages" (call t 0x4F ~al:0 ~si:0x100 ~di:0x300) corrupt_map;
  let before = mapped t in
  set_words t 0x300 [ 1; 4; h; 0 ];
  check "4F01h page 4" (call t 0x4F ~al:1 ~si:0x300) corrupt_map;
  (* five entries, each one valid: only the count is wrong *)
  set_words t 0x300 [ 5; 0; h; 0; 1; h; 0; 2; h; 0; 3; h; 0; 0; h; 1 ];
  check "4F01h five entries" (call t 0x4F ~al:1 ~si:0x300) corrupt_map;
  set_words t 0x300 [ 2; 0; h; 1; 1; 0x77; 0 ];
  check "4F01h unknown handle second" (call t 0x4F ~al:1 ~si:0x300) corrupt_map;
  check_b "4F01h refused arrays changed nothing" (t.ems_mapped = before);
  check "4F03h" (call t 0x4F ~al:3) invalid_subfunction

(* Function 17: map in order, stop at the first bad entry and keep what
   came before it. *)
let () =
  let t, h = fresh 3 in
  set_words t 0x100 [ 0; 2; 1; 1 ];
  check "5000h" (call t 0x50 ~al:0 ~dx:h ~cx:2 ~si:0x100) ok;
  check_b "5000h mapped two" (t.ems_mapped = [| None; Some (h, 1); Some (h, 0); None |]);
  set_words t 0x100 [ 2; 0; 9; 1; 0; 3 ];
  check "5000h bad page second" (call t 0x50 ~al:0 ~dx:h ~cx:3 ~si:0x100) logical_out_of_range;
  check_b "5000h kept the first, not the third"
    (t.ems_mapped = [| Some (h, 2); Some (h, 1); Some (h, 0); None |]);
  set_words t 0x100 [ 0; 3; empty; 0 ];
  check "5000h with an unmap" (call t 0x50 ~al:0 ~dx:h ~cx:2 ~si:0x100) ok;
  check_b "5000h unmapped page 0" (t.ems_mapped = [| None; Some (h, 1); Some (h, 0); Some (h, 0) |]);
  set_words t 0x100 [ 0; 4 ];
  check "5000h physical page 4" (call t 0x50 ~al:0 ~dx:h ~cx:1 ~si:0x100) physical_out_of_range;
  let before = mapped t in
  set_words t 0x100 [ 0; 0; 0; 1; 0; 2; 0; 3; 1; 0 ];
  check "5000h five entries" (call t 0x50 ~al:0 ~dx:h ~cx:5 ~si:0x100) physical_out_of_range;
  check_b "5000h five entries mapped nothing" (t.ems_mapped = before);
  set_words t 0x100 [ 0; 0 ];
  check "5000h unknown handle" (call t 0x50 ~al:0 ~dx:0x77 ~cx:1 ~si:0x100) invalid_handle;
  check "5000h no entries" (call t 0x50 ~al:0 ~dx:h ~cx:0 ~si:0x100) ok;
  set_words t 0x100 [ 2; 0xD800 ];
  check "5001h" (call t 0x50 ~al:1 ~dx:h ~cx:1 ~si:0x100) ok;
  check_b "5001h mapped at D800h" (t.ems_mapped.(2) = Some (h, 2));
  set_words t 0x100 [ 1; 0xD900 ];
  check "5001h a segment between pages" (call t 0x50 ~al:1 ~dx:h ~cx:1 ~si:0x100)
    physical_out_of_range;
  set_words t 0x100 [ 1; 0xCC00 ];
  check "5001h the segment before the frame" (call t 0x50 ~al:1 ~dx:h ~cx:1 ~si:0x100)
    physical_out_of_range;
  check "5002h" (call t 0x50 ~al:2 ~dx:h) invalid_subfunction

(* Function 26. *)
let () =
  let t, _ = fresh 3 in
  check "5900h" (call t 0x59 ~al:0 ~di:0x100) ok;
  check_b "5900h array" (words t 0x100 5 = [ 0x400; 0; map_bytes; 0; 0 ]);
  check "5901h" (call t 0x59 ~al:1) ok;
  check "5901h BX = free raw pages" (bx t) (total - 3);
  check "5901h DX = total raw pages" (dx t) total;
  check "5902h" (call t 0x59 ~al:2) invalid_subfunction

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "EMS map arrays: all passed\n"
