(* XMS 3.0 driver: the control function's registers, the extended memory
   allocator and moves. Expected answers come from the XMS 3.0
   specification, HimemX and DOSBox-staging xms.cpp (see dos_xms.mli). *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

(* Register numbers: 16-bit AX CX DX BX SP BP SI DI, 8-bit AL CL DL BL AH
   CH DH BH, segments ES CS SS DS. *)
let ax = 0 and dx = 2 and bx = 3 and si = 6
let bl = 3 and ah = 4 and bh = 7
let es = 0 and ds = 3

type rig = { x : Dos_xms.t; cpu : Cpu86.t; mem : Bytes.t }

let rig () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let read a = Char.code (Bytes.get mem (a land 0xFFFFF)) in
  let write a v = Bytes.set mem (a land 0xFFFFF) (Char.chr (v land 0xFF)) in
  let cpu =
    Cpu86.create ~model:Cpu86.I80286 ~read ~write ~port_in:(fun _ -> 0xFF)
      ~port_out:(fun _ _ -> ()) ()
  in
  { x = Dos_xms.create (); cpu; mem }

let read r a = Char.code (Bytes.get r.mem a)
let write r a v = Bytes.set r.mem a (Char.chr (v land 0xFF))

(* Far-calls the control function with AH = [fn] and the given 16-bit
   registers, after filling every register with a marker. *)
let marker = 0x5A5A

let call r fn regs =
  List.iter (fun reg -> Cpu86.set_reg16 r.cpu reg marker) [ ax; 1; dx; bx; 5; si; 7 ];
  List.iter (fun (reg, v) -> Cpu86.set_reg16 r.cpu reg v) regs;
  Cpu86.set_reg8 r.cpu ah fn;
  Dos_xms.control r.x r.cpu ~read:(read r) ~write:(write r)

let reg r n = Cpu86.reg16 r.cpu n
let reg8 r n = Cpu86.reg8 r.cpu n

let ok r name = check_int (name ^ ": AX=1") (reg r ax) 1
let err r name code =
  check_int (name ^ ": AX=0") (reg r ax) 0;
  check_int (name ^ ": BL") (reg8 r bl) code

let alloc r kb =
  call r 0x09 [ (dx, kb) ];
  ok r (Printf.sprintf "allocate %d KB" kb);
  reg r dx

let lock_address r h =
  call r 0x0C [ (dx, h) ];
  ok r "lock";
  (reg r dx lsl 16) lor reg r bx

let unlock r h = call r 0x0D [ (dx, h) ]; ok r "unlock"

let free_kb r = call r 0x08 []; (reg r ax, reg r dx, reg8 r bl)

let test_version () =
  let r = rig () in
  call r 0x00 [];
  check_int "version 3.00" (reg r ax) 0x0300;
  check_int "driver revision" (reg r bx) 0x0301;
  check_int "no HMA" (reg r dx) 0

let test_multiplex () =
  let r = rig () in
  let entry = (0xF000, 0x0160) in
  Cpu86.set_reg16 r.cpu ax 0x4300;
  Cpu86.set_reg16 r.cpu bx 0x1234;
  Dos_xms.multiplex r.cpu ~entry;
  check_int "4300h: AL=80h, AH kept" (reg r ax) 0x4380;
  check_int "4300h: BX kept" (reg r bx) 0x1234;
  Cpu86.set_reg16 r.cpu ax 0x4310;
  Dos_xms.multiplex r.cpu ~entry;
  check_int "4310h: ES" (Cpu86.seg r.cpu es) 0xF000;
  check_int "4310h: BX" (reg r bx) 0x0160;
  check_int "4310h: AX kept" (reg r ax) 0x4310;
  Cpu86.set_seg r.cpu es 0x1111;
  Cpu86.set_reg16 r.cpu bx 0x2222;
  List.iter
    (fun v ->
      Cpu86.set_reg16 r.cpu ax v;
      Dos_xms.multiplex r.cpu ~entry;
      check_int (Printf.sprintf "%04Xh: AX kept" v) (reg r ax) v;
      check_int (Printf.sprintf "%04Xh: BX kept" v) (reg r bx) 0x2222;
      check_int (Printf.sprintf "%04Xh: ES kept" v) (Cpu86.seg r.cpu es) 0x1111)
    [ 0x4301; 0x4311; 0x1600; 0x4400 ]

let test_hma_and_a20 () =
  let r = rig () in
  List.iter
    (fun (fn, code, name) ->
      call r fn [ (bx, 0x77FF) ];
      err r name code;
      check_int (name ^ ": BH kept") (reg8 r bh) 0x77)
    [ (0x01, 0x90, "01h request HMA"); (0x02, 0x90, "02h release HMA");
      (0x03, 0x82, "03h global enable"); (0x05, 0x82, "05h local enable");
      (0x06, 0x82, "06h local disable with none outstanding") ];
  call r 0x04 [];
  ok r "04h global disable";
  check_int "04h: BL=0" (reg8 r bl) 0;
  call r 0x07 [];
  check_int "07h: A20 off" (reg r ax) 0;
  check_int "07h: BL=0" (reg8 r bl) 0

let test_unimplemented () =
  let r = rig () in
  List.iter
    (fun fn -> call r fn []; err r (Printf.sprintf "%02Xh" fn) 0x80)
    [ 0x10; 0x11; 0x12; 0x13; 0x88; 0x89; 0x8E; 0x8F; 0xFF ]

let test_allocate () =
  let r = rig () in
  let cap = Dos_xms.capacity_kb in
  let largest, total, code = free_kb r in
  check_int "all free: 15 MB is the largest block" largest 15360;
  check_int "all free: 15 MB in all" total 15360;
  check_int "all free: BL" code 0;
  let h = alloc r 100 in
  check_int "first handle" h 1;
  check_int "allocate: BL=0" (reg8 r bl) 0;
  let largest, total, _ = free_kb r in
  check_int "after 100 KB: largest" largest (cap - 100);
  check_int "after 100 KB: total" total (cap - 100);
  let h0 = alloc r 0 in
  check_int "0 KB block gets a handle" h0 2;
  call r 0x09 [ (dx, cap) ];
  err r "more than is free" 0xA0;
  check_int "failed allocate: DX=0" (reg r dx) 0;
  ignore (alloc r (cap - 100));
  let largest, total, code = free_kb r in
  check_int "full: largest" largest 0;
  check_int "full: total" total 0;
  check_int "full: BL=A0h" code 0xA0;
  check_int "0 KB still fits when full" (alloc r 0) 4

let test_handles_run_out () =
  let r = rig () in
  for i = 1 to 32 do check_int "handle number" (alloc r 0) i done;
  call r 0x09 [ (dx, 1) ];
  err r "33rd handle" 0xA1;
  call r 0x0A [ (dx, 7) ];
  ok r "free handle 7";
  check_int "the freed handle is reused" (alloc r 1) 7;
  let r = rig () in
  ignore (alloc r Dos_xms.capacity_kb);
  for _ = 2 to Dos_xms.handle_count do ignore (alloc r 0) done;
  call r 0x09 [ (dx, 1) ];
  err r "no handle and no memory: the handle is checked first" 0xA1

let test_holes () =
  let r = rig () in
  let cap = Dos_xms.capacity_kb in
  let a = alloc r 100 in
  let b = alloc r 100 in
  let c = alloc r 100 in
  check_int "c sits after a and b" (lock_address r c) (0x100000 + (200 * 1024));
  unlock r c;
  call r 0x0A [ (dx, b) ];
  ok r "free the middle block";
  let largest, total, _ = free_kb r in
  check_int "hole: total" total (cap - 200);
  check_int "hole: largest is the tail" largest (cap - 300);
  let d = alloc r 50 in
  check_int "first fit lands in the hole" (lock_address r d) (0x100000 + (100 * 1024));
  check_int "a is at 100000h" (lock_address r a) 0x100000

let test_locks () =
  let r = rig () in
  let h = alloc r 4 in
  ignore (lock_address r h);
  ignore (lock_address r h);
  call r 0x0E [ (dx, h) ];
  ok r "info";
  check_int "info: lock count" (reg8 r bh) 2;
  check_int "info: free handles" (reg8 r bl) (Dos_xms.handle_count - 1);
  check_int "info: size" (reg r dx) 4;
  call r 0x0A [ (dx, h) ];
  err r "free a locked block" 0xAB;
  call r 0x0F [ (bx, 8); (dx, h) ];
  err r "resize a locked block" 0xAB;
  unlock r h;
  unlock r h;
  call r 0x0D [ (dx, h) ];
  err r "unlock an unlocked block" 0xAA;
  for _ = 1 to 255 do ignore (lock_address r h) done;
  call r 0x0C [ (dx, h) ];
  err r "256th lock" 0xAC;
  List.iter
    (fun (fn, name) ->
      List.iter
        (fun bad ->
          call r fn [ (dx, bad); (bx, 1) ];
          err r (Printf.sprintf "%s handle %d" name bad) 0xA2)
        [ 0; 2; Dos_xms.handle_count + 1; 0xFFFF ])
    [ (0x0A, "free"); (0x0C, "lock"); (0x0D, "unlock"); (0x0E, "info"); (0x0F, "resize") ]

(* The move structure at 0000:0600. *)
let struct_at = 0x600

let put16 r a v = write r a v; write r (a + 1) (v lsr 8)
let put32 r a v = put16 r a (v land 0xFFFF); put16 r (a + 2) (v lsr 16)

let conv seg off = (seg lsl 16) lor off

let do_move r ~length ~src_handle ~src ~dst_handle ~dst =
  put32 r struct_at length;
  put16 r (struct_at + 4) src_handle;
  put32 r (struct_at + 6) src;
  put16 r (struct_at + 10) dst_handle;
  put32 r (struct_at + 12) dst;
  Cpu86.set_seg r.cpu ds 0;
  call r 0x0B [ (si, struct_at); (bx, 0x0077) ]

let test_move () =
  let r = rig () in
  let h = alloc r 2 in
  for i = 0 to 9 do write r (0x2010 + i) (0x30 + i) done;
  do_move r ~length:10 ~src_handle:0 ~src:(conv 0x0200 0x0010) ~dst_handle:h ~dst:6;
  ok r "conventional to block";
  check_int "move success leaves BL" (reg8 r bl) 0x77;
  do_move r ~length:10 ~src_handle:h ~src:6 ~dst_handle:0 ~dst:(conv 0x0300 0x0000);
  ok r "block to conventional";
  check "round trip" (List.for_all (fun i -> read r (0x3000 + i) = 0x30 + i) (List.init 10 Fun.id));
  do_move r ~length:4 ~src_handle:h ~src:6 ~dst_handle:h ~dst:2;
  ok r "within a block, source above destination";
  do_move r ~length:4 ~src_handle:h ~src:2 ~dst_handle:0 ~dst:(conv 0x0400 0);
  check "the block copy went forward"
    (List.map (fun i -> read r (0x4000 + i)) [ 0; 1; 2; 3 ] = [ 0x30; 0x31; 0x32; 0x33 ]);
  do_move r ~length:0 ~src_handle:h ~src:0 ~dst_handle:0 ~dst:(conv 0 0);
  ok r "zero length";
  do_move r ~length:2048 ~src_handle:h ~src:0 ~dst_handle:0 ~dst:(conv 0x0500 0);
  ok r "the whole block";
  check_int "new block memory reads 0" (read r 0x5000) 0;
  check_int "the copied bytes land at their offset" (read r 0x5006) 0x30

let test_move_errors () =
  let r = rig () in
  let h = alloc r 1 in
  write r 0x2002 0xEE;
  let e name code = err r name code in
  do_move r ~length:3 ~src_handle:9 ~src:0 ~dst_handle:h ~dst:0;
  e "odd length before a bad handle" 0xA7;
  do_move r ~length:2 ~src_handle:9 ~src:0 ~dst_handle:9 ~dst:0;
  e "bad source before bad destination" 0xA3;
  do_move r ~length:2 ~src_handle:h ~src:1024 ~dst_handle:0 ~dst:(conv 0x200 0);
  e "source offset at the end" 0xA4;
  do_move r ~length:4 ~src_handle:h ~src:1022 ~dst_handle:0 ~dst:(conv 0x200 0);
  e "source runs past its block" 0xA7;
  do_move r ~length:2 ~src_handle:0 ~src:(conv 0x200 0) ~dst_handle:5 ~dst:0;
  e "bad destination handle" 0xA5;
  do_move r ~length:2 ~src_handle:0 ~src:(conv 0x200 0) ~dst_handle:h ~dst:2048;
  e "destination offset past the end" 0xA6;
  do_move r ~length:4 ~src_handle:0 ~src:(conv 0x200 0) ~dst_handle:h ~dst:1022;
  e "destination runs past its block" 0xA7;
  do_move r ~length:2 ~src_handle:0 ~src:(conv 0xFFFF 0x000E) ~dst_handle:h ~dst:0;
  ok r "a range that ends at 100000h";
  do_move r ~length:2 ~src_handle:0 ~src:(conv 0xFFFF 0x0010) ~dst_handle:h ~dst:0;
  e "a range past 100000h" 0xA7;
  do_move r ~length:4 ~src_handle:0 ~src:(conv 0x200 0) ~dst_handle:0 ~dst:(conv 0x200 2);
  e "source below an overlapping destination" 0xA8;
  check_int "a refused move copies nothing" (read r 0x2002) 0xEE;
  write r 0x2000 0x11; write r 0x2001 0x22; write r 0x2002 0x33; write r 0x2003 0x44;
  do_move r ~length:4 ~src_handle:0 ~src:(conv 0x200 2) ~dst_handle:0 ~dst:(conv 0x200 0);
  ok r "source above an overlapping destination";
  check "it copied forward"
    (List.map (fun i -> read r (0x2000 + i)) [ 0; 1 ] = [ 0x33; 0x44 ]);
  do_move r ~length:2 ~src_handle:0 ~src:(conv 0x200 0) ~dst_handle:0 ~dst:(conv 0x200 0);
  ok r "source equal to destination";
  do_move r ~length:2 ~src_handle:0 ~src:(conv 0x200 0) ~dst_handle:0 ~dst:(conv 0x200 2);
  ok r "ranges that only touch";
  do_move r ~length:4 ~src_handle:h ~src:2 ~dst_handle:h ~dst:4;
  e "within a block, source below an overlapping destination" 0xA8

(* Lengths and offsets are dwords: 64 KB and more need the high word. *)
let test_long_move () =
  let r = rig () in
  let h = alloc r 128 in
  write r 0x1FFFF 0xC3;
  do_move r ~length:0x10000 ~src_handle:0 ~src:(conv 0x1000 0) ~dst_handle:h ~dst:0x10000;
  ok r "64 KB into the upper half";
  do_move r ~length:2 ~src_handle:h ~src:0x1FFFE ~dst_handle:0 ~dst:(conv 0x3000 0);
  ok r "read the last word back";
  check_int "the 65536th byte arrived at offset 1FFFFh" (read r 0x30001) 0xC3

let read_block r h ~at ~len =
  do_move r ~length:len ~src_handle:h ~src:at ~dst_handle:0 ~dst:(conv 0x7000 0);
  ok r "read back";
  List.init len (fun i -> read r (0x70000 + i))

let fill_block r h ~at bytes =
  List.iteri (fun i v -> write r (0x60000 + i) v) bytes;
  do_move r ~length:(List.length bytes) ~src_handle:0 ~src:(conv 0x6000 0) ~dst_handle:h ~dst:at;
  ok r "fill"

let test_resize () =
  let r = rig () in
  let a = alloc r 10 in
  fill_block r a ~at:0 [ 1; 2; 3; 4 ];
  call r 0x0F [ (bx, 20); (dx, a) ];
  ok r "grow into the free tail";
  check_int "grown block stays" (lock_address r a) 0x100000;
  unlock r a;
  call r 0x0E [ (dx, a) ];
  check_int "new size" (reg r dx) 20;
  check "bytes survive growth" (read_block r a ~at:0 ~len:4 = [ 1; 2; 3; 4 ]);
  check "grown space reads 0" (read_block r a ~at:(10 * 1024) ~len:2 = [ 0; 0 ]);
  let b = alloc r 10 in
  check_int "b follows a" (lock_address r b) (0x100000 + (20 * 1024));
  unlock r b;
  call r 0x0F [ (bx, 30); (dx, a) ];
  ok r "grow past b";
  check_int "a moved past b" (lock_address r a) (0x100000 + (30 * 1024));
  unlock r a;
  check "bytes survive the move" (read_block r a ~at:0 ~len:4 = [ 1; 2; 3; 4 ]);
  call r 0x0F [ (bx, 2); (dx, a) ];
  ok r "shrink";
  check_int "shrunk block stays" (lock_address r a) (0x100000 + (30 * 1024));
  unlock r a;
  call r 0x0F [ (bx, 1); (dx, b) ];
  ok r "shrink b";
  check_int "b does not slide into the hole before it" (lock_address r b)
    (0x100000 + (20 * 1024));
  unlock r b;
  call r 0x0F [ (bx, Dos_xms.capacity_kb); (dx, a) ];
  err r "grow past all memory" 0xA0;
  call r 0x0E [ (dx, a) ];
  check_int "a failed resize keeps the size" (reg r dx) 2;
  let r = rig () in
  let x = alloc r 10 in
  let c = alloc r 10 in
  let d = alloc r 10 in
  call r 0x0A [ (dx, x) ];
  ok r "free the first block";
  ignore d;
  call r 0x0F [ (bx, 15); (dx, c) ];
  ok r "grow c into the hole before it";
  check_int "c grew down over its own space" (lock_address r c) 0x100000

let snapshot x =
  let w = Dos_snap_codec.writer () in
  Dos_xms.write_state w x;
  Dos_snap_codec.contents w

let restore s =
  let x = Dos_xms.create () in
  let rd = Dos_snap_codec.reader s in
  Dos_xms.read_state rd x;
  Dos_snap_codec.end_of_input rd;
  x

let test_snapshot () =
  let r = rig () in
  let a = alloc r 3 in
  let b = alloc r 5 in
  call r 0x0A [ (dx, a) ];
  ok r "free a";
  fill_block r b ~at:100 [ 9; 8; 7; 6 ];
  ignore (lock_address r b);
  let r2 = { r with x = restore (snapshot r.x) } in
  check "the restored driver saves the same bytes" (snapshot r2.x = snapshot r.x);
  call r2 0x0E [ (dx, b) ];
  check_int "restored lock count" (reg8 r2 bh) 1;
  check_int "restored size" (reg r2 dx) 5;
  check_int "restored place" (lock_address r2 b) (0x100000 + (3 * 1024));
  check "restored bytes" (read_block r2 b ~at:100 ~len:4 = [ 9; 8; 7; 6 ]);
  call r2 0x0E [ (dx, a) ];
  err r2 "a freed handle stays free" 0xA2

(* Hand-written snapshots the reader must refuse. *)
let crafted blocks =
  let w = Dos_snap_codec.writer () in
  let kb = Dos_snap_codec.range 0 Dos_xms.capacity_kb in
  for i = 0 to Dos_xms.handle_count - 1 do
    match List.nth_opt blocks i with
    | Some (base, size, bytes) ->
      Dos_snap_codec.put_bool w true;
      Dos_snap_codec.put w ~what:"base" kb base;
      Dos_snap_codec.put w ~what:"size" kb size;
      Dos_snap_codec.put w ~what:"locks" Dos_snap_codec.byte 0;
      Dos_snap_codec.put_bytes w (Bytes.make bytes '\000')
    | None -> Dos_snap_codec.put_bool w false
  done;
  Dos_snap_codec.contents w

let refused name s =
  match restore s with
  | _ -> check (name ^ " is refused") false
  | exception Dos_snap_codec.Invalid _ -> ()

let test_crafted () =
  ignore (restore (crafted [ (0, 1, 1024); (1, 1, 1024) ]));
  refused "overlapping blocks" (crafted [ (0, 2, 2048); (1, 1, 1024) ]);
  refused "a block past the end" (crafted [ (Dos_xms.capacity_kb - 1, 2, 2048) ]);
  refused "bytes that are not the size" (crafted [ (0, 1, 1000) ])

let () =
  test_version ();
  test_multiplex ();
  test_hma_and_a20 ();
  test_unimplemented ();
  test_allocate ();
  test_handles_run_out ();
  test_holes ();
  test_locks ();
  test_move ();
  test_move_errors ();
  test_long_move ();
  test_resize ();
  test_snapshot ();
  test_crafted ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos XMS: all passed\n"
