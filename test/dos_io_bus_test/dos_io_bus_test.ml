(* DOS moves file bytes the way its own REP MOVS does: through the CPU's
   bus (#117). In mode 0Dh the A0000h window is the four EGA planes, so a
   file read into it lands in the planes and a file written from it takes
   the planes' bytes, not the RAM under the window. *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax v = "\xb8" ^ w v
let mov_ah n = "\xb4" ^ b n
let mov_dx v = "\xba" ^ w v
let mov_cx v = "\xb9" ^ w v
let int_ n = "\xcd" ^ b n
let mov_bx_ax = "\x89\xc3"
let push_ds = "\x1e"
let pop_ds = "\x1f"
let mov_ds_ax = "\x8e\xd8"
let mov_es_ax = "\x8e\xc0"
let store_ax m = "\xa3" ^ w m
let store_al m = "\xa2" ^ w m
let out_dx_ax = "\xef"
let load_al_es m = "\x26\xa0" ^ w m
let store_es_imm8 m v = "\x26\xc6\x06" ^ w m ^ b v
let quit = mov_ax 0x4C00 ^ int_ 0x21

let video_seg = 0xA000
let window = video_seg * 16
let gc_index = 0x3CE
let gc_read_map = 0x04

(* Data in the program's segment, 1000h. *)
let psp_base = 0x1000 * 16
let com_origin = 0x100
let pic_name = 0x2000
let out_name = 0x2010
let read_count = 0x2100
let plane3_byte = 0x2102
let write_count = 0x2104
let fcb_open = 0x2106
let fcb_read = 0x2107
let fcb = 0x2200
let fcb_dta = 0x0100
let stored_at = 0x0010
let picture = "\xaa\x55"
let stored = 0x77

(* AX = segment, then DS = it for one DOS call that takes DS:DX. *)
let with_video_ds body = push_ds ^ mov_ax video_seg ^ mov_ds_ax ^ body ^ pop_ds

let read_map plane = mov_dx gc_index ^ mov_ax ((plane lsl 8) lor gc_read_map) ^ out_dx_ax

let program =
  mov_ax 0x000D ^ int_ 0x10
  (* 3Fh: the file's two bytes into A000:0000, one read each, so the
     second starts at file position 1 *)
  ^ mov_dx pic_name ^ mov_ax 0x3D00 ^ int_ 0x21 ^ mov_bx_ax
  ^ with_video_ds (mov_dx 0 ^ mov_cx 1 ^ mov_ah 0x3F ^ int_ 0x21)
  ^ with_video_ds (mov_dx 1 ^ mov_cx 1 ^ mov_ah 0x3F ^ int_ 0x21)
  ^ store_ax read_count
  ^ mov_ah 0x3E ^ int_ 0x21
  ^ read_map 3 ^ mov_ax video_seg ^ mov_es_ax ^ load_al_es 0 ^ store_al plane3_byte
  ^ read_map 0
  (* 40h: a byte the CPU stored in the planes, written to a file *)
  ^ store_es_imm8 stored_at stored
  ^ mov_dx out_name ^ mov_cx 0 ^ mov_ah 0x3C ^ int_ 0x21 ^ mov_bx_ax
  ^ with_video_ds (mov_dx stored_at ^ mov_cx 1 ^ mov_ah 0x40 ^ int_ 0x21)
  ^ store_ax write_count
  ^ mov_ah 0x3E ^ int_ 0x21
  (* FCB 14h: a record into a DTA inside the window *)
  ^ with_video_ds (mov_dx fcb_dta ^ mov_ah 0x1A ^ int_ 0x21)
  ^ mov_dx fcb ^ mov_ah 0x0F ^ int_ 0x21 ^ store_al fcb_open
  ^ mov_dx fcb ^ mov_ah 0x14 ^ int_ 0x21 ^ store_al fcb_read
  ^ quit

let place image off s =
  Bytes.blit_string s 0 image (off - com_origin) (String.length s)

let image () =
  let image = Bytes.make (fcb + 0x40 - com_origin) '\x00' in
  place image com_origin program;
  place image pic_name "PIC.BIN\x00";
  place image out_name "OUT.BIN\x00";
  place image fcb ("\x00" ^ "PIC     BIN");
  Bytes.to_string image

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

let () =
  let m = Dos_machine.create () in
  Dos_machine.mount_file m "PIC.BIN" picture;
  Dos_machine.load_com m (image ());
  Dos_machine.run m ~max_steps:200_000;
  check "the program exited" (Bool.to_int (Dos_machine.exited m)) 1;
  let peek off = Dos_machine.mem_read m (psp_base + off) in
  let bus a = Cpu86.bus_read m.Dos_state.cpu a in
  check "3Fh read one byte" (peek read_count) 1;
  check "3Fh: plane 0 holds the first byte" (bus window) 0xAA;
  check "3Fh: plane 0 holds the second byte" (bus (window + 1)) 0x55;
  check "3Fh: every plane took it (plane 3)" (peek plane3_byte) 0xAA;
  check "3Fh: RAM under the window did not" (Dos_machine.mem_read m window) 0;
  check "40h wrote one byte" (peek write_count) 1;
  check "40h took the planes' byte"
    (match Dos_machine.read_mounted m "OUT.BIN" with
     | Some s when String.length s = 1 -> Char.code s.[0]
     | Some _ | None -> -1)
    stored;
  check "FCB open" (peek fcb_open) 0;
  check "14h: the record reached the planes" (bus (window + fcb_dta)) 0xAA;
  check "14h: RAM under the window did not" (Dos_machine.mem_read m (window + fcb_dta)) 0;
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos I/O through the bus: all passed\n"
