(* XMS in the machine: a guest program finds the driver through INT 2Fh,
   far-calls its control function, allocates a block and moves bytes into
   it and back. A move into the planar video window goes through the
   graphics controller, as HIMEM's own MOVSW would. *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax v = "\xb8" ^ w v
let mov_ah n = "\xb4" ^ b n
let mov_dx v = "\xba" ^ w v
let mov_si v = "\xbe" ^ w v
let int_ n = "\xcd" ^ b n
let store_al m = "\xa2" ^ w m
let store_ax m = "\xa3" ^ w m
let store_bx m = "\x89\x1e" ^ w m
let store_dx m = "\x89\x16" ^ w m
let store_es m = "\x8c\x06" ^ w m
let store_cs m = "\x8c\x0e" ^ w m
let store_sp m = "\x89\x26" ^ w m
let store_imm m v = "\xc7\x06" ^ w m ^ w v
let call_far m = "\xff\x1e" ^ w m
let quit = mov_ax 0x4C00 ^ int_ 0x21

(* Data in the program's segment (the PSP's, 1000h). *)
let psp_seg = 0x1000
let psp_base = psp_seg * 16
let com_origin = 0x100
let installed = 0x2000
let other_ax = 0x2002
let entry = 0x2004
let version = 0x2008
let alloc_ok = 0x200A
let handle = 0x200C
let move_in = 0x200E
let move_out = 0x2010
let sp_before = 0x2012
let sp_after = 0x2014
let move_struct = 0x2020
let source = 0x2040
let dest = 0x2050
let message = "XMSOK!"

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %X want %X\n%!" name got want
  end

let peek m off = Dos_machine.mem_read m (psp_base + off)
let peek16 m off = peek m off lor (peek m (off + 1) lsl 8)

(* The move structure: length, source handle and offset, destination
   handle and offset (XMS 3.0, function 0Bh). [`Here off] is this
   segment's offset off; [`Seg (s, o)] another segment. *)
let fill_struct ~length ~src ~dst =
  let endpoint at = function
    | `Handle_from_dx off -> store_dx at ^ store_imm (at + 2) off ^ store_imm (at + 4) 0
    | `Here off -> store_imm at 0 ^ store_imm (at + 2) off ^ store_cs (at + 4)
    | `Seg (s, o) -> store_imm at 0 ^ store_imm (at + 2) o ^ store_imm (at + 4) s
  in
  store_imm move_struct length ^ store_imm (move_struct + 2) 0
  ^ endpoint (move_struct + 4) src
  ^ endpoint (move_struct + 10) dst

let call_move result = mov_si move_struct ^ mov_ah 0x0B ^ call_far entry ^ store_ax result

let image code data =
  let pad = String.make (source - com_origin - String.length code) '\x90' in
  code ^ pad ^ data

let run code data =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (image code data);
  Dos_machine.run m ~max_steps:200_000;
  check "the program exited" (Bool.to_int (Dos_machine.exited m)) 1;
  m

let detect_and_round_trip =
  mov_ax 0x4300 ^ int_ 0x2F ^ store_al installed
  ^ mov_ax 0x1600 ^ int_ 0x2F ^ store_ax other_ax
  ^ mov_ax 0x4310 ^ int_ 0x2F ^ store_bx entry ^ store_es (entry + 2)
  ^ store_sp sp_before ^ mov_ah 0x00 ^ call_far entry ^ store_sp sp_after ^ store_ax version
  ^ mov_ah 0x09 ^ mov_dx 1 ^ call_far entry ^ store_ax alloc_ok ^ store_dx handle
  ^ fill_struct ~length:(String.length message) ~src:(`Here source) ~dst:(`Handle_from_dx 0)
  ^ call_move move_in
  ^ fill_struct ~length:(String.length message) ~src:(`Handle_from_dx 0) ~dst:(`Here dest)
  ^ call_move move_out
  ^ quit

let block_bytes m h len =
  let out = Bytes.make len '\000' in
  let r =
    Dos_xms.move m.Dos_state.xms ~read:(fun _ -> 0)
      ~write:(fun a v -> Bytes.set out (a - 0x90000) (Char.chr v))
      { Dos_xms.length = len; source = Block { handle = h; offset = 0 };
        dest = Conventional 0x90000 }
  in
  (match r with Ok () -> () | Error _ -> check "read the block back" 0 1);
  Bytes.to_string out

let test_round_trip () =
  let m = run detect_and_round_trip message in
  check "4300h: AL=80h" (peek m installed) 0x80;
  check "1600h: AX untouched" (peek16 m other_ax) 0x1600;
  check "entry offset" (peek16 m entry) 0x0060;
  check "entry segment" (peek16 m (entry + 2)) 0xF000;
  check "version" (peek16 m version) 0x0300;
  check "the far call returns with the stack it found" (peek16 m sp_after) (peek16 m sp_before);
  check "allocate" (peek16 m alloc_ok) 1;
  check "handle" (peek16 m handle) 1;
  check "move in" (peek16 m move_in) 1;
  check "move out" (peek16 m move_out) 1;
  String.iteri (fun i c -> check "bytes came back" (peek m (dest + i)) (Char.code c)) message;
  check "the block holds them"
    (Bool.to_int (block_bytes m 1 (String.length message) = message)) 1;
  let rom = 0xF0000 + 0x0060 in
  List.iteri
    (fun i v -> check "hookable entry" (Dos_machine.mem_read m (rom + i)) v)
    [ 0xEB; 0x03; 0x90; 0x90; 0x90 ];
  check "IVT[43h] offset stays the IRET stub" (Dos_machine.mem_read m (0x43 * 4)) 0x20;
  match Dos_snapshot.save m with
  | Error _ -> check "save" 0 1
  | Ok s ->
    (match Dos_snapshot.restore s with
     | Error _ -> check "restore" 0 1
     | Ok m2 ->
       check "a restored machine keeps the block"
         (Bool.to_int (block_bytes m2 1 (String.length message) = message)) 1)

(* Mode 0Dh, then a move from conventional memory to A000:0000. With the
   default map mask every plane takes the byte; plain RAM under the window
   stays 0. *)
let test_planar_window () =
  let code =
    mov_ax 0x000D ^ int_ 0x10
    ^ mov_ax 0x4310 ^ int_ 0x2F ^ store_bx entry ^ store_es (entry + 2)
    ^ fill_struct ~length:2 ~src:(`Here source) ~dst:(`Seg (0xA000, 0))
    ^ call_move move_in
    ^ quit
  in
  let m = run code "\xff\xff" in
  check "move into the window" (peek16 m move_in) 1;
  check "plane 0 took the byte" (Cpu86.bus_read m.Dos_state.cpu 0xA0000) 0xFF;
  check "RAM under the window did not" (Dos_machine.mem_read m 0xA0000) 0x00

let () =
  test_round_trip ();
  test_planar_window ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos XMS machine: all passed\n"
