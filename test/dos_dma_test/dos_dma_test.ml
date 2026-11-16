(* The 8237A DMA controller: ports 00h-0Fh, the page registers, and a
   device pulling bytes.

   Contract: address and count go low byte then high byte through one
   flip-flop, which 0Ch resets. A device pulls one byte per cycle from
   page:address; the address moves by one inside its page, the count drops
   by one, and the byte that takes the count below zero is TC. The status
   shows TC until it is read. At TC an auto-init channel reloads, any other
   masks itself with its count at FFFFh. Masked channels move nothing. *)

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

let machine () = Dos_machine.create ()
let ports m = m.Dos_state.ports
let out m p v = Dos_ports.port_out (ports m) p v
let inp m p = Dos_ports.port_in (ports m) p
let dma m = Dos_ports.dma (ports m)
let pull m ch = Dos_dma.pull (dma m) ~mem:m.Dos_state.mem ch

let read16 m p =
  let lo = inp m p in
  let hi = inp m p in
  (hi lsl 8) lor lo

let write16 m p v = out m p (v land 0xFF); out m p (v lsr 8)

(* Channel 1, the Sound Blaster's: address 02h, count 03h, page 83h. *)
let ch1_address = 0x02
let ch1_count = 0x03
let ch1_page = 0x83
let status = 0x08
let single_mask = 0x0A
let mode = 0x0B
let clear_flip_flop = 0x0C
let master_clear = 0x0D
let clear_masks = 0x0E
let all_masks = 0x0F

(* Mode bytes: single mode (bits 7-6 = 01), channel 1. *)
let read_once = 0x49        (* memory to device *)
let read_auto = 0x59        (* memory to device, auto-init *)
let read_down = 0x69        (* memory to device, address decrement *)
let verify_once = 0x41

let unmask_ch1 m = out m single_mask 0x01

(* Puts [bytes] at [page]:[addr] and sets channel 1 up to read them. *)
let setup m ~page ~addr ~mode:md bytes =
  String.iteri (fun i c -> Bytes.set m.Dos_state.mem ((page lsl 16) + addr + i) c) bytes;
  out m clear_flip_flop 0;
  out m mode md;
  write16 m ch1_address addr;
  write16 m ch1_count (String.length bytes - 1);
  out m ch1_page page;
  unmask_ch1 m

let expect_byte name got want ~terminal =
  match got with
  | Dos_dma.Byte b ->
    check_int (name ^ " value") b.value want;
    check (name ^ " terminal") (b.terminal = terminal)
  | Dos_dma.Masked -> check (name ^ ": masked") false
  | Dos_dma.No_data _ -> check (name ^ ": no data") false

let test_power_on () =
  let m = machine () in
  check_int "status at power-on" (inp m status) 0;
  check_int "address at power-on" (read16 m ch1_address) 0;
  check_int "page at power-on" (inp m ch1_page) 0;
  check "every channel masked at power-on" (pull m Dos_dma.Channel1 = Dos_dma.Masked)

let test_flip_flop () =
  let m = machine () in
  out m clear_flip_flop 0;
  write16 m ch1_address 0x1234;
  check_int "address reads back low then high" (read16 m ch1_address) 0x1234;
  write16 m ch1_count 0xBEEF;
  check_int "count reads back" (read16 m ch1_count) 0xBEEF;
  (* One stray byte leaves the flip-flop high; 0Ch puts it back. *)
  out m ch1_address 0x99;
  out m clear_flip_flop 0;
  out m ch1_address 0x78;
  out m clear_flip_flop 0;
  check_int "0Ch makes the next byte the low one" (read16 m ch1_address) 0x1278;
  (* The flip-flop is the controller's, not the channel's. *)
  out m clear_flip_flop 0;
  out m 0x00 0x11;
  out m 0x06 0x22;
  out m clear_flip_flop 0;
  check_int "channel 3 got the high byte" (read16 m 0x06) 0x2200;
  check_int "channel 0 got the low byte" (read16 m 0x00) 0x0011

let test_pull_once () =
  let m = machine () in
  setup m ~page:0x02 ~addr:0x3000 ~mode:read_once "abc";
  expect_byte "first byte" (pull m Dos_dma.Channel1) (Char.code 'a') ~terminal:false;
  check_int "count after one byte" (read16 m ch1_count) 1;
  check_int "address after one byte" (read16 m ch1_address) 0x3001;
  expect_byte "second byte" (pull m Dos_dma.Channel1) (Char.code 'b') ~terminal:false;
  check_int "no TC yet" (inp m status) 0;
  expect_byte "last byte is TC" (pull m Dos_dma.Channel1) (Char.code 'c') ~terminal:true;
  check_int "status shows TC on channel 1" (inp m status) 0x02;
  check_int "reading the status cleared it" (inp m status) 0;
  check_int "count wraps to FFFFh" (read16 m ch1_count) 0xFFFF;
  check_int "address stays past the block" (read16 m ch1_address) 0x3003;
  check "channel masks itself at TC" (pull m Dos_dma.Channel1 = Dos_dma.Masked)

let test_auto_init () =
  let m = machine () in
  setup m ~page:0x01 ~addr:0x0100 ~mode:read_auto "xy";
  ignore (pull m Dos_dma.Channel1);
  expect_byte "auto-init TC" (pull m Dos_dma.Channel1) (Char.code 'y') ~terminal:true;
  check_int "count reloaded" (read16 m ch1_count) 1;
  check_int "address reloaded" (read16 m ch1_address) 0x0100;
  expect_byte "auto-init starts over" (pull m Dos_dma.Channel1) (Char.code 'x') ~terminal:false

let test_decrement () =
  let m = machine () in
  setup m ~page:0x01 ~addr:0x0200 ~mode:read_down "pq";
  Bytes.set m.Dos_state.mem 0x101FF 'o';
  expect_byte "decrement reads the start" (pull m Dos_dma.Channel1) (Char.code 'p') ~terminal:false;
  expect_byte "then the byte below" (pull m Dos_dma.Channel1) (Char.code 'o') ~terminal:true

let test_page_wrap () =
  let m = machine () in
  (* Page 4: a carry into bit 16 would land on 50000h, not 40000h. *)
  setup m ~page:0x04 ~addr:0xFFFF ~mode:read_once "z";
  out m clear_flip_flop 0;
  write16 m ch1_count 1;
  unmask_ch1 m;
  Bytes.set m.Dos_state.mem 0x40000 'w';
  Bytes.set m.Dos_state.mem 0x50000 'n';
  expect_byte "last byte of the page" (pull m Dos_dma.Channel1) (Char.code 'z') ~terminal:false;
  expect_byte "the page does not carry" (pull m Dos_dma.Channel1) (Char.code 'w') ~terminal:true

let test_verify () =
  let m = machine () in
  setup m ~page:0 ~addr:0x500 ~mode:verify_once "v";
  (match pull m Dos_dma.Channel1 with
   | Dos_dma.No_data { terminal } -> check "verify reaches TC" terminal
   | Dos_dma.Byte _ | Dos_dma.Masked -> check "verify moves no data" false);
  check_int "verify still counts" (read16 m ch1_count) 0xFFFF

let test_masks () =
  let m = machine () in
  setup m ~page:0 ~addr:0x600 ~mode:read_auto "m";
  out m single_mask 0x05;
  check "single mask sets" (pull m Dos_dma.Channel1 = Dos_dma.Masked);
  out m clear_masks 0;
  check "0Eh clears every mask" (pull m Dos_dma.Channel1 <> Dos_dma.Masked);
  out m all_masks 0x02;
  check "0Fh bit 1 masks channel 1" (pull m Dos_dma.Channel1 = Dos_dma.Masked);
  out m all_masks 0x0D;
  check "0Fh bit 1 clear unmasks it" (pull m Dos_dma.Channel1 <> Dos_dma.Masked)

let test_master_clear () =
  let m = machine () in
  setup m ~page:0 ~addr:0x700 ~mode:read_once "k";
  ignore (pull m Dos_dma.Channel1);
  setup m ~page:0 ~addr:0x700 ~mode:read_auto "k";
  out m ch1_address 0x00;   (* the low byte again: leaves the flip-flop high *)
  out m master_clear 0;
  check_int "master clear drops TC" (inp m status) 0;
  check "master clear masks" (pull m Dos_dma.Channel1 = Dos_dma.Masked);
  check_int "master clear resets the flip-flop, keeps the address" (read16 m ch1_address) 0x0700;
  unmask_ch1 m;
  expect_byte "mode survives master clear" (pull m Dos_dma.Channel1) (Char.code 'k') ~terminal:true;
  check_int "auto-init survived" (read16 m ch1_count) 0

let test_write_only_reads () =
  let m = machine () in
  check_int "mode register reads FFh" (inp m mode) 0xFF;
  check_int "mask register reads FFh" (inp m all_masks) 0xFF;
  check_int "temporary register reads 0" (inp m master_clear) 0

(* The same programming from a guest: OUT and IN through the CPU.
   out 0Ch; out 02h,34h; out 02h,12h; out 83h,0Ah; in 02h x2; in 83h;
   store them at 0200h; int 20h *)
let guest_com =
  "\xE6\x0C"
  ^ "\xB0\x34\xE6\x02\xB0\x12\xE6\x02"
  ^ "\xB0\x0A\xE6\x83"
  ^ "\xE4\x02\xA2\x00\x02\xE4\x02\xA2\x01\x02"
  ^ "\xE4\x83\xA2\x02\x02"
  ^ "\xCD\x20"

let test_guest_ports () =
  let m = machine () in
  Dos_machine.load_com m guest_com;
  let seg = Cpu86.seg m.Dos_state.cpu 3 in
  Dos_machine.run m ~max_steps:1_000;
  let at off = Char.code (Bytes.get m.Dos_state.mem ((seg lsl 4) + off)) in
  check_int "guest reads address low" (at 0x200) 0x34;
  check_int "guest reads address high" (at 0x201) 0x12;
  check_int "guest reads the page" (at 0x202) 0x0A

let test_snapshot () =
  let m = machine () in
  setup m ~page:0x04 ~addr:0x0800 ~mode:read_auto "snap";
  ignore (pull m Dos_dma.Channel1);
  out m ch1_address 0x00;   (* flip-flop high in the snapshot *)
  match Dos_snapshot.save m with
  | Error e -> check ("save: " ^ Dos_snapshot.save_error_to_string e) false
  | Ok s ->
    match Dos_snapshot.restore s with
    | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
    | Ok r ->
      out r ch1_address 0x08;
      out r clear_flip_flop 0;
      check_int "restored count" (read16 r ch1_count) 2;
      check_int "restored flip-flop took the high byte" (read16 r ch1_address) 0x0800;
      check_int "restored page" (inp r ch1_page) 0x04;
      ignore (pull r Dos_dma.Channel1);
      ignore (pull r Dos_dma.Channel1);
      expect_byte "restored auto-init" (pull r Dos_dma.Channel1) (Char.code 'a') ~terminal:true;
      check_int "restored channel reloads" (read16 r ch1_count) 3

let () =
  test_power_on ();
  test_flip_flop ();
  test_pull_once ();
  test_auto_init ();
  test_decrement ();
  test_page_wrap ();
  test_verify ();
  test_masks ();
  test_master_clear ();
  test_write_only_reads ();
  test_guest_ports ();
  test_snapshot ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos dma: all passed\n"
