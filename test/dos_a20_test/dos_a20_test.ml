(* The memory board and the A20 gate (RFC 0005 stage 2).

   A 386 machine has 16 MB on 24 address lines and a gate on bit 20; an 8086,
   186 or 286 machine keeps 1 MB that wraps and has no gate. The gate opens
   and closes through the 8042 (D1h, DDh, DFh) and port 92h. The XMS blocks of
   a 386 machine are its RAM above 1 MB. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %02Xh want %02Xh\n%!" name got want
  end

let m386 () = Dos_machine.create ~model:Cpu86.I80386 ()
let m286 () = Dos_machine.create ~model:Cpu86.I80286 ()
let cpu m = m.Dos_state.cpu
let ports m = m.Dos_state.ports
let gate m = Dos_ports.a20 (ports m)
let rd m a = Cpu86.bus_read (cpu m) a
let wr m a v = Cpu86.bus_write (cpu m) a v

(* ---------- the board ---------- *)

let () =
  let one = Dos_a20.create Dos_a20.One_megabyte
  and big = Dos_a20.create Dos_a20.Sixteen_megabytes in
  check_int "1 MB board: mask" one.Dos_a20.mask 0xFFFFF;
  check_int "16 MB board starts closed: bit 20 off" big.Dos_a20.mask 0xEFFFFF;
  check "a board starts closed" (not (Dos_a20.enabled one) && not (Dos_a20.enabled big));
  Dos_a20.set one true;
  check "a 1 MB board has no gate: set changes nothing" (not (Dos_a20.enabled one));
  check_int "1 MB board: mask after set" one.Dos_a20.mask 0xFFFFF;
  Dos_a20.set big true;
  check "16 MB board: open" (Dos_a20.enabled big);
  check_int "16 MB board open: 24 bits" big.Dos_a20.mask 0xFFFFFF;
  Dos_a20.set big false;
  check_int "16 MB board closed again" big.Dos_a20.mask 0xEFFFFF;
  check_int "memory_bytes 1 MB" (Dos_a20.memory_bytes Dos_a20.One_megabyte) 0x100000;
  check_int "memory_bytes 16 MB" (Dos_a20.memory_bytes Dos_a20.Sixteen_megabytes) 0x1000000

(* ---------- the machines ---------- *)

let () =
  List.iter
    (fun (name, model, bytes) ->
      let m = Dos_machine.create ~model () in
      check_int (name ^ ": RAM size") (Bytes.length m.Dos_state.mem) bytes)
    [ ("8086", Cpu86.I8086, 0x100000); ("186", Cpu86.I80186, 0x100000);
      ("286", Cpu86.I80286, 0x100000); ("386", Cpu86.I80386, 0x1000000) ];
  check "a 386 machine starts with the gate closed" (not (Dos_a20.enabled (gate (m386 ()))))

(* ---------- the bus ---------- *)

let () =
  let m = m386 () in
  (* closed: FFFF:0010 is 0:0000 *)
  wr m 0x500 0x11;
  wr m 0x100500 0x22;
  check_int "closed: 100500h writes 500h" (rd m 0x500) 0x22;
  check_int "closed: 100500h reads 500h" (rd m 0x100500) 0x22;
  wr m 0x300000 0x33;
  check_int "closed: 300000h is 200000h" (rd m 0x200000) 0x33;
  Dos_a20.set (gate m) true;
  check_int "open: 100500h is its own byte (still 0)" (rd m 0x100500) 0;
  check_int "open: 500h kept the closed write" (rd m 0x500) 0x22;
  wr m 0x100500 0x44;
  check_int "open: a write at 100500h leaves 500h" (rd m 0x500) 0x22;
  check_int "open: and reads back" (rd m 0x100500) 0x44;
  wr m 0xFFFFFF 0x55;
  check_int "open: the last byte of 16 MB" (rd m 0xFFFFFF) 0x55;
  wr m 0x1000500 0x66;
  check_int "24 address lines: 1000500h is 500h" (rd m 0x500) 0x66;
  check_int "24 address lines: a 32-bit address keeps its low 24 bits"
    (rd m 0xAB000500) 0x66;
  check_int "open: 300000h is its own byte" (rd m 0x300000) 0;
  Dos_a20.set (gate m) false;
  check_int "closed again: 100500h is 500h" (rd m 0x100500) 0x66

let () =
  let m = m286 () in
  wr m 0x500 0x11;
  wr m 0x100500 0x22;
  check_int "286: 100500h writes 500h" (rd m 0x500) 0x22;
  check_int "286: 1MB board wraps even for a 24-bit address" (rd m 0xF00500) 0x22;
  Dos_a20.set (gate m) true;
  check "286: the gate cannot be opened" (not (Dos_a20.enabled (gate m)));
  wr m 0x100500 0x33;
  check_int "286: still wraps after set" (rd m 0x500) 0x33

(* The controller alone, on a 16 MB board: D0h shows the gate in bit 1 of the
   output port, and D1h replaces the whole port with the byte written. *)
let () =
  let a20 = Dos_a20.create Dos_a20.Sixteen_megabytes in
  let k = Dos_kbc.create ~a20 () in
  let hz = Dos_ports.speed_hz Dos_ports.pc_speed in
  let now = ref 0 in
  let command v = Dos_kbc.write_command k ~hz v in
  let data v = Dos_kbc.write_data k ~hz ~now:!now v in
  let answer () =
    now := !now + hz;
    ignore (Dos_kbc.latch k ~hz ~now:!now);
    Dos_kbc.read_data k ~hz ~now:!now
  in
  command 0xD0;
  check_int "kbc: the output port after POST, gate closed" (answer ()) 0xCD;
  command 0xDF;
  command 0xD0;
  check_int "kbc: DFh sets bit 1" (answer ()) 0xCF;
  check "kbc: and the gate" (Dos_a20.enabled a20);
  command 0xD1; data 0x00;
  check "kbc: D1h with bit 1 clear closes it" (not (Dos_a20.enabled a20));
  command 0xD1; data 0x02;
  check "kbc: D1h with bit 1 set opens it" (Dos_a20.enabled a20);
  command 0xD0;
  check_int "kbc: D1h wrote the whole port: 02h, not CFh" (answer ()) 0x02

(* ---------- guest programs ---------- *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax v = "\xb8" ^ w v
let mov_al v = "\xb0" ^ b v
let xor_ax_ax = "\x31\xc0"
let mov_ds_ax = "\x8e\xd8"
let mov_es_ax = "\x8e\xc0"
let ds_imm8 off v = "\xc6\x06" ^ w off ^ b v        (* mov byte ds:[off], v *)
let es_imm8 off v = "\x26\xc6\x06" ^ w off ^ b v    (* mov byte es:[off], v *)
let al_from_ds off = "\xa0" ^ w off
let al_from_es off = "\x26\xa0" ^ w off
let al_to_ds off = "\xa2" ^ w off
let in_al p = "\xe4" ^ b p
let out_al p = "\xe6" ^ b p
let quit = mov_ax 0x4C00 ^ "\xcd\x21"

(* The classic 8042 handshake: wait for the input buffer to empty, then write. *)
let kbc_wait_input = in_al 0x64 ^ "\xa8\x02\x75\xfa"
let kbc_wait_output = in_al 0x64 ^ "\xa8\x01\x74\xfa"
let kbc_command c = kbc_wait_input ^ mov_al c ^ out_al 0x64
let kbc_data d = kbc_wait_input ^ mov_al d ^ out_al 0x60

(* CLI so the BIOS keyboard handler does not take the controller's answer
   before the program polls for it. DS = 0, ES = FFFF: ds:[off] is off,
   es:[off] is 100000h + off - 10h. *)
let setup = "\xfa" ^ xor_ax_ax ^ mov_ds_ax ^ mov_ax 0xFFFF ^ mov_es_ax

let result_base = 0x600
let results m n = List.init n (fun i -> Dos_machine.mem_read m (result_base + i))

let run_com m code =
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:200_000;
  check "the program ended" (Dos_machine.exited m)

(* The 8042 way: probe, open with D1h/DFh, probe, read the output port, close
   with DDh, probe. Results at 600h on. *)
let kbc_program =
  setup ^ ds_imm8 0x500 0x11 ^ es_imm8 0x510 0x22
  ^ al_from_ds 0x500 ^ al_to_ds 0x600
  ^ kbc_command 0xD1 ^ kbc_data 0xDF
  ^ es_imm8 0x510 0x33
  ^ al_from_ds 0x500 ^ al_to_ds 0x601
  ^ al_from_es 0x510 ^ al_to_ds 0x602
  ^ kbc_command 0xD0 ^ kbc_wait_output ^ in_al 0x60 ^ al_to_ds 0x603
  ^ kbc_command 0xDD
  ^ al_from_es 0x510 ^ al_to_ds 0x604
  ^ kbc_command 0xDF
  ^ al_from_es 0x510 ^ al_to_ds 0x605
  ^ quit

let () =
  let m = m386 () in
  run_com m kbc_program;
  (match results m 6 with
   | [ closed; kept; opened; output; closed_again; reopened ] ->
     check_int "8042: closed, FFFF:0010 wrapped onto 0:0500" closed 0x22;
     check_int "8042: after D1h DFh 0:0500 keeps the old byte" kept 0x22;
     check_int "8042: and FFFF:0010 is the byte above 1 MB" opened 0x33;
     check "8042: D0h reads the A20 bit set" (output land 0x02 <> 0);
     check_int "8042: DDh closed it, FFFF:0010 is 0:0500 again" closed_again 0x22;
     check_int "8042: DFh reopened it, the byte above 1 MB is back" reopened 0x33
   | _ -> check "8042: six results" false);
  check "8042: the gate ends open" (Dos_a20.enabled (gate m))

let () =
  let m = m286 () in
  run_com m kbc_program;
  (match results m 6 with
   | [ closed; kept; opened; output; closed_again; reopened ] ->
     check_int "286 8042: closed" closed 0x22;
     check_int "286 8042: DFh changed nothing, FFFF:0010 wrote 0:0500" kept 0x33;
     check_int "286 8042: and reads back" opened 0x33;
     check "286 8042: D0h reads the A20 bit clear" (output land 0x02 = 0);
     check_int "286 8042: DDh" closed_again 0x33;
     check_int "286 8042: DFh again" reopened 0x33
   | _ -> check "286 8042: six results" false);
  check "286 8042: no gate opens" (not (Dos_a20.enabled (gate m)))

(* The port 92h way, and the two ways share one line. *)
let fast_program =
  setup ^ ds_imm8 0x500 0x11
  ^ in_al 0x92 ^ al_to_ds 0x600
  ^ mov_al 0x02 ^ out_al 0x92
  ^ es_imm8 0x510 0x44
  ^ al_from_ds 0x500 ^ al_to_ds 0x601
  ^ in_al 0x92 ^ al_to_ds 0x602
  ^ kbc_command 0xDD
  ^ in_al 0x92 ^ al_to_ds 0x603
  ^ al_from_es 0x510 ^ al_to_ds 0x604
  ^ mov_al 0x00 ^ out_al 0x92
  ^ kbc_command 0xD0 ^ kbc_wait_output ^ in_al 0x60 ^ al_to_ds 0x605
  ^ quit

let () =
  let m = m386 () in
  run_com m fast_program;
  (match results m 6 with
   | [ before; kept; after; after_8042; wrapped; output ] ->
     check_int "92h: closed at start" before 0;
     check_int "92h: out 92h,2 opened it, 0:0500 untouched" kept 0x11;
     check_int "92h: reads bit 1 set" after 0x02;
     check_int "92h: DDh closed the same line" after_8042 0;
     check_int "92h: so FFFF:0010 is 0:0500 again (a write above 1 MB stayed above)" wrapped 0x11;
     check "92h: D0h reads the A20 bit clear after out 92h,0" (output land 0x02 = 0)
   | _ -> check "92h: six results" false)

let () =
  let m = m286 () in
  run_com m fast_program;
  (match results m 3 with
   | [ before; kept; after ] ->
     check_int "286 92h: no such port, reads FFh" before 0xFF;
     check_int "286 92h: out 92h,2 changed nothing: FFFF:0010 wrote 0:0500" kept 0x44;
     check_int "286 92h: still FFh" after 0xFF
   | _ -> check "286 92h: three results" false);
  check "286 92h: no gate" (not (Dos_a20.enabled (gate m)))

(* ---------- INT 15h AH=88h ---------- *)

let () =
  List.iter
    (fun (name, m, want) ->
      (* DS = 0; mov ah,88h; int 15h; mov [600h],ax *)
      Dos_machine.load_com m ("\x31\xc0\x8e\xd8" ^ "\xb4\x88\xcd\x15" ^ "\xa3\x00\x06" ^ quit);
      Dos_machine.run m ~max_steps:100_000;
      let got = Dos_machine.mem_read m 0x600 lor (Dos_machine.mem_read m 0x601 lsl 8) in
      check_int (name ^ ": INT 15h AH=88h") got want)
    [ ("386", m386 (), Dos_xms.capacity_kb); ("286", m286 (), 0) ]

(* ---------- XMS on a 386 machine ---------- *)

let ok_exn = function Ok v -> v | Error _ -> failwith "XMS refused"

let xms_move m ~length ~source ~dest =
  Dos_xms.move m.Dos_state.xms ~read:(rd m) ~write:(wr m) { Dos_xms.length; source; dest }

let () =
  let m = m386 () in
  let x = m.Dos_state.xms in
  (* stale bytes where the block will be *)
  wr m 0x100000 0x77;
  Dos_a20.set (gate m) true;
  wr m 0x100000 0x77;
  wr m 0x100001 0x78;
  let h = ok_exn (Dos_xms.allocate x ~kb:2) in
  let at = ok_exn (Dos_xms.lock x h) in
  check_int "XMS: the first block is at 100000h" at 0x100000;
  check_int "XMS: a new block reads 0 over stale bytes" (rd m 0x100000) 0;
  check_int "XMS: and the byte after it" (rd m 0x100001) 0;
  (* conventional -> block, read through the CPU *)
  wr m 0x500 0xAB;
  wr m 0x501 0xCD;
  ok_exn (xms_move m ~length:2 ~source:(Dos_xms.Conventional 0x500)
            ~dest:(Dos_xms.Block { handle = h; offset = 4 }));
  check_int "XMS: a move into the block shows at its address" (rd m (at + 4)) 0xAB;
  check_int "XMS: both bytes" (rd m (at + 5)) 0xCD;
  (* CPU store -> block -> conventional *)
  wr m (at + 8) 0xEF;
  wr m (at + 9) 0x12;
  ok_exn (xms_move m ~length:2 ~source:(Dos_xms.Block { handle = h; offset = 8 })
            ~dest:(Dos_xms.Conventional 0x600));
  check_int "XMS: a CPU store shows in a move out" (rd m 0x600) 0xEF;
  check_int "XMS: both bytes out" (rd m 0x601) 0x12;
  (* blocks do not overlap each other or low memory *)
  let h2 = ok_exn (Dos_xms.allocate x ~kb:1) in
  check_int "XMS: the second block follows" (ok_exn (Dos_xms.lock x h2)) (0x100000 + 2048);
  check_int "XMS: low memory is not the block" (rd m 0x500) 0xAB

(* A block that moves down keeps its bytes (the copy overlaps itself) and the
   new tail reads 0. *)
let () =
  let m = m386 () in
  let x = m.Dos_state.xms in
  Dos_a20.set (gate m) true;
  let a = ok_exn (Dos_xms.allocate x ~kb:1) in
  let b = ok_exn (Dos_xms.allocate x ~kb:2) in
  let _c = ok_exn (Dos_xms.allocate x ~kb:1) in   (* blocks b from growing in place *)
  let at_b = ok_exn (Dos_xms.lock x b) in
  ok_exn (Dos_xms.unlock x b);
  check_int "resize: b follows a" at_b (0x100000 + 1024);
  for i = 0 to 2047 do wr m (at_b + i) ((i * 7 + 3) land 0xff) done;
  ok_exn (Dos_xms.release x a);
  ok_exn (Dos_xms.resize x b ~kb:3);
  let at_b' = ok_exn (Dos_xms.lock x b) in
  check_int "resize: b moved into the hole" at_b' 0x100000;
  let intact = ref true in
  for i = 0 to 2047 do if rd m (at_b' + i) <> (i * 7 + 3) land 0xff then intact := false done;
  check "resize: the bytes moved down intact" !intact;
  let zero = ref true in
  for i = 2048 to 3071 do if rd m (at_b' + i) <> 0 then zero := false done;
  check "resize: the new tail reads 0" !zero

(* A 286 machine's XMS is unchanged: its bytes are out of the CPU's reach. *)
let () =
  let m = m286 () in
  let x = m.Dos_state.xms in
  let h = ok_exn (Dos_xms.allocate x ~kb:1) in
  let at = ok_exn (Dos_xms.lock x h) in
  check_int "286 XMS: the lock address is 100000h" at 0x100000;
  wr m 0x500 0x5A;
  ok_exn (xms_move m ~length:2 ~source:(Dos_xms.Conventional 0x500)
            ~dest:(Dos_xms.Block { handle = h; offset = 0 }));
  check_int "286 XMS: the CPU does not see the block (100000h wraps to 0)" (rd m at) (rd m 0);
  check "286 XMS: the move still reads back"
    (match xms_move m ~length:2 ~source:(Dos_xms.Block { handle = h; offset = 0 })
             ~dest:(Dos_xms.Conventional 0x700) with
     | Ok () -> rd m 0x700 = 0x5A
     | Error _ -> false)

(* ---------- snapshots ---------- *)

let save_exn m =
  match Dos_snapshot.save m with Ok s -> s | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let restore_exn s =
  match Dos_snapshot.restore s with
  | Ok m -> m
  | Error e -> failwith (Dos_snapshot.error_to_string e)

let () =
  let m = m386 () in
  Dos_a20.set (gate m) true;
  let x = m.Dos_state.xms in
  let h = ok_exn (Dos_xms.allocate x ~kb:4) in
  let at = ok_exn (Dos_xms.lock x h) in
  wr m (at + 100) 0xA1;
  wr m 0xFFFFFF 0xA2;
  wr m 0x500 0xA3;
  let s = save_exn m in
  check "386 snapshot: saving twice gives the same bytes" (save_exn m = s);
  let r = restore_exn s in
  check "386 snapshot: the gate comes back open" (Dos_a20.enabled (gate r));
  check_int "386 snapshot: its mask is applied" (gate r).Dos_a20.mask 0xFFFFFF;
  check_int "386 snapshot: RAM is 16 MB" (Bytes.length r.Dos_state.mem) 0x1000000;
  check_int "386 snapshot: a byte of the XMS block" (rd r (at + 100)) 0xA1;
  check_int "386 snapshot: the last byte of RAM" (rd r 0xFFFFFF) 0xA2;
  check_int "386 snapshot: low memory" (rd r 0x500) 0xA3;
  check "386 snapshot: a restored machine saves the same bytes" (save_exn r = s);
  (match Dos_xms.info r.Dos_state.xms h with
   | Ok { Dos_xms.lock_count; size_kb; _ } ->
     check_int "386 snapshot: XMS lock count" lock_count 1;
     check_int "386 snapshot: XMS block size" size_kb 4
   | Error _ -> check "386 snapshot: the XMS handle survives" false);
  (* the block's bytes are the RAM's: written once. A 4 KB block adds its
     record (a flag and three words), not 4 KB. *)
  let empty = save_exn (m386 ()) in
  check "386 snapshot: a 4 KB block adds no bytes of its own"
    (String.length s - String.length empty < 100);
  (* a closed gate comes back closed *)
  Dos_a20.set (gate m) false;
  let closed = restore_exn (save_exn m) in
  check "386 snapshot: a closed gate comes back closed" (not (Dos_a20.enabled (gate closed)));
  check_int "386 snapshot: closed mask" (gate closed).Dos_a20.mask 0xEFFFFF

let () =
  let m = m286 () in
  let x = m.Dos_state.xms in
  let h = ok_exn (Dos_xms.allocate x ~kb:2) in
  ignore (ok_exn (Dos_xms.lock x h));
  let s = save_exn m in
  let r = restore_exn s in
  check_int "286 snapshot: RAM is 1 MB" (Bytes.length r.Dos_state.mem) 0x100000;
  check "286 snapshot: a restored machine saves the same bytes" (save_exn r = s);
  check "286 snapshot: its XMS block keeps its own bytes"
    (String.length s > String.length (save_exn (m286 ())))

let () =
  if !failed > 0 then begin
    Printf.printf "dos a20: %d failed\n" !failed;
    exit 1
  end
  else print_endline "dos a20: all passed"
