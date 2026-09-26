(* Whole-machine snapshot round trip.

   Contract: run N steps, snapshot, restore into a fresh machine, run M more,
   and the machine is the one that ran N+M directly. "The same" is measured
   as byte-equal snapshots, which covers the CPU, RAM, video planes, ports,
   files, handles, EXEC frames, EMS and the mouse at once. A snapshot of a
   restored machine is also byte-equal to the one it came from.

   The game round trip runs only when SAMGUK3_DIR names a 삼국지3 directory
   (KOEI.COM and its files); the game is not in the repository. *)

let failed = ref 0

let check name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let sorted_bindings tbl =
  List.sort (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

let run t n = ignore (Dos_machine.run_until t ~max_steps:n ~stop:(fun _ -> false))

let save_exn t =
  match Dos_snapshot.save t with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let restore_exn s =
  match Dos_snapshot.restore s with
  | Ok t -> t
  | Error e -> failwith (Dos_snapshot.error_to_string e)

let round_trip ~name ~boot ~n ~m =
  let direct = boot () in
  run direct (n + m);
  let a = boot () in
  run a n;
  let saved = save_exn a in
  check (name ^ ": saving twice gives the same bytes") (save_exn a = saved);
  let b = restore_exn saved in
  check (name ^ ": a restored machine saves the same bytes") (save_exn b = saved);
  run b m;
  check (name ^ ": N, restore, M is the machine that ran N+M")
    (save_exn b = save_exn direct);
  check (name ^ ": same screen")
    (Dos_machine.screen_digest b = Dos_machine.screen_digest direct);
  saved

(* The header's fields, at their fixed offsets. *)
let magic_len = String.length "OCAML-DOS-SNAPSHOT\000"
let version_at = magic_len
let core_at = magic_len + 8

let with_byte s i f =
  let b = Bytes.of_string s in
  Bytes.set b i (f s.[i]);
  Bytes.to_string b

(* Creates A.TXT, dups the handle, then writes one byte through each handle
   in turn, forever. After dup the two numbers share one record, so a
   snapshot that copied the record per number would split the position and
   the file would come out different. *)
let dup_com =
  "\xB4\x3C\x31\xC9\xBA\x24\x01\xCD\x21"    (* create A.TXT -> ax *)
  ^ "\x89\xC3\xB4\x45\xCD\x21\x89\xC6"      (* bx = it, si = dup bx *)
  ^ "\xB4\x40\xB9\x01\x00\xBA\x23\x01\xCD\x21" (* loop: write 1 byte via bx *)
  ^ "\x87\xDE"                              (* swap bx and si *)
  ^ "\xFE\x06\x23\x01"                      (* next byte value *)
  ^ "\xEB\xEE"                              (* jmp loop *)
  ^ "A" ^ "A.TXT\000"

let boot_dup () =
  let t = Dos_machine.create () in
  Dos_machine.load_com t dup_com;
  t

let () =
  let saved = round_trip ~name:"dup-com" ~boot:boot_dup ~n:2_000 ~m:3_000 in
  let t = restore_exn saved in
  run t 1_000;
  check "dup-com: the program still runs" (not (Dos_machine.exited t));
  (match Dos_snapshot.header saved with
   | Ok { Dos_snapshot.format; core } ->
     check "header: format" (format = Dos_snapshot.format_version);
     check "header: core digest" (core = Dos_core_identity.source_digest)
   | Error _ -> check "header reads" false);
  (* another format version is refused *)
  let other = with_byte saved (version_at + 7) (fun c -> Char.chr (Char.code c + 1)) in
  (match Dos_snapshot.restore other with
   | Error (Dos_snapshot.Wrong_format { saved; supported }) ->
     check "other format: numbers" (saved = Dos_snapshot.format_version + 1
                                    && supported = Dos_snapshot.format_version)
   | _ -> check "another format version is refused" false);
  (* another core digest is shown, not compared *)
  let other_core = with_byte saved core_at (fun c -> if c = '0' then '1' else '0') in
  (match Dos_snapshot.restore other_core with
   | Ok t -> check "another core's snapshot restores" (save_exn t = saved)
   | Error _ -> check "another core's snapshot restores" false);
  (* one flipped payload bit is refused *)
  let flipped = with_byte saved (String.length saved - 1)
      (fun c -> Char.chr (Char.code c lxor 1)) in
  (match Dos_snapshot.restore flipped with
   | Error (Dos_snapshot.Corrupt _) -> ()
   | _ -> check "a flipped byte is refused" false);
  (* a truncated one too: the checksum no longer holds *)
  (match Dos_snapshot.restore (String.sub saved 0 (String.length saved - 1)) with
   | Error (Dos_snapshot.Corrupt _) -> ()
   | _ -> check "a truncated snapshot is refused" false);
  List.iter
    (fun garbage ->
      match Dos_snapshot.restore garbage with
      | Error Dos_snapshot.Not_a_snapshot -> ()
      | _ -> check (Printf.sprintf "garbage %S is refused" garbage) false)
    [ ""; "not a snapshot"; String.sub saved 0 (magic_len + 4) ]

(* #35: two SEPARATE opens (AH=3Dh twice, not [dup]) on one name share only
   the underlying bytes, each keeping its own position. The snapshot codec
   already pooled handle *records* by physical identity (that's what the
   [dup] test above proves survives a round trip); this fix adds a second
   pool, for the *buffer* a name's separate opens share. Cutting the round
   trip right after both opens -- before either write -- means restoring
   has to rebuild that sharing itself, not just carry over bytes that
   already matched: if the two handles came back with independent copies of
   the buffer instead, each write below would land in its own copy and only
   whichever handle closed last would reach the file. *)
let separate_opens_com =
  "\xeb\x08"                                          (* jmp +8 *)
  ^ "A" ^ "B" ^ "F.DAT\x00"
  ^ "\xb8\x02\x3d\xba\x04\x01\xcd\x21\x89\xc7"    (* open -> handle A in di *)
  ^ "\xb8\x02\x3d\xba\x04\x01\xcd\x21\x89\xc6"    (* open -> handle B in si *)
  ^ "\x89\xfb"                                          (* bx = di (A) *)
  ^ "\xb4\x40\xb9\x01\x00\xba\x02\x01\xcd\x21"    (* write 'A' via A at pos 0 *)
  ^ "\x89\xf3"                                          (* bx = si (B) *)
  ^ "\xb4\x42\xb0\x00\xb9\x00\x00\xba\x01\x00\xcd\x21" (* seek B to offset 1 *)
  ^ "\x89\xf3"                                          (* bx = si (B) *)
  ^ "\xb4\x40\xb9\x01\x00\xba\x03\x01\xcd\x21"    (* write 'B' via B at pos 1 *)
  ^ "\x89\xfb\xb4\x3e\xcd\x21"                        (* close A *)
  ^ "\x89\xf3\xb4\x3e\xcd\x21"                        (* close B *)
  ^ "\xb8\x00\x4c\xcd\x21"                            (* exit *)

let boot_separate_opens () =
  let t = Dos_machine.create () in
  Dos_machine.mount_file t "F.DAT" "";
  Dos_machine.load_com t separate_opens_com;
  t

let () =
  (* [round_trip] returns the snapshot at the cut (after n steps), the same
     as [boot_dup]'s use above -- checking anything past that point means
     restoring it again and running on, which is what follows.
     [read_mounted] only reflects a name's bytes from the moment something
     closes it (that is what flushes a handle's buffer back to
     [host_files]), so measuring directly: at n=20 the file still reads
     empty and the machine has not exited -- both opens are done (a shared
     buffer exists) but neither write, either close, or exit has happened
     yet. m=5,000 clears the rest of the program with plenty of room
     ([run_until] stops at [exited] regardless, so extra budget past
     completion is harmless). *)
  let saved = round_trip ~name:"separate-opens" ~boot:boot_separate_opens ~n:20 ~m:5000 in
  let t = restore_exn saved in
  run t 5000;
  check "separate-opens: the program exited" (Dos_machine.exited t);
  check "separate-opens: two handles opened separately still share one buffer \
         after a snapshot round trip, so both offsets' writes survive"
    (Dos_machine.read_mounted t "F.DAT" = Some "AB")

(* ---------- a machine with every subsystem away from its default ----------

   A parent shrinks its block, EXECs C1.COM (exits with code 42h) and then
   C2.COM, which never returns, so one EXEC frame stays open and the last
   child code is 42h. C2 sets mode 12h and writes a planar byte, sets DAC
   entry 5, sets its DTA, leaves two names in the findfirst queue, opens an
   FCB and reads one record, opens a handle, allocates two EMS pages and
   leaves both mapped with a byte written through the frame, latches PIT
   counter 0, then CLI and spins, so the next timer tick stays pending.
   The host attaches the mouse at (123, 45) with the left button down.

   Its round trip also compares the restored fields to the original's one by
   one, so a reader that drops or swaps a field fails by name even where the
   bytes alone would say only "different". *)

let rich_parent =
  let n1 = 0x100 + 7 + 11 + 11 + 2 in
  let n2 = n1 + String.length "C1.COM\000" in
  let epb = n2 + String.length "C2.COM\000" in
  let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff)) in
  let exec name = "\xb8\x00\x4b\xba" ^ w name ^ "\xbb" ^ w epb ^ "\xcd\x21" in
  "\xb4\x4a\xbb\x20\x00\xcd\x21"                (* shrink to 20h paras *)
  ^ exec n1 ^ exec n2
  ^ "\xeb\xfe"
  ^ "C1.COM\000" ^ "C2.COM\000" ^ String.make 14 '\000'

let rich_child1 = "\xb8\x42\x4c\xcd\x21"         (* exit 42h *)

(* Data lives at fixed offsets past the code: pattern 300h, FCB 310h,
   DTA 340h, handle file name 3C0h. *)
let rich_child2 =
  let code =
    "\xb8\x12\x00\xcd\x10"                       (* mode 12h *)
    ^ "\xb8\x00\xa0\x8e\xc0\x26\xc6\x06\x00\x00\x5a" (* A000:0 = 5Ah *)
    ^ "\xba\xc8\x03\xb0\x05\xee\x42"            (* DAC write index 5 *)
    ^ "\xb0\x0a\xee\xb0\x14\xee\xb0\x1e\xee"    (* = (10, 20, 30) *)
    ^ "\xb4\x1a\xba\x40\x03\xcd\x21"            (* DTA = DS:340h *)
    ^ "\xb4\x4e\xb9\x00\x00\xba\x00\x03\xcd\x21" (* findfirst *.DAT *)
    ^ "\xb4\x0f\xba\x10\x03\xcd\x21"            (* FCB open F1.DAT *)
    ^ "\xb4\x14\xba\x10\x03\xcd\x21"            (* FCB read one record *)
    ^ "\xb8\x02\x3d\xba\xc0\x03\xcd\x21"        (* open F2.DAT -> handle 5 *)
    ^ "\xb4\x43\xbb\x02\x00\xcd\x67"            (* EMS: 2 pages -> DX *)
    ^ "\xb8\x00\x44\xbb\x01\x00\xcd\x67"        (* page 1 -> frame 0 *)
    ^ "\xb8\x00\xd0\x8e\xc0\x26\xc6\x06\x00\x00\x77" (* D000:0 = 77h *)
    ^ "\xb8\x00\x44\xbb\x00\x00\xcd\x67"        (* page 0 -> frame 0 *)
    ^ "\xb8\x01\x44\xbb\x01\x00\xcd\x67"        (* page 1 -> frame 1 *)
    ^ "\xb0\x00\xe6\x43"                        (* latch PIT counter 0 *)
    ^ "\xfa\xeb\xfe"                            (* cli; spin *)
  in
  assert (String.length code <= 0x200);
  let pad s n = s ^ String.make (n - String.length s) '\000' in
  pad code 0x200
  ^ pad "*.DAT\000" 0x10
  ^ pad ("\000F1      DAT") 0x30
  ^ String.make 0x80 '\000'
  ^ "F2.DAT\000"

let boot_rich () =
  let t = Dos_machine.create () in
  Dos_machine.mount_file t "C1.COM" rich_child1;
  Dos_machine.mount_file t "C2.COM" rich_child2;
  Dos_machine.mount_file t "F1.DAT" (String.init 200 (fun i -> Char.chr (i land 0xff)));
  Dos_machine.mount_file t "F2.DAT" "second";
  Dos_machine.mount_file t "F3.DAT" "third";
  Dos_machine.attach_mouse t;
  Dos_machine.set_mouse t ~x:123 ~y:45 ~buttons:1;
  Dos_machine.load_com t rich_parent;
  t

let ports_bytes (t : Dos_machine.t) =
  let w = Dos_snap_codec.writer () in
  Dos_ports.write_state w t.Dos_state.ports;
  Dos_snap_codec.contents w

let same_fields name (a : Dos_machine.t) (b : Dos_machine.t) =
  let open Dos_state in
  let c what cond = check (Printf.sprintf "%s: restored %s" name what) cond in
  c "registers" (Cpu86.save_state a.cpu = Cpu86.save_state b.cpu);
  c "RAM" (Bytes.equal a.mem b.mem);
  c "video planes" (Dos_video.planes a.video = Dos_video.planes b.video);
  c "video mode" (Dos_video.mode a.video = Dos_video.mode b.video);
  c "DAC" (Dos_ports.palette a.ports = Dos_ports.palette b.ports);
  c "ports (PIT latch)" (ports_bytes a = ports_bytes b);
  c "EXEC frame count" (List.length a.exec_frames = List.length b.exec_frames);
  List.iter2
    (fun fa fb ->
      c "EXEC frame" (fa.parent = fb.parent && fa.parent_psp = fb.parent_psp
                      && fa.parent_dta = fb.parent_dta && fa.parent_blocks = fb.parent_blocks
                      && List.map fst fa.parent_handles = List.map fst fb.parent_handles))
    a.exec_frames b.exec_frames;
  c "last child code" (a.last_child_code = b.last_child_code);
  c "pending IRQ0" (a.pending_irq0 = b.pending_irq0);
  c "last tick" (a.last_tick = b.last_tick);
  c "EMS mapping" (a.ems_mapped = b.ems_mapped);
  c "EMS pages" (sorted_bindings a.ems_pages = sorted_bindings b.ems_pages);
  c "mouse x" (a.mouse.mouse_x = b.mouse.mouse_x);
  c "mouse y" (a.mouse.mouse_y = b.mouse.mouse_y);
  c "mouse buttons" (a.mouse.mouse_buttons = b.mouse.mouse_buttons);
  c "mouse present" (a.mouse.mouse_present = b.mouse.mouse_present);
  c "FCBs" (sorted_bindings a.fcbs = sorted_bindings b.fcbs);
  c "DTA" (a.dta = b.dta);
  c "find queue" (a.find_queue = b.find_queue);
  c "handles"
    (List.map (fun (n, h) -> (n, h.hname, h.data, h.pos)) (sorted_bindings a.handles)
     = List.map (fun (n, h) -> (n, h.hname, h.data, h.pos)) (sorted_bindings b.handles));
  c "psp" (a.psp_seg = b.psp_seg);
  c "blocks" (a.blocks = b.blocks)

let () =
  let open Dos_state in
  let n = 60_000 in
  let a = boot_rich () in
  run a n;
  (* The state this test is about must be there, or the round trip below
     proves nothing. *)
  let pre what cond = check ("rich machine has " ^ what) cond in
  pre "one open EXEC frame" (List.length a.exec_frames = 1);
  pre "last child code 42h" (a.last_child_code = 0x42);
  pre "a pending timer tick" a.pending_irq0;
  pre "mode 12h" (Dos_video.mode a.video = 0x12);
  pre "a planar byte" (Bytes.get (Dos_video.planes a.video).(0) 0 = '\x5a');
  pre "DAC entry 5" ((Dos_ports.palette a.ports).(5) = (10, 20, 30));
  pre "two EMS pages mapped"
    (match a.ems_mapped with [| (h, 0); (h', 1); (0, 0); (0, 0) |] -> h = h' && h > 0 | _ -> false);
  pre "the EMS byte in page 1"
    (Hashtbl.fold (fun _ pages acc -> acc || Bytes.get pages.(1) 0 = '\x77') a.ems_pages false);
  pre "an FCB one record in" (List.map (fun (_, (_, p)) -> p) (sorted_bindings a.fcbs) = [ 128 ]);
  pre "two names left in the find queue" (List.length a.find_queue = 2);
  pre "handle 5 open" (Hashtbl.mem a.handles 5);
  pre "the mouse at (123, 45), button 1"
    (a.mouse.mouse_x = 123 && a.mouse.mouse_y = 45 && a.mouse.mouse_buttons = 1);
  let saved = save_exn a in
  let b = restore_exn saved in
  same_fields "rich" a b;
  ignore (round_trip ~name:"rich" ~boot:boot_rich ~n ~m:40_000)

(* ---------- save refuses what restore would refuse ---------- *)

let refused_at_save name t =
  match Dos_snapshot.save t with
  | Error (Dos_snapshot.Unsaveable _) -> ()
  | Ok s ->
    check (name ^ ": save refuses it") false;
    (match Dos_snapshot.restore s with
     | Ok _ -> ()
     | Error _ -> check (name ^ ": ... and wrote a snapshot restore refuses") false)

(* INT 21h AH=1Ah with DS=FFFF DX=0020: an 8086 wraps FFFF:0020 to physical
   00010. The DTA used to come out as 100010h, which save wrote and restore
   refused. *)
let () =
  let com = "\xb8\xff\xff\x8e\xd8\xba\x20\x00\xb4\x1a\xcd\x21\xeb\xfe" in
  let t = Dos_machine.create () in
  Dos_machine.load_com t com;
  run t 100;
  check "FFFF:0020 wraps to physical 10h" (t.Dos_state.dta = 0x10);
  ignore (restore_exn (save_exn t));
  t.Dos_state.dta <- 0x100010;
  refused_at_save "a DTA past 1MB" t

(* Sixteen opens: handles 5..19 fill the 20-entry table, the sixteenth is
   DOS error 4. Closing 7 and opening again reuses 7, the lowest free. *)
let () =
  let code =
    "\xb9\x10\x00\xbf\x00\x02"                   (* cx = 16, di = 200h *)
    ^ "\xb8\x00\x3d\xba\x00\x03\xcd\x21"        (* L: open F.DAT *)
    ^ "\x89\x05\x47\x47\xe2\xf2"                (* [di] = ax; di += 2; loop L *)
    ^ "\xb4\x3e\xbb\x07\x00\xcd\x21"            (* close 7 *)
    ^ "\xb8\x00\x3d\xba\x00\x03\xcd\x21"        (* open again *)
    ^ "\x89\x05\xeb\xfe"                        (* [di] = ax; spin *)
  in
  let com = code ^ String.make (0x200 - String.length code) '\000' ^ "F.DAT\000" in
  let t = Dos_machine.create () in
  Dos_machine.mount_file t "F.DAT" "x";
  Dos_machine.load_com t com;
  run t 2_000;
  let word i = Dos_machine.mem_read t (0x10200 + (2 * i))
               lor (Dos_machine.mem_read t (0x10201 + (2 * i)) lsl 8) in
  check "handles 5..19" (List.init 15 word = List.init 15 (fun i -> i + 5));
  check "the sixteenth open is error 4" (word 15 = 4);
  check "a closed number is reused" (word 16 = 7);
  ignore (restore_exn (save_exn t));
  Hashtbl.replace t.Dos_state.handles Dos_state.max_handles
    { Dos_state.hname = "F.DAT"; data = ref Bytes.empty; pos = 0 };
  refused_at_save "a handle number past the table" t

(* 255 EMS allocations: handles 1..254, then 85h. Freeing 3 and allocating
   again gives 3. *)
let () =
  let com =
    "\xb9\xff\x00"                               (* cx = 255 *)
    ^ "\xb4\x43\xbb\x01\x00\xcd\x67\xe2\xf7"    (* L: allocate 1 page; loop L *)
    ^ "\xa3\x00\x02"                            (* [200h] = ax *)
    ^ "\xb4\x45\xba\x03\x00\xcd\x67"            (* free handle 3 *)
    ^ "\xb4\x43\xbb\x01\x00\xcd\x67"            (* allocate again *)
    ^ "\xa3\x02\x02\x89\x16\x04\x02"            (* [202h] = ax; [204h] = dx *)
    ^ "\xeb\xfe"
  in
  let t = Dos_machine.create () in
  Dos_machine.load_com t com;
  run t 5_000;
  let rd i = Dos_machine.mem_read t (0x10200 + i) in
  check "EMS: the 255th allocation is 85h" (rd 1 = 0x85);
  check "EMS: allocation after a free succeeds" (rd 3 = 0);
  check "EMS: the freed number is reused" (rd 4 = 3 && rd 5 = 0);
  check "EMS: 254 handles" (Hashtbl.length t.Dos_state.ems_pages = 254);
  ignore (restore_exn (save_exn t))

(* Inconsistent EMS state: refused by save, and by restore when the bytes
   are crafted past save. *)
let () =
  let t = boot_rich () in
  run t 60_000;
  let saved = save_exn t in
  let open Dos_state in
  let mapped = Array.copy t.ems_mapped in
  t.ems_mapped.(2) <- (9, 0);
  refused_at_save "an EMS frame mapping a handle that does not exist" t;
  Array.blit mapped 0 t.ems_mapped 0 (Array.length mapped);
  let h, _ = t.ems_mapped.(0) in
  let pages = Hashtbl.find t.ems_pages h in
  let page = pages.(0) in
  pages.(0) <- Bytes.make 3 '\000';
  refused_at_save "an EMS page that is not 16KB" t;
  pages.(0) <- page;
  (* The payload ends with the frame mapping (count, then four pairs) and
     the mouse (42 bytes). Point frame page 2 at handle 9, which does not
     exist, and fix the checksum so only the consistency check can refuse. *)
  let header_len = magic_len + 8 + 32 + 16 in
  let mouse_len = 1 + (3 * 8) + 1 + (2 * 8) in
  let at = String.length saved - mouse_len - (4 * 16) + (2 * 16) in
  let b = Bytes.of_string saved in
  Bytes.set_int64_be b at 9L;
  let payload = Bytes.sub_string b header_len (Bytes.length b - header_len) in
  Bytes.blit_string (Digest.string payload) 0 b (header_len - 16) 16;
  match Dos_snapshot.restore (Bytes.to_string b) with
  | Error (Dos_snapshot.Corrupt m) ->
    check "a crafted EMS mapping is refused by the consistency check"
      (m = "an EMS frame page maps a page that does not exist")
  | Ok _ -> check "a crafted EMS mapping is refused at restore" false
  | Error e -> check ("crafted EMS mapping: " ^ Dos_snapshot.error_to_string e) false

let () =
  match Sys.getenv_opt "SAMGUK3_DIR" with
  | None -> print_endline "SAMGUK3_DIR unset: game round trip skipped"
  | Some dir ->
    let read path = In_channel.with_open_bin path In_channel.input_all in
    let files =
      Sys.readdir dir |> Array.to_list |> List.sort compare
      |> List.filter (fun f -> not (Sys.is_directory (Filename.concat dir f)))
      |> List.map (fun f -> (f, read (Filename.concat dir f)))
    in
    let steps name default =
      Option.fold ~none:default ~some:int_of_string (Sys.getenv_opt name)
    in
    let n = steps "SNAPSHOT_N" 30_000_000 and m = steps "SNAPSHOT_M" 20_000_000 in
    let boot () =
      let t = Dos_machine.create () in
      List.iter (fun (f, d) -> Dos_machine.mount_file t f d) files;
      Dos_machine.load_com t (List.assoc "KOEI.COM" files);
      t
    in
    let saved = round_trip ~name:"samguk3" ~boot ~n ~m in
    let t0 = Unix.gettimeofday () in
    let t = restore_exn saved in
    let t1 = Unix.gettimeofday () in
    ignore (save_exn t);
    let t2 = Unix.gettimeofday () in
    Printf.printf "samguk3: %d bytes after %d steps, mode %02xh, restore %.1f ms, save %.1f ms\n"
      (String.length saved) n (Dos_machine.video_mode t)
      ((t1 -. t0) *. 1000.) ((t2 -. t1) *. 1000.)

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos snapshot: all passed"
