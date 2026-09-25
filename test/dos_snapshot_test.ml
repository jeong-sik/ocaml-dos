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

let run t n = ignore (Dos_machine.run_until t ~max_steps:n ~stop:(fun _ -> false))

let restore_exn s =
  match Dos_snapshot.restore s with
  | Ok t -> t
  | Error e -> failwith (Dos_snapshot.error_to_string e)

let round_trip ~name ~boot ~n ~m =
  let direct = boot () in
  run direct (n + m);
  let a = boot () in
  run a n;
  let saved = Dos_snapshot.save a in
  check (name ^ ": saving twice gives the same bytes") (Dos_snapshot.save a = saved);
  let b = restore_exn saved in
  check (name ^ ": a restored machine saves the same bytes") (Dos_snapshot.save b = saved);
  run b m;
  check (name ^ ": N, restore, M is the machine that ran N+M")
    (Dos_snapshot.save b = Dos_snapshot.save direct);
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
   | Ok t -> check "another core's snapshot restores" (Dos_snapshot.save t = saved)
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
    ignore (Dos_snapshot.save t);
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
