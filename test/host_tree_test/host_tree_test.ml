(* A game folder on the host becomes the files a machine mounts, including
   the ones in folders below it. *)

let failed = ref 0

let check name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let write path data = Out_channel.with_open_bin path (fun oc -> output_string oc data)

let rec remove path =
  if Sys.is_directory path then begin
    Array.iter (fun n -> remove (Filename.concat path n)) (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

let with_tree build f =
  let root = Filename.temp_dir "host_tree_test" "" in
  Fun.protect ~finally:(fun () -> remove root) (fun () -> build root; f root)

let names = function Ok l -> List.map fst l | Error _ -> [ "<error>" ]

let test_folders_below () =
  with_tree
    (fun root ->
      write (Filename.concat root "A.TXT") "a";
      Sys.mkdir (Filename.concat root "DATA") 0o755;
      write (Filename.concat root "DATA/ITEMS.DAT") "items";
      Sys.mkdir (Filename.concat root "DATA/DEEP") 0o755;
      write (Filename.concat root "DATA/DEEP/X.BIN") "x";
      Sys.mkdir (Filename.concat root "EMPTY") 0o755)
    (fun root ->
      let got = Host_tree.files root in
      check "names are relative, with / between folders, sorted"
        (names got = [ "A.TXT"; "DATA/DEEP/X.BIN"; "DATA/ITEMS.DAT" ]);
      check "an empty folder adds no file" (not (List.mem "EMPTY" (names got)));
      let m = Dos_machine.create () in
      (match got with
       | Error _ -> check "mounting" false
       | Ok files ->
         List.iter
           (fun (name, path) ->
             match Dos_machine.mount_file m name (In_channel.with_open_bin path In_channel.input_all) with
             | Ok () -> ()
             | Error _ -> check ("mount " ^ name) false)
           files);
      check "a file in a folder is the guest's C:\\DATA\\ITEMS.DAT"
        (Dos_machine.read_mounted m "DATA\\ITEMS.DAT" = Some "items");
      check "two folders down"
        (Dos_machine.read_mounted m "DATA\\DEEP\\X.BIN" = Some "x");
      check "a file at the top" (Dos_machine.read_mounted m "A.TXT" = Some "a"))

let nest root levels =
  let rec go dir n =
    if n = 0 then write (Filename.concat dir "F.TXT") "f"
    else begin
      let below = Filename.concat dir "D" in
      Sys.mkdir below 0o755;
      go below (n - 1)
    end
  in
  go root levels

let test_depth_limit () =
  with_tree
    (fun root -> nest root Host_tree.max_depth)
    (fun root ->
      match Host_tree.files root with
      | Ok [ (name, _) ] ->
        check "the deepest allowed level keeps its file"
          (String.length name = (2 * Host_tree.max_depth) + 5)
      | _ -> check "max_depth levels are accepted" false);
  with_tree
    (fun root -> nest root (Host_tree.max_depth + 1))
    (fun root ->
      match Host_tree.files root with
      | Error e -> check "the error names the limit" (String.length e > 0)
      | Ok _ -> check "one level too deep is refused" false)

let () =
  test_folders_below ();
  test_depth_limit ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "host tree: all passed"
