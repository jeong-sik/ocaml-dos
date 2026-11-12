(* Directories: Dos_path reading names as MS-DOS 5 does, and the INT 21h
   services that use them, driven by COM programs assembled here. Code
   starts at 0x100, strings at 0x400, results at 0x800 (AX, then FLAGS). *)

let failed = ref 0

let check name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* ---------- Dos_path ---------- *)

let parsed ?(cwd = Dos_path.root) s =
  Result.map (fun (p : Dos_path.t) -> (p :> string list)) (Dos_path.parse ~cwd s)

let under dir = match Dos_path.parse ~cwd:Dos_path.root dir with
  | Ok p -> p | Error _ -> failwith dir

let test_path () =
  let open Dos_path in
  check "a relative name starts at the root" (parsed "data\\x.dat" = Ok [ "DATA"; "X.DAT" ]);
  check "C: and / are DOS too" (parsed "C:\\DATA/X.DAT" = Ok [ "DATA"; "X.DAT" ]);
  check "C: without a separator is the current directory"
    (parsed ~cwd:(under "DATA") "c:x" = Ok [ "DATA"; "X" ]);
  check ".. goes up" (parsed ~cwd:(under "DATA") "..\\Y" = Ok [ "Y" ]);
  check ". stays" (parsed ~cwd:(under "DATA") ".\\Z" = Ok [ "DATA"; "Z" ]);
  check "a leading separator is the root" (parsed ~cwd:(under "DATA") "\\Z" = Ok [ "Z" ]);
  check "a lone separator is the root" (parsed ~cwd:(under "DATA") "\\" = Ok []);
  check ".. above the root" (parsed ".." = Error Above_root);
  check "another drive" (parsed "D:\\X" = Error Bad_drive);
  check "a long name is cut to 8.3" (parsed "LongFileName.Text" = Ok [ "LONGFILE.TEX" ]);
  List.iter
    (fun s -> check (Printf.sprintf "%S is not a name" s) (parsed s = Error Bad_name))
    [ ""; "A.B.C"; "A\\\\B"; "DATA\\"; "A B"; "A*"; ".X"; "A?B"; "A|B"; "..." ];
  check "a pattern keeps its wildcards"
    (Result.map (fun ((d : Dos_path.t), p) -> ((d :> string list), p)) (parse_pattern ~cwd:root "data\\*.dat")
     = Ok ([ "DATA" ], "*.DAT"));
  check "a bare pattern searches the current directory"
    (Result.map (fun ((d : Dos_path.t), p) -> ((d :> string list), p)) (parse_pattern ~cwd:(under "DATA") "*.*")
     = Ok ([ "DATA" ], "*.*"));
  check "the key joins with \\" (key (under "data\\x.dat") = "DATA\\X.DAT");
  check "the root's key is empty" (key root = "" && of_key "" = Some root);
  check "a key reads back" (of_key "DATA\\X.DAT" = Some (under "DATA\\X.DAT"));
  List.iter
    (fun k -> check (Printf.sprintf "%S is not a key" k) (of_key k = None))
    [ "data\\x"; "A\\\\B"; "A\\.\\B"; "\\A"; "LONGFILENAME" ];
  check "within itself" (is_within ~dir:(under "DATA") (under "DATA"));
  check "within, below" (is_within ~dir:(under "DATA") (under "DATA\\X"));
  check "a prefix of the name is not a parent" (not (is_within ~dir:(under "DA") (under "DATA")));
  check "everything is within the root" (is_within ~dir:root (under "A\\B"))

(* ---------- programs ---------- *)

let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let mov_ax v = "\xb8" ^ le16 v
let mov_cx v = "\xb9" ^ le16 v
let mov_dx v = "\xba" ^ le16 v
let mov_si v = "\xbe" ^ le16 v
let mov_di v = "\xbf" ^ le16 v
let int21 = "\xcd\x21"
let mov_bx_ax = "\x89\xc3"
let rep_movsb = "\xfc\xf3\xa4"   (* cld; rep movsb *)
let quit = mov_ax 0x4C00 ^ int21

let code_end = 0x400
let results = 0x800

(* AX into the slot, then FLAGS after it. *)
let save slot = "\xa3" ^ le16 slot ^ "\x9c\x58" ^ "\xa3" ^ le16 (slot + 2)
let slot i = results + (4 * i)

(* Strings from 0x400, each NUL-terminated; [at name] is its address. *)
let layout strings =
  let _, placed =
    List.fold_left
      (fun (off, acc) s -> (off + String.length s + 1, (s, off) :: acc))
      (code_end, []) strings
  in
  let at s = List.assoc s placed in
  let data = String.concat "" (List.map (fun s -> s ^ "\000") strings) in
  (at, data)

let image code data =
  if String.length code > code_end - 0x100 then failwith "code runs into the strings";
  code ^ String.make (code_end - 0x100 - String.length code) '\x90' ^ data

let machine () = Dos_machine.create ()

let run m =
  ignore (Dos_machine.run_until m ~max_steps:200_000 ~stop:(fun _ -> false))

let word m off = Dos_state.rd16 m ((m.Dos_state.psp_seg lsl 4) + off)
let bytes_at m off n = String.init n (fun i -> Char.chr (Dos_state.rd8 m ((m.Dos_state.psp_seg lsl 4) + off + i)))
let asciiz m off = match String.index_opt (bytes_at m off 64) '\000' with
  | Some i -> String.sub (bytes_at m off 64) 0 i | None -> bytes_at m off 64

let ok_at m i = word m (slot i + 2) land 1 = 0
let error_at m i = if ok_at m i then None else Some (word m (slot i))

let dta_name = 0x80 + 0x1E
let names_at = 0xA00   (* the found names, 16 bytes apart *)

let copy_found k = mov_si dta_name ^ mov_di (names_at + (16 * k)) ^ mov_cx 13 ^ rep_movsb

let test_make_change_find () =
  let at, data =
    layout [ "SAVE"; "NOPE\\X"; "GAME.SAV"; ".."; "SAVE\\GAME.SAV"; "SAVE\\*.*"; "CWD" ]
  in
  let cwd_buf = 0xC00 in
  let code =
    String.concat ""
      [ mov_ax 0x3900; mov_dx (at "SAVE"); int21; save (slot 0);
        mov_ax 0x3900; mov_dx (at "SAVE"); int21; save (slot 1);
        mov_ax 0x3900; mov_dx (at "NOPE\\X"); int21; save (slot 2);
        mov_ax 0x3B00; mov_dx (at "SAVE"); int21; save (slot 3);
        mov_ax 0x4700; mov_dx 0; mov_si cwd_buf; int21; save (slot 4);
        mov_ax 0x3C00; mov_cx 0; mov_dx (at "GAME.SAV"); int21; mov_bx_ax; save (slot 5);
        mov_ax 0x3E00; int21; save (slot 6);
        mov_ax 0x3B00; mov_dx (at ".."); int21; save (slot 7);
        mov_ax 0x3D00; mov_dx (at "SAVE\\GAME.SAV"); int21; save (slot 8);
        mov_ax 0x3A00; mov_dx (at "SAVE"); int21; save (slot 9);
        mov_ax 0x4E00; mov_cx 0x10; mov_dx (at "SAVE\\*.*"); int21; save (slot 10); copy_found 0;
        mov_ax 0x4F00; int21; save (slot 11); copy_found 1;
        mov_ax 0x4F00; int21; save (slot 12); copy_found 2;
        mov_ax 0x4F00; int21; save (slot 13);
        quit ]
  in
  let m = machine () in
  Dos_machine.load_com m (image code data);
  run m;
  check "the program ran to its end" (Dos_machine.exited m);
  check "MKDIR makes a directory" (ok_at m 0);
  check "MKDIR on a name that exists is access denied" (error_at m 1 = Some 5);
  check "MKDIR under a missing directory is path not found" (error_at m 2 = Some 3);
  check "CHDIR into it" (ok_at m 3);
  check "GETCWD names it without a leading backslash" (ok_at m 4 && asciiz m cwd_buf = "SAVE");
  check "CREATE in the current directory" (ok_at m 5 && ok_at m 6);
  check "CHDIR .." (ok_at m 7);
  check "OPEN by a path through the directory" (ok_at m 8);
  check "RMDIR of a directory that holds a file is access denied" (error_at m 9 = Some 5);
  check "FINDFIRST in a subdirectory shows . first" (ok_at m 10 && asciiz m names_at = ".");
  check "then .." (ok_at m 11 && asciiz m (names_at + 16) = "..");
  check "then the file" (ok_at m 12 && asciiz m (names_at + 32) = "GAME.SAV");
  check "then no more files" (error_at m 13 = Some 18);
  check "the host sees the file under its directory" (Dos_machine.read_mounted m "save/game.sav" = Some "");
  check "the program left the root current" ((m.Dos_state.cwd :> string list) = [])

let save_snapshot m = match Dos_snapshot.save m with
  | Ok s -> s | Error e -> failwith (Dos_snapshot.save_error_to_string e)
let restore s = match Dos_snapshot.restore s with
  | Ok m -> m | Error e -> failwith (Dos_snapshot.error_to_string e)

let test_mounted_tree () =
  let at, data =
    layout [ "C:\\DATA\\level1.dat"; "DATA"; "LEVEL1.DAT"; "..\\DATA\\LEVEL1.DAT"; "\\LEVEL1.DAT"; "D:\\X"; "\\DATA" ]
  in
  let opens = [ "LEVEL1.DAT"; "..\\DATA\\LEVEL1.DAT"; "\\LEVEL1.DAT"; "D:\\X" ] in
  let code =
    String.concat ""
      ([ mov_ax 0x3D00; mov_dx (at "C:\\DATA\\level1.dat"); int21; save (slot 0);
         mov_ax 0x3B00; mov_dx (at "DATA"); int21; save (slot 1) ]
       @ List.mapi (fun i s -> mov_ax 0x3D00 ^ mov_dx (at s) ^ int21 ^ save (slot (2 + i))) opens
       @ [ mov_ax 0x3A00; mov_dx (at "\\DATA"); int21; save (slot 6);
           "\xeb\xfe" ])   (* jmp $ — stay, so the snapshot sees DATA current *)
  in
  let m = machine () in
  check "a file mounts under a directory" (Dos_machine.mount_file m "DATA/LEVEL1.DAT" "L1" = Ok ());
  Dos_machine.load_com m (image code data);
  run m;
  check "an absolute name with the drive and lower case" (ok_at m 0);
  check "CHDIR into the mounted directory" (ok_at m 1);
  check "a name in the current directory" (ok_at m 2);
  check "a name through .." (ok_at m 3);
  check "the root does not hold the file" (error_at m 4 = Some 2);
  check "another drive is path not found" (error_at m 5 = Some 3);
  check "RMDIR of the current directory" (error_at m 6 = Some 16);
  let r = restore (save_snapshot m) in
  check "the current directory survives a snapshot" ((r.Dos_state.cwd :> string list) = [ "DATA" ]);
  check "the directory table survives a snapshot" (Hashtbl.mem r.Dos_state.dirs "DATA");
  check "the restored machine is the same" (save_snapshot r = save_snapshot m)

let test_mount_names () =
  let m = machine () in
  let refused name = match Dos_machine.mount_file m name "x" with Error _ -> true | Ok () -> false in
  ignore (Dos_machine.mount_file m "DATA/A.DAT" "a");
  check "a name DOS cannot hold is refused" (refused "A.B.C" && refused "FOO BAR" && refused "");
  check "another drive is refused" (refused "D:\\X");
  check "a directory's name is refused" (refused "DATA");
  check "the root is refused" (refused "\\");
  check "a long name is cut as DOS cuts it"
    (Dos_machine.mount_file m "LongFileName.Text" "l" = Ok ()
     && Dos_machine.read_mounted m "LONGFILE.TEX" = Some "l");
  check "mounted names are the path keys"
    (Dos_machine.mounted_names m = [ "DATA\\A.DAT"; "LONGFILE.TEX" ])

let () =
  test_path ();
  test_make_change_find ();
  test_mounted_tree ();
  test_mount_names ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos directories: all passed"
