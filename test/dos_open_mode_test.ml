(* The mode AH=3Dh takes in AL: access, sharing and inheritance, from guest
   programs. Each INT 21h call leaves AX, DX and the carry (as CX = 0 or
   FFFFh) in a 6-byte record. *)

let failed = ref 0

let fail_with fmt = Printf.ksprintf (fun s -> incr failed; prerr_endline ("FAIL " ^ s)) fmt

let check name got want = if got <> want then fail_with "%s: got %04Xh want %04Xh" name got want

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax n = "\xb8" ^ w n
let mov_bx n = "\xbb" ^ w n
let mov_cx n = "\xb9" ^ w n
let mov_dx n = "\xba" ^ w n
let int_ n = "\xcd" ^ b n
let store_ax a = "\xa3" ^ w a
let store_cx a = "\x89\x0e" ^ w a
let store_dx a = "\x89\x16" ^ w a
let sbb_cx_cx = "\x19\xc9"
let quit = mov_ax 0x4c00 ^ int_ 0x21

let psp_base = 0x1000 * 16
let com_origin = 0x100
let results = 0x2000
let record_bytes = 6
let name_at = 0x2100
let second_name_at = 0x2110
let third_name_at = 0x2120
let epb_at = 0x2140
let text_at = 0x2160
let buffer_at = 0x2200

let peek m off = Dos_machine.mem_read m (psp_base + off)
let peek16 m off = peek m off lor (peek m (off + 1) lsl 8)
let ax_of m i = peek16 m (results + (i * record_bytes))
let dx_of m i = peek16 m (results + (i * record_bytes) + 2)
let carry_of m i = peek16 m (results + (i * record_bytes) + 4) <> 0

let call ?(bx = 0) ?(cx = 0) ?(dx = 0) i ~ax =
  let r = results + (i * record_bytes) in
  mov_bx bx ^ mov_cx cx ^ mov_dx dx ^ mov_ax ax ^ int_ 0x21
  ^ sbb_cx_cx ^ store_ax r ^ store_dx (r + 2) ^ store_cx (r + 4)

(* [code] at 100h and the exit after it, then each (address, bytes) of
   [data] in ascending order. *)
let layout code data =
  let image, _ =
    List.fold_left
      (fun (image, at) (addr, bytes) ->
        (image ^ String.make (addr - at) '\x00' ^ bytes, addr + String.length bytes))
      (code ^ quit, com_origin + String.length code + String.length quit)
      data
  in
  image

let boot ?(mounts = []) ?(data = []) code =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> Dos_machine.mount_file m n d) mounts;
  Dos_machine.load_com m (layout code data);
  m

let run ?mounts ?data code =
  let m = boot ?mounts ?data code in
  Dos_machine.run m ~max_steps:200_000;
  m

let ok name m i = if carry_of m i then fail_with "%s: carry set, AX=%04Xh" name (ax_of m i)

let error name m i code =
  if not (carry_of m i) then fail_with "%s: no carry" name
  else check (name ^ ": error") (ax_of m i) code

let file_is name m file want =
  match Hashtbl.find_opt m.Dos_state.host_files file with
  | Some got when Bytes.to_string got = want -> ()
  | Some got -> fail_with "%s: %s holds %S, want %S" name file (Bytes.to_string got) want
  | None -> fail_with "%s: %s is gone" name file

let open_ mode = 0x3D00 lor mode
let create = 0x3C00
let create_new = 0x5B00
let read = 0x3F00
let write = 0x4000
let seek_set = 0x4200
let dup = 0x4500
let device_info = 0x4400
let file_not_found = 2
let too_many_open_files = 4
let access_denied = 5
let invalid_access = 0x0C
let file_exists = 0x50

(* A read-only open cannot write, even 0 bytes; a write-only open cannot
   read, even 0 bytes. The mode is checked before the count. *)
let () =
  let m =
    run ~mounts:[ ("R.DAT", "AB"); ("W.DAT", "AB") ]
      ~data:[ (name_at, "R.DAT\000"); (second_name_at, "W.DAT\000"); (text_at, "Z") ]
      (call 0 ~ax:(open_ 0x00) ~dx:name_at
       ^ call 1 ~ax:write ~bx:5 ~cx:1 ~dx:text_at
       ^ call 2 ~ax:write ~bx:5 ~dx:text_at
       ^ call 3 ~ax:read ~bx:5 ~cx:1 ~dx:buffer_at
       ^ call 4 ~ax:(open_ 0x01) ~dx:second_name_at
       ^ call 5 ~ax:read ~bx:6 ~cx:1 ~dx:buffer_at
       ^ call 6 ~ax:read ~bx:6 ~dx:buffer_at
       ^ call 7 ~ax:write ~bx:6 ~cx:1 ~dx:text_at
       ^ call 8 ~ax:0x3E00 ~bx:6
       ^ call 9 ~ax:dup ~bx:5
       ^ call 10 ~ax:write ~bx:6 ~cx:1 ~dx:text_at)
  in
  error "write to a read-only open" m 1 access_denied;
  error "write 0 bytes to a read-only open" m 2 access_denied;
  ok "read a read-only open" m 3;
  check "read a read-only open: byte" (peek m buffer_at) (Char.code 'A');
  error "read a write-only open" m 5 access_denied;
  error "read 0 bytes from a write-only open" m 6 access_denied;
  ok "write a write-only open" m 7;
  file_is "write-only" m "W.DAT" "ZB";
  file_is "read-only" m "R.DAT" "AB";
  check "dup of the read-only open takes the closed 6" (ax_of m 9) 6;
  error "write through the dup" m 10 access_denied

(* A created file is open for reading and writing. *)
let () =
  let m =
    run ~data:[ (name_at, "C.DAT\000"); (text_at, "Q") ]
      (call 0 ~ax:create ~dx:name_at
       ^ call 1 ~ax:write ~bx:5 ~cx:1 ~dx:text_at
       ^ call 2 ~ax:seek_set ~bx:5
       ^ call 3 ~ax:read ~bx:5 ~cx:1 ~dx:buffer_at)
  in
  ok "write a created file" m 1;
  ok "read a created file" m 3;
  check "read a created file: byte" (peek m buffer_at) (Char.code 'Q')

(* Sharing modes up to 40h are accepted and, without SHARE, change
   nothing; above 40h, or an access above 2, is error 0Ch. Bit 7 is not
   part of either. *)
let () =
  let modes =
    [ (0x02, true); (0x12, true); (0x12, true); (0x22, true); (0x32, true); (0x42, true);
      (0x80, true); (0xC2, true); (0x03, false); (0x0F, false); (0x50, false);
      (0x70, false); (0xF0, false); (0x83, false) ]
  in
  let m =
    run ~mounts:[ ("S.DAT", "S") ] ~data:[ (name_at, "S.DAT\000") ]
      (String.concat "" (List.mapi (fun i (mode, _) -> call i ~ax:(open_ mode) ~dx:name_at) modes))
  in
  List.iteri
    (fun i (mode, good) ->
      let name = Printf.sprintf "open mode %02Xh" mode in
      if good then ok name m i else error name m i invalid_access)
    modes

(* A free number comes first, then the mode, then the name. *)
let () =
  let fill = List.init 15 (fun i -> call i ~ax:(open_ 0x00) ~dx:name_at) in
  let m =
    run ~mounts:[ ("S.DAT", "S") ]
      ~data:[ (name_at, "S.DAT\000"); (second_name_at, "NONE.DAT\000") ]
      (call 20 ~ax:(open_ 0x03) ~dx:second_name_at
       ^ call 21 ~ax:(open_ 0x00) ~dx:second_name_at
       ^ call 22 ~ax:create_new ~dx:name_at
       ^ String.concat "" fill
       ^ call 15 ~ax:(open_ 0x03) ~dx:second_name_at
       ^ call 16 ~ax:create ~dx:second_name_at
       ^ call 17 ~ax:create_new ~dx:name_at)
  in
  error "bad mode, no such file" m 20 invalid_access;
  error "no such file" m 21 file_not_found;
  error "create new over a file" m 22 file_exists;
  check "the table fills at 19" (ax_of m 14) 19;
  error "table full, bad mode, no such file" m 15 too_many_open_files;
  error "table full, create" m 16 too_many_open_files;
  error "table full, create new over a file" m 17 too_many_open_files

(* EXEC's child does not get an open made with bit 7, and its own open
   takes that number; the parent gets its handle back when the child
   ends. A created file is inherited. The child reports in its exit
   code: 1 = handle 5 is not open, 2 = handle 6 is not open, 4 = its open
   got 5, 8 = handle 7 is not open. *)
let child =
  let code =
    mov_ax 0x4400 ^ mov_bx 5 ^ int_ 0x21 ^ "\x18\xc9"           (* sbb cl,cl *)
    ^ mov_ax 0x4400 ^ mov_bx 6 ^ int_ 0x21 ^ "\x18\xed"       (* sbb ch,ch *)
    ^ "\x80\xe1\x01" ^ "\x80\xe5\x02" ^ "\x08\xe9"           (* and cl,1; and ch,2; or cl,ch *)
    ^ mov_ax 0x4400 ^ mov_bx 7 ^ int_ 0x21 ^ "\x18\xd2"       (* sbb dl,dl *)
    ^ "\x80\xe2\x08" ^ "\x08\xd1"                           (* and dl,8; or cl,dl *)
  in
  let after_open =
    "\x3d\x05\x00" ^ "\x75\x03" ^ "\x80\xc9\x04"              (* cmp ax,5; jne +3; or cl,4 *)
    ^ "\x88\xc8" ^ "\xb4\x4c" ^ int_ 0x21                      (* mov al,cl; 4Ch *)
  in
  let open_len = String.length (mov_ax 0 ^ mov_dx 0 ^ int_ 0x21) in
  let name_offset = com_origin + String.length code + open_len + String.length after_open in
  code ^ mov_ax 0x3D00 ^ mov_dx name_offset ^ int_ 0x21 ^ after_open ^ "C.DAT\000"

let parent_paragraphs = 0x300

let () =
  let m =
    run
      ~mounts:[ ("A.DAT", "A1"); ("B.DAT", "B1"); ("C.DAT", "C1"); ("CHILD.COM", child) ]
      ~data:
        [ (name_at, "A.DAT\000"); (second_name_at, "B.DAT\000");
          (third_name_at, "CHILD.COM\000"); (epb_at, String.make 14 '\000');
          (text_at, "D.DAT\000") ]
      (call 0 ~ax:(open_ 0x80) ~dx:name_at
       ^ call 1 ~ax:(open_ 0x00) ~dx:second_name_at
       ^ call 7 ~ax:create ~dx:text_at
       ^ call 2 ~ax:0x4A00 ~bx:parent_paragraphs
       ^ call 3 ~ax:0x4B00 ~dx:third_name_at ~bx:epb_at
       ^ call 4 ~ax:0x4D00
       ^ call 5 ~ax:device_info ~bx:5
       ^ call 6 ~ax:read ~bx:5 ~cx:1 ~dx:buffer_at)
  in
  check "no-inherit open is 5" (ax_of m 0) 5;
  check "created file is 7" (ax_of m 7) 7;
  ok "EXEC" m 3;
  check "the child saw 5 closed, 6 and 7 open, and got 5" (ax_of m 4 land 0xFF) 5;
  ok "the parent's 5 after the child" m 5;
  check "the parent's 5 is a file" (dx_of m 5) 0x0002;
  check "the parent's 5 still reads A.DAT" (peek m buffer_at) (Char.code 'A')

(* A snapshot keeps an open's mode and inheritance. *)
let () =
  let m =
    boot ~mounts:[ ("S.DAT", "S") ] ~data:[ (name_at, "S.DAT\000") ]
      (call 0 ~ax:(open_ 0x81) ~dx:name_at)
  in
  Dos_machine.run m ~max_steps:200_000;
  let restored =
    match Dos_snapshot.save m with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok t -> t
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  match Hashtbl.find_opt restored.Dos_state.handles 5 with
  | Some { Dos_state.access = Write_only; inheritable = false; _ } -> ()
  | Some _ -> fail_with "snapshot: handle 5 came back with another mode"
  | None -> fail_with "snapshot: handle 5 is gone"

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "open modes: all passed"
