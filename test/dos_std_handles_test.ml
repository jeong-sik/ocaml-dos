(* The standard handles 0-4 are entries in the handle table, as SYSINIT
   opens them: guest programs redirect, close, reuse and seek them. Each
   INT 21h call leaves AX, DX and the carry (as CX = 0 or FFFFh) in a
   6-byte record. *)

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

(* where the programs keep their names, text and buffers *)
let first_name = 0x2100
let second_name = 0x2110
let text_at = 0x2120
let buffer_at = 0x2200

let peek m off = Dos_machine.mem_read m (psp_base + off)
let peek16 m off = peek m off lor (peek m (off + 1) lsl 8)
let ax_of m i = peek16 m (results + (i * record_bytes))
let dx_of m i = peek16 m (results + (i * record_bytes) + 2)
let carry_of m i = peek16 m (results + (i * record_bytes) + 4) <> 0

(* call [i]: INT 21h with these registers, then its record *)
let call ?(bx = 0) ?(cx = 0) ?(dx = 0) i ~ax =
  let r = results + (i * record_bytes) in
  mov_bx bx ^ mov_cx cx ^ mov_dx dx ^ mov_ax ax ^ int_ 0x21
  ^ sbb_cx_cx ^ store_ax r ^ store_dx (r + 2) ^ store_cx (r + 4)

(* [code] at 100h and the exit after it, then each (address, bytes) of
   [data] in ascending order. Without the exit the CPU would run on into
   the records and execute them. *)
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

let host_file m name = Hashtbl.find_opt m.Dos_state.host_files name

let screen_has m s =
  let screen = Dos_machine.screen_text m in
  let n = String.length s in
  let rec at i = i + n <= String.length screen && (String.sub screen i n = s || at (i + 1)) in
  at 0

let create = 0x3C00
let open_read = 0x3D00
let close = 0x3E00
let read = 0x3F00
let write = 0x4000
let seek_set = 0x4200
let seek_cur = 0x4201
let seek_end = 0x4202
let device_info = 0x4400
let force_dup = 0x4600
let access_denied = 5
let invalid_handle = 6

(* AH=40h on 1 writes the screen until AH=46h puts a file under stdout;
   then it writes the file. *)
let () =
  let shown = "ON" and text = "HI" in
  let shown_at = text_at + String.length text in
  let m =
    run
      ~data:[ (first_name, "OUT.TXT\000"); (text_at, text ^ shown) ]
      (call 0 ~ax:write ~bx:1 ~cx:(String.length shown) ~dx:shown_at
       ^ call 1 ~ax:create ~dx:first_name
       ^ call 2 ~ax:force_dup ~bx:5 ~cx:1
       ^ call 3 ~ax:write ~bx:1 ~cx:(String.length text) ~dx:text_at
       ^ call 4 ~ax:close ~bx:1)
  in
  ok "write to stdout" m 0;
  if not (screen_has m shown) then fail_with "stdout: %S did not reach the screen" shown;
  check "create: lowest free is 5" (ax_of m 1) 5;
  ok "46h 5 -> 1" m 2;
  ok "write through 1" m 3;
  check "write through 1: count" (ax_of m 3) (String.length text);
  ok "close 1" m 4;
  if host_file m "OUT.TXT" <> Some (Bytes.of_string text) then
    fail_with "stdout redirected: OUT.TXT does not hold %S" text;
  if screen_has m text then fail_with "stdout redirected: %S reached the screen" text

(* AH=46h puts a file under stdin: AH=3Fh on 0 reads the file at once
   instead of waiting for the keyboard. *)
let () =
  let text = "QZ" in
  let m =
    run ~mounts:[ ("IN.TXT", text) ] ~data:[ (first_name, "IN.TXT\000") ]
      (call 0 ~ax:open_read ~dx:first_name
       ^ call 1 ~ax:force_dup ~bx:5 ~cx:0
       ^ call 2 ~ax:read ~bx:0 ~cx:(String.length text) ~dx:buffer_at)
  in
  ok "46h 5 -> 0" m 1;
  ok "read through 0" m 2;
  check "read through 0: count" (ax_of m 2) (String.length text);
  check "read through 0: bytes" (peek16 m buffer_at)
    (Char.code text.[0] lor (Char.code text.[1] lsl 8));
  check "the guest never waited for a key" m.Dos_state.kbd_requests 0

(* The standard handles close like any other, and a closed number is the
   next one handed out. *)
let () =
  let m =
    run
      ~data:[ (first_name, "A.TXT\000"); (second_name, "B.TXT\000"); (text_at, "X") ]
      (call 0 ~ax:close ~bx:3
       ^ call 1 ~ax:create ~dx:first_name
       ^ call 2 ~ax:close ~bx:0 ^ call 3 ~ax:close ~bx:2 ^ call 4 ~ax:close ~bx:4
       ^ call 5 ~ax:device_info ~bx:0
       ^ call 6 ~ax:create ~dx:second_name
       ^ call 7 ~ax:close ~bx:1
       ^ call 8 ~ax:write ~bx:1 ~cx:1 ~dx:text_at)
  in
  ok "close AUX" m 0;
  check "AUX's number is reused" (ax_of m 1) 3;
  ok "close stdin" m 2;
  ok "close stderr" m 3;
  ok "close PRN" m 4;
  error "4400h on closed stdin" m 5 invalid_handle;
  check "stdin's number is reused" (ax_of m 6) 0;
  ok "close stdout" m 7;
  error "write to closed stdout" m 8 invalid_handle

(* AH=46h closes CX first: a file there is written back, and BX = CX
   leaves the handle closed with error 6. *)
let () =
  let m =
    run
      ~data:[ (first_name, "A.TXT\000"); (second_name, "B.TXT\000"); (text_at, "X") ]
      (call 0 ~ax:create ~dx:first_name
       ^ call 1 ~ax:write ~bx:5 ~cx:1 ~dx:text_at
       ^ call 2 ~ax:force_dup ~bx:1 ~cx:5
       ^ call 3 ~ax:create ~dx:second_name
       ^ call 4 ~ax:force_dup ~bx:6 ~cx:6
       ^ call 5 ~ax:device_info ~bx:6
       ^ call 6 ~ax:device_info ~bx:5
       ^ call 7 ~ax:force_dup ~bx:1 ~cx:Dos_state.max_handles)
  in
  ok "46h 1 -> 5" m 2;
  if host_file m "A.TXT" <> Some (Bytes.of_string "X") then
    fail_with "46h onto an open file: A.TXT was not written back";
  check "B.TXT is 6" (ax_of m 3) 6;
  error "46h 6 -> 6" m 4 invalid_handle;
  error "6 is closed after 46h 6 -> 6" m 5 invalid_handle;
  check "5 is CON now" (dx_of m 6) 0x80D3;
  error "46h past the table" m 7 invalid_handle

(* AUX and PRN: PRN was opened write-only, so reading it is error 5 even
   for 0 bytes. A count of 0 never reaches either device. *)
let () =
  let m =
    run
      (call 0 ~ax:read ~bx:4 ~dx:buffer_at
       ^ call 1 ~ax:read ~bx:4 ~cx:1 ~dx:buffer_at
       ^ call 2 ~ax:read ~bx:3 ~dx:buffer_at
       ^ call 3 ~ax:write ~bx:3 ~dx:buffer_at
       ^ call 4 ~ax:write ~bx:4 ~dx:buffer_at)
  in
  error "read 0 bytes from PRN" m 0 access_denied;
  error "read 1 byte from PRN" m 1 access_denied;
  List.iter
    (fun (name, i) -> ok name m i; check (name ^ ": count") (ax_of m i) 0)
    [ ("read 0 bytes from AUX", 2); ("write 0 bytes to AUX", 3); ("write 0 bytes to PRN", 4) ]

(* Bytes to or from AUX or PRN need the BIOS ports and INT 24h; the
   machine stops and names the device. *)
let () =
  List.iter
    (fun (name, code, device) ->
      match run code with
      | _ -> fail_with "%s: the machine went on" name
      | exception Cpu86.Unsupported msg ->
        let n = String.length device in
        let rec names i = i + n <= String.length msg && (String.sub msg i n = device || names (i + 1)) in
        if not (names 0) then fail_with "%s: %S does not name %s" name msg device)
    [ ("write to PRN", call 0 ~ax:write ~bx:4 ~cx:1 ~dx:buffer_at, "PRN");
      ("write to AUX", call 0 ~ax:write ~bx:3 ~cx:1 ~dx:buffer_at, "AUX");
      ("read from AUX", call 0 ~ax:read ~bx:3 ~cx:1 ~dx:buffer_at, "AUX") ]

(* A device keeps a position that only AH=42h moves; its size is 0. 0, 1
   and 2 are one open, so they share it; AUX is another. *)
let () =
  let m =
    run
      (call 0 ~ax:seek_set ~bx:1 ~dx:5
       ^ call 1 ~ax:seek_cur ~bx:2
       ^ call 2 ~ax:seek_end ~bx:1 ~dx:2
       ^ call 3 ~ax:seek_cur ~bx:3)
  in
  List.iter
    (fun (name, i, want) ->
      ok name m i;
      check name (ax_of m i) want;
      check (name ^ ": DX") (dx_of m i) 0)
    [ ("stdout from the start", 0, 5); ("stderr shares stdout's position", 1, 5);
      ("from the end of a device", 2, 2); ("AUX has its own position", 3, 0) ]

(* A snapshot keeps 0, 1 and 2 as one record. *)
let () =
  let m = boot "" in
  let restored =
    match Dos_snapshot.save m with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok t -> t
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  let entry h = Hashtbl.find restored.Dos_state.handles h in
  if not (entry 0 == entry 1 && entry 1 == entry 2) then
    fail_with "snapshot: stdin, stdout and stderr came back as separate records";
  let targets = List.map (fun h -> (entry h).target) [ 0; 3; 4 ] in
  if targets <> Dos_state.[ Device Con; Device Aux; Device Prn ] then
    fail_with "snapshot: the standard devices came back changed"

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "standard handles: all passed"
