(* AH=42h positions, AH=40h growth and the disk AH=36h describes, from
   guest programs. Each INT 21h call leaves AX, DX and the carry (as CX = 0
   or FFFFh) in a 6-byte record. *)

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
let store_bx a = "\x89\x1e" ^ w a
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
let text_at = 0x2120
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

(* AH=42h with the 32-bit offset split into CX:DX *)
let seek i ~ax ~bx offset = call i ~ax ~bx ~cx:(offset lsr 16) ~dx:(offset land 0xFFFF)

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

let run ?(mounts = []) ?(data = []) code =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> Dos_machine.mount_file m n d) mounts;
  Dos_machine.load_com m (layout code data);
  Dos_machine.run m ~max_steps:2_000_000;
  m

let ok name m i = if carry_of m i then fail_with "%s: carry set, AX=%04Xh" name (ax_of m i)

let error name m i code =
  if not (carry_of m i) then fail_with "%s: no carry" name
  else check (name ^ ": error") (ax_of m i) code

let position name m i want =
  ok name m i;
  check (name ^ ": AX") (ax_of m i) (want land 0xFFFF);
  check (name ^ ": DX") (dx_of m i) (want lsr 16)

let host_file m name = Hashtbl.find_opt m.Dos_state.host_files name

let file_is name m file want =
  match host_file m file with
  | Some got when Bytes.to_string got = want -> ()
  | Some got -> fail_with "%s: %s holds %S, want %S" name file (Bytes.to_string got) want
  | None -> fail_with "%s: %s is gone" name file

let open_rw = 0x3D02
let create = 0x3C00
let close = 0x3E00
let read = 0x3F00
let write = 0x4000
let seek_set = 0x4200
let seek_cur = 0x4201
let seek_end = 0x4202
let invalid_function = 1
let invalid_handle = 6
let last_position = 0xFFFF_FFFF

(* Methods past 2 are error 1, after the handle check. Positions are a
   32-bit sum stored without a sign check: 0 - 1 is FFFFFFFFh, and adding
   2 to it wraps to 1. Past the end a read gets nothing and a write finds
   the disk full. *)
let () =
  let m =
    run ~mounts:[ ("F.DAT", "ABC") ] ~data:[ (name_at, "F.DAT\000") ]
      (call 0 ~ax:open_rw ~dx:name_at
       ^ seek 1 ~ax:0x4203 ~bx:5 0
       ^ seek 2 ~ax:0x4203 ~bx:9 0
       ^ seek 3 ~ax:seek_cur ~bx:5 last_position
       ^ seek 4 ~ax:seek_cur ~bx:5 2
       ^ call 5 ~ax:read ~bx:5 ~cx:1 ~dx:buffer_at
       ^ seek 6 ~ax:seek_end ~bx:5 (last_position - 1)
       ^ seek 7 ~ax:seek_set ~bx:5 last_position
       ^ call 8 ~ax:read ~bx:5 ~cx:4 ~dx:buffer_at
       ^ call 9 ~ax:write ~bx:5 ~cx:1 ~dx:name_at
       ^ call 10 ~ax:close ~bx:5)
  in
  check "open: handle 5" (ax_of m 0) 5;
  error "method 3" m 1 invalid_function;
  error "method 3 on a handle not open" m 2 invalid_handle;
  position "0 - 1" m 3 last_position;
  position "FFFFFFFFh + 2 wraps" m 4 1;
  ok "read after the wrap" m 5;
  check "read after the wrap: count" (ax_of m 5) 1;
  check "read after the wrap: from position 1" (peek m buffer_at) (Char.code 'B');
  position "end - 2" m 6 1;
  position "the last position" m 7 last_position;
  ok "read past the end" m 8;
  check "read past the end: count" (ax_of m 8) 0;
  ok "write past the disk" m 9;
  check "write past the disk: count" (ax_of m 9) 0;
  file_is "write past the disk" m "F.DAT" "ABC"

(* Reading a little past the end gets 0 bytes. Before, copying those 0
   bytes from past the end raised Invalid_argument and stopped the
   machine. *)
let () =
  let m =
    run ~mounts:[ ("F.DAT", "ABC") ] ~data:[ (name_at, "F.DAT\000") ]
      (call 0 ~ax:open_rw ~dx:name_at
       ^ seek 1 ~ax:seek_set ~bx:5 10
       ^ call 2 ~ax:read ~bx:5 ~cx:1 ~dx:buffer_at)
  in
  ok "read past the end" m 2;
  check "read past the end: count" (ax_of m 2) 0

(* A write may add as many clusters as AH=36h says are free, not one
   more. *)
let cluster_bytes = 4 * 512
let free_bytes = 0x1000 * cluster_bytes

let () =
  let m =
    run ~data:[ (name_at, "E.DAT\000"); (second_name_at, "G.DAT\000"); (text_at, "Z") ]
      (call 0 ~ax:create ~dx:name_at
       ^ call 1 ~ax:create ~dx:second_name_at
       ^ seek 2 ~ax:seek_set ~bx:5 (free_bytes - 1)
       ^ call 3 ~ax:write ~bx:5 ~cx:1 ~dx:text_at
       ^ seek 4 ~ax:seek_set ~bx:6 free_bytes
       ^ call 5 ~ax:write ~bx:6 ~cx:1 ~dx:text_at
       ^ call 6 ~ax:close ~bx:5 ^ call 7 ~ax:close ~bx:6)
  in
  ok "fill every free cluster" m 3;
  check "fill every free cluster: count" (ax_of m 3) 1;
  (match host_file m "E.DAT" with
   | Some e when Bytes.length e = free_bytes && Bytes.get e (free_bytes - 1) = 'Z'
                 && Bytes.get e 0 = '\000' -> ()
   | Some e -> fail_with "E.DAT: %d bytes, want %d ending in Z" (Bytes.length e) free_bytes
   | None -> fail_with "E.DAT is gone");
  ok "one cluster more" m 5;
  check "one cluster more: count" (ax_of m 5) 0;
  file_is "one cluster more" m "G.DAT" ""

(* A write of 0 bytes sets the length to the position: shorter, longer
   with zeros, or unchanged when the disk cannot hold it. *)
let () =
  let m =
    run ~mounts:[ ("T.DAT", "ABCDE"); ("U.DAT", "XY") ]
      ~data:[ (name_at, "T.DAT\000"); (second_name_at, "U.DAT\000") ]
      (call 0 ~ax:open_rw ~dx:name_at
       ^ seek 1 ~ax:seek_set ~bx:5 2
       ^ call 2 ~ax:write ~bx:5
       ^ seek 3 ~ax:seek_end ~bx:5 0
       ^ seek 4 ~ax:seek_set ~bx:5 4
       ^ call 5 ~ax:write ~bx:5
       ^ seek 6 ~ax:seek_end ~bx:5 0
       ^ call 7 ~ax:close ~bx:5
       ^ call 8 ~ax:open_rw ~dx:second_name_at
       ^ seek 9 ~ax:seek_set ~bx:5 (2 * free_bytes)
       ^ call 10 ~ax:write ~bx:5
       ^ call 11 ~ax:close ~bx:5)
  in
  ok "truncate" m 2;
  check "truncate: count" (ax_of m 2) 0;
  position "length after truncating" m 3 2;
  position "length after extending" m 6 4;
  file_is "truncate, then extend" m "T.DAT" "AB\000\000";
  ok "extend past the disk" m 10;
  check "extend past the disk: count" (ax_of m 10) 0;
  file_is "extend past the disk" m "U.DAT" "XY"

(* AH=36h names the same disk *)
let () =
  let r = results in
  let m =
    run (mov_ax 0x3600 ^ mov_dx 0 ^ int_ 0x21
         ^ store_ax r ^ store_bx (r + 2) ^ store_cx (r + 4) ^ store_dx (r + 6))
  in
  List.iter
    (fun (name, off, want) -> check ("36h " ^ name) (peek16 m (r + off)) want)
    [ ("sectors per cluster", 0, 4); ("free clusters", 2, 0x1000);
      ("bytes per sector", 4, 512); ("clusters", 6, 0x2000) ]

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "seek and file size: all passed"
