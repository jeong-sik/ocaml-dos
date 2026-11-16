(* INT 21h AH=63h·65h·66h — MS-DOS 4.0 GETSET.ASM $ECS_call·$GetExtCntry·
   $GetSetCdPg, 표는 DOSMES.ASM COUNTRY_CDPG. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end
let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %S want %S\n%!" name got want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let out = 0x7000
let buf = 0x7800
let filler = '\xee'
let buf_room = 64
let cf = 1
let rom = 0xF000 * 16
let us_437 = 437

(* AX·BX·CX·DX·DI 를 넣고 CF=1 로 INT 21h. AX·BX·CX·DX·FLAGS 를
   out+10k 에 둔다. *)
let call k ~ax ?(bx = 0) ?(cx = 0) ?(dx = 0) ?(di = 0) () =
  let at = out + (10 * k) in
  "\xb8" ^ w ax ^ "\xbb" ^ w bx ^ "\xb9" ^ w cx ^ "\xba" ^ w dx ^ "\xbf" ^ w di
  ^ "\xf9\xcd\x21" ^ "\x9c"
  ^ "\xa3" ^ w at ^ "\x89\x1e" ^ w (at + 2) ^ "\x89\x0e" ^ w (at + 4)
  ^ "\x89\x16" ^ w (at + 6) ^ "\x58" ^ "\xa3" ^ w (at + 8)

let quit = "\xb8\x00\x4c\xcd\x21"

type result = {
  regs : int -> int * int * int * int * int;     (* AX BX CX DX CF *)
  bytes : int -> int -> string;                  (* DS 기준 *)
  phys : int -> int;
  psp : int;
}

let run ?(prefill = []) ?ds code =
  let m = Dos_machine.create () in
  let set_ds = match ds with
    | None -> ""
    | Some seg -> "\xb8" ^ w seg ^ "\x8e\xd8\x8e\xc0"     (* mov ax,seg; mov ds,ax; mov es,ax *)
  in
  Dos_machine.load_com m (set_ds ^ code ^ quit);
  let base = Option.value ds ~default:(Dos_machine.psp_seg_of m) * 16 in
  let at a = base + (a land 0xffff) in
  for i = 0 to buf_room - 1 do Dos_machine.mem_write m (at (buf + i)) (Char.code filler) done;
  List.iter
    (fun (a, s) -> String.iteri (fun i c -> Dos_machine.mem_write m (at (a + i)) (Char.code c)) s)
    prefill;
  Dos_machine.run m ~max_steps:100_000;
  if not (Dos_machine.exited m) then begin incr failed; prerr_endline "FAIL program did not exit" end;
  let byte a = Dos_machine.mem_read m (at a) in
  let word a = byte a lor (byte (a + 1) lsl 8) in
  let reg k i = word (out + (10 * k) + (2 * i)) in
  { regs = (fun k -> (reg k 0, reg k 1, reg k 2, reg k 3, reg k 4 land cf));
    bytes = (fun a n -> String.init n (fun i -> Char.chr (byte (a + i))));
    phys = Dos_machine.mem_read m;
    psp = Dos_machine.psp_seg_of m }

let rom_word r off = r.phys (rom + off) lor (r.phys (rom + off + 1) lsl 8)
let rom_bytes r off n = String.init n (fun i -> Char.chr (r.phys (rom + off + i)))
let untouched n = String.make n filler

(* ---------- AH=65h, AL=01h ---------- *)

let case_map = w 0x0400 ^ w 0xF000
let us_info =
  "\x00\x00" ^ "$\x00\x00\x00\x00" ^ ",\x00" ^ ".\x00" ^ "-\x00" ^ ":\x00"
  ^ "\x00" ^ "\x02" ^ "\x00" ^ case_map ^ ",\x00"
let ext = w 1 ^ w us_437 ^ us_info ^ String.make 10 '\000'

let general name ~cx ?(bx = 0xFFFF) ?(dx = 0xFFFF) want_len =
  let r = run (call 0 ~ax:0x6501 ~bx ~cx ~dx ~di:buf ()) in
  let a, _, c, _, f = r.regs 0 in
  let n = 3 + want_len in
  check (name ^ ": AX") a us_437;
  check (name ^ ": CX") c n;
  check (name ^ ": CF") f 0;
  check_s (name ^ ": record") (r.bytes buf buf_room)
    ("\x01" ^ w want_len ^ String.sub ext 0 want_len ^ untouched (buf_room - n))

let refused name ~ax ?(bx = 0xFFFF) ?(cx = 64) ?(dx = 0xFFFF) () =
  let r = run (call 0 ~ax ~bx ~cx ~dx ~di:buf ()) in
  let a, _, c, _, f = r.regs 0 in
  check (name ^ ": AX") a 1;
  check (name ^ ": CX kept") c cx;
  check (name ^ ": CF") f cf;
  check_s (name ^ ": buffer untouched") (r.bytes buf buf_room) (untouched buf_room)

let () =
  check "extended record is 38 bytes" (String.length ext) 38;
  general "CX=41" ~cx:41 38;
  general "CX=64 copies 38" ~cx:64 38;
  general "CX=5" ~cx:5 2;
  general "CX=10" ~cx:10 7;
  general "explicit US 437" ~cx:41 ~bx:us_437 ~dx:1 38;
  refused "CX=4" ~ax:0x6501 ~cx:4 ();
  refused "country 2" ~ax:0x6501 ~dx:2 ();
  refused "code page 850" ~ax:0x6501 ~bx:850 ();
  refused "info 0" ~ax:0x6500 ();
  refused "info 3 (lower case)" ~ax:0x6503 ();
  refused "info 8" ~ax:0x6508 ();
  refused "table with CX=4" ~ax:0x6502 ~cx:4 ()

(* ES:DI 는 세그먼트 안에서 돈다. *)
let () =
  let r = run ~ds:0x2000 (call 0 ~ax:0x6501 ~cx:41 ~bx:0xFFFF ~dx:0xFFFF ~di:0xFFF8 ()) in
  let _, _, c, _, f = r.regs 0 in
  check "wrap: CX" c 41; check "wrap: CF" f 0;
  let record = "\x01" ^ w 38 ^ ext in
  check_s "wrap: end" (r.bytes 0xFFF8 8) (String.sub record 0 8);
  check_s "wrap: start" (r.bytes 0 33) (String.sub record 8 33)

(* ---------- AH=65h, 표 포인터 ---------- *)

let pointer name ~al =
  let r = run (call 0 ~ax:(0x6500 lor al) ~bx:0xFFFF ~cx:5 ~dx:0xFFFF ~di:buf ()) in
  let a, _, c, _, f = r.regs 0 in
  check (name ^ ": AX") a us_437;
  check (name ^ ": CX") c 5;
  check (name ^ ": CF") f 0;
  check_s (name ^ ": id") (r.bytes buf 1) (String.make 1 (Char.chr al));
  check_s (name ^ ": segment") (r.bytes (buf + 3) 2) (w 0xF000);
  check_s (name ^ ": after") (r.bytes (buf + 5) 4) (untouched 4);
  (r, Char.code (r.bytes (buf + 1) 1).[0] lor (Char.code (r.bytes (buf + 2) 1).[0] lsl 8))

let () =
  let r, ucase = pointer "upper case" ~al:2 in
  check "upper case: size" (rom_word r ucase) 128;
  check_s "upper case: table"
    (rom_bytes r (ucase + 2) 128)
    (String.init 128 (fun i -> Char.chr (Dos_country.upcase (0x80 + i))));
  List.iter
    (fun (c, want) -> check (Printf.sprintf "upper case %02Xh" c) (r.phys (rom + ucase + 2 + c - 0x80)) want)
    [ 0x80, 0x80; 0x81, 0x9A; 0x84, 0x8E; 0x87, 0x80; 0xA0, 0x41; 0xA4, 0xA5; 0xA7, 0xA7; 0xA8, 0xA8; 0xFF, 0xFF ];
  (* 38h 의 대문자 루틴(F000:0400)은 이 표를 읽는다: mov bx,표+2-80h *)
  check "case map reads this table" (rom_word r (0x0400 + 6)) (ucase + 2 - 0x80);
  let r, file_ucase = pointer "file upper case" ~al:4 in
  check "file upper case is its own table" (Bool.to_int (file_ucase <> ucase)) 1;
  check "file upper case: size" (rom_word r file_ucase) 128;
  check_s "file upper case: table" (rom_bytes r (file_ucase + 2) 128) (rom_bytes r (ucase + 2) 128);
  let r, chars = pointer "file chars" ~al:5 in
  check "file chars: size" (rom_word r chars) 22;
  check_s "file chars: table" (rom_bytes r (chars + 2) 22)
    "\x01\x00\xff\x00\x00\x20\x02\x0e.\"/\\[]:|<>+=;,";
  check_s "file chars: reserved" (rom_bytes r (chars + 2 + 22) 24) (String.make 24 '\000');
  let r, collate = pointer "collate" ~al:6 in
  check "collate: size" (rom_word r collate) 256;
  List.iter
    (fun (c, want) -> check (Printf.sprintf "collate %02Xh" c) (r.phys (rom + collate + 2 + c)) want)
    [ 0x00, 0x00; 0x41, 0x41; 0x61, 0x41; 0x7A, 0x5A; 0x7B, 0x7B; 0x80, Char.code 'C';
      0x81, Char.code 'U'; 0x91, Char.code 'A'; 0x98, Char.code 'Y'; 0x9B, Char.code '$';
      0x9F, Char.code '$'; 0xA4, Char.code 'N'; 0xA6, 0xA6; 0xA8, Char.code '?';
      0xAD, Char.code '!'; 0xAE, Char.code '"'; 0xAF, Char.code '"'; 0xB0, 0xB0;
      0xE0, 0xE0; 0xE1, Char.code 'S'; 0xE2, 0xE2; 0xFF, 0xFF ];
  let r, dbcs = pointer "dbcs" ~al:7 in
  check "dbcs: no ranges" (rom_word r dbcs) 0;
  check "dbcs: end mark" (rom_word r (dbcs + 2)) 0

(* ---------- AH=63h ---------- *)

let () =
  (* stc; mov ax,6300h; int 21h; pushf; push ds; push si; push cs; pop ds;
     pop [r]; pop [r+2]; pop [r+4]; mov [r+6],ax *)
  let r0 = out in
  let code al =
    "\xbe\x34\x12" ^ "\xf9" ^ "\xb8" ^ w (0x6300 lor al) ^ "\xcd\x21" ^ "\x9c\x1e\x56\x0e\x1f"
    ^ "\x8f\x06" ^ w r0 ^ "\x8f\x06" ^ w (r0 + 2) ^ "\x8f\x06" ^ w (r0 + 4) ^ "\xa3" ^ w (r0 + 6)
  in
  let r = run (code 0) in
  let word a = Char.code (r.bytes a 1).[0] lor (Char.code (r.bytes (a + 1) 1).[0] lsl 8) in
  let _, dbcs = pointer "dbcs again" ~al:7 in
  check "63h: SI" (word r0) (dbcs + 2);
  check "63h: DS" (word (r0 + 2)) 0xF000;
  check "63h: CF" (word (r0 + 4) land cf) 0;
  check "63h: AX" (word (r0 + 6)) 0x6300;
  List.iter
    (fun al ->
      let r = run (code al) in
      let word a = Char.code (r.bytes a 1).[0] lor (Char.code (r.bytes (a + 1) 1).[0] lsl 8) in
      let name = Printf.sprintf "63%02Xh" al in
      check (name ^ ": SI kept") (word r0) 0x1234;
      check (name ^ ": DS kept") (word (r0 + 2)) r.psp;
      check (name ^ ": CF") (word (r0 + 4) land cf) 0;
      check (name ^ ": AX") (word (r0 + 6)) (0x6300 lor al))
    [ 1; 2; 0xFF ]

(* ---------- AH=65h, 대문자 ---------- *)

let () =
  List.iter
    (fun (c, want) ->
      let r = run (call 0 ~ax:0x6520 ~dx:(0x5500 lor c) ()) in
      let a, _, _, d, f = r.regs 0 in
      let name = Printf.sprintf "6520h %02Xh" c in
      check (name ^ ": DL") (d land 0xff) want;
      check (name ^ ": DH kept") (d lsr 8) 0x55;
      check (name ^ ": AX") a (0x6500 lor want);
      check (name ^ ": CF") f 0)
    [ 0x61, 0x41; 0x7A, 0x5A; 0x60, 0x60; 0x7B, 0x7B; 0x31, 0x31; 0x81, 0x9A; 0xA4, 0xA5; 0xA8, 0xA8 ]

let text = "hello, \x81\x87 wORLD{"
let text_up = "HELLO, \x9A\x80 WORLD{"

let () =
  let n = String.length text in
  let r = run ~prefill:[ buf, text ] (call 0 ~ax:0x6521 ~cx:n ~dx:buf ()) in
  let a, _, c, _, f = r.regs 0 in
  check "6521h: AX" a 0x657B; check "6521h: CX kept" c n; check "6521h: CF" f 0;
  check_s "6521h: text" (r.bytes buf (n + 2)) (text_up ^ untouched 2);
  let r = run ~prefill:[ buf, text ] (call 0 ~ax:0x6521 ~cx:5 ~dx:buf ()) in
  let a, _, _, _, _ = r.regs 0 in
  check "6521h CX=5: AX" a 0x654F;
  check_s "6521h CX=5: text" (r.bytes buf n) ("HELLO" ^ String.sub text 5 (n - 5));
  let r = run ~prefill:[ buf, text ] (call 0 ~ax:0x6521 ~cx:0 ~dx:buf ()) in
  let a, _, _, _, f = r.regs 0 in
  check "6521h CX=0: AX" a 0x6521; check "6521h CX=0: CF" f 0;
  check_s "6521h CX=0: text" (r.bytes buf n) text;
  (* DS:DX 는 세그먼트 안에서 돈다 *)
  let r = run ~ds:0x2000 ~prefill:[ 0xFFFE, "ab"; 0, "cd" ] (call 0 ~ax:0x6521 ~cx:4 ~dx:0xFFFE ()) in
  check_s "6521h wrap: end" (r.bytes 0xFFFE 2) "AB";
  check_s "6521h wrap: start" (r.bytes 0 2) "CD"

let () =
  let r = run ~prefill:[ buf, "ab\x82\x00cd" ] (call 0 ~ax:0x6522 ~dx:buf ()) in
  let a, _, _, _, f = r.regs 0 in
  check "6522h: AX" a 0x6500; check "6522h: CF" f 0;
  check_s "6522h: text" (r.bytes buf 6) "AB\x45\x00cd";
  let r = run ~prefill:[ buf, "\x00ab" ] (call 0 ~ax:0x6522 ~dx:buf ()) in
  check_s "6522h empty" (r.bytes buf 3) "\x00ab"

let () =
  List.iter
    (fun (c, want) ->
      let r = run (call 0 ~ax:0x6523 ~dx:c ()) in
      let a, _, _, d, f = r.regs 0 in
      let name = Printf.sprintf "6523h %02Xh" c in
      check (name ^ ": AX") a want; check (name ^ ": CF") f 0; check (name ^ ": DX kept") d c)
    [ Char.code 'Y', 1; Char.code 'y', 1; Char.code 'N', 0; Char.code 'n', 0;
      Char.code 'x', 2; Char.code 'O', 2; 0, 2; 0x4E00 lor Char.code 'Q', 2 ]

(* A0h..A3h 는 MS-DOS 4.0~7.10 에서 잘못된 기능이다(RBIL). *)
let () =
  List.iter
    (fun fn ->
      let r = run ~prefill:[ buf, "abc\x00" ] (call 0 ~ax:(0x6500 lor fn) ~cx:3 ~dx:buf ()) in
      let a, _, _, _, f = r.regs 0 in
      let name = Printf.sprintf "65%02Xh" fn in
      check (name ^ ": AX") a 1; check (name ^ ": CF") f cf)
    [ 0xA0; 0xA1; 0xA2; 0xA3; 0x24; 0x7F; 0x80; 0xFF ];
  let r = run ~prefill:[ buf, "abc\x00" ] (call 0 ~ax:0x65A1 ~cx:3 ~dx:buf ()) in
  check_s "65A1h leaves the text" (r.bytes buf 4) "abc\x00";
  let r = run (call 0 ~ax:0x65A0 ~dx:0x61 ()) in
  let _, _, _, d, _ = r.regs 0 in
  check "65A0h leaves DL" d 0x61

(* ---------- AH=66h ---------- *)

let () =
  let r = run (call 0 ~ax:0x6601 ~bx:0x1111 ~dx:0x2222 ()) in
  let a, b, _, d, f = r.regs 0 in
  check "6601h: AX" a 0x6601; check "6601h: BX" b us_437; check "6601h: DX" d us_437;
  check "6601h: CF" f 0;
  List.iter
    (fun al ->
      let r = run (call 0 ~ax:(0x6600 lor al) ~bx:850 ~dx:0x2222 ()) in
      let a, b, _, d, f = r.regs 0 in
      let name = Printf.sprintf "66%02Xh" al in
      check (name ^ ": AX") a 1; check (name ^ ": CF") f cf;
      check (name ^ ": BX kept") b 850; check (name ^ ": DX kept") d 0x2222)
    [ 0; 2; 3 ]

(* 순수 함수 *)
let () =
  List.iter
    (fun (id, want) -> check (Printf.sprintf "table_of_id %d" id) (Bool.to_int (Dos_country.table_of_id id = want)) 1)
    Dos_country.[ 0, None; 1, None; 2, Some Ucase; 3, None; 4, Some File_ucase;
                  5, Some File_chars; 6, Some Collate; 7, Some Dbcs; 8, None ]

let () =
  if !failed = 0 then print_endline "dos nls: all passed"
  else begin
    Printf.eprintf "dos nls: %d failures\n%!" !failed;
    exit 1
  end
