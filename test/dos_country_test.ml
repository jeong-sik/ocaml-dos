(* INT 21h AH=38h — MS-DOS 4.0 GETSET.ASM $INTERNATIONAL, DOSCNTRY.INC,
   커널 기본표 DOSMES.ASM COUNTRY_CDPG·UCASE_TAB·MAP_CASE. *)

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
let out = 0x200
let buf = 0x300
let cf = 1
let filler = '\xee'
let buf_room = 34                          (* RBIL 표: 24 + 예약 10 *)

(* AX·BX·DX 를 넣고 INT 21h. AX·BX·FLAGS 를 out+6k 에 둔다. *)
let call k ~ax ?(bx = 0) ~dx () =
  let at = out + (6 * k) in
  "\xb8" ^ w ax ^ "\xbb" ^ w bx ^ "\xba" ^ w dx ^ "\xcd\x21"
  ^ "\x9c" ^ "\xa3" ^ w at ^ "\x89\x1e" ^ w (at + 2) ^ "\x58" ^ "\xa3" ^ w (at + 4)

let quit = "\xb8\x00\x4c\xcd\x21"

(* [at] 에 [filler] 를 깔고 돌린다. 돌려주는 값: k 번째 AX·BX·CF, 그리고
   메모리 읽기. *)
let run ?(fill_at = buf) ?ds code =
  let m = Dos_machine.create () in
  let set_ds = match ds with
    | None -> ""
    | Some seg -> "\xb8" ^ w seg ^ "\x8e\xd8"          (* mov ax,seg; mov ds,ax *)
  in
  Dos_machine.load_com m (set_ds ^ code ^ quit);
  let base = Option.value ds ~default:(Dos_machine.psp_seg_of m) * 16 in
  for i = 0 to buf_room - 1 do
    Dos_machine.mem_write m (base + ((fill_at + i) land 0xffff)) (Char.code filler)
  done;
  Dos_machine.run m ~max_steps:100_000;
  if not (Dos_machine.exited m) then begin incr failed; prerr_endline "FAIL program did not exit" end;
  let byte a = Dos_machine.mem_read m (base + (a land 0xffff)) in
  let word a = byte a lor (byte (a + 1) lsl 8) in
  let reg k i = word (out + (6 * k) + (2 * i)) in
  let bytes at n = String.init n (fun i -> Char.chr (byte (at + i))) in
  ((fun k -> (reg k 0, reg k 1, reg k 2 land cf)), byte, bytes)

let case_map = w 0x0400 ^ w 0xF000
let us_info =
  "\x00\x00" ^ "$\x00\x00\x00\x00" ^ ",\x00" ^ ".\x00" ^ "-\x00" ^ ":\x00"
  ^ "\x00" ^ "\x02" ^ "\x00" ^ case_map ^ ",\x00"
let untouched = String.make buf_room filler
let with_info = us_info ^ String.make (buf_room - String.length us_info) filler

let get name ~ax ?bx () =
  let regs, _, bytes = run (call 0 ~ax ?bx ~dx:buf ()) in
  let a, b, c = regs 0 in
  check (name ^ ": AX") a 1;
  check (name ^ ": BX") b 1;
  check (name ^ ": CF") c 0;
  check_s (name ^ ": table") (bytes buf buf_room) with_info

let refused name ~ax ?bx ~dx () =
  let regs, _, bytes = run (call 0 ~ax ?bx ~dx ()) in
  let a, _, c = regs 0 in
  check (name ^ ": AX") a 1;
  check (name ^ ": CF") c cf;
  check_s (name ^ ": table untouched") (bytes buf buf_room) untouched

(* a-z 의 양 끝과 그 바로 바깥 *)
let () =
  List.iter
    (fun (c, want) -> check (Printf.sprintf "upcase %C" c) (Dos_country.upcase (Char.code c)) (Char.code want))
    [ '`', '`'; 'a', 'A'; 'z', 'Z'; '{', '{'; '@', '@'; 'A', 'A'; 'Z', 'Z' ]

let () =
  check "table is 24 bytes" (String.length us_info) 24;
  get "current country" ~ax:0x3800 ();
  get "country 1 by AL" ~ax:0x3801 ();
  get "country 1 by BX" ~ax:0x38FF ~bx:1 ();
  get "AL=FFh, BX=0 is the current country" ~ax:0x38FF ~bx:0 ();
  refused "United Kingdom (44)" ~ax:0x382C ~dx:buf ();
  refused "Korea (82) by BX" ~ax:0x38FF ~bx:82 ~dx:buf ();
  (* DX=FFFFh: 나라 바꾸기. 성공해도 BX 는 들어온 값 그대로다. *)
  let regs, _, _ = run (call 0 ~ax:0x3801 ~bx:0x1234 ~dx:0xFFFF ()) in
  let a, b, c = regs 0 in
  check "set country 1: AX" a 1; check "set country 1: BX kept" b 0x1234;
  check "set country 1: CF" c 0;
  let regs, _, _ = run (call 0 ~ax:0x38FF ~bx:1 ~dx:0xFFFF ()) in
  let a, b, c = regs 0 in
  check "set country 1 by BX: AX" a 1; check "set country 1 by BX: BX" b 1;
  check "set country 1 by BX: CF" c 0;
  refused "set country 0" ~ax:0x3800 ~dx:0xFFFF ();
  refused "set country 2" ~ax:0x3802 ~dx:0xFFFF ()

(* DX 가 FFF0h 면 DS 안에서 0 으로 돈다. COM 의 스택(SP=FFFEh)과 겹치지
   않게 DS 를 2000h 로 옮긴다. *)
let () =
  let regs, _, bytes = run ~ds:0x2000 ~fill_at:0xFFF0 (call 0 ~ax:0x3800 ~dx:0xFFF0 ()) in
  let _, _, c = regs 0 in
  check "wrap: CF" c 0;
  check_s "wrap: end of the segment" (bytes 0xFFF0 16) (String.sub us_info 0 16);
  check_s "wrap: start of the segment" (bytes 0 8) (String.sub us_info 16 8)

(* 표의 대문자 루틴을 FAR CALL 로 부른다. BX 는 그대로다. *)
let () =
  let cases = [ 0x81, 0x9A; 0x87, 0x80; 0xA4, 0xA5; 0xA7, 0xA7; 0xA8, 0xA8;
                0xE1, 0xE1; 0xFF, 0xFF; 0x80, 0x80; 0x61, 0x61; 0x7F, 0x7F ] in
  let result = 0x280 in
  let code =
    call 0 ~ax:0x3800 ~dx:buf ()
    ^ String.concat ""
        (List.mapi
           (fun i (c, _) ->
             "\xbb\x34\x12" ^ "\xb0" ^ String.make 1 (Char.chr c)
             ^ "\xff\x1e" ^ w (buf + 0x12)               (* call far [buf+12h] *)
             ^ "\xa2" ^ w (result + (3 * i))             (* mov [r],al *)
             ^ "\x89\x1e" ^ w (result + (3 * i) + 1))    (* mov [r+1],bx *)
           cases)
  in
  let _, byte, _ = run code in
  List.iteri
    (fun i (c, want) ->
      let name = Printf.sprintf "case map %02Xh" c in
      check name (byte (result + (3 * i))) want;
      check (name ^ ": BX kept") (byte (result + (3 * i) + 1) lor (byte (result + (3 * i) + 2) lsl 8)) 0x1234)
    cases

let () =
  if !failed = 0 then print_endline "dos country: all passed"
  else begin
    Printf.eprintf "dos country: %d failures\n%!" !failed;
    exit 1
  end
