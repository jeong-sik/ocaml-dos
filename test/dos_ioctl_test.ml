(* INT 21h AH=44h from guest programs: each call leaves AX, DX and the
   carry (as CX = 0 or FFFFh) in a 6-byte record. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04Xh want %04Xh\n%!" name got want
  end

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax n = "\xb8" ^ w n
let mov_bx n = "\xbb" ^ w n
let mov_cx n = "\xb9" ^ w n
let mov_dx n = "\xba" ^ w n
let mov_ah n = "\xb4" ^ b n
let mov_bx_ax = "\x89\xc3"
let int_ n = "\xcd" ^ b n
let store_ax a = "\xa3" ^ w a
let store_cx a = "\x89\x0e" ^ w a
let store_dx a = "\x89\x16" ^ w a
let sbb_cx_cx = "\x19\xc9"
let quit = mov_ax 0x4c00 ^ int_ 0x21

let psp_base = 0x1000 * 16
let results = 0x2000
let record_bytes = 6
let name_at = 0x2100
let buffer_at = 0x2200

let peek m off = Dos_machine.mem_read m (psp_base + off)
let peek16 m off = peek m off lor (peek m (off + 1) lsl 8)

(* AX, DX and the carry of call [i] *)
let ax_of m i = peek16 m (results + (i * record_bytes))
let dx_of m i = peek16 m (results + (i * record_bytes) + 2)
let carry_of m i = peek16 m (results + (i * record_bytes) + 4) <> 0

(* [ioctl i ~ax] with BX already set: the call and its record *)
let record i =
  let r = results + (i * record_bytes) in
  sbb_cx_cx ^ store_ax r ^ store_dx (r + 2) ^ store_cx (r + 4)

let ioctl ?bx i ~ax =
  (match bx with Some v -> mov_bx v | None -> "")
  ^ mov_ax ax ^ mov_dx 0 ^ int_ 0x21 ^ record i

let run ?(keys = []) ?(mounts = []) code =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> Dos_machine.mount_file m n d) mounts;
  List.iter (Dos_machine.push_key m) keys;
  Dos_machine.load_com m (code ^ quit);
  Dos_machine.run m ~max_steps:200_000;
  m

let ok name m i =
  if carry_of m i then begin
    incr failed;
    Printf.eprintf "FAIL %s: carry set, AX=%04Xh\n%!" name (ax_of m i)
  end

let error name m i code =
  if not (carry_of m i) then begin
    incr failed;
    Printf.eprintf "FAIL %s: no carry\n%!" name
  end
  else check (name ^ ": error") (ax_of m i) code

(* 4400h on the five handles DOS opens at boot, and on one it did not *)
let () =
  let handles = [ 0; 1; 2; 3; 4; 7 ] in
  let m = run (String.concat "" (List.mapi (fun i h -> ioctl ~bx:h i ~ax:0x4400) handles)) in
  List.iteri
    (fun i (name, want) ->
      ok name m i;
      check name (dx_of m i) want;
      check (name ^ ": AX = DX") (ax_of m i) want)
    [ ("stdin (CON)", 0x80D3); ("stdout (CON)", 0x80D3); ("stderr (CON)", 0x80D3);
      ("stdaux (AUX)", 0x80C0); ("stdprn (PRN)", 0xA0C0) ];
  error "handle 7 is not open" m 5 6;
  check "handle 7: DX untouched" (dx_of m 5) 0

(* a file: 4400h, input status before and after its end, output status,
   and a file put under handle 1 by AH=46h *)
let () =
  let name = "A.DAT" in
  let code =
    mov_ax 0x3D00 ^ mov_dx name_at ^ int_ 0x21 ^ mov_bx_ax
    ^ ioctl 0 ~ax:0x4400
    ^ ioctl 1 ~ax:0x4406
    ^ mov_ah 0x3F ^ mov_cx 2 ^ mov_dx buffer_at ^ int_ 0x21
    ^ ioctl 2 ~ax:0x4406
    ^ ioctl 3 ~ax:0x4407
    ^ mov_ah 0x46 ^ mov_cx 1 ^ int_ 0x21
    ^ ioctl ~bx:1 4 ~ax:0x4400
  in
  let pad = String.make (name_at - 0x100 - String.length code - String.length quit) '\x90' in
  let m = run ~mounts:[ (name, "AB") ] (code ^ quit ^ pad ^ name ^ "\x00") in
  ok "file 4400h" m 0;
  check "file 4400h: drive C:" (dx_of m 0) 0x0002;
  check "file 4400h: AX = DX" (ax_of m 0) 0x0002;
  ok "file 4406h" m 1;
  check "file before its end: ready" (ax_of m 1 land 0xFF) 0xFF;
  check "file at its end: not ready" (ax_of m 2 land 0xFF) 0x00;
  ok "file 4407h" m 3;
  check "file output: ready" (ax_of m 3 land 0xFF) 0xFF;
  ok "4400h on handle 1 after 46h" m 4;
  check "handle 1 after 46h is the file" (dx_of m 4) 0x0002

(* the console's input status follows the keyboard, as AH=0Bh does *)
let () =
  let m = run (ioctl ~bx:0 0 ~ax:0x4406) in
  ok "console 4406h, no key" m 0;
  check "no key: not ready" (ax_of m 0 land 0xFF) 0x00;
  check "no key: the harness hears the guest wait" (Bool.to_int (m.Dos_state.kbd_requests > 0)) 1;
  let key = 0x1E61 in
  let m =
    run ~keys:[ key ] (ioctl ~bx:0 0 ~ax:0x4406 ^ mov_ah 0 ^ int_ 0x16 ^ store_ax 0x2100)
  in
  check "a key waits: ready" (ax_of m 0 land 0xFF) 0xFF;
  check "4406h leaves the key" (peek16 m 0x2100) key

(* handles not open, AUX and PRN status, drives, other subfunctions *)
let () =
  let m =
    run
      (ioctl ~bx:9 0 ~ax:0x4406 ^ ioctl ~bx:9 1 ~ax:0x4407
       ^ ioctl ~bx:3 2 ~ax:0x4406 ^ ioctl ~bx:4 3 ~ax:0x4407
       ^ ioctl ~bx:0 4 ~ax:0x4408 ^ ioctl ~bx:3 5 ~ax:0x4408
       ^ ioctl ~bx:1 6 ~ax:0x4408 ^ ioctl ~bx:4 7 ~ax:0x4408
       ^ ioctl ~bx:0 8 ~ax:0x4402)
  in
  error "4406h on a handle not open" m 0 6;
  error "4407h on a handle not open" m 1 6;
  check "AUX input status as before" (ax_of m 2 land 0xFF) 0xFF;
  check "PRN output status as before" (ax_of m 3 land 0xFF) 0xFF;
  ok "4408h default drive" m 4;
  check "default drive: fixed" (ax_of m 4) 1;
  ok "4408h C:" m 5;
  check "C: fixed" (ax_of m 5) 1;
  error "4408h A:" m 6 0x0F;
  error "4408h D:" m 7 0x0F;
  error "4402h" m 8 1

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "IOCTL: all passed"
