(* INT 21h 의 빈 칸과 범위 밖 — MS-DOS 4.0 DISP.ASM BADCALL·$GET_CURRENT_PDB,
   MS_TABLE.ASM CPMFunc·MAXCOM, MS_CODE.ASM CPMFunc. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let out = 0x7000
let cf = 1
let bx_in = 0x1111
let cx_in = 0x2222
let dx_in = 0x3333
let si_in = 0x4444
let di_in = 0x5555
let stc = "\xf9"
let clc = "\xf8"

(* 레지스터를 채우고 CF 를 정한 뒤 INT 21h. AX BX CX DX SI DI FLAGS 를
   out+14k 에 둔다. *)
let call k ~ax ~carry =
  let at = out + (14 * k) in
  "\xb8" ^ w ax ^ "\xbb" ^ w bx_in ^ "\xb9" ^ w cx_in ^ "\xba" ^ w dx_in
  ^ "\xbe" ^ w si_in ^ "\xbf" ^ w di_in ^ (if carry then stc else clc) ^ "\xcd\x21"
  ^ "\x9c" ^ "\xa3" ^ w at ^ "\x89\x1e" ^ w (at + 2) ^ "\x89\x0e" ^ w (at + 4)
  ^ "\x89\x16" ^ w (at + 6) ^ "\x89\x36" ^ w (at + 8) ^ "\x89\x3e" ^ w (at + 10)
  ^ "\x58" ^ "\xa3" ^ w (at + 12)

let quit = "\xb8\x00\x4c\xcd\x21"

type regs = { ax : int; bx : int; cx : int; dx : int; si : int; di : int; carry : int }

let run calls =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (String.concat "" (List.mapi (fun k (ax, carry) -> call k ~ax ~carry) calls) ^ quit);
  Dos_machine.run m ~max_steps:100_000;
  if not (Dos_machine.exited m) then begin incr failed; prerr_endline "FAIL program did not exit" end;
  let base = Dos_machine.psp_seg_of m * 16 in
  let word a = Dos_machine.mem_read m (base + a) lor (Dos_machine.mem_read m (base + a + 1) lsl 8) in
  let regs k =
    let r i = word (out + (14 * k) + (2 * i)) in
    { ax = r 0; bx = r 1; cx = r 2; dx = r 3; si = r 4; di = r 5; carry = r 6 land cf }
  in
  (List.mapi (fun k _ -> regs k) calls, Dos_machine.psp_seg_of m)

(* AL 만 0 이 되고 나머지는 그대로인 경우 *)
let al_cleared name ~ax =
  let results, _ = run [ (ax, true); (ax, false) ] in
  List.iter2
    (fun r carry ->
      let name = Printf.sprintf "%s %04Xh CF=%d" name ax carry in
      check (name ^ ": AX") r.ax (ax land 0xFF00);
      check (name ^ ": BX") r.bx bx_in;
      check (name ^ ": CX") r.cx cx_in;
      check (name ^ ": DX") r.dx dx_in;
      check (name ^ ": SI") r.si si_in;
      check (name ^ ": DI") r.di di_in;
      check (name ^ ": CF kept") r.carry carry)
    results [ 1; 0 ]

let () =
  (* 범위 밖: 6Ch 위 *)
  List.iter (fun ax -> al_cleared "out of range" ~ax) [ 0x6D01; 0x71A0; 0x7303; 0x7FFF; 0x80AA; 0xFF55 ];
  (* 빈 칸 *)
  List.iter (fun ax -> al_cleared "unused" ~ax) [ 0x1877; 0x1D01; 0x1EFF; 0x2042; 0x6199 ]

(* 6Ch 는 마지막 기능($Extended_Open)이라 BADCALL 이 아니다. 아직 옮기지
   않은 기능이라 AL 을 건드리지 않는다. *)
let () =
  let results, _ = run [ (0x6C42, true) ] in
  let r = List.hd results in
  check "6Ch is not BADCALL: AX" r.ax 0x6C42

let () =
  let results, psp = run [ (0x5100, true); (0x5100, false); (0x6200, true) ] in
  List.iter2
    (fun r (name, carry) ->
      check (name ^ ": BX") r.bx psp;
      check (name ^ ": AX") r.ax ((if name = "62h" then 0x62 else 0x51) lsl 8);
      check (name ^ ": CX") r.cx cx_in;
      check (name ^ ": DX") r.dx dx_in;
      check (name ^ ": CF kept") r.carry carry)
    results [ ("51h", 1); ("51h", 0); ("62h", 1) ]

let () =
  let results, _ = run [ (0x6A00, true); (0x6800, true) ] in
  List.iter2
    (fun r name -> check (name ^ ": CF") r.carry 0)
    results [ "6Ah"; "68h" ]

let () =
  if !failed = 0 then print_endline "dos int21 dispatch: all passed"
  else begin
    Printf.eprintf "dos int21 dispatch: %d failures\n%!" !failed;
    exit 1
  end
