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

(* 6Ch 는 마지막 기능($Extended_Open)이라 BADCALL 이 아니다. DX=3333h 는
   파일이 있을 때 할 일이 3 이라 FILEMODE.INC 범위 밖이다 — 에러 1(잘못된
   기능)에 CF=1 이다. BADCALL 이면 AL 만 0 이 되고 CF 는 그대로다. *)
let error_invalid_function = 1

let () =
  let results, _ = run [ (0x6C42, false) ] in
  List.iter
    (fun r ->
      check "6Ch DX=3333h: AX" r.ax error_invalid_function;
      check "6Ch DX=3333h: CF" r.carry cf)
    results

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

(* 50h 는 BX 를 현재 PSP 로 둔다(DISP.ASM $SET_CURRENT_PDB: MOV [CurrentPDB],BX).
   프로그램은 PSP 를 읽어 두고(51h), 5555h 로 바꿔 62h 로 읽은 뒤(AX·CX·DX 와 CF
   도 본다), 원래 값으로 되돌리고 끝난다. *)
let () =
  let m = Dos_machine.create () in
  let prog =
    "\xb4\x51\xcd\x21" ^ "\x53"                                     (* mov ah,51h; int 21h; push bx *)
    ^ "\xb8\x34\x12\xb9\x22\x22\xba\x33\x33"                        (* ax=1234h cx=2222h dx=3333h *)
    ^ "\xbb\x55\x55" ^ "\xf9" ^ "\xb4\x50" ^ "\xcd\x21"            (* bx=5555h; stc; mov ah,50h; int 21h *)
    ^ "\xa3" ^ w out ^ "\x89\x0e" ^ w (out + 2) ^ "\x89\x16" ^ w (out + 4)  (* ax cx dx *)
    ^ "\x9c\x58\xa3" ^ w (out + 8)                                  (* pushf; pop ax; mov [out+8],ax *)
    ^ "\xb4\x62" ^ "\xcd\x21" ^ "\x89\x1e" ^ w (out + 6)            (* mov ah,62h; int 21h; mov [out+6],bx *)
    ^ "\x5b" ^ "\xb4\x50" ^ "\xcd\x21" ^ quit                        (* pop bx; mov ah,50h; int 21h; exit *)
  in
  Dos_machine.load_com m prog;
  Dos_machine.run m ~max_steps:100_000;
  check "50h: the program exits through the restored PSP" (if Dos_machine.exited m then 1 else 0) 1;
  let base = Dos_machine.psp_seg_of m * 16 in
  let word a = Dos_machine.mem_read m (base + a) lor (Dos_machine.mem_read m (base + a + 1) lsl 8) in
  check "50h: 62h reads back 5555h" (word (out + 6)) 0x5555;
  check "50h: AX is what the caller left (AH=50h, AL=34h)" (word out) 0x5034;
  check "50h: CX unchanged" (word (out + 2)) 0x2222;
  check "50h: DX unchanged" (word (out + 4)) 0x3333;
  check "50h: CF kept" (word (out + 8) land cf) cf

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
