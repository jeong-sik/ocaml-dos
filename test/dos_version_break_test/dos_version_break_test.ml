(* INT 21h AH=33h — MS-DOS 4.0 DISP.ASM $SET_CTRL_C_TRAPPING(00h-05h),
   CONST2.ASM SWAP_START 의 CNTCFLAG(0x17), RBIL INT 21/AX=3306h(DOS 5+
   진짜 버전). DISP.ASM 은 33h 를 DSKROUT 앞에서 끝내 확장 에러를 건드리지
   않는다. AH=30h — GETSET.ASM $GET_VERSION, DOSMES.ASM OEMNUM, RBIL
   INT 21/AH=30h(DOS 5+ AL=01h). *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let psp = 0x10000
let out = 0x7000
let slot = 10
let cf = 1
let quit = "\xb8\x00\x4c\xcd\x21"
let bx_in = 0x5A5A
let cx_in = 0x3C3C
let dx_in = 0x1234
let name_at = 0x5000
let missing = "NOFILE.XXX\000"

(* CF=1 로 INT 21h. AX BX CX DX FLAGS 를 out+10k 에 둔다. *)
let call ~ax ?(bx = bx_in) ?(dx = dx_in) k =
  let at = out + (slot * k) in
  "\xb8" ^ w ax ^ "\xbb" ^ w bx ^ "\xb9" ^ w cx_in ^ "\xba" ^ w dx ^ "\xf9\xcd\x21"
  ^ "\x9c" ^ "\xa3" ^ w at ^ "\x89\x1e" ^ w (at + 2) ^ "\x89\x0e" ^ w (at + 4)
  ^ "\x89\x16" ^ w (at + 6) ^ "\x58" ^ "\xa3" ^ w (at + 8)

(* 게스트가 깃발을 찾는 길: AH=34h 의 ES:BX 는 INDOS(SWAP_START+1)다.
   CNTCFLAG 는 SWAP_START+17h 라 INDOS+16h 다. 그 바이트를 AL 칸에 둔다. *)
let cntc_from_in_dos = 0x17 - 1
let flag_via_in_dos k =
  let at = out + (slot * k) in
  "\xb4\x34\xcd\x21"                                  (* mov ah,34h; int 21h *)
  ^ "\x26\x8a\x47" ^ String.make 1 (Char.chr cntc_from_in_dos)   (* mov al,es:[bx+d8] *)
  ^ "\xa2" ^ w at                                     (* mov [at],al *)

type regs = { ax : int; bx : int; cx : int; dx : int; carry : int }

let run calls =
  let m = Dos_machine.create () in
  let code = String.concat "" (List.mapi (fun k c -> c k) calls) ^ quit in
  let pad = String.make (name_at - 0x100 - String.length code) '\x90' in
  Dos_machine.load_com m (code ^ pad ^ missing);
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let word a = Dos_state.rd16 m (psp + a) in
  let regs k =
    let r i = word (out + (slot * k) + (2 * i)) in
    { ax = r 0; bx = r 1; cx = r 2; dx = r 3; carry = r 4 land cf }
  in
  List.mapi (fun k _ -> regs k) calls

let () =
  let byte = Dos_state.rd8 (Dos_machine.create ()) (Dos_state.sda Dos_state.sda_cntc_flag) in
  check "boot: extended break off" byte 0

let () =
  let rs =
    run [ call ~ax:0x3300;
          call ~ax:0x3301 ~dx:0x5603;            (* AND DL,1 *)
          call ~ax:0x3300 ~dx:0;
          call ~ax:0x3302 ~dx:0xAB02;            (* 2 AND 1 = 0 *)
          call ~ax:0x3300 ~dx:0;
          call ~ax:0x3302 ~dx:0x00FF;
          call ~ax:0x3303 ~dx:0x7777;
          call ~ax:0x3304 ~dx:0;
          call ~ax:0x3300 ~dx:0;
          call ~ax:0x3305 ~dx:0x1200;
          call ~ax:0x3306 ~bx:0xFFFF ~dx:0xFFFF;
          call ~ax:0x3307 ~dx:0x7777;
          call ~ax:0x33FF ~dx:0x7777;
          flag_via_in_dos;
          call ~ax:0x3000 ~bx:0xFFFF;
          call ~ax:0x3001 ~bx:0xFFFF;
          call ~ax:0x3002 ~bx:0xFFFF ]
  in
  let r = List.nth rs in
  let expect name k ?(bx = bx_in) ?(cx = cx_in) ~ax ~dx () =
    check (name ^ ": AX") (r k).ax ax;
    check (name ^ ": BX") (r k).bx bx;
    check (name ^ ": CX") (r k).cx cx;
    check (name ^ ": DX") (r k).dx dx;
    check (name ^ ": CF kept") (r k).carry cf
  in
  expect "3300h at boot" 0 ~ax:0x3300 ~dx:0x1200 ();
  expect "3301h keeps DX" 1 ~ax:0x3301 ~dx:0x5603 ();
  expect "3300h after 3301h DL=3" 2 ~ax:0x3300 ~dx:0x0001 ();
  expect "3302h gives the old flag" 3 ~ax:0x3302 ~dx:0xAB01 ();
  expect "3300h after 3302h DL=2" 4 ~ax:0x3300 ~dx:0 ();
  expect "3302h DL=FFh" 5 ~ax:0x3302 ~dx:0 ();
  expect "3303h does nothing" 6 ~ax:0x3303 ~dx:0x7777 ();
  expect "3304h does nothing" 7 ~ax:0x3304 ~dx:0 ();
  expect "3300h: 3304h left the flag" 8 ~ax:0x3300 ~dx:0x0001 ();
  expect "3305h boot drive C:" 9 ~ax:0x3305 ~dx:0x1203 ();
  expect "3306h true version 5.00" 10 ~ax:0x3306 ~bx:0x0005 ~dx:0 ();
  expect "3307h is Windows 95" 11 ~ax:0x33FF ~dx:0x7777 ();
  expect "33FFh" 12 ~ax:0x33FF ~dx:0x7777 ();
  check "the flag at INDOS+16h" ((r 13).ax land 0xff) 1;
  (* BH OEM FFh(Microsoft), BL:CX 사용자 번호 0, AL.AH = 5.00 *)
  expect "3000h" 14 ~ax:0x0005 ~bx:0xFF00 ~cx:0 ~dx:dx_in ();
  expect "3001h: BH is the version flag" 15 ~ax:0x0005 ~bx:0x0000 ~cx:0 ~dx:dx_in ();
  expect "3002h: BH is the OEM number" 16 ~ax:0x0005 ~bx:0xFF00 ~cx:0 ~dx:dx_in ();
  (* 30h 의 AL.AH 와 3306h 의 BL.BH 는 SETVER 가 없으니 같다 *)
  check "30h and 3306h agree" (r 14).ax (r 10).bx

(* 열기 실패(locus 2: Disk) 뒤 59h 가 주는 값은 그 사이에 33h 를 불러도
   같다. DSKROUT 을 탔다면 locus 가 1(Unknown)로 지워진다. *)
let () =
  let open_missing = call ~ax:0x3D00 ~dx:name_at in
  let direct = run [ open_missing; call ~ax:0x5900 ~bx:0 ] in
  let between = run [ open_missing; call ~ax:0x3300; call ~ax:0x5900 ~bx:0 ] in
  let d = List.nth direct 1 and b = List.nth between 2 in
  check "the open fails: file not found" d.ax 2;
  check "locus Disk" (d.cx lsr 8) 2;
  check "33h between: AX" b.ax d.ax;
  check "33h between: BX" b.bx d.bx;
  check "33h between: CX" b.cx d.cx;
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos version and break: all passed"
