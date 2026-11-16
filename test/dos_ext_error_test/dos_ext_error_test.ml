(* INT 21h AH=59h — MS-DOS 4.0 GETSET.ASM $GetExtendedError, MS_CODE.ASM
   SYS_RET_ERR·ETAB_LK·CAL_LK, MS_TABLE.ASM I21_MAP_E_TAB·ERR_TABLE_21,
   DISP.ASM DSKROUT. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let psp = 0x10000
let out = 0x7000
let name_at = 0x6000
let cf = 1
let cl_in = 0x34
let quit = "\xb8\x00\x4c\xcd\x21"

(* CF=1 로 INT 21h. AX BX CX DX DI ES FLAGS 를 out+14k 에 둔다. *)
let call k ~ax ?(bx = 0) ?(cx = cl_in) ?(dx = 0) () =
  let at = out + (14 * k) in
  "\xb8" ^ w ax ^ "\xbb" ^ w bx ^ "\xb9" ^ w cx ^ "\xba" ^ w dx ^ "\xbf\xff\xff"
  ^ "\xf9\xcd\x21" ^ "\x9c"
  ^ "\xa3" ^ w at ^ "\x89\x1e" ^ w (at + 2) ^ "\x89\x0e" ^ w (at + 4)
  ^ "\x89\x16" ^ w (at + 6) ^ "\x89\x3e" ^ w (at + 8) ^ "\x8c\x06" ^ w (at + 10)
  ^ "\x0e\x07"                                   (* push cs; pop es *)
  ^ "\x58" ^ "\xa3" ^ w (at + 12)

type regs = { ax : int; bx : int; cx : int; dx : int; di : int; es : int; carry : int }

let run calls =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (String.concat "" (List.mapi (fun k c -> c k) calls) ^ quit);
  String.iteri (fun i c -> Dos_state.wr8 m (psp + name_at + i) (Char.code c)) "NOPE.DAT\000";
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  if not (Dos_machine.exited m) then begin incr failed; prerr_endline "FAIL program did not exit" end;
  let word a = Dos_state.rd16 m (psp + a) in
  let regs k =
    let r i = word (out + (14 * k) + (2 * i)) in
    { ax = r 0; bx = r 1; cx = r 2; dx = r 3; di = r 4; es = r 5; carry = r 6 land cf }
  in
  (m, List.mapi (fun k _ -> regs k) calls)

let get_ext k = call k ~ax:0x5900 ()

let expect name r ~code ~cls ~action ~locus =
  check (name ^ ": AX") r.ax code;
  check (name ^ ": BH class") (r.bx lsr 8) cls;
  check (name ^ ": BL action") (r.bx land 0xff) action;
  check (name ^ ": CH locus") (r.cx lsr 8) locus;
  check (name ^ ": CL kept") (r.cx land 0xff) cl_in;
  check (name ^ ": CF") r.carry 0

let not_found = 8 and application = 7
let user = 3 and abort = 4
let unknown = 1 and disk = 2

let () =
  let m, rs =
    run [ get_ext;
          (fun k -> call k ~ax:0x3D00 ~dx:name_at ());     (* 없는 파일 *)
          get_ext; get_ext;
          (fun k -> call k ~ax:0x0241 ~dx:0x41 ());        (* 콘솔: DSKROUT 아님 *)
          get_ext;
          (fun k -> call k ~ax:0x3000 ());                 (* 성공해도 locus 는 Unk 로 *)
          get_ext;
          (fun k -> call k ~ax:0x3E00 ~bx:0x55 ());        (* 없는 핸들 *)
          get_ext;
          (fun k -> call k ~ax:0x4455 ~bx:1 ());           (* 없는 IOCTL *)
          get_ext ]
  in
  let r = List.nth rs in
  expect "boot" (r 0) ~code:0 ~cls:0 ~action:0 ~locus:0;
  check "boot: DI" (r 0).di 0; check "boot: ES" (r 0).es 0;
  check "open: AX" (r 1).ax 2; check "open: CF" (r 1).carry cf;
  expect "after open" (r 2) ~code:2 ~cls:not_found ~action:user ~locus:disk;
  expect "59h twice" (r 3) ~code:2 ~cls:not_found ~action:user ~locus:disk;
  expect "after console call" (r 5) ~code:2 ~cls:not_found ~action:user ~locus:disk;
  (* $GET_VERSION 은 SYS_RET_OK 가 아니라 return 으로 끝나 CF 를 건드리지 않는다 *)
  check "30h keeps CF" (r 6).carry cf;
  expect "after a successful call" (r 7) ~code:2 ~cls:not_found ~action:user ~locus:unknown;
  check "close: AX" (r 8).ax 6; check "close: CF" (r 8).carry cf;
  expect "after close" (r 9) ~code:6 ~cls:application ~action:abort ~locus:unknown;
  check "ioctl: AX" (r 10).ax 1; check "ioctl: CF" (r 10).carry cf;
  expect "after ioctl" (r 11) ~code:1 ~cls:application ~action:abort ~locus:unknown;
  check "WPErr" (Dos_state.rd8 m ((0x70 * 16) + 2)) 0xFF;
  check "ErrorMode" (Dos_state.rd8 m (0x70 * 16)) 0

(* 호스트에서 직접: DSKROUT 와 SYS_RET_ERR 의 짜임 *)
let () =
  let m = Dos_machine.create () in
  let sda off = (0x70 * 16) + off in
  check "boot WPErr" (Dos_state.rd8 m (sda 2)) 0xFF;
  Dos_state.wr8 m (sda 2) 0;
  let cpu = Dos_machine.cpu_of m in
  Cpu86.set_reg16 cpu 0 0x3E00;
  Dos_state.enter_call m;
  check "DSKROUT WPErr" (Dos_state.rd8 m (sda 2)) 0xFF;
  check "USER_IN_AX" (Dos_state.rd16 m (sda 0x1A)) 0x3E00;
  Dos_state.fail m 5;
  check "close returns 6 for 5" (Cpu86.reg16 cpu 0) 6;
  check "EXTERR keeps the real 5" (Dos_state.rd16 m (sda 4)) 5;
  check "class of 5" (Dos_state.rd8 m (sda 7)) 3;
  check "action of 5" (Dos_state.rd8 m (sda 6)) 3;
  check "locus of 5 stays Unk" (Dos_state.rd8 m (sda 3)) 1;
  (* 01h..0Ch 는 USER_IN_AX 를 건드리지 않는다 *)
  Cpu86.set_reg16 cpu 0 0x0200;
  Dos_state.enter_call m;
  check "console call keeps USER_IN_AX" (Dos_state.rd16 m (sda 0x1A)) 0x3E00

(* 순수 표 *)
let () =
  let rc fn code = Dos_error.returned_code ~fn code in
  check "close keeps 6" (rc 0x3E 6) 6;
  check "close maps 5 to 6" (rc 0x3E 5) 6;
  check "open maps 80 to its last (5)" (rc 0x3D 80) 5;
  check "open keeps 12" (rc 0x3D 12) 12;
  check "exec maps 18 to 5" (rc 0x4B 18) 5;
  check "find next maps 2 to 18" (rc 0x4F 2) 18;
  check "no list keeps the code" (rc 0x36 80) 80;
  check "FCB call keeps the code" (rc 0x0F 2) 2;
  let locus code = Option.map (fun i -> i.Dos_error.locus) (Dos_error.info code) in
  check "invalid function: call-specific locus" (Bool.to_int (locus 1 = Some None)) 1;
  check "access denied: call-specific locus" (Bool.to_int (locus 5 = Some None)) 1;
  check "file exists: disk" (Bool.to_int (locus 80 = Some (Some Dos_error.Disk))) 1;
  check "not in table" (Bool.to_int (Dos_error.info 14 = None)) 1;
  check "class codes" (Dos_error.class_code Dos_error.Unknown_class) 13;
  check "action codes" (Dos_error.action_code Dos_error.Interrupt_retry) 7;
  check "locus codes" (Dos_error.locus_code Dos_error.Memory) 5

let () =
  if !failed = 0 then print_endline "dos ext error: all passed"
  else begin
    Printf.eprintf "dos ext error: %d failures\n%!" !failed;
    exit 1
  end
