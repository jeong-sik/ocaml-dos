(* INT 21h AH=2Eh·54h·37h — MS-DOS 4.0 GETSET.ASM $SET_VERIFY_ON_WRITE,
   $GET_VERIFY_ON_WRITE, $CHAR_OPER, CONST2.ASM VERFLG·chSwitch 초기값,
   DISP.ASM LEAVEDOS(AL 을 돌려준다). *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let psp = 0x10000
let out = 0x7000
let cf = 1
let quit = "\xb8\x00\x4c\xcd\x21"
let dx_in = 0x1234

(* CF=1 로 INT 21h. AX DX FLAGS 를 out+6k 에 둔다. *)
let call ~ax ?(dx = dx_in) k =
  let at = out + (6 * k) in
  "\xb8" ^ w ax ^ "\xba" ^ w dx ^ "\xf9\xcd\x21" ^ "\x9c"
  ^ "\xa3" ^ w at ^ "\x89\x16" ^ w (at + 2) ^ "\x58" ^ "\xa3" ^ w (at + 4)

type regs = { ax : int; dx : int; carry : int }

let run calls =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (String.concat "" (List.mapi (fun k c -> c k) calls) ^ quit);
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let word a = Dos_state.rd16 m (psp + a) in
  let regs k =
    let r i = word (out + (6 * k) + (2 * i)) in
    { ax = r 0; dx = r 1; carry = r 2 land cf }
  in
  (m, List.mapi (fun k _ -> regs k) calls)

let slash = Char.code '/' and dash = Char.code '-'

let () =
  let fresh = Dos_machine.create () in
  let byte off = Dos_state.rd8 fresh (Dos_state.sda off) in
  check "boot: verify off" (byte Dos_state.pre_swap_verify) 0;
  check "boot: switch char" (byte Dos_state.pre_swap_switch_char) slash

let () =
  let m, rs =
    run [ call ~ax:0x5400;
          call ~ax:0x2E03;                       (* AND AL,1 *)
          call ~ax:0x5400;
          call ~ax:0x2EFE;
          call ~ax:0x5400;
          call ~ax:0x2E01;
          call ~ax:0x3700;
          call ~ax:0x3701 ~dx:(0xAB00 lor dash);
          call ~ax:0x3700 ~dx:0;
          call ~ax:0x3702 ~dx:0x5600;
          call ~ax:0x3703 ~dx:0;
          call ~ax:0x3700 ~dx:0;
          call ~ax:0x3704 ~dx:0x7777;
          call ~ax:0x37FF ~dx:0x7777 ]
  in
  let r = List.nth rs in
  let expect name k ~ax ~dx =
    check (name ^ ": AX") (r k).ax ax;
    check (name ^ ": DX") (r k).dx dx;
    check (name ^ ": CF kept") (r k).carry cf
  in
  expect "54h at boot" 0 ~ax:0x5400 ~dx:dx_in;
  expect "2Eh AL=3 keeps bit 0" 1 ~ax:0x2E01 ~dx:dx_in;
  expect "54h after on" 2 ~ax:0x5401 ~dx:dx_in;
  expect "2Eh AL=FEh" 3 ~ax:0x2E00 ~dx:dx_in;
  expect "54h after off" 4 ~ax:0x5400 ~dx:dx_in;
  expect "2Eh AL=1" 5 ~ax:0x2E01 ~dx:dx_in;
  expect "3700h keeps DH" 6 ~ax:0x3700 ~dx:(0x1200 lor slash);
  expect "3701h keeps AL and DX" 7 ~ax:0x3701 ~dx:(0xAB00 lor dash);
  expect "3700h after set" 8 ~ax:0x3700 ~dx:dash;
  expect "3702h" 9 ~ax:0x3702 ~dx:0x56FF;
  expect "3703h does nothing" 10 ~ax:0x3703 ~dx:0;
  expect "3700h after 3703h" 11 ~ax:0x3700 ~dx:dash;
  expect "3704h" 12 ~ax:0x37FF ~dx:0x7777;
  expect "37FFh" 13 ~ax:0x37FF ~dx:0x7777;
  (* 게스트 메모리에 있어 스냅숏이 같이 싣는다 *)
  let restored =
    match Dos_snapshot.save m with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok m -> m
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  let byte off = Dos_state.rd8 restored (Dos_state.sda off) in
  check "snapshot: verify" (byte Dos_state.pre_swap_verify) 1;
  check "snapshot: switch char" (byte Dos_state.pre_swap_switch_char) dash

let () =
  if !failed = 0 then print_endline "dos switch verify: all passed"
  else begin
    Printf.eprintf "dos switch verify: %d failures\n%!" !failed;
    exit 1
  end
