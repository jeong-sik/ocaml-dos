(* INT 15h AH=84h — 조이스틱 지원(RBIL INT 15/AH=84h). DX=0 은 버튼(AL 비트
   7-4), DX=1 은 축 위치(AX BX CX DX), 둘 다 CF=0. 조이스틱이 없는 이 기계는
   버튼을 모두 뗀 0F0h 와 위치 0 을 준다. 그 밖의 DX 는 CF=1·AH=86h. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04X want %04X\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let psp = 0x10000

(* mov ax,8400h; mov bx,1111h; mov cx,2222h; mov dx,[dx]; int 15h;
   AX BX CX DX 와 CF 를 0300h.. 에 남긴다 *)
let program dx =
  "\xb8" ^ w16 0x8400 ^ "\xbb" ^ w16 0x1111 ^ "\xb9" ^ w16 0x2222 ^ "\xba" ^ w16 dx
  ^ "\xcd\x15"
  ^ "\xa3" ^ w16 0x300                      (* mov [0300h],ax *)
  ^ "\x89\x1e" ^ w16 0x302                  (* mov [0302h],bx *)
  ^ "\x89\x0e" ^ w16 0x304                  (* mov [0304h],cx *)
  ^ "\x89\x16" ^ w16 0x306                  (* mov [0306h],dx *)
  ^ "\x9c\x58\x25\x01\x00"                  (* pushf; pop ax; and ax,1 *)
  ^ "\xa3" ^ w16 0x308                      (* mov [0308h],ax *)
  ^ "\xb8\x00\x4c\xcd\x21"                  (* mov ax,4C00h; int 21h *)

(* AX BX CX DX CF *)
let call dx =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (program dx);
  Dos_machine.run m ~max_steps:10_000;
  check "program exited" (if Dos_machine.exited m then 1 else 0) 1;
  let w off = Dos_machine.mem_read m (psp + off) lor (Dos_machine.mem_read m (psp + off + 1) lsl 8) in
  (w 0x300, w 0x302, w 0x304, w 0x306, w 0x308)

let () =
  (* DX=0: AL = 버튼 윗 네 비트, 나머지 레지스터는 그대로, CF=0 *)
  let ax, bx, cx, dx, cf = call 0 in
  check "switches: AL" (ax land 0xFF) 0xF0;
  check "switches: AH kept" (ax lsr 8) 0x84;
  check "switches: BX kept" bx 0x1111;
  check "switches: CX kept" cx 0x2222;
  check "switches: DX kept" dx 0;
  check "switches: CF" cf 0;
  (* DX=1: 네 축 위치가 모두 0, CF=0 *)
  let ax, bx, cx, dx, cf = call 1 in
  check "positions: AX" ax 0;
  check "positions: BX" bx 0;
  check "positions: CX" cx 0;
  check "positions: DX" dx 0;
  check "positions: CF" cf 0;
  (* 그 밖의 DX 는 지원 밖: CF=1, AH=86h, 다른 레지스터는 그대로 *)
  List.iter
    (fun d ->
       let ax, bx, cx, _, cf = call d in
       check (Printf.sprintf "DX=%04X: CF" d) cf 1;
       check (Printf.sprintf "DX=%04X: AH" d) (ax lsr 8) 0x86;
       check (Printf.sprintf "DX=%04X: BX kept" d) bx 0x1111;
       check (Printf.sprintf "DX=%04X: CX kept" d) cx 0x2222)
    [ 2; 3; 0x8000; 0xFFFF ]

let () =
  if !failed = 0 then print_endline "dos int15 joystick: all passed"
  else begin
    Printf.eprintf "dos int15 joystick: %d failures\n%!" !failed;
    exit 1
  end
