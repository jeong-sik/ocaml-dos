(* INT 21h AH=58h 와 48h 의 자리 고르기 — MS-DOS 4.0 ALLOC.ASM $AllocOper,
   $ALLOC(alloc_best 의 JBE, alloc_do_split 의 CMP AllocMethod,1,
   alloc_do_split_high), CONST2.ASM AllocMethod. *)

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
let cl_in = 0x34
let quit = "\xb8\x00\x4c\xcd\x21"
let slot k = out + (2 * k)

let shrink_self = "\xb4\x4a\xbb" ^ w 0x1000 ^ "\xcd\x21"
let alloc paras k = "\xb4\x48\xbb" ^ w paras ^ "\xcd\x21" ^ "\xa3" ^ w (slot k)
let free k = "\x8e\x06" ^ w (slot k) ^ "\xb4\x49\xcd\x21"
let set_strategy bl = "\xb8\x01\x58\xbb" ^ w bl ^ "\xcd\x21"

(* CF=1 로 INT 21h. AX BX CX FLAGS 를 칸 k..k+3 에 둔다. *)
let call ~ax ~bx k =
  "\xb8" ^ w ax ^ "\xbb" ^ w bx ^ "\xb9" ^ w (0x1200 lor cl_in) ^ "\xf9\xcd\x21\x9c"
  ^ "\xa3" ^ w (slot k) ^ "\x89\x1e" ^ w (slot (k + 1)) ^ "\x89\x0e" ^ w (slot (k + 2))
  ^ "\x58\xa3" ^ w (slot (k + 3))

let request = 0x18

(* 틈: s0 0x30 | s1 | s2 0x20 | s3 | s4 0x20 | s5 | 맨 위까지.
   0x18 은 first 면 s0, best 면 s2(s4 와 크기가 같아 앞의 것), last 면 맨 위
   틈의 위쪽 끝이다. *)
let program =
  String.concat ""
    [ shrink_self;
      alloc 0x30 0; alloc 0x10 1; alloc 0x20 2; alloc 0x10 3; alloc 0x20 4; alloc 0x10 5;
      free 0; free 2; free 4;
      set_strategy 0; alloc request 6; free 6;
      set_strategy 1; alloc request 7; free 7;
      set_strategy 2; alloc request 8; free 8;
      set_strategy 0x80; alloc request 9; free 9;
      set_strategy 0; alloc request 10; free 10;
      set_strategy 0x81; call ~ax:0x5800 ~bx:0 11;
      call ~ax:0x5801 ~bx:2 15;
      call ~ax:0x5802 ~bx:0 19;
      call ~ax:0x5900 ~bx:0 23;
      call ~ax:0x4800 ~bx:0xFFFF 27;
      quit ]

let () =
  let boot = Dos_machine.create () in
  check "boot: AllocMethod" (Dos_state.rd8 boot (Dos_state.sda Dos_state.pre_swap_alloc_method)) 0;
  let m = Dos_machine.create () in
  Dos_machine.load_com m program;
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let r k = Dos_state.rd16 m (psp + slot k) in
  let s0 = r 0 in
  check "s0 right after the shrunk PSP" s0 0x2000;
  List.iteri (fun i off -> check (Printf.sprintf "s%d" (i + 1)) (r (i + 1)) (s0 + off))
    [ 0x30; 0x40; 0x60; 0x70; 0x90 ];
  let top = m.Dos_state.free_top in
  check "first fit" (r 6) s0;
  check "best fit, ties keep the lower one" (r 7) (s0 + 0x40);
  check "last fit splits the top gap high" (r 8) (top - request);
  check "BL=80h is last fit in 4.0" (r 9) (top - request);
  check "back to first fit" (r 10) s0;
  let regs k name ~ax ~carry =
    check (name ^ ": AX") (r k) ax;
    check (name ^ ": CF") (r (k + 3) land cf) carry
  in
  regs 11 "5800h returns the byte set" ~ax:0x0081 ~carry:0;
  regs 15 "5801h keeps AX" ~ax:0x5801 ~carry:0;
  regs 19 "5802h" ~ax:1 ~carry:cf;
  regs 23 "59h after 5802h" ~ax:1 ~carry:0;
  check "59h: BH class app, BL action abort" (r 24) 0x0704;
  check "59h: CH locus mem" (r 25) (0x0500 lor cl_in);
  regs 27 "48h too big under last fit" ~ax:8 ~carry:cf;
  check "48h too big: BX = largest gap" (r 28) (top - (s0 + 0xA0));
  check "strategy left by 5801h" (Dos_state.rd8 m (Dos_state.sda Dos_state.pre_swap_alloc_method)) 2

let () =
  if !failed = 0 then print_endline "dos alloc strategy: all passed"
  else begin
    Printf.eprintf "dos alloc strategy: %d failures\n%!" !failed;
    exit 1
  end
