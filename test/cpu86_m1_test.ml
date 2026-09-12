(* Cpu86 M1 유닛테스트 — 새로운 명령 집합: call/ret, 그룹1 imm ALU,
   shift/rotate, mul/div, string+rep, lea, xchg, loop, mov rm,imm, INT 훅.
   M0 테스트의 메모리/헬퍼 패턴을 그대로 반복한다. *)

let mem = Bytes.make (1024 * 1024) '\000'
let read a = Char.code (Bytes.get mem (a land 0xfffff))
let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff))

let make () =
  Cpu86.create ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) ()

let run_from code =
  String.iteri (fun i c -> Bytes.set mem i c) code;
  let t = make () in
  Cpu86.set_seg t 1 0x0000;
  Cpu86.set_seg t 3 0x0000;
  Cpu86.set_ip t 0;
  t

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let check_list name got want =
  if got <> want then begin
    incr failed;
    let show l =
      String.concat "; " (List.map (fun (a, b) -> Printf.sprintf "(%x,%x)" a b) l)
    in
    Printf.eprintf "FAIL %s: got [%s] want [%s]\n%!" name (show got) (show want)
  end

let checkb name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let steps t n = for _ = 1 to n do ignore (Cpu86.step t) done

let () =
  (* 그룹1: add word [bx], imm8 부호확장 (83 07 7F) with [bx]=0xFF00, bx=0x100 *)
  Bytes.set mem 0x100 '\x00'; Bytes.set mem 0x101 '\xff';
  let t = run_from "\xbb\x00\x01\x83\x07\x7f\xf4" in
  steps t 2;
  check "group1 add rm16,imm8s lo" (read 0x100) 0x7f;
  check "group1 add rm16,imm8s hi" (read 0x101) 0xff;
  (* 그룹1: cmp byte [bx], imm8 (80 3F 05) with [bx]=5 → ZF *)
  Bytes.set mem 0x100 '\x05';
  let t = run_from "\xbb\x00\x01\x80\x3f\x05\xf4" in
  steps t 2;
  check "group1 cmp ZF" (Cpu86.flags t land Cpu86.f_zero) Cpu86.f_zero;
  check "group1 cmp no write" (read 0x100) 5;
  (* call rel16 / ret: call +4; mov ax,0xBEEF; ret 는 hlt 뒤로 돌아오지 않게
     레이아웃 — 0: E8 04 00 (call 7) / 3: F4(hlt) / 4: B8 EF BE / 7: C3 / 8: F4 *)
  let t = run_from "\xe8\x02\x00\xf4\x90\xb8\xef\xbe\xc3\xf4" in
  steps t 1;   (* call → ip=7 *)
  check "call pushed ret addr" (read (0xfffe)) 3;
  steps t 2;   (* mov ax, ret *)
  check "ret back to 3" (Cpu86.dump_ip t) 3;
  (* ret imm16: 프롤로그 형태 (C2) *)
  let t = run_from "\xb8\x01\x00\x50\xc3" in
  ignore t;
  (* lea bx,[si+0x10] (8D 1C? — modrm: mod=1 rm=4(SI) reg=3(BX) → 0x5C) *)
  let t = run_from "\xbe\x30\x00\x8d\x5c\x10\xf4" in
  steps t 2;
  check "lea bx,[si+0x10]" (Cpu86.reg16 t 3) 0x40;
  (* xchg ax,bx (93) *)
  let t = run_from "\xb8\x11\x11\xbb\x22\x22\x93\xf4" in
  steps t 3;
  check "xchg ax" (Cpu86.reg16 t 0) 0x2222;
  check "xchg bx" (Cpu86.reg16 t 3) 0x1111;
  (* shl ax,1 (D1 E0): 0x4000<<1 = 0x8000, CF=0, SF=1 *)
  let t = run_from "\xb8\x00\x40\xd1\xe0\xf4" in
  steps t 2;
  check "shl value" (Cpu86.reg16 t 0) 0x8000;
  check "shl SF" (Cpu86.flags t land Cpu86.f_sign) Cpu86.f_sign;
  (* shr ax,cl (D3 E8) with cl=4: 0x00F0 >> 4 = 0x0F *)
  let t = run_from "\xb8\xf0\x00\xb1\x04\xd3\xe8\xf4" in
  steps t 3;
  check "shr ax,cl" (Cpu86.reg16 t 0) 0x0f;
  (* sar: -16 >> 2 = -4 *)
  let t = run_from "\xb8\xf0\xff\xb1\x02\xd3\xf8\xf4" in
  steps t 3;
  check "sar -16>>2" (Cpu86.reg16 t 0) 0xfffc;
  (* rol al,1 (D0 C0): 0x80 → 0x01, CF=1 *)
  let t = run_from "\xb0\x80\xd0\xc0\xf4" in
  steps t 2;
  check "rol al" (Cpu86.reg8 t 0) 0x01;
  check "rol CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  (* neg bl (F6 DB) with bl=5 → -5, CF=1 *)
  let t = run_from "\xb3\x05\xf6\xdb\xf4" in
  steps t 2;
  check "neg bl" (Cpu86.reg8 t 3) 0xfb;
  check "neg CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  (* mul bl (F6 E3): AL=16, BL=16 → AX=256, CF=1 *)
  let t = run_from "\xb0\x10\xb3\x10\xf6\xe3\xf4" in
  steps t 3;
  check "mul AX" (Cpu86.reg16 t 0) 256;
  check "mul CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  (* div: word 100/7 → AL=14, AH=2 (F6 F3) *)
  let t = run_from "\xb8\x64\x00\xb3\x07\xf6\xf3\xf4" in
  steps t 3;
  check "div quotient" (Cpu86.reg8 t 0) 14;
  check "div remainder" (Cpu86.reg8 t 4) 2;
  (* imul 16: -3 * 5 = -15 → DX:AX = 0xFFFF:0xFFF1, CF=0 (16비트 안) *)
  let t = run_from "\xb8\xfd\xff\xbb\x05\x00\xf7\xeb\xf4" in
  steps t 3;
  check "imul AX" (Cpu86.reg16 t 0) 0xfff1;
  check "imul no OF" (Cpu86.flags t land Cpu86.f_overflow) 0;
  (* movsb (A4): DS:0x10 → ES:0x200 — ES=0x20. 코드(0번지)와 데이터(0x10)
     를 분리해 opcode 심기가 원본을 덮는 착오를 없앤다. *)
  let t = make () in
  Bytes.set mem 0 '\xa4'; Bytes.set mem 1 '\xf4';
  Bytes.set mem 0x10 '\xab';
  Cpu86.set_seg t 1 0; Cpu86.set_seg t 3 0; Cpu86.set_seg t 0 0x20;
  Cpu86.set_reg16 t 6 0x10;  (* SI *)
  Cpu86.set_reg16 t 7 0;     (* DI *)
  Cpu86.set_ip t 0;
  steps t 1;
  check "movsb copy" (read 0x200) 0xab;
  check "movsb si" (Cpu86.reg16 t 6) 0x11;
  check "movsb di" (Cpu86.reg16 t 7) 1;
  (* rep stosb (F3 AA): CX=4, AL=0x5A → ES:0x300..303 *)
  let t = make () in
  Bytes.set mem 0 '\xf3'; Bytes.set mem 1 '\xaa'; Bytes.set mem 2 '\xf4';
  Cpu86.set_seg t 1 0; Cpu86.set_seg t 3 0; Cpu86.set_seg t 0 0x30;
  Cpu86.set_reg16 t 1 4;   (* CX *)
  Cpu86.set_reg8 t 0 0x5a; (* AL *)
  Cpu86.set_reg16 t 7 0;   (* DI *)
  Cpu86.set_ip t 0;
  steps t 1;
  check "rep stosb count" (read 0x303) 0x5a;
  check "rep stosb cx" (Cpu86.reg16 t 1) 0;
  check "rep stosb di" (Cpu86.reg16 t 7) 4;
  (* loop (E2): 5회 반복 후 탈출 — 루프 카운터 CX 감소 검증 *)
  let t = run_from "\xb9\x03\x00\xe2\xfe\xf4" in
  ignore (Cpu86.step t);      (* mov cx,3 *)
  let spins = ref 0 in
  while not (Cpu86.halted t) && !spins < 10 do
    ignore (Cpu86.step t);
    incr spins
  done;
  checkb "loop exits" (Cpu86.halted t) true;
  check "loop spin count" !spins 4;
  (* mov rm16,imm16 (C7 07) *)
  let t = run_from "\xbb\x00\x01\xc7\x07\xcd\xab\xf4" in
  steps t 2;
  check "mov rm,imm" (read 0x100) 0xcd;
  check "mov rm,imm hi" (read 0x101) 0xab;
  (* INT 훅: int 21h (CD 21) — 훅이 AX 를 바꾸면 다음 명령에서 보인다 *)
  let t = run_from "\xcd\x21\xb8\x77\x88\xf4" in
  Cpu86.set_int_hook t (fun n ->
      if n = 0x21 then Cpu86.set_reg16 t 0 0x4242);
  steps t 1;
  check "int hook ran" (Cpu86.reg16 t 0) 0x4242;
  check "int hook continues" (Cpu86.dump_ip t) 2;
  (* INT 훅 없는 INT 는 예외 *)
  (try
     let t = run_from "\xcd\x10\xf4" in
     ignore (Cpu86.step t);
     incr failed;
     Printf.eprintf "FAIL unhooked int: no exception\n%!"
   with Cpu86.Unsupported _ -> ());
  (* xlat (D7): AL = DS:[BX+AL] *)
  Bytes.set mem 0x105 '\x99';
  let t = run_from "\bb\x00\x01" in
  ignore t;
  let t = make () in
  Bytes.set mem 0 '\xd7'; Bytes.set mem 1 '\xf4';
  Cpu86.set_seg t 1 0; Cpu86.set_seg t 3 0;
  Cpu86.set_reg16 t 3 0x100; (* BX *)
  Cpu86.set_reg8 t 0 5;      (* AL *)
  Cpu86.set_ip t 0;
  Bytes.set mem 0x105 '\x99';
  steps t 1;
  check "xlat" (Cpu86.reg8 t 0) 0x99;
  (* --- 186 확장: popa / enter / bound / salc / 포트 워드 --- *)
  (* pusha 로 여덟 레지스터를 밀고, 값을 뒤섞은 뒤 popa 로 되돌린다.
     레지스터를 하나라도 빠뜨리거나 순서를 틀리면 여기서 드러난다. *)
  let t = run_from "\x60\x61\xf4" in
  Cpu86.set_seg t 2 0;
  Cpu86.set_reg16 t 4 0x0800;
  let seed = [| 0x1111; 0x2222; 0x3333; 0x4444; 0x0800; 0x6666; 0x7777; 0x8888 |] in
  Array.iteri (fun i v -> if i <> 4 then Cpu86.set_reg16 t i v) seed;
  steps t 1;                                  (* pusha *)
  Array.iteri (fun i _ -> if i <> 4 then Cpu86.set_reg16 t i 0xDEAD) seed;
  steps t 1;                                  (* popa *)
  Array.iteri
    (fun i v ->
      if i <> 4 then
        check (Printf.sprintf "popa 레지스터 %d" i) (Cpu86.reg16 t i) v)
    seed;
  check "popa 뒤 SP" (Cpu86.reg16 t 4) 0x0800;
  (* enter 0x10,0 : BP 는 프레임 바닥, SP 는 그보다 0x10 아래 *)
  let t = run_from "\xc8\x10\x00\x00\xf4" in
  Cpu86.set_seg t 2 0;
  Cpu86.set_reg16 t 4 0x0800;
  Cpu86.set_reg16 t 5 0x0123;
  steps t 1;
  check "enter 가 BP 를 프레임에 둔다" (Cpu86.reg16 t 5) 0x07FE;
  check "enter 가 지역변수 자리를 잡는다" (Cpu86.reg16 t 4) 0x07EE;
  check "enter 가 옛 BP 를 밀었다" (read 0x7FE lor (read 0x7FF lsl 8)) 0x0123;
  (* leave 가 되돌린다 *)
  let t = run_from "\xc8\x10\x00\x00\xc9\xf4" in
  Cpu86.set_seg t 2 0;
  Cpu86.set_reg16 t 4 0x0800;
  Cpu86.set_reg16 t 5 0x0123;
  steps t 2;
  check "leave 가 SP 를 되돌린다" (Cpu86.reg16 t 4) 0x0800;
  check "leave 가 BP 를 되돌린다" (Cpu86.reg16 t 5) 0x0123;
  (* bound: 범위 안이면 지나가고, 밖이면 INT 5 훅이 불린다 *)
  let fired = ref (-1) in
  let t = run_from "\x62\x06\x00\x02\xf4" in  (* bound ax,[0200] *)
  Cpu86.set_int_hook t (fun n -> fired := n);
  Cpu86.set_seg t 3 0;
  Bytes.set mem 0x200 '\x0a'; Bytes.set mem 0x201 '\x00';
  Bytes.set mem 0x202 '\x14'; Bytes.set mem 0x203 '\x00';
  Cpu86.set_reg16 t 0 12;
  steps t 1;
  check "bound 범위 안" !fired (-1);
  let t = run_from "\x62\x06\x00\x02\xf4" in
  Cpu86.set_int_hook t (fun n -> fired := n);
  Cpu86.set_seg t 3 0;
  Cpu86.set_reg16 t 0 100;
  steps t 1;
  check "bound 범위 밖은 INT 5" !fired 5;
  (* salc: CF 를 AL 전체로 편다 *)
  let t = run_from "\xf9\xd6\xf4" in         (* stc; salc *)
  steps t 2;
  check "salc CF=1" (Cpu86.reg8 t 0) 0xFF;
  let t = run_from "\xf8\xd6\xf4" in         (* clc; salc *)
  steps t 2;
  check "salc CF=0" (Cpu86.reg8 t 0) 0x00;
  (* 워드 포트 출력은 바이트 두 번 — 포트 번호가 하나 늘어난다 *)
  let seen = ref [] in
  let t =
    let tt =
      Cpu86.create ~read ~write ~port_in:(fun _ -> 0xff)
        ~port_out:(fun p v -> seen := (p, v) :: !seen) ()
    in
    String.iteri (fun i c -> Bytes.set mem i c) "\xba\x40\x00\xef\xf4";
    Cpu86.set_seg tt 1 0; Cpu86.set_ip tt 0; tt
  in
  Cpu86.set_reg16 t 0 0xBEEF;
  steps t 2;
  check_list "워드 OUT 은 포트 두 개" (List.rev !seen) [ (0x40, 0xEF); (0x41, 0xBE) ];
  if !failed = 0 then print_endline "cpu86 M1: all passed"
  else begin
    Printf.eprintf "cpu86 M1: %d failures\n%!" !failed;
    exit 1
  end
