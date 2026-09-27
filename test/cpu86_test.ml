(* Cpu86 M0 유닛테스트. 메모리는 로컬 1MB 바이트 배열 — 코어의 콜백
   계약(20비트 물리 주소)만 쓴다. 기대값은 8086 명령 세미antics 의
   손계산 — 외부 검증 벡터는 M1 에 도입한다. *)

let mem = Bytes.make (1024 * 1024) '\000'

let read a = Char.code (Bytes.get mem (a land 0xfffff))
let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff))

let make () =
  Cpu86.create ~read ~write
    ~port_in:(fun _ -> 0xff)
    ~port_out:(fun _ _ -> ())
    ()

(* CS:0 에 코드를 심고 IP 를 0 에서 시작. 한 명령씩 밀며 검사. *)
let run_from code =
  String.iteri (fun i c -> Bytes.set mem i c) code;
  let t = make () in
  Cpu86.set_seg t 1 0x0000;  (* CS *)
  Cpu86.set_seg t 3 0x0000;  (* DS *)
  Cpu86.set_ip t 0;
  t

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let checkb name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let () =
  (* mov ax,0x1234 (B8 34 12) *)
  let t = run_from "\xb8\x34\x12\xf4" in
  ignore (Cpu86.step t);
  check "mov ax,imm" (Cpu86.reg16 t 0) 0x1234;
  (* mov al,0xFF; mov bl,al (B0 FF 88 C3) *)
  let t = run_from "\xb0\xff\x88\xc3\xf4" in
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "mov rm8,r8 (bl)" (Cpu86.reg8 t 3) 0xff;
  (* add ax,0x0001 → 0x1235; ZF=0 CF=0 (05 01 00) *)
  let t = run_from "\xb8\x34\x12\x05\x01\x00\xf4" in
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "add ax,imm" (Cpu86.reg16 t 0) 0x1235;
  check "add ZF clear" (Cpu86.flags t land Cpu86.f_zero) 0;
  (* sub ax,ax → ZF=1 (2B C0) *)
  let t = run_from "\x2b\xc0\xf4" in
  ignore (Cpu86.step t);
  check "sub ax,ax ZF" (Cpu86.flags t land Cpu86.f_zero) Cpu86.f_zero;
  check "sub ax,ax value" (Cpu86.reg16 t 0) 0;
  (* cmp ax,1 with ax=1 → ZF (3D 01 00) *)
  let t = run_from "\xb8\x01\x00\x3d\x01\x00\xf4" in
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "cmp equal ZF" (Cpu86.flags t land Cpu86.f_zero) Cpu86.f_zero;
  check "cmp preserves dest" (Cpu86.reg16 t 0) 1;
  (* 캐리: add al,1 with al=0xFF → AL=0, CF=1 (04 01) *)
  let t = run_from "\xb0\xff\x04\x01\xf4" in
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "add wrap value" (Cpu86.reg8 t 0) 0;
  check "add wrap CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  check "add wrap ZF" (Cpu86.flags t land Cpu86.f_zero) Cpu86.f_zero;
  (* 오버플로: add ax,0x7FFF with ax=1 → 0x8000, OF=1 SF=1 (05 FF 7F) *)
  let t = run_from "\xb8\x01\x00\x05\xff\x7f\xf4" in
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "add signed OF" (Cpu86.flags t land Cpu86.f_overflow) Cpu86.f_overflow;
  check "add signed SF" (Cpu86.flags t land Cpu86.f_sign) Cpu86.f_sign;
  (* inc ax with ax=0xFFFF → 0, ZF=1, CF 는 건드리지 않는다 (40) *)
  let t = run_from "\xb8\xff\xff\x40\xf4" in
  ignore (Cpu86.step t);
  Cpu86.set_flags t (Cpu86.flags t lor Cpu86.f_carry);
  ignore (Cpu86.step t);
  check "inc wrap value" (Cpu86.reg16 t 0) 0;
  check "inc wrap ZF" (Cpu86.flags t land Cpu86.f_zero) Cpu86.f_zero;
  check "inc preserves CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  (* 메모리 피연산자: mov [bx],ax (89 07) with DS=0, BX=0x100 *)
  let t = run_from "\xb8\x78\x56\xbb\x00\x01\x89\x07\xf4" in
  ignore (Cpu86.step t);   (* mov ax *)
  ignore (Cpu86.step t);   (* mov bx *)
  ignore (Cpu86.step t);   (* mov [bx],ax *)
  check "mem rm16 write lo" (read 0x100) 0x78;
  check "mem rm16 write hi" (read 0x101) 0x56;
  (* 세그먼트 물리: DS=0x1000, BX=0x0030 → 0x10030 (89 07) *)
  let t = make () in
  Bytes.set mem 0 '\x89'; Bytes.set mem 1 '\x07'; Bytes.set mem 2 '\xf4';
  Cpu86.set_seg t 1 0; Cpu86.set_seg t 3 0x1000;
  Cpu86.set_reg16 t 0 0xbeef;
  Cpu86.set_reg16 t 3 0x0030;  (* BX *)
  Cpu86.set_ip t 0;
  ignore (Cpu86.step t);
  check "seg:off physical" (read 0x10030) 0xef;
  (* push/pop 왕복: push ax; pop bx (50 5B) *)
  let t = run_from "\xb8\x21\x43\xbc\x00\x02\x50\x5b\xf4" in
  for _ = 1 to 4 do ignore (Cpu86.step t) done;
  check "push/pop roundtrip" (Cpu86.reg16 t 3) 0x4321;
  check "sp restored" (Cpu86.reg16 t 4) 0x0200;
  (* jz rel8: sub ax,ax → ZF; jz +2 로 hlt 를 건너뛴다 (2B C0 74 02 F4 B8 99 99) *)
  let t = run_from "\x2b\xc0\x74\x01\xf4\xb8\x99\x99\xf4" in
  for _ = 1 to 3 do ignore (Cpu86.step t) done;
  check "jcc taken skips hlt" (Cpu86.reg16 t 0) 0x9999;
  checkb "not halted" (Cpu86.halted t) false;
  (* hlt *)
  let t = run_from "\xf4" in
  ignore (Cpu86.step t);
  checkb "hlt sets halted" (Cpu86.halted t) true;
  (* wait 는 8087 이 없는 기계에선 그냥 지나간다 — 예외가 아니다 *)
  let t = run_from "\x9b\xb8\x34\x12" in
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "wait falls through" (Cpu86.reg16 t 0) 0x1234;
  (* 아직 아무도 구현하지 않은 opcode 는 예외로 죽는다 (0xF1) *)
  (try
     let t = run_from "\xf1" in
     ignore (Cpu86.step t);
     incr failed;
     Printf.eprintf "FAIL unsupported 0xf1: no exception\n%!"
   with Cpu86.Unsupported _ -> ());
  (* #31: a fault used to leave [ip] past the faulting instruction, so a
     second [step] ran whatever bytes followed as a fresh one instead of
     re-faulting on the same one. [lea ax, ax] (8d c0) followed by the
     zero bytes [run_from]'s backing memory starts as -- the shape a COM
     image's tail actually has -- used to execute those zero bytes as
     [add [bx+si], al] and return normally on the second step. *)
  let t = run_from "\x8d\xc0" in
  (try
     ignore (Cpu86.step t);
     incr failed;
     Printf.eprintf "FAIL lea on register: first step did not fault\n%!"
   with Cpu86.Unsupported _ -> ());
  (try
     ignore (Cpu86.step t);
     incr failed;
     Printf.eprintf "FAIL lea on register: second step ran past the fault instead of re-faulting\n%!"
   with Cpu86.Unsupported _ -> ());
  if !failed = 0 then print_endline "cpu86 M0: all passed"
  else begin
    Printf.eprintf "cpu86 M0: %d failures\n%!" !failed;
    exit 1
  end
