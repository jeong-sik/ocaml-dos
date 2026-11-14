(* 386 의 66h 형태: 피연산자 크기 접두가 폭을 32 비트로 바꾸는 명령. 주소 크기는
   따로 [ctx.addr] 가 정한다 — 67h 가 없으면 오프셋·SI·DI·CX 는 16 비트이고, 있으면
   32 비트다. 실모드 스택은 어느 쪽이든 SP(16 비트)다. 접두가 붙어도 폭이 그대로인
   명령(바이트 명령 등)은 [step] 의 16 비트 경로가 돌린다. 어느 쪽인지는
   [Cpu86_decode.operand_sized] 가 정한다. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode
open Cpu86_alu
open Cpu86_string

let sx8_32 d = if d >= 0x80 then d lor 0xffffff00 else d

(* 32 비트 EIP 로 간다. 실모드 CS 한계가 FFFFh 라 그 위는 그 명령 자신의 INT 13
   이고 EIP 와 스택은 바뀌지 않는다. SingleStepTests/80386 66C3 에서 FFFFh 위
   EIP 를 꺼낸 128 건이 모두 그렇다. 점프는 같은 규칙으로 둔다 — 66E9 의 rel32 는
   ±64K 안이라 그런 점프가 데이터에 없다. *)
let check_target target =
  let target = target land dword_mask in
  if target > 0xffff then fault vector_segment_overrun;
  target

let jump_rel t d = t.ip <- check_target (t.ip + d)

(* [atomic] 은 아래 16 비트만 되돌린다. ENTER 의 디스플레이 복사는 EBP 를 4 씩
   줄이며 읽어 위 절반까지 바꾸는데, 그 읽기가 넘쳐 결함이 나면 EBP 전체가
   명령 앞 값이다(SingleStepTests/80386 66C8 의 결함 128 건). 다른 32 비트 스택
   명령은 결함 전에 위 절반을 건드리지 않아 [atomic] 으로 된다. *)
let atomic32 t f =
  let hi = Array.copy t.regs_hi in
  atomic t (fun () ->
      try f () with Fault _ as e -> Array.blit hi 0 t.regs_hi 0 (Array.length hi); raise e)

(* 세그먼트 레지스터의 32 비트 PUSH·POP 은 SP 를 4 움직이고 워드 하나만
   오간다. 위 워드는 쓰지도 읽지도 않아 SP=FFFEh 의 POP 도 결함이 아니다
   (SingleStepTests/80386 6606·6607: 쓴 바이트가 둘, SP=FFFE 의 POP 36 건). *)
let push_sreg t sreg =
  let sp = (t.regs.(4) - 4) land 0xffff in
  write16 t sreg_ss sp (seg t sreg);
  t.regs.(4) <- sp

let pop_sreg t sreg =
  let v = read16 t sreg_ss t.regs.(4) in
  set_seg t sreg v;
  t.regs.(4) <- (t.regs.(4) + 4) land 0xffff

let alu_group32 t ctx op =
  match ctx.opcode land 7 with
  | 1 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let a = op_read32 t rm in
    let r = alu_result t op a (get32 t regf) 32 in
    if op <> 7 then op_write32 t rm r;
    3
  | 3 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let b = op_read32 t rm in
    let r = alu_result t op (get32 t regf) b 32 in
    if op <> 7 then put32 t regf r;
    3
  | 5 ->
    let imm = fetch32 t in
    let r = alu_result t op (get32 t 0) imm 32 in
    if op <> 7 then put32 t 0 r;
    4
  | _ -> unsupported t ctx "alu form (66h)"

let inc_dec t v op =
  let saved_cf = t.cf in
  let r = alu_result t op v 1 32 in
  t.cf <- saved_cf;
  r

(* far 포인터 m16:32 와 BOUND 의 두 경계가 차지하는 바이트. *)
let far_pointer32_bytes = 6
let bounds32_bytes = 8

let far_pointer t ctx rm what =
  check_span t rm far_pointer32_bytes;
  match rm with
  | Mem (s, o) | Mem32 (s, o) ->
    let off = read32 t s o in
    let sg = read16 t s (o + 4) in
    (off, sg)
  | Reg _ -> undefined_on_286 t true; unsupported t ctx what

let far_call t off sg =
  atomic t (fun () ->
      let target = check_target off in
      push32 t (seg t sreg_cs);
      push32 t t.ip;
      set_seg t sreg_cs sg;
      t.ip <- target;
      13)

let far_jump t off sg =
  let target = check_target off in
  set_seg t sreg_cs sg;
  t.ip <- target

let group_ff t ctx =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  (match regf with
   | 0 -> op_write32 t rm (inc_dec t (op_read32 t rm) 0)
   | 1 -> op_write32 t rm (inc_dec t (op_read32 t rm) 5)
   | 2 ->
     let target = check_target (op_read32 t rm) in
     push32 t t.ip;
     t.ip <- target
   | 3 ->
     let off, sg = far_pointer t ctx rm "call far reg (66h)" in
     ignore (far_call t off sg)
   | 4 -> t.ip <- check_target (op_read32 t rm)
   | 5 ->
     let off, sg = far_pointer t ctx rm "jmp far reg (66h)" in
     far_jump t off sg
   | 6 | 7 -> push32 t (op_read32 t rm)
   | _ -> unsupported t ctx "group FF (66h)");
  15

let imul_imm t ctx =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let a = op_read32 t rm in
  let imm = if ctx.opcode = 0x69 then fetch32 t else sx8_32 (fetch8 t) in
  put32 t regf (low32 (imul32 t a imm));
  21

let enter32 t =
  let size = fetch16 t in
  let nesting = fetch8 t land 0x1f in
  atomic32 t (fun () ->
      push32 t (get32 t 5);
      let frame = t.regs.(4) in
      if nesting > 0 then begin
        for _ = 1 to nesting - 1 do
          put32 t 5 ((get32 t 5) - 4);
          push32 t (read32 t sreg_ss t.regs.(5))
        done;
        push32 t frame
      end;
      put32 t 5 frame;
      set_reg16 t 4 ((t.regs.(4) - size) land 0xffff);
      19)

(* 16 비트 스택의 POPAD 는 ESP 자리를 버리지 않는다: SP 는 32 만큼 오르고 ESP
   의 위 16 비트는 그 자리 값의 위 16 비트가 된다(SingleStepTests/80386 6661,
   2,500 건 모두). *)
let popad t =
  restore_sp_on_fault t (fun () ->
      put32 t 7 (pop32 t);
      put32 t 6 (pop32 t);
      put32 t 5 (pop32 t);
      let esp_slot = pop32 t in
      t.regs_hi.(4) <- esp_slot lsr 16;
      put32 t 3 (pop32 t);
      put32 t 2 (pop32 t);
      put32 t 1 (pop32 t);
      put32 t 0 (pop32 t);
      25)

(* PUSHAD 는 EAX 를 SP-4 에, EDI 를 SP-32 에 두는데 쓰기는 EDI 부터 위로
   한다. 세그먼트 끝을 넘는 첫 자리에서 INT 12 이고, 그 아래에 이미 쓴
   더블워드는 남는다. SP 는 그대로다(SingleStepTests/80386 6660, SP 가 32
   보다 작은 8 건). *)
let pushad_slots = 8
let pushad t =
  let sp = t.regs.(4) in
  let esp = get32 t 4 in
  let value = function 4 -> esp | n -> get32 t n in
  for n = pushad_slots - 1 downto 0 do
    write32 t sreg_ss ((sp - (4 * (n + 1))) land 0xffff) (value n)
  done;
  t.regs.(4) <- (sp - (4 * pushad_slots)) land 0xffff;
  36

let bound32 t ctx =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  check_span t rm bounds32_bytes;
  (match rm with
   | Mem (s, o) | Mem32 (s, o) ->
     let idx = sign32 (get32 t regf) in
     let lower = sign32 (read32 t s o) in
     let upper = sign32 (read32 t s (o + 4)) in
     if idx < lower || idx > upper then fault vector_bound
   | Reg _ -> undefined_on_286 t true; unsupported t ctx "bound on register (66h)");
  13

(* 카운터는 주소 크기를 따른다: 67h 면 ECX, 아니면 CX. *)
let loop32 t ctx =
  let d = sx8 (fetch8 t) in
  let wide = address32 ctx.addr in
  let cx = if wide then (get32 t 1 - 1) land offset32_mask else (t.regs.(1) - 1) land 0xffff in
  let take =
    match ctx.opcode with
    | 0xe0 -> cx <> 0 && not t.zf
    | 0xe1 -> cx <> 0 && t.zf
    | _ -> cx <> 0
  in
  if take then jump_rel t d;
  if wide then put32 t 1 cx else t.regs.(1) <- cx;
  5

let ret_near t n =
  atomic t (fun () ->
      t.ip <- check_target (pop32 t);
      t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
      10)

let ret_far t n =
  atomic t (fun () ->
      let target = check_target (pop32 t) in
      let sg = pop32 t land 0xffff in
      set_seg t sreg_cs sg;
      t.ip <- target;
      t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
      18)

let iretd t =
  atomic t (fun () ->
      let target = check_target (pop32 t) in
      let sg = pop32 t land 0xffff in
      let fl = pop32 t in
      set_seg t sreg_cs sg;
      t.ip <- target;
      set_flags t (fl land 0xffff);
      8)

(* [ctx.opcode] 에서 [operand32_form] 을 떼고 돌린다 — 아래 함수와 같이 쓰는
   16 비트 모듈은 opcode 바이트를 본다. *)
let dispatch t ctx =
  let opcode = ctx.opcode land opcode_byte in
  let ctx = { ctx with opcode } in
  match opcode with
  | 0x01 | 0x03 | 0x05 -> alu_group32 t ctx 0
  | 0x09 | 0x0b | 0x0d -> alu_group32 t ctx 1
  | 0x11 | 0x13 | 0x15 -> alu_group32 t ctx 2
  | 0x19 | 0x1b | 0x1d -> alu_group32 t ctx 3
  | 0x21 | 0x23 | 0x25 -> alu_group32 t ctx 4
  | 0x29 | 0x2b | 0x2d -> alu_group32 t ctx 5
  | 0x31 | 0x33 | 0x35 -> alu_group32 t ctx 6
  | 0x39 | 0x3b | 0x3d -> alu_group32 t ctx 7
  | 0x06 -> push_sreg t sreg_es; 10
  | 0x0e -> push_sreg t sreg_cs; 10
  | 0x16 -> push_sreg t sreg_ss; 10
  | 0x1e -> push_sreg t sreg_ds; 10
  | 0x07 -> pop_sreg t sreg_es; 8
  | 0x17 -> pop_sreg t sreg_ss; 8
  | 0x1f -> pop_sreg t sreg_ds; 8
  | b when b >= 0x40 && b <= 0x47 -> put32 t (b land 7) (inc_dec t (get32 t (b land 7)) 0); 3
  | b when b >= 0x48 && b <= 0x4f -> put32 t (b land 7) (inc_dec t (get32 t (b land 7)) 5); 3
  | b when b >= 0x50 && b <= 0x57 -> push32 t (get32 t (b land 7)); 10
  | b when b >= 0x58 && b <= 0x5f -> put32 t (b land 7) (pop32 t); 8
  | 0x60 -> pushad t
  | 0x61 -> popad t
  | 0x62 -> bound32 t ctx
  | 0x68 -> push32 t (fetch32 t); 10
  | 0x6a -> push32 t (sx8_32 (fetch8 t)); 9
  | 0x69 | 0x6b -> imul_imm t ctx
  | 0x6d -> string_op t ctx 32 Ins; 14
  | 0x6f -> string_op t ctx 32 Outs; 14
  | b when b >= 0x70 && b <= 0x7f ->
    let d = sx8 (fetch8 t) in
    if condition t (b land 0xf) then begin jump_rel t d; 8 end else 4
  | 0x81 | 0x83 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let imm = if opcode = 0x83 then sx8_32 (fetch8 t) else fetch32 t in
    let r = alu_result t regf (op_read32 t rm) imm 32 in
    if regf <> 7 then op_write32 t rm r;
    4
  | 0x85 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let a = op_read32 t rm in
    ignore (alu_result t 4 a (get32 t regf) 32);
    3
  | 0x87 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let a = op_read32 t rm in
    let b = get32 t regf in
    put32 t regf a;
    op_write32 t rm b;
    4
  | 0x89 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    op_write32 t rm (get32 t regf);
    2
  | 0x8b ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    put32 t regf (op_read32 t rm);
    2
  | 0x8c ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let sreg = sreg_field t regf ~load:false in
    (match rm with
     | Reg n -> put32 t n (seg t sreg)
     | Mem _ | Mem32 _ -> op_write t 16 rm (seg t sreg));
    2
  | 0x8d ->
    let _, regf, _, ea = decode_modrm t ~addr:ctx.addr in
    (match ea with
     | Some off -> put32 t regf off
     | None -> undefined_on_286 t true; unsupported t ctx "lea on register (66h)");
    2
  | 0x8e ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let sreg = sreg_field t regf ~load:true in
    set_seg t sreg (op_read t 16 rm);
    2
  | 0x8f ->
    restore_sp_on_fault t (fun () ->
        let _, regf, rm, _ = decode_pop_target t ~addr:ctx.addr ~bytes:4 in
        undefined_on_286 t (regf <> 0);
        op_write32 t rm (pop32 t));
    8
  | b when b >= 0x90 && b <= 0x97 ->
    let n = b land 7 in
    let a = get32 t 0 in
    put32 t 0 (get32 t n);
    put32 t n a;
    3
  | 0x98 ->
    let ax = t.regs.(0) in
    put32 t 0 (if ax land 0x8000 <> 0 then ax lor 0xffff0000 else ax);
    2
  | 0x99 -> put32 t 2 (if get32 t 0 land dword_sign <> 0 then dword_mask else 0); 2
  | 0x9a ->
    let off = fetch32 t in
    let sg = fetch16 t in
    far_call t off sg
  | 0x9c -> push32 t (flags t); 10
  | 0x9d -> set_flags t (pop32 t land 0xffff); 8
  | 0xa1 | 0xa3 ->
    let opd = moffs t ctx.addr in
    if opcode = 0xa1 then put32 t 0 (op_read32 t opd) else op_write32 t opd (get32 t 0);
    8
  | 0xa5 -> string_op t ctx 32 Movs; 9
  | 0xa7 -> string_op t ctx 32 Cmps; 9
  | 0xab -> string_op t ctx 32 Stos; 7
  | 0xad -> string_op t ctx 32 Lods; 7
  | 0xaf -> string_op t ctx 32 Scas; 7
  | 0xa9 ->
    let a = get32 t 0 in
    ignore (alu_result t 4 a (fetch32 t) 32);
    4
  | b when b >= 0xb8 && b <= 0xbf -> put32 t (b land 7) (fetch32 t); 4
  | 0xc1 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    shift_group32 t regf rm (fetch8 t);
    6
  | 0xc2 -> ret_near t (fetch16 t)
  | 0xc3 -> ret_near t 0
  | 0xc4 | 0xc5 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let off, sg = far_pointer t ctx rm "les/lds reg (66h)" in
    put32 t regf off;
    set_seg t (if opcode = 0xc4 then sreg_es else sreg_ds) sg;
    2
  | 0xc7 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let v = fetch32 t in
    undefined_on_286 t (regf <> 0);
    op_write32 t rm v;
    4
  | 0xc8 -> enter32 t
  | 0xc9 ->
    atomic t (fun () ->
        t.regs.(4) <- t.regs.(5);
        put32 t 5 (pop32 t);
        8)
  | 0xca -> ret_far t (fetch16 t)
  | 0xcb -> ret_far t 0
  | 0xcf -> iretd t
  | 0xd1 | 0xd3 ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let count = if opcode = 0xd1 then 1 else reg8 t 1 in
    shift_group32 t regf rm count;
    if opcode = 0xd1 then 4 else 8
  | 0xe0 | 0xe1 | 0xe2 -> loop32 t ctx
  | 0xe3 ->
    let d = sx8 (fetch8 t) in
    let cx = if address32 ctx.addr then get32 t 1 else t.regs.(1) in
    if cx = 0 then jump_rel t d;
    5
  | 0xe5 -> put32 t 0 (port_in32 t (fetch8 t)); 10
  | 0xe7 -> port_out32 t (fetch8 t) (get32 t 0); 10
  | 0xed -> put32 t 0 (port_in32 t t.regs.(2)); 8
  | 0xef -> port_out32 t t.regs.(2) (get32 t 0); 8
  | 0xe8 ->
    let d = fetch32 t in
    let target = check_target (t.ip + d) in
    push32 t t.ip;
    t.ip <- target;
    13
  | 0xe9 -> jump_rel t (fetch32 t); 7
  | 0xea ->
    let off = fetch32 t in
    let sg = fetch16 t in
    far_jump t off sg;
    7
  | 0xeb -> jump_rel t (sx8 (fetch8 t)); 7
  | 0xf7 -> group3_32 t ctx
  | 0xff -> group_ff t ctx
  | _ -> unsupported t ctx (Printf.sprintf "opcode %02x (66h)" opcode)
