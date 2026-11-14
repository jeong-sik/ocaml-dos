(* 386 의 두 바이트 명령: 0Fh 뒤의 바이트가 정한다. 16 비트 피연산자 형태만
   있다 — 66h 가 붙으면 [Cpu86_decode.operand_sized] 가 0Fh 에 표시를 실어
   [Cpu86_exec32] 로 보내고, 거기서는 아직 [Unsupported] 다. 주소 크기(67h)는
   [ctx.addr] 로 modrm 이 따른다. RFC 0001 3 단계를 데이터 파일 단위로 더한다:
   Jcc rel16, SETcc, MOVZX·MOVSX, FS·GS 의 PUSH·POP, BT 계열, SHLD·SHRD,
   IMUL r16,r/m16, BSF·BSR, LSS·LFS·LGS, CLTS 다. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode
open Cpu86_alu

let jcc_near_first = 0x80
let jcc_near_last = 0x8f
let setcc_first = 0x90
let setcc_last = 0x9f
let condition_field = 0xf

let sx16_of_byte v = if v >= 0x80 then v lor 0xff00 else v
let sign16 v = if v >= 0x8000 then v - 0x10000 else v
let word_mask = 0xffff
let word_msb = 0x8000
let word_bits = 16
let bit_index_mask = word_bits - 1
let word_bytes = 2
let shift_count_mask = 0x1f

(* 16 비트 피연산자의 근거리 점프는 IP 를 16 비트에서 감는다 — 70h-7Fh 와
   같다. *)
let jump_near16 t d = t.ip <- (t.ip + d) land 0xffff

(* BT·BTS·BTR·BTC: CF 에 비트를 옮기고 셋은 그 비트를 고쳐 쓴다. *)
type bit_op = Bt | Bts | Btr | Btc

(* 0F BA 의 reg 4-7. 0-3 은 INT 6 이다(80386.csv 의 0FBA ex 0-3). *)
let bit_op_of_group = function
  | 4 -> Some Bt
  | 5 -> Some Bts
  | 6 -> Some Btr
  | 7 -> Some Btc
  | _ -> None

let apply_bit_op t op opd bit =
  let v = op_read t 16 opd in
  let mask = 1 lsl bit in
  t.cf <- v land mask <> 0;
  match op with
  | Bt -> ()
  | Bts -> op_write t 16 opd (v lor mask)
  | Btr -> op_write t 16 opd (v land lnot mask land word_mask)
  | Btc -> op_write t 16 opd (v lxor mask)

(* 레지스터가 비트 번호를 주면 번호는 부호 있는 16 비트다. 메모리 피연산자는 그
   번호의 워드 몫만큼 옮겨 간다(EA + 2 * (번호 asr 4)). 레지스터 피연산자는
   번호를 16 으로 나눈 나머지만 쓴다. *)
let bit_by_register t ctx op =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let number = reg16 t regf in
  let shift = word_bytes * (sign16 number asr 4) in
  let opd =
    match rm with
    | Reg _ -> rm
    | Mem (b, o) -> Mem (b, (o + shift) land word_mask)
    | Mem32 (b, o) -> Mem32 (b, (o + shift) land offset32_mask)
  in
  apply_bit_op t op opd (number land bit_index_mask);
  6

let bit_by_immediate t ctx =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  match bit_op_of_group regf with
  | None -> fault vector_invalid_opcode
  | Some op ->
    let number = fetch8 t in
    apply_bit_op t op rm (number land bit_index_mask);
    6

(* SHLD·SHRD r/m16, r16 의 결과와 CF. 횟수는 5 비트이고 0 이면 결과도 플래그도
   바뀌지 않는다. 16 을 넘는 횟수는 설명서가 정의하지 않는다. 칩은 원천을 한 번 더
   이어 붙인 48 비트(SHLD 는 목적지:원천:원천, SHRD 는 원천:원천:목적지)를 민다 —
   그래서 결과는 원천을 (횟수-16) 만큼 돌린 값이다(SingleStepTests/80386 0FA4
   의 횟수 17-31 레지스터 형태: c=18 s=4088h → 0221h). 16 이하에서는 보통의
   32 비트 이어 밀기와 같다. *)
let shld16 dst src count =
  let joined = (dst lsl (2 * word_bits)) lor (src lsl word_bits) lor src in
  ((joined lsl count) lsr (2 * word_bits) land word_mask,
   (joined lsr (3 * word_bits - count)) land 1 = 1)

let shrd16 dst src count =
  let joined = (src lsl (2 * word_bits)) lor (src lsl word_bits) lor dst in
  ((joined lsr count) land word_mask, (joined lsr (count - 1)) land 1 = 1)

let shift_double t ctx ~left ~by_cl =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let count = (if by_cl then reg8 t 1 else fetch8 t) land shift_count_mask in
  (* 횟수가 0 이어도 메모리 피연산자는 읽는다 — 한계 밖이면 그 결함이 난다
     (0FA4·0FA5·0FAC·0FAD 와 67h 형태의 횟수 0 결함 90 건). *)
  let dst = op_read t 16 rm in
  if count <> 0 then begin
    let src = reg16 t regf in
    let r, cf = if left then shld16 dst src count else shrd16 dst src count in
    op_write t 16 rm r;
    t.cf <- cf;
    t.zf <- r = 0;
    t.sf <- r land word_msb <> 0;
    t.pf <- parity_even r;
    (* 1 번일 때만 정의된다: 부호가 바뀌었는가 *)
    t.of_ <- (r lxor dst) land word_msb <> 0
  end;
  3

(* 16 비트 값에서 가장 낮은(forward) 또는 가장 높은 1 비트의 자리. 0 에는
   자리가 없다. *)
let set_bit_index ~forward v =
  let rec go n step =
    if n < 0 || n > bit_index_mask then None
    else if v land (1 lsl n) <> 0 then Some n
    else go (n + step) step
  in
  if forward then go 0 1 else go bit_index_mask (-1)

(* BSF·BSR: 원천이 0 이면 ZF=1 이고 목적지는 그대로다(0FBC·0FBD 에서 ZF=1 로
   끝나는 158·163 건). *)
let bit_scan t ctx ~forward =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  (match set_bit_index ~forward (op_read t 16 rm) with
   | None -> t.zf <- true
   | Some n ->
     t.zf <- false;
     set_reg16 t regf n);
  10

(* LSS·LFS·LGS r16, m16:16: LES·LDS 와 같다. 레지스터 피연산자는 INT 6 이다. *)
let load_far_pointer t ctx sreg =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  check_span t rm far_pointer16_bytes;
  (match rm with
   | Mem (b, o) | Mem32 (b, o) ->
     let off = read16 t b o in
     let segv = read16 t b (o + word_bytes) in
     set_reg16 t regf off;
     set_seg t sreg segv
   | Reg _ -> fault vector_invalid_opcode);
  7

let dispatch t ctx =
  let op2 = fetch8 t in
  match op2 with
  | b when b >= jcc_near_first && b <= jcc_near_last ->
    let d = fetch16 t in
    if condition t (b land condition_field) then begin jump_near16 t d; 8 end else 4
  | b when b >= setcc_first && b <= setcc_last ->
    let _, _, rm, _ = decode_modrm t ~addr:ctx.addr in
    op_write t 8 rm (Bool.to_int (condition t (b land condition_field)));
    4
  | 0xa0 -> push16 t (seg t sreg_fs); 10
  | 0xa1 -> set_seg t sreg_fs (pop16 t); 8
  | 0xa8 -> push16 t (seg t sreg_gs); 10
  | 0xa9 -> set_seg t sreg_gs (pop16 t); 8
  (* MOVZX·MOVSX r16: B6/BE 는 바이트를 넓히고, B7/BF 는 16 비트 피연산자에서
     워드를 그대로 옮긴다. *)
  | 0xb6 | 0xb7 | 0xbe | 0xbf ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let v =
      match op2 with
      | 0xb6 -> op_read t 8 rm
      | 0xbe -> sx16_of_byte (op_read t 8 rm)
      | _ -> op_read t 16 rm
    in
    set_reg16 t regf v;
    3
  (* CLTS: 실모드에서 CR0 의 TS 를 지운다. CR0 는 모델에 없어 할 일이 없다. *)
  | 0x06 -> 2
  | 0xa3 -> bit_by_register t ctx Bt
  | 0xab -> bit_by_register t ctx Bts
  | 0xb3 -> bit_by_register t ctx Btr
  | 0xbb -> bit_by_register t ctx Btc
  | 0xba -> bit_by_immediate t ctx
  | 0xa4 -> shift_double t ctx ~left:true ~by_cl:false
  | 0xa5 -> shift_double t ctx ~left:true ~by_cl:true
  | 0xac -> shift_double t ctx ~left:false ~by_cl:false
  | 0xad -> shift_double t ctx ~left:false ~by_cl:true
  | 0xaf ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    set_reg16 t regf (imul16 t (reg16 t regf) (op_read t 16 rm));
    21
  | 0xbc -> bit_scan t ctx ~forward:true
  | 0xbd -> bit_scan t ctx ~forward:false
  | 0xb2 -> load_far_pointer t ctx sreg_ss
  | 0xb4 -> load_far_pointer t ctx sreg_fs
  | 0xb5 -> load_far_pointer t ctx sreg_gs
  | _ -> unsupported t ctx (Printf.sprintf "0f %02x (386)" op2)
