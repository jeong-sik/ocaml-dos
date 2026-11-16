(* 386 의 두 바이트 명령: 0Fh 뒤의 바이트가 정한다. 피연산자 폭은 [width] 다 —
   66h 가 없으면 [Cpu86_exec] 가 [Word] 로, 있으면 [Cpu86_exec32] 가 [Dword] 로
   부른다. 주소 크기(67h)는 [ctx.addr] 로 modrm 이 따른다. RFC 0001 3 단계의
   명령이다: Jcc rel16·rel32, SETcc, MOVZX·MOVSX, FS·GS 의 PUSH·POP, BT 계열,
   SHLD·SHRD, IMUL r,r/m, BSF·BSR, LSS·LFS·LGS, CLTS. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode
open Cpu86_alu

type width = Word | Dword

let jcc_near_first = 0x80
let jcc_near_last = 0x8f
let setcc_first = 0x90
let setcc_last = 0x9f
let condition_field = 0xf
let shift_count_mask = 0x1f
let byte_bits = 8

(* 16 비트 주소와 16 비트 IP 가 감기는 자리. *)
let offset16_mask = 0xffff

let bits = function Word -> 16 | Dword -> 32
let mask w = width_mask (bits w)
let msb w = width_sign (bits w)
let unit_bytes w = bits w / byte_bits
let signed w v = if v land msb w <> 0 then v - (mask w + 1) else v

(* [from] 비트의 값을 [w] 폭으로 넓히며 부호 비트를 위로 채운다. *)
let sign_extend ~from w v =
  if v land width_sign from <> 0 then (v lor lnot (width_mask from)) land mask w else v

let read w t rm = match w with Word -> op_read t 16 rm | Dword -> op_read32 t rm
let write w t rm v = match w with Word -> op_write t 16 rm v | Dword -> op_write32 t rm v
let reg w t n = match w with Word -> reg16 t n | Dword -> get32 t n
let set_reg w t n v = match w with Word -> set_reg16 t n v | Dword -> put32 t n v

(* 16 비트 피연산자의 근거리 점프는 IP 를 16 비트에서 감는다 — 70h-7Fh 와
   같다. 32 비트는 EIP 로 세고 FFFFh 를 넘으면 INT 13 이다
   ([Cpu86_flow32.jump_rel]). 660F80-8F 8,000 건에는 그런 점프가 없어 66C3 에서
   본 규칙을 따른다. *)
let jcc_near t w op2 =
  let d = match w with Word -> fetch16 t | Dword -> fetch32 t in
  if condition t (op2 land condition_field) then begin
    (match w with
     | Word -> t.ip <- (t.ip + d) land offset16_mask
     | Dword -> Cpu86_flow32.jump_rel t d);
    8
  end
  else 4

let push_seg t w sreg =
  match w with Word -> push16 t (seg t sreg) | Dword -> Cpu86_flow32.push_sreg t sreg

let pop_seg t w sreg =
  match w with Word -> set_seg t sreg (pop16 t) | Dword -> Cpu86_flow32.pop_sreg t sreg

(* BT·BTS·BTR·BTC: CF 에 비트를 옮기고 셋은 그 비트를 고쳐 쓴다. *)
type bit_op = Bt | Bts | Btr | Btc

(* 0F BA 의 reg 4-7. 0-3 은 INT 6 이다(80386.csv 의 0FBA ex 0-3). *)
let bit_op_of_group = function
  | 4 -> Some Bt
  | 5 -> Some Bts
  | 6 -> Some Btr
  | 7 -> Some Btc
  | _ -> None

let apply_bit_op t w op opd bit =
  let v = read w t opd in
  let m = 1 lsl bit in
  t.cf <- v land m <> 0;
  match op with
  | Bt -> ()
  | Bts -> write w t opd (v lor m)
  | Btr -> write w t opd (v land lnot m land mask w)
  | Btc -> write w t opd (v lxor m)

(* 비트 번호에서 피연산자 안의 자리를 고르는 아래 비트 수(16 = 2^4, 32 = 2^5). *)
let index_bits = function Word -> 4 | Dword -> 5
let index_mask w = bits w - 1

(* 레지스터가 비트 번호를 주면 번호는 피연산자 폭의 부호 있는 값이다. 메모리
   피연산자는 그 번호의 단위 몫만큼 옮겨 간다(EA + 단위 바이트 * (번호 asr
   index_bits)). 레지스터 피연산자는 번호의 아래 자리만 쓴다. *)
let bit_by_register t ctx w op =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let number = reg w t regf in
  let shift = unit_bytes w * (signed w number asr index_bits w) in
  let opd =
    match rm with
    | Reg _ -> rm
    | Mem (b, o) -> Mem (b, (o + shift) land offset16_mask)
    | Mem32 (b, o) -> Mem32 (b, (o + shift) land offset32_mask)
  in
  apply_bit_op t w op opd (number land index_mask w);
  6

let bit_by_immediate t ctx w =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  match bit_op_of_group regf with
  | None -> fault vector_invalid_opcode
  | Some op ->
    let number = fetch8 t in
    apply_bit_op t w op rm (number land index_mask w);
    6

(* SHLD·SHRD r/m16, r16 의 결과와 CF. 16 을 넘는 횟수는 설명서가 정의하지 않는다.
   칩은 원천을 한 번 더 이어 붙인 48 비트(SHLD 는 목적지:원천:원천, SHRD 는
   원천:원천:목적지)를 민다 — 그래서 결과는 원천을 (횟수-16) 만큼 돌린 값이다
   (SingleStepTests/80386 0FA4 의 횟수 17-31 레지스터 형태: c=18 s=4088h →
   0221h). 16 이하에서는 보통의 32 비트 이어 밀기와 같다. *)
let shld16 dst src count =
  let n = bits Word in
  let joined = (dst lsl (2 * n)) lor (src lsl n) lor src in
  ((joined lsl count) lsr (2 * n) land mask Word, (joined lsr (3 * n - count)) land 1 = 1)

let shrd16 dst src count =
  let n = bits Word in
  let joined = (src lsl (2 * n)) lor (src lsl n) lor dst in
  ((joined lsr count) land mask Word, (joined lsr (count - 1)) land 1 = 1)

(* 32 비트는 횟수(1-31)가 폭보다 작아 64 비트 이어 밀기다. 위로 밀려난 비트는
   OCaml int 에서 잘려도 아래 32 비트에 닿지 않는다. *)
let shld32 dst src count =
  let n = bits Dword in
  (((dst lsl count) lor (src lsr (n - count))) land mask Dword, (dst lsr (n - count)) land 1 = 1)

let shrd32 dst src count =
  let n = bits Dword in
  (((dst lsr count) lor (src lsl (n - count))) land mask Dword, (dst lsr (count - 1)) land 1 = 1)

let shift_double t ctx w ~left ~by_cl =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let count = (if by_cl then reg8 t 1 else fetch8 t) land shift_count_mask in
  (* 횟수가 0 이어도 메모리 피연산자는 읽는다 — 한계 밖이면 그 결함이 난다
     (0FA4·0FA5·0FAC·0FAD 와 67h 형태의 횟수 0 결함 90 건). 0 이면 결과도
     플래그도 바뀌지 않는다. *)
  let dst = read w t rm in
  if count <> 0 then begin
    let src = reg w t regf in
    let r, cf =
      match w, left with
      | Word, true -> shld16 dst src count
      | Word, false -> shrd16 dst src count
      | Dword, true -> shld32 dst src count
      | Dword, false -> shrd32 dst src count
    in
    write w t rm r;
    t.cf <- cf;
    t.zf <- r = 0;
    t.sf <- r land msb w <> 0;
    t.pf <- parity_even r;
    (* 1 번일 때만 정의된다: 부호가 바뀌었는가 *)
    t.of_ <- (r lxor dst) land msb w <> 0
  end;
  3

let imul_reg t ctx w =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  (match w with
   | Word -> set_reg16 t regf (imul16 t (reg16 t regf) (op_read t 16 rm))
   | Dword -> put32 t regf (low32 (imul32 t (get32 t regf) (op_read32 t rm))));
  21

(* 값에서 가장 낮은(forward) 또는 가장 높은 1 비트의 자리. 0 에는 자리가 없다. *)
let set_bit_index w ~forward v =
  let top = index_mask w in
  let rec go n step =
    if n < 0 || n > top then None
    else if v land (1 lsl n) <> 0 then Some n
    else go (n + step) step
  in
  if forward then go 0 1 else go top (-1)

(* BSF·BSR: 원천이 0 이면 ZF=1 이고 목적지는 그대로다(0FBC·0FBD 에서 ZF=1 로
   끝나는 158·163 건). *)
let bit_scan t ctx w ~forward =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  (match set_bit_index w ~forward (read w t rm) with
   | None -> t.zf <- true
   | Some n ->
     t.zf <- false;
     set_reg w t regf n);
  10

let far_pointer_bytes = function Word -> far_pointer16_bytes | Dword -> far_pointer32_bytes

(* LSS·LFS·LGS r, m16:16 또는 m16:32: LES·LDS 와 같다. 레지스터 피연산자는
   INT 6 이다. *)
let load_far_pointer t ctx w sreg =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  check_span t rm (far_pointer_bytes w);
  (match rm with
   | Mem (b, o) | Mem32 (b, o) ->
     let off = match w with Word -> read16 t b o | Dword -> read32 t b o in
     let segv = read16 t b (o + unit_bytes w) in
     set_reg w t regf off;
     set_seg t sreg segv
   | Reg _ -> fault vector_invalid_opcode);
  7

(* MOVZX·MOVSX: B6·BE 는 바이트를, B7·BF 는 워드를 넓힌다. 16 비트 피연산자의
   B7·BF 는 워드를 그대로 옮긴다. *)
let move_extended t ctx w op2 =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let from, extend_sign =
    match op2 with
    | 0xb6 -> (byte_bits, false)
    | 0xb7 -> (bits Word, false)
    | 0xbe -> (byte_bits, true)
    | _ -> (bits Word, true)
  in
  let v = op_read t from rm in
  set_reg w t regf (if extend_sign then sign_extend ~from w v else v);
  3

let dispatch t ctx w =
  let op2 = fetch8 t in
  match op2 with
  | b when b >= jcc_near_first && b <= jcc_near_last -> jcc_near t w b
  | b when b >= setcc_first && b <= setcc_last ->
    let _, _, rm, _ = decode_modrm t ~addr:ctx.addr in
    op_write t 8 rm (Bool.to_int (condition t (b land condition_field)));
    4
  | 0xa0 -> push_seg t w sreg_fs; 10
  | 0xa1 -> pop_seg t w sreg_fs; 8
  | 0xa8 -> push_seg t w sreg_gs; 10
  | 0xa9 -> pop_seg t w sreg_gs; 8
  | 0xb6 | 0xb7 | 0xbe | 0xbf -> move_extended t ctx w op2
  (* CLTS: 실모드에서 CR0 의 TS 를 지운다. CR0 는 모델에 없어 할 일이 없다. *)
  | 0x06 -> 2
  | 0xa3 -> bit_by_register t ctx w Bt
  | 0xab -> bit_by_register t ctx w Bts
  | 0xb3 -> bit_by_register t ctx w Btr
  | 0xbb -> bit_by_register t ctx w Btc
  | 0xba -> bit_by_immediate t ctx w
  | 0xa4 -> shift_double t ctx w ~left:true ~by_cl:false
  | 0xa5 -> shift_double t ctx w ~left:true ~by_cl:true
  | 0xac -> shift_double t ctx w ~left:false ~by_cl:false
  | 0xad -> shift_double t ctx w ~left:false ~by_cl:true
  | 0xaf -> imul_reg t ctx w
  | 0xbc -> bit_scan t ctx w ~forward:true
  | 0xbd -> bit_scan t ctx w ~forward:false
  | 0xb2 -> load_far_pointer t ctx w sreg_ss
  | 0xb4 -> load_far_pointer t ctx w sreg_fs
  | 0xb5 -> load_far_pointer t ctx w sreg_gs
  | _ ->
    let form = match w with Word -> "386" | Dword -> "386, 66h" in
    unsupported t ctx (Printf.sprintf "0f %02x (%s)" op2 form)
