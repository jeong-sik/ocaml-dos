(* ALU 8종과 그룹2 shift/rotate. 플래그는 8086 규칙이다. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode

(* 폭 [width](8·16·32)의 값 마스크와 부호 비트. *)
let width_mask width = (1 lsl width) - 1
let width_sign width = 1 lsl (width - 1)

(* 피연산자 폭 — 8·16·32 비트 산술의 플래그 계산에 쓴다. *)
let alu_result t op a b width =
  let mask = width_mask width in
  let sign_bit = width_sign width in
  let entry_cf_saved = t.cf in
  let r =
    match op with
    | 0 (* add *) | 2 (* adc *) ->
      let carry = if op = 2 && t.cf then 1 else 0 in
      let full = a + b + carry in
      t.cf <- full > mask;
      t.af <- ((a lxor b lxor full) land 0x10) <> 0;
      full land mask
    | 3 (* sbb *) | 5 (* sub *) | 7 (* cmp *) ->
      let borrow = if op = 3 && t.cf then 1 else 0 in
      let full = a - b - borrow in
      t.cf <- full < 0;
      t.af <- ((a lxor b lxor full) land 0x10) <> 0;
      full land mask
    | 1 (* or *) -> a lor b
    | 4 (* and *) -> a land b
    | 6 (* xor *) -> a lxor b
    | _ -> assert false
  in
  (match op with
   | 1 | 4 | 6 -> t.cf <- false; t.of_ <- false; t.af <- false
   | _ ->
     (* add/sub 계열 OF: 같은 부호 피연산자의 결과 부호 반전. adc/sbb 는
        명령 진입 시의 CF 로 계산한다 — 위에서 t.cf 를 갱신한 뒤 다시
        읽으면 새 캐리를 더하는 오류(실칩 adc 관측: OF 만 어긋남). *)
     let ov =
       match op with
       | 0 | 2 ->
         let full = a + b + (if op = 2 && entry_cf_saved then 1 else 0) in
         ((a lxor (full land mask)) land (b lxor (full land mask)) land sign_bit) <> 0
       | _ ->
         let full = a - b - (if op = 3 && entry_cf_saved then 1 else 0) in
         ((a lxor b) land (a lxor full) land sign_bit) <> 0
     in
     t.of_ <- ov);
  t.zf <- r = 0;
  t.sf <- r land sign_bit <> 0;
  t.pf <- parity_even r;
  r

let alu_group t ctx op =
  (* 00-3F ALU 8종의 여섯 형태. x6/x7 (push/pop 세그먼트 등) 는
     ALU 가 아니라 이 디스패치에 오지 않는다. *)
  match ctx.opcode land 7 with
  | 0 | 1 ->
    let width = if ctx.opcode land 1 = 0 then 8 else 16 in
    let _, regf, rm, _ = decode_modrm t ~ovr:ctx.ovr in
    let a = op_read t width rm in
    let b = if width = 8 then reg8 t regf else reg16 t regf in
    let r = alu_result t op a b width in
    if op <> 7 then op_write t width rm r;
    3
  | 2 | 3 ->
    let width = if ctx.opcode land 1 = 0 then 8 else 16 in
    let _, regf, rm, _ = decode_modrm t ~ovr:ctx.ovr in
    let a = if width = 8 then reg8 t regf else reg16 t regf in
    let b = op_read t width rm in
    let r = alu_result t op a b width in
    if op <> 7 then op_write t width (Reg regf) r;
    3
  | 4 | 5 ->
    let width = if ctx.opcode land 1 = 0 then 8 else 16 in
    let a = if width = 8 then reg8 t 0 else reg16 t 0 in
    let imm = if width = 8 then fetch8 t else fetch16 t in
    let r = alu_result t op a imm width in
    if op <> 7 then op_write t width (Reg 0) r;
    4
  | _ -> unsupported t ctx "alu form"

(* 그룹2 shift/rotate: regf 0=rol 1=ror 2=rcl 3=rcr 4=shl 5=shr
   6=shl(undoc sal) 7=sar. count 0 은 플래그도 건드리지 않는다.
   ZF/SF/PF 는 논리 shift 만 갱신(rotate 는 갱신 안 함 — 8086 규칙),
   OF 는 count=1 일 때만 정의된다. [shift_value] 는 [v] 를 [count](1 이상,
   잘린 뒤) 번 밀거나 돌린 값과 플래그를 낸다. 피연산자 읽기·쓰기는
   [shift_group]·[shift_group32] 몫이다. *)
let shift_value t regf width v count =
  let mask = width_mask width in
  let msb = width_sign width in
  let bits = width in
  let v = ref v in
  let original = !v in
  for _ = 1 to count do
    match regf with
    | 0 ->
      t.cf <- !v land msb <> 0;
      v := ((!v lsl 1) lor (!v lsr (bits - 1))) land mask
    | 1 ->
      t.cf <- !v land 1 <> 0;
      v := ((!v lsr 1) lor (!v lsl (bits - 1))) land mask
    | 2 ->
      let c = t.cf in
      t.cf <- !v land msb <> 0;
      v := ((!v lsl 1) lor (if c then 1 else 0)) land mask
    | 3 ->
      let c = t.cf in
      t.cf <- !v land 1 <> 0;
      v := ((!v lsr 1) lor (if c then msb else 0)) land mask
    | 5 ->
      t.cf <- !v land 1 <> 0;
      v := !v lsr 1
    | 7 ->
      t.cf <- !v land 1 <> 0;
      let sv = if !v land msb <> 0 then !v - (mask + 1) else !v in
      v := (sv asr 1) land mask
    | _ (* 4 shl / 6 sal *) ->
      t.cf <- !v land msb <> 0;
      v := (!v lsl 1) land mask
  done;
  (* 386 의 바이트 SHL·SAL·SHR 은 횟수가 8 의 배수(8·16·24)면 CF 가 횟수
     8 일 때와 같다: SHL·SAL 은 원래 비트 0, SHR 은 비트 7 이다. 그 밖의
     9 이상 횟수는 하나씩 민 셈과 같이 0 이다(SingleStepTests/80386 D2
     /4·/5·/6 레지스터 형태 1,182 건 전부, C0 도 같다). *)
  (match t.model with
   | I80386 ->
     if width = 8 && count land 7 = 0 then
       (match regf with
        | 4 | 6 -> t.cf <- original land 1 <> 0
        | 5 -> t.cf <- original land msb <> 0
        | _ -> ())
   | I8086 | I80186 | I80286 -> ());
  if regf >= 4 then begin
    t.zf <- !v = 0;
    t.sf <- !v land msb <> 0;
    t.pf <- parity_even !v
  end;
  if count = 1 then begin
    match regf with
    | 0 | 2 | 4 | 6 -> t.of_ <- t.cf <> (!v land msb <> 0)
    | 7 -> t.of_ <- false
    | 5 ->
      (* shr 1: OF 는 원본의 최상위 비트 — 부호가 사라졌는지를
         알린다. 결과의 두 비트를 보는 rotate 규칙과 다르다. *)
      t.of_ <- original land msb <> 0
    | _ ->
      (* ror/rcr: 결과 최상위 두 비트가 다르면 OF. *)
      let b1 = !v land msb <> 0 in
      let b2 = !v land (msb lsr 1) <> 0 in
      t.of_ <- b1 <> b2
  end;
  !v

let shift_group t regf width rm_op count =
  (* 186 부터 횟수는 아래 5비트만 쓴다. 8086 은 자르지 않고 전부 돈다. *)
  let count = match t.model with I8086 -> count | I80186 | I80286 | I80386 -> count land 0x1f in
  (* 286 은 횟수가 0 이어도 피연산자를 읽는다 — 오프셋 FFFF 의 워드면
     INT 13 이다(SingleStepTests/80286 C1·D3, 횟수 80h·E0h). *)
  (match t.model with
   | I80286 | I80386 -> if count = 0 then ignore (op_read t width rm_op)
   | I8086 | I80186 -> ());
  if count > 0 && t.model = I8086 && regf = 6 then begin
    (* SETMO/SETMOC — 8086·8088 의 문서 외 명령. 피연산자를 전부 1
       로 만들고 논리 연산처럼 플래그를 세운다. 186 부터 이 자리는
       SHL 로 바뀌었다. *)
    op_write t width rm_op (width_mask width);
    t.cf <- false; t.of_ <- false; t.af <- false;
    t.zf <- false; t.sf <- true; t.pf <- true
  end
  else if count > 0 then
    op_write t width rm_op (shift_value t regf width (op_read t width rm_op) count)

(* 386 의 66h 그룹2(C1·D1·D3): 횟수는 아래 5 비트이고, 0 이어도 피연산자를
   읽는다(286 과 같은 규칙). *)
let shift_group32 t regf rm_op count =
  let count = count land 0x1f in
  if count = 0 then ignore (op_read32 t rm_op)
  else op_write32 t rm_op (shift_value t regf 32 (op_read32 t rm_op) count)

(* 386 바이트 IDIV 의 넘침 예외 조건(group3 의 idiv 참고). *)
let idiv8_edge_bias = 0x80
let idiv8_edge_remainder = 0x7f

(* 그룹3: test/not/neg/mul/imul/div/idiv *)
let group3 t ctx =
  let width = if ctx.opcode = 0xf6 then 8 else 16 in
  let mask = if width = 8 then 0xff else 0xffff in
  let msb = if width = 8 then 0x80 else 0x8000 in
  let _, regf, rm, _ = decode_modrm t ~ovr:ctx.ovr in
  (match regf with
   | 0 | 1 ->
     let imm = if width = 8 then fetch8 t else fetch16 t in
     ignore (alu_result t 4 (op_read t width rm) imm width)
   | 2 (* not — 플래그 무변화 *) ->
     op_write t width rm (lnot (op_read t width rm) land mask)
   | 3 (* neg *) ->
     let v = op_read t width rm in
     let r = alu_result t 5 0 v width in
     op_write t width rm r
   | 4 (* mul, 부호 없음 *) ->
     if width = 8 then begin
       let p = (reg8 t 0) * (op_read t 8 rm) in
       set_reg16 t 0 p;
       t.cf <- p > 0xff; t.of_ <- p > 0xff
     end else begin
       let p = (reg16 t 0) * (op_read t 16 rm) in
       set_reg16 t 0 (p land 0xffff);
       set_reg16 t 2 (p lsr 16);
       t.cf <- p > 0xffff; t.of_ <- p > 0xffff
     end
   | 5 (* imul, 부호 *) ->
     let sx v = if v land msb <> 0 then v - (mask + 1) else v in
     if width = 8 then begin
       let p = sx (reg8 t 0) * sx (op_read t 8 rm) in
       set_reg16 t 0 (p land 0xffff);
       t.cf <- p > 0x7f || p < -0x80;
       t.of_ <- p > 0x7f || p < -0x80
     end else begin
       let p = sx (reg16 t 0) * sx (op_read t 16 rm) in
       set_reg16 t 0 (p land 0xffff);
       set_reg16 t 2 ((p lsr 16) land 0xffff);
       t.cf <- p > 0x7fff || p < -0x8000;
       t.of_ <- p > 0x7fff || p < -0x8000
     end
   | 6 | 7 (* div/idiv — 0 나누기·몫 오버플로는 인터럽트 0 *) ->
     let sx v = if v land msb <> 0 then v - (mask + 1) else v in
     let divisor = op_read t width rm in
     if divisor = 0 then divide_error t ctx
     else begin
       let dividend =
         if width = 8 then reg16 t 0
         else ((reg16 t 2) lsl 16) lor (reg16 t 0)
       in
       if regf = 6 then begin
         let q = dividend / divisor and r = dividend mod divisor in
         if q > mask then divide_error t ctx
         else begin
           if width = 8 then begin
             set_reg8 t 0 q; set_reg8 t 4 r
           end else begin
             set_reg16 t 0 q; set_reg16 t 2 r
           end
         end
       end else begin
         let sd = sx divisor in
         (* 16비트 피젯수(8비트 폭) / 32비트(16비트 폭) 의 부호 해석 *)
         let sdiv =
           if width = 8 then (if dividend land 0x8000 <> 0 then dividend - 0x10000 else dividend)
           else begin
             (* DX:AX 를 32비트 부호로 *)
             if dividend land 0x80000000 <> 0 then dividend - 0x100000000 else dividend
           end
         in
         let q = sdiv / sd and r = sdiv mod sd in
         let lo = if width = 8 then -0x80 else -0x8000 in
         let hi = if width = 8 then 0x7f else 0x7fff in
         (* 286·386 의 바이트 IDIV 는 몫이 음수이고 |피젯수| >> 7 이
            |젯수| + 80h 이며 |피젯수| 의 아래 7 비트가 |젯수| 보다 작으면 넘쳐도
            INT 0 이 아니다. AL=80h 이고 AH 는 그 아래 7 비트에 피젯수 부호를
            붙인 나머지다. SingleStepTests/80286 F6.7 에서 넘치는 2,809 건
            가운데 이 조건 4 건이 그렇고, 앞 두 조건만 맞고 나머지가 크거나
            같은 3 건은 INT 0 이다. SingleStepTests/80386 F6.7 에서는 넘치는
            94 건 가운데 6 건이 이 조건이고 모두 그렇다. 워드 IDIV 는 두
            데이터에 이 조건이 없어 손대지 않는다. *)
         let magnitude = abs sdiv in
         let edge_quotient =
           match t.model with
           | I80286 | I80386 ->
             width = 8 && (sdiv < 0) <> (sd < 0)
             && magnitude lsr 7 = abs sd + idiv8_edge_bias
             && magnitude land idiv8_edge_remainder < abs sd
           | I8086 | I80186 -> false
         in
         if edge_quotient then begin
           let r = magnitude land idiv8_edge_remainder in
           set_reg8 t 0 idiv8_edge_bias;
           set_reg8 t 4 ((if sdiv < 0 then - r else r) land 0xff)
         end
         else if q > hi || q < lo then divide_error t ctx
         else begin
           if width = 8 then begin
             set_reg8 t 0 (q land 0xff); set_reg8 t 4 (r land 0xff)
           end else begin
             set_reg16 t 0 (q land 0xffff); set_reg16 t 2 (r land 0xffff)
           end
         end
       end
     end
   | _ -> unsupported t ctx "group3");
  30

(* ---------- 32 비트(386, 66h) ---------- *)

let dword_mask = width_mask 32
let dword_sign = width_sign 32
let sign32 v = if v land dword_sign <> 0 then v - (dword_mask + 1) else v

(* 32 비트 곱의 아래·위 32 비트. 곱이 64 비트라 OCaml int(63 비트)를 넘을 수
   있어 Int64 로 한다 — 부호 없는 곱도 2^64 안이라 Int64.mul 의 잘린 값이
   그대로 맞다. *)
let mul64 a b = Int64.mul (Int64.of_int a) (Int64.of_int b)
let low32 p = Int64.to_int (Int64.logand p 0xffffffffL)
let high32 p = Int64.to_int (Int64.logand (Int64.shift_right_logical p 32) 0xffffffffL)

(* IMUL r32, r/m32, imm(69·6B 의 66h 형태)와 F7 /5 가 같이 쓴다. CF=OF 는 곱이
   32 비트 부호 값에 들지 않을 때다. *)
let imul32 t a b =
  let p = mul64 (sign32 a) (sign32 b) in
  let fits = Int64.equal p (Int64.of_int (sign32 (low32 p))) in
  t.cf <- not fits;
  t.of_ <- not fits;
  p

(* 그룹3 의 66h 형태: test/not/neg/mul/imul/div/idiv r/m32. 곱셈·나눗셈은
   EDX:EAX 를 쓴다. *)
let group3_32 t ctx =
  let _, regf, rm, _ = decode_modrm t ~ovr:ctx.ovr in
  (match regf with
   | 0 | 1 ->
     let imm = fetch32 t in
     ignore (alu_result t 4 (op_read32 t rm) imm 32)
   | 2 -> op_write32 t rm (lnot (op_read32 t rm) land dword_mask)
   | 3 -> op_write32 t rm (alu_result t 5 0 (op_read32 t rm) 32)
   | 4 ->
     let p = mul64 (get32 t 0) (op_read32 t rm) in
     put32 t 0 (low32 p);
     put32 t 2 (high32 p);
     t.cf <- high32 p <> 0;
     t.of_ <- high32 p <> 0
   | 5 ->
     let p = imul32 t (get32 t 0) (op_read32 t rm) in
     put32 t 0 (low32 p);
     put32 t 2 (high32 p)
   | 6 | 7 ->
     let divisor = op_read32 t rm in
     if divisor = 0 then divide_error t ctx
     else begin
       let dividend = Int64.logor (Int64.shift_left (Int64.of_int (get32 t 2)) 32)
           (Int64.of_int (get32 t 0)) in
       let d = Int64.of_int (if regf = 6 then divisor else sign32 divisor) in
       let q, r =
         if regf = 6 then (Int64.unsigned_div dividend d, Int64.unsigned_rem dividend d)
         else (Int64.div dividend d, Int64.rem dividend d)
       in
       let fits =
         if regf = 6 then Int64.unsigned_compare q 0xffffffffL <= 0
         else Int64.compare q (-0x80000000L) >= 0 && Int64.compare q 0x7fffffffL <= 0
       in
       if not fits then divide_error t ctx
       else begin
         put32 t 0 (low32 q);
         put32 t 2 (low32 r)
       end
     end
   | _ -> unsupported t ctx "group3");
  30
