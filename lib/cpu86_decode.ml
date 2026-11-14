(* 명령 바이트와 피연산자: 페치, 접두, modrm, 메모리·포트·스택 접근. *)

open Cpu86_state
open Cpu86_fault

(* ---------- 명령 페치와 피연산자 ---------- *)

let fetch8 t =
  if t.ip = t.fetch_end then fault vector_segment_overrun;
  let phys = physical t ~seg:t.segs.(1) ~off:t.ip in
  t.ip <- (t.ip + 1) land t.fetch_ip_mask;
  t.read phys

let fetch16 t =
  let lo = fetch8 t in
  let hi = fetch8 t in
  lo lor (hi lsl 8)

(* 66h 의 imm32. 아래 워드부터 읽는다 — OCaml 은 [lor] 의 오른쪽을 먼저
   계산하니 순서를 let 으로 묶는다. *)
let fetch32 t =
  let lo = fetch16 t in
  let hi = fetch16 t in
  lo lor (hi lsl 16)

type operand =
  | Reg of int       (** 레지스터 번호 (폭은 문맥) *)
  | Mem of int * int (** 세그먼트 레지스터 번호, 세그먼트 안 16비트 오프셋.
                         워드 접근은 오프셋이 세그먼트 안에서 감긴다 —
                         물리 주소 하나만 들고 있으면 offset 0xFFFF 에서
                         옆 세그먼트를 읽는다(실칩 관측). 베이스 대신
                         레지스터를 드는 것은 386 이 세그먼트 끝을 넘은
                         SS 접근과 다른 접근을 다른 결함으로 받아서다. *)

(* modrm 디코드. 반환: (mod, reg필드, 피연산자, 유효주소 오프셋).
   오프셋은 메모리 피연산자일 때만 Some — LEA 가 세그먼트를 더하기 전
   오프셋을 필요로 한다. 세그먼트 override 가 있으면 [~ovr], 없으면
   기본 규칙(BP 기반 = SS, 나머지 = DS). *)
let decode_modrm t ~ovr =
  let byte = fetch8 t in
  let m = byte lsr 6 in
  let regf = (byte lsr 3) land 7 in
  let rm = byte land 7 in
  let operand, ea =
    if m = 3 then (Reg rm, None)
    else begin
      let base =
        match rm with
        | 0 -> t.regs.(3) + t.regs.(6)   (* BX+SI *)
        | 1 -> t.regs.(3) + t.regs.(7)   (* BX+DI *)
        | 2 -> t.regs.(5) + t.regs.(6)   (* BP+SI *)
        | 3 -> t.regs.(5) + t.regs.(7)   (* BP+DI *)
        | 4 -> t.regs.(6)                (* SI *)
        | 5 -> t.regs.(7)                (* DI *)
        | 6 -> if m = 0 then 0 else t.regs.(5)  (* BP; mod=0 이면 disp16 *)
        | _ -> t.regs.(3)                (* BX *)
      in
      let disp =
        match m with
        | 0 -> if rm = 6 then fetch16 t else 0
        | 1 ->
          let d = fetch8 t in
          if d >= 0x80 then d - 0x100 else d
        | _ -> fetch16 t
      in
      let segsel =
        match ovr with
        | Some s -> s
        | None -> if rm = 2 || rm = 3 || (rm = 6 && m <> 0) then sreg_ss else sreg_ds
      in
      let off = (base + disp) land 0xffff in
      (Mem (segsel, off), Some off)
    end
  in
  (m, regf, operand, ea)

(* 세그먼트 베이스 + 오프셋 → 물리 주소. 오프셋은 16비트에서 감긴다. *)
let at_base t base off = (base + (off land 0xffff)) land t.addr_mask
let at t sreg off = at_base t (t.segs.(sreg) lsl 4) off

(* 286 실모드에서 오프셋 FFFF 의 워드는 세그먼트 끝을 넘는다. 8086 처럼
   오프셋 0 으로 감지 않고 INT 13 이며, 바이트는 하나도 오가지 않는다
   (SingleStepTests/80286: 워드 피연산자 파일마다 30-50 건). 두 바이트
   메모리 접근은 모두 이 둘을 지난다. *)
let overruns t off =
  match t.model with
  | I80286 | I80386 -> off land 0xffff = 0xffff
  | I8086 | I80186 -> false

(* 세그먼트 끝을 넘은 접근의 결함. 386 은 SS 면 INT 12(#SS), 다른
   세그먼트면 INT 13(#GP)이다(SingleStepTests/80386: SP=FFFF 의 POP·LEAVE 는
   12, SI=FFFF 의 LODSW 는 13). 286 은 모두 INT 13 이다. *)
let overrun_vector t sreg =
  match t.model with
  | I80386 -> if sreg = sreg_ss then vector_stack_fault else vector_segment_overrun
  | I8086 | I80186 | I80286 -> vector_segment_overrun

let check_word t sreg off = if overruns t off then fault (overrun_vector t sreg)

let read16 t s o =
  check_word t s o;
  let base = t.segs.(s) lsl 4 in
  t.read (at_base t base o) lor (t.read (at_base t base (o + 1)) lsl 8)

(* 386 의 IP 는 10000h 가 될 수 있어([fetch_ip_mask]) 윗바이트도 자른다. *)
let write16 t s o v =
  check_word t s o;
  let base = t.segs.(s) lsl 4 in
  t.write (at_base t base o) (v land 0xff);
  t.write (at_base t base (o + 1)) ((v lsr 8) land 0xff)

(* 더블워드(386, 66h). 오프셋 FFFDh 부터는 네 바이트가 세그먼트 끝을
   넘는다 — 바이트 하나 오가기 전에 [overrun_vector] 결함이다. *)
let dword_last_offset = 0xfffc

let check_dword t sreg off =
  if off land 0xffff > dword_last_offset then fault (overrun_vector t sreg)

let read32 t s o =
  check_dword t s o;
  let base = t.segs.(s) lsl 4 in
  let b i = t.read (at_base t base (o + i)) in
  let b0 = b 0 in
  let b1 = b 1 in
  let b2 = b 2 in
  let b3 = b 3 in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let write32 t s o v =
  check_dword t s o;
  let base = t.segs.(s) lsl 4 in
  for i = 0 to 3 do
    t.write (at_base t base (o + i)) ((v lsr (8 * i)) land 0xff)
  done

let op_read32 t = function
  | Reg n -> get32 t n
  | Mem (s, o) -> read32 t s o

let op_write32 t opd v =
  match opd with
  | Reg n -> put32 t n v
  | Mem (s, o) -> write32 t s o v

let op_read t width = function
  | Reg n -> if width = 8 then reg8 t n else reg16 t n
  | Mem (b, o) -> if width = 8 then t.read (at t b o) else read16 t b o

let op_write t width opd v =
  match opd with
  | Reg n -> if width = 8 then set_reg8 t n v else set_reg16 t n v
  | Mem (b, o) -> if width = 8 then t.write (at t b o) v else write16 t b o v

(* 포트는 바이트 폭이다. 워드 접근은 p 와 p+1 두 번 — 한 번으로
   줄이면 상위 바이트가 늘 0 이 되고, 장치 쪽에서도 두 번째 포트의
   부수효과(DAC 데이터 레지스터 등)가 사라진다. IN/OUT 과 INSW/OUTSW 가
   같이 쓴다. *)
let port_in16 t p =
  (t.port_in p land 0xff) lor ((t.port_in ((p + 1) land 0xffff) land 0xff) lsl 8)

let port_out16 t p v =
  t.port_out p (v land 0xff);
  t.port_out ((p + 1) land 0xffff) ((v lsr 8) land 0xff)

let port_in32 t p =
  let lo = port_in16 t p in
  let hi = port_in16 t ((p + 2) land 0xffff) in
  lo lor (hi lsl 16)

let port_out32 t p v =
  port_out16 t p (v land 0xffff);
  port_out16 t ((p + 2) land 0xffff) ((v lsr 16) land 0xffff)

(* SP 는 쓰기가 끝난 뒤에 내린다 — 넘치면 SP 도 그대로다. *)
let push16 t v =
  let sp = (t.regs.(4) - 2) land 0xffff in
  write16 t sreg_ss sp v;
  t.regs.(4) <- sp

let pop16 t =
  let v = read16 t sreg_ss t.regs.(4) in
  t.regs.(4) <- (t.regs.(4) + 2) land 0xffff;
  v

(* 실모드 스택은 16 비트(SS 의 B 비트 0)라 32 비트 push·pop 도 SP 만
   움직이고 ESP 의 위 절반은 그대로다. *)
let push32 t v =
  let sp = (t.regs.(4) - 4) land 0xffff in
  write32 t sreg_ss sp v;
  t.regs.(4) <- sp

let pop32 t =
  let v = read32 t sreg_ss t.regs.(4) in
  t.regs.(4) <- (t.regs.(4) + 4) land 0xffff;
  v

(* ---------- jcc 조건 ---------- *)

let condition t n =
  match n with
  | 0 -> t.of_
  | 1 -> not t.of_
  | 2 -> t.cf
  | 3 -> not t.cf
  | 4 -> t.zf
  | 5 -> not t.zf
  | 6 -> t.cf || t.zf
  | 7 -> not (t.cf || t.zf)
  | 8 -> t.sf
  | 9 -> not t.sf
  | 10 -> t.pf
  | 11 -> not t.pf
  | 12 -> t.sf <> t.of_
  | 13 -> t.sf = t.of_
  | 14 -> (t.sf <> t.of_) || t.zf
  | _ -> (t.sf = t.of_) && not t.zf

(* 세그먼트 override 접두. 64h·65h(FS·GS)는 386 부터다 — 그 앞 모델에서 이
   바이트는 접두가 아니라 opcode 다. *)
(* 386 이 LOCK 을 받는 명령과 modrm reg 필드. 비트 r 이 켜져 있으면 /r 이
   받는다. None 이면 그 opcode 는 받지 않는다. 메모리를 읽고 고쳐 쓰는
   명령만 들어 있다: ALU 일곱(CMP 는 아니다), 80-83 의 /0-/6, XCHG,
   NOT·NEG, INC·DEC (Intel 80386 Programmer's Reference Manual 1986 의
   LOCK, SingleStepTests/80386 80386.csv 의 lck 열). *)
let lock_any_reg = 0xff
let lock_regs_alu_imm = 0x7f   (* /0-/6 *)
let lock_regs_not_neg = 0x0c   (* /2 NOT, /3 NEG *)
let lock_regs_inc_dec = 0x03   (* /0 INC, /1 DEC *)

let lock_regs = function
  | 0x00 | 0x01 | 0x08 | 0x09 | 0x10 | 0x11 | 0x18 | 0x19
  | 0x20 | 0x21 | 0x28 | 0x29 | 0x30 | 0x31 | 0x86 | 0x87 -> Some lock_any_reg
  | 0x80 | 0x81 | 0x82 | 0x83 -> Some lock_regs_alu_imm
  | 0xf6 | 0xf7 -> Some lock_regs_not_neg
  | 0xfe | 0xff -> Some lock_regs_inc_dec
  | _ -> None

(* 386 은 LOCK 을 받지 않는 명령에 붙은 LOCK 을 INT 6 결함으로 거절한다.
   받는 명령도 고칠 피연산자가 레지스터면(modrm mod=3) INT 6 이다
   (SingleStepTests/80386 00 의 lock add dh,bh 같은 테스트). 0F 는 둘째
   바이트가 정하는데 386 의 0F 가 아직 없다. 8086·186·286 은 LOCK 을
   버스 신호로만 쓰고 명령은 그대로 돈다. modrm 은 들여다보기만 하고 IP 를
   되돌린다. LOCK 접두가 붙은 명령에서만 부른다. *)
let check_lock t ctx =
  match t.model with
  | I8086 | I80186 | I80286 -> ()
  | I80386 ->
    if ctx.opcode = 0x0f then unsupported t ctx "lock 0f (386)";
    (match lock_regs ctx.opcode with
     | None -> fault vector_invalid_opcode
     | Some regs ->
       let after_opcode = t.ip in
       let modrm = fetch8 t in
       t.ip <- after_opcode;
       let register_operand = modrm lsr 6 = 3 in
       if register_operand || regs land (1 lsl ((modrm lsr 3) land 7)) = 0 then
         fault vector_invalid_opcode)

(* SingleStepTests/80386 80386.csv 의 66 열이 1 인 한 바이트 opcode 155 개: 66h 가
   폭을 32 비트로 바꾸는 명령이다. 표 밖의 opcode(바이트 명령, 0Fh 등)는 66h 가
   붙어도 16 비트 경로가 돌린다. *)
let operand_sized_opcodes =
  [ 0x01; 0x03; 0x05; 0x06; 0x07; 0x09; 0x0b; 0x0d; 0x0e; 0x11; 0x13; 0x15;
    0x16; 0x17; 0x19; 0x1b; 0x1d; 0x1e; 0x1f; 0x21; 0x23; 0x25; 0x29; 0x2b;
    0x2d; 0x31; 0x33; 0x35; 0x39; 0x3b; 0x3d;
    0x40; 0x41; 0x42; 0x43; 0x44; 0x45; 0x46; 0x47; 0x48; 0x49; 0x4a; 0x4b;
    0x4c; 0x4d; 0x4e; 0x4f; 0x50; 0x51; 0x52; 0x53; 0x54; 0x55; 0x56; 0x57;
    0x58; 0x59; 0x5a; 0x5b; 0x5c; 0x5d; 0x5e; 0x5f;
    0x60; 0x61; 0x62; 0x63; 0x68; 0x69; 0x6a; 0x6b; 0x6d; 0x6f;
    0x70; 0x71; 0x72; 0x73; 0x74; 0x75; 0x76; 0x77; 0x78; 0x79; 0x7a; 0x7b;
    0x7c; 0x7d; 0x7e; 0x7f;
    0x81; 0x83; 0x85; 0x87; 0x89; 0x8b; 0x8c; 0x8d; 0x8e; 0x8f;
    0x90; 0x91; 0x92; 0x93; 0x94; 0x95; 0x96; 0x97; 0x98; 0x99; 0x9a; 0x9c;
    0x9d; 0xa1; 0xa3; 0xa5; 0xa7; 0xa9; 0xab; 0xad; 0xaf;
    0xb8; 0xb9; 0xba; 0xbb; 0xbc; 0xbd; 0xbe; 0xbf;
    0xc1; 0xc2; 0xc3; 0xc4; 0xc5; 0xc7; 0xc8; 0xc9; 0xca; 0xcb; 0xcf;
    0xd1; 0xd3; 0xe0; 0xe1; 0xe2; 0xe3; 0xe5; 0xe7; 0xe8; 0xe9; 0xea; 0xeb;
    0xed; 0xef; 0xf7; 0xff ]

let operand_sized =
  let table = Array.make byte_values false in
  List.iter (fun op -> table.(op) <- true) operand_sized_opcodes;
  table

(* 접두를 읽어 문맥을 만든다. [ovr]·[rep] 은 지금까지 본 접두다 — 같은
   종류의 접두가 여럿이면 마지막 것이 남는다. LOCK 이 있을 때만
   [check_lock] 을 부른다 — 명령마다 부르면 삼국지3 3천만 스텝의 기계어
   명령이 2.2% 는다. 66h 를 보면 [prefixes_after_66] 로 넘어가, 66h 가 없는
   명령은 그 표시를 들고 다니지 않는다. *)
let rec prefixes_after t ~base_ip ovr rep lock =
  let b = fetch8 t in
  match t.prefixes.(b) with
  | Not_prefix ->
    let ctx = { base_ip; opcode = b; ovr; rep } in
    if lock then check_lock t ctx;
    ctx
  | Segment s -> prefixes_after t ~base_ip (Some s) rep lock
  | Lock -> prefixes_after t ~base_ip ovr rep true
  | Rep z -> prefixes_after t ~base_ip ovr (Some z) lock
  | Operand_size -> prefixes_after_66 t ~base_ip ovr rep lock

and prefixes_after_66 t ~base_ip ovr rep lock =
  let b = fetch8 t in
  match t.prefixes.(b) with
  | Not_prefix ->
    let ctx = { base_ip; opcode = b; ovr; rep } in
    if lock then check_lock t ctx;
    if operand_sized.(b) then { ctx with opcode = b lor operand32_form } else ctx
  | Segment s -> prefixes_after_66 t ~base_ip (Some s) rep lock
  | Lock -> prefixes_after_66 t ~base_ip ovr rep true
  | Rep z -> prefixes_after_66 t ~base_ip ovr (Some z) lock
  | Operand_size -> prefixes_after_66 t ~base_ip ovr rep lock

let decode_prefixes t ~base_ip = prefixes_after t ~base_ip None None false
