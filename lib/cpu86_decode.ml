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
  | Mem32 of int * int (** 67h 의 32 비트 오프셋. 16 비트에서 감기지 않고,
                           실모드 한계 FFFFh 를 넘는 바이트가 있으면 접근이
                           [overrun_vector] 결함이다([within]). *)

let sx8 d = if d >= 0x80 then d - 0x100 else d

(* 67h 의 32 비트 주소: rm=4 는 SIB(배율·인덱스·베이스), mod=0 의 rm=5 와 SIB
   베이스 5 는 disp32 만이다. 베이스가 ESP·EBP 면 기본 세그먼트가 SS 다(Intel
   80386 Programmer's Reference 17.2.1). 바이트 순서는 modrm, SIB, disp 다.
   SIB 의 인덱스가 없음(4)이면 설명서는 배율을 말하지 않지만 실칩은 배율을
   베이스에 곱한다. 데이터 README 의 Undefined Behavior 절이 같은 것을 적었고,
   `67 00 9C E0 C5 E7 FF FF`(add [eax*8-183Bh],bl, EAX=1296h, DS=F269h)는
   FA305h 를 읽고 쓴다. 67h 파일 첫 실행에서 틀린 5,205 건 가운데 5,166 건이
   이 규칙으로 풀렸다. *)
let offset32_mask = 0xffffffff
let sib_byte = 4
let no_base = 5
let no_index = 4

let mem32 t m rm addr =
  let base, stack_based =
    if rm = sib_byte then begin
      let sib = fetch8 t in
      let index = (sib lsr 3) land 7 and b = sib land 7 and scale = sib lsr 6 in
      if index = no_index then begin
        if b = no_base && m = 0 then (fetch32 t, false)
        else (get32 t b lsl scale, b = 4 || b = 5)
      end else begin
        let scaled = get32 t index lsl scale in
        if b = no_base && m = 0 then (fetch32 t + scaled, false)
        else (get32 t b + scaled, b = 4 || b = 5)
      end
    end
    else if rm = no_base && m = 0 then (fetch32 t, false)
    else (get32 t rm, rm = 5)
  in
  let disp = match m with 0 -> 0 | 1 -> sx8 (fetch8 t) | _ -> fetch32 t in
  let off = (base + disp) land offset32_mask in
  let segsel = segment_for addr ~default:(if stack_based then sreg_ss else sreg_ds) in
  (Mem32 (segsel, off), Some off)

(* modrm 디코드. 반환: (mod, reg필드, 피연산자, 유효주소 오프셋).
   오프셋은 메모리 피연산자일 때만 Some — LEA 가 세그먼트를 더하기 전
   오프셋을 필요로 한다. 세그먼트 접두가 있으면 [~addr] 에 실린 것, 없으면
   기본 규칙(BP 기반 = SS, 나머지 = DS)이다. *)
let decode_modrm t ~addr =
  let byte = fetch8 t in
  let m = byte lsr 6 in
  let regf = (byte lsr 3) land 7 in
  let rm = byte land 7 in
  let operand, ea =
    if m = 3 then (Reg rm, None)
    else
      match addr with
      | A32 | A32_seg _ -> mem32 t m rm addr
      | A16 | A16_seg _ ->
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
        segment_for addr
          ~default:(if rm = 2 || rm = 3 || (rm = 6 && m <> 0) then sreg_ss else sreg_ds)
      in
      let off = (base + disp) land 0xffff in
      (Mem (segsel, off), Some off)
  in
  (m, regf, operand, ea)

(* POP r/m 의 실효 주소는 SP 를 올린 뒤의 ESP 로 센다 — [esp+...] 는 pop 한
   뒤의 값이다(Intel SDM POP 설명과 같다. SingleStepTests/80386 678F·67668F 의
   ESP 베이스 45 건, 이 규칙 없이 35 건이 틀린다). 16 비트 주소에는 SP 가
   들어가지 않는다. 명령 바이트를 pop 읽기보다 먼저 가져오려고 SP 를 [bytes]
   만큼 올려 주소만 세고 되돌린다. 가져오다 결함이 나면 호출자의
   restore_sp_on_fault 가 SP 를 되돌린다. SP 가 FFFFh 를 넘어 감기는 경우는
   데이터에 없어 16 비트로 감는다(SDM 은 프로세서마다 다르다고 적는다). *)
let decode_pop_target t ~addr ~bytes =
  match addr with
  | A16 | A16_seg _ -> decode_modrm t ~addr
  | A32 | A32_seg _ ->
    let sp = t.regs.(4) in
    t.regs.(4) <- (sp + bytes) land 0xffff;
    let decoded = decode_modrm t ~addr in
    t.regs.(4) <- sp;
    decoded

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

(* [Mem32] 의 [size] 바이트가 실모드 한계 FFFFh 안에 드는지. 넘으면 바이트
   하나 오가기 전에 결함이다. 16 비트 오프셋과 달리 감기지 않는다. *)
let segment_limit = 0xffff

let within t sreg off size =
  if off + size - 1 > segment_limit then fault (overrun_vector t sreg)

(* 16 비트 far 포인터 m16:16 이 차지하는 바이트(LES·LDS·LSS·LFS·LGS, FF /3·/5). *)
let far_pointer16_bytes = 4

(* 32 비트 far 포인터 m16:32 가 차지하는 바이트(66h 의 LES·LDS·LSS·LFS·LGS,
   FF /3·/5). *)
let far_pointer32_bytes = 6

(* 한 피연산자를 여러 번 나눠 읽는 명령(far 포인터, BOUND, ESC)이 먼저 부른다:
   [Mem32] 면 [size] 바이트 전체가 한계 안인지 따진다. [Mem] 은 지금처럼 접근
   하나하나가 따진다. *)
let check_span t opd size =
  match opd with
  | Mem32 (s, o) -> within t s o size
  | Mem _ | Reg _ -> ()

(* A0-A3 의 직접 주소: 67h 면 imm32 오프셋이다. *)
let moffs t addr =
  let sreg = segment_for addr ~default:sreg_ds in
  match addr with
  | A16 | A16_seg _ -> Mem (sreg, fetch16 t)
  | A32 | A32_seg _ -> Mem32 (sreg, fetch32 t)

let op_read32 t = function
  | Reg n -> get32 t n
  | Mem (s, o) -> read32 t s o
  | Mem32 (s, o) -> within t s o 4; read32 t s o

let op_write32 t opd v =
  match opd with
  | Reg n -> put32 t n v
  | Mem (s, o) -> write32 t s o v
  | Mem32 (s, o) -> within t s o 4; write32 t s o v

let op_read t width = function
  | Reg n -> if width = 8 then reg8 t n else reg16 t n
  | Mem (b, o) -> if width = 8 then t.read (at t b o) else read16 t b o
  | Mem32 (b, o) ->
    within t b o (width / 8);
    if width = 8 then t.read (at t b o) else read16 t b o

let op_write t width opd v =
  match opd with
  | Reg n -> if width = 8 then set_reg8 t n v else set_reg16 t n v
  | Mem (b, o) -> if width = 8 then t.write (at t b o) v else write16 t b o v
  | Mem32 (b, o) ->
    within t b o (width / 8);
    if width = 8 then t.write (at t b o) v else write16 t b o v

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

(* 두 바이트 명령의 머리. *)
let two_byte_escape = 0x0f

(* 0F 가운데 LOCK 을 받는 형태: 메모리 피연산자의 BTS·BTR·BTC r/m16,r16(0F AB·
   B3·BB)과 BTS·BTR·BTC r/m16,imm8(0F BA /5-7)이다. 80386.csv 의 lck 열은 0F B3
   을 0 으로 적었지만 칩은 받는다(0FB3·670FB3 의 LOCK BTR 99 건이 결함 없이 돈다). *)
let lockable_0f op2 modrm =
  modrm lsr 6 <> 3
  && (match op2 with
      | 0xab | 0xb3 | 0xbb -> true
      | 0xba -> (modrm lsr 3) land 7 >= 5
      | _ -> false)

(* 386 은 LOCK 을 받지 않는 명령에 붙은 LOCK 을 INT 6 결함으로 거절한다.
   받는 명령도 고칠 피연산자가 레지스터면(modrm mod=3) INT 6 이다
   (SingleStepTests/80386 00 의 lock add dh,bh 같은 테스트). 0F 는 둘째
   바이트와 modrm 이 정한다([lockable_0f]). 8086·186·286 은 LOCK 을
   버스 신호로만 쓰고 명령은 그대로 돈다. modrm 은 들여다보기만 하고 IP 를
   되돌린다. LOCK 접두가 붙은 명령에서만 부른다. *)
let check_lock t ctx =
  match t.model with
  | I8086 | I80186 | I80286 -> ()
  | I80386 ->
    if ctx.opcode = two_byte_escape then begin
      let after_opcode = t.ip in
      let op2 = fetch8 t in
      let modrm = fetch8 t in
      t.ip <- after_opcode;
      if not (lockable_0f op2 modrm) then fault vector_invalid_opcode
    end else
      match lock_regs ctx.opcode with
      | None -> fault vector_invalid_opcode
      | Some regs ->
        let after_opcode = t.ip in
        let modrm = fetch8 t in
        t.ip <- after_opcode;
        let register_operand = modrm lsr 6 = 3 in
        if register_operand || regs land (1 lsl ((modrm lsr 3) land 7)) = 0 then
          fault vector_invalid_opcode

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

(* 0Fh 는 두 바이트 명령의 머리라 66h 가 폭을 바꾸는지는 둘째 바이트가 정한다.
   66h 를 본 표시를 실어 [Cpu86_exec32] 로 보낸다. *)
let operand_sized =
  let table = Array.make byte_values false in
  List.iter (fun op -> table.(op) <- true) (two_byte_escape :: operand_sized_opcodes);
  table

(* 접두를 읽어 문맥을 만든다. [addr]·[rep] 은 지금까지 본 접두다 — 같은
   종류의 접두가 여럿이면 마지막 것이 남는다. LOCK 이 있을 때만
   [check_lock] 을 부른다 — 명령마다 부르면 삼국지3 3천만 스텝의 기계어
   명령이 2.2% 는다. 66h 를 보면 [prefixes_after_66] 로 넘어가, 66h 가 없는
   명령은 그 표시를 들고 다니지 않는다. *)
(* 세그먼트 접두는 주소 크기를 그대로 두고, 67h 는 세그먼트를 그대로 둔다 —
   둘은 어느 순서로 와도 된다. *)
let with_segment addr s =
  match addr with
  | A16 | A16_seg _ -> A16_seg s
  | A32 | A32_seg _ -> A32_seg s

let with_address32 = function
  | A16 | A32 -> A32
  | A16_seg s | A32_seg s -> A32_seg s

let rec prefixes_after t ~base_ip addr rep lock =
  let b = fetch8 t in
  match t.prefixes.(b) with
  | Not_prefix ->
    let ctx = { base_ip; opcode = b; addr; rep } in
    if lock then check_lock t ctx;
    ctx
  | Segment s -> prefixes_after t ~base_ip (with_segment addr s) rep lock
  | Lock -> prefixes_after t ~base_ip addr rep true
  | Rep z -> prefixes_after t ~base_ip addr (Some z) lock
  | Operand_size -> prefixes_after_66 t ~base_ip addr rep lock
  | Address_size -> prefixes_after t ~base_ip (with_address32 addr) rep lock

and prefixes_after_66 t ~base_ip addr rep lock =
  let b = fetch8 t in
  match t.prefixes.(b) with
  | Not_prefix ->
    let ctx = { base_ip; opcode = b; addr; rep } in
    if lock then check_lock t ctx;
    if operand_sized.(b) then { ctx with opcode = b lor operand32_form } else ctx
  | Segment s -> prefixes_after_66 t ~base_ip (with_segment addr s) rep lock
  | Lock -> prefixes_after_66 t ~base_ip addr rep true
  | Rep z -> prefixes_after_66 t ~base_ip addr (Some z) lock
  | Operand_size -> prefixes_after_66 t ~base_ip addr rep lock
  | Address_size -> prefixes_after_66 t ~base_ip (with_address32 addr) rep lock

let decode_prefixes t ~base_ip = prefixes_after t ~base_ip A16 None false
