(* step: 명령 하나를 디코드해 돌린다. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode
open Cpu86_alu
open Cpu86_bcd
open Cpu86_string

(* 16 비트 far 포인터 m16:16 과 BOUND 의 두 경계가 차지하는 바이트. *)
let far_pointer16_bytes = 4
let bounds16_bytes = 4

let idle t =
  t.cycles <- t.cycles + 2;
  2

let step t =
  if t.halted then idle t
  else begin
    let base_ip = t.ip in
    (* 트랩 플래그는 명령 진입 시점 값으로 판정한다 — 그 명령이 TF 를
       켰다면(popf/iret) 다음 명령부터 걸리는 게 실기 동작이다. *)
    let entry_tf = t.tf in
    (match t.model with
     | I80286 -> t.fetch_end <- (base_ip + t.fetch_limit) land 0xffff
     | I80386 -> t.fetch_end <- min (base_ip + t.fetch_limit) segment_end
     | I8086 | I80186 -> ());
    let used_cycles =
      try begin
      let ctx = decode_prefixes t ~base_ip in
      let opcode = ctx.opcode in
      match opcode with
      (* --- BCD 조정: Cpu86_bcd --- *)
      | 0x27 -> daa t
      | 0x2f -> das t
      | 0x37 -> aaa t
      | 0x3f -> aas t
      (* --- 8086 에서 0x60-0x6F 는 새 명령이 아니라 0x70-0x7F(jcc) 의
             거울이다. 186 이 그 자리에 pusha/popa/push imm/imul/bound/
             ins/outs 를 놓았다. 어느 쪽인지는 모델이 정한다. --- *)
      | b when t.model = I8086 && b >= 0x60 && b <= 0x6f ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        if condition t (opcode land 0xf) then begin
          t.ip <- (t.ip + d) land 0xffff;
          8
        end else 4
      (* 8086 에서 0xC0/0xC1 은 ret, 0xC8/0xC9 는 retf 의 거울이다. *)
      | 0xc0 when t.model = I8086 ->
        let n = fetch16 t in
        t.ip <- pop16 t;
        t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
        10
      | 0xc1 when t.model = I8086 -> t.ip <- pop16 t; 8
      | 0xc8 when t.model = I8086 ->
        let n = fetch16 t in
        t.ip <- pop16 t;
        set_seg t 1 (pop16 t);
        t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
        18
      | 0xc9 when t.model = I8086 ->
        t.ip <- pop16 t;
        set_seg t 1 (pop16 t);
        17
      (* --- ALU 8종: opcode lsr 3 = 연산, 하위 3비트 = 형태. --- *)
      | 0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 -> alu_group t ctx 0
      | 0x08 | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d -> alu_group t ctx 1
      | 0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 -> alu_group t ctx 2
      | 0x18 | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d -> alu_group t ctx 3
      | 0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 -> alu_group t ctx 4
      | 0x28 | 0x29 | 0x2a | 0x2b | 0x2c | 0x2d -> alu_group t ctx 5
      | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 -> alu_group t ctx 6
      | 0x38 | 0x39 | 0x3a | 0x3b | 0x3c | 0x3d -> alu_group t ctx 7
      (* --- mov --- *)
      | 0x88 | 0x89 ->
        let width = if opcode = 0x88 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let v = if width = 8 then reg8 t regf else reg16 t regf in
        op_write t width rm v;
        2
      | 0x8a | 0x8b ->
        let width = if opcode = 0x8a then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let v = op_read t width rm in
        op_write t width (Reg regf) v;
        2
      | b when b >= 0xb0 && b <= 0xb7 ->
        let v = fetch8 t in
        set_reg8 t (opcode land 7) v;
        4
      | b when b >= 0xb8 && b <= 0xbf ->
        let v = fetch16 t in
        set_reg16 t (opcode land 7) v;
        4
      (* --- inc/dec 레지스터 (CF 유지) --- *)
      | b when b >= 0x40 && b <= 0x47 ->
        let n = opcode land 7 in
        let a = reg16 t n in
        let saved_cf = t.cf in
        let r = alu_result t 0 a 1 16 in
        t.cf <- saved_cf;
        set_reg16 t n r;
        3
      | b when b >= 0x48 && b <= 0x4f ->
        let n = opcode land 7 in
        let a = reg16 t n in
        let saved_cf = t.cf in
        let r = alu_result t 5 a 1 16 in
        t.cf <- saved_cf;
        set_reg16 t n r;
        3
      (* --- push/pop. PUSH SP(0x54) 는 8086 이 감소된 SP 를 push 한다
         (286 부터 이전 값을 push — 실칩 SingleStepTests 관측). --- *)
      | 0x54 ->
        (match t.model with
         | I80286 | I80386 -> push16 t t.regs.(4)
         | I8086 | I80186 ->
           t.regs.(4) <- (t.regs.(4) - 2) land 0xffff;
           let a = physical t ~seg:t.segs.(2) ~off:t.regs.(4) in
           t.write a (t.regs.(4) land 0xff);
           t.write (a + 1) (t.regs.(4) lsr 8));
        10
      | b when b >= 0x50 && b <= 0x57 -> push16 t (reg16 t (opcode land 7)); 10
      | b when b >= 0x58 && b <= 0x5f -> set_reg16 t (opcode land 7) (pop16 t); 8
      (* --- jcc rel8 --- *)
      | b when b >= 0x70 && b <= 0x7f ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        if condition t (opcode land 0xf) then begin
          t.ip <- (t.ip + d) land 0xffff;
          8
        end else 4
      (* --- jmp --- *)
      | 0xeb ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        t.ip <- (t.ip + d) land 0xffff;
        7
      | 0xe8 ->
        let d = fetch16 t in
        push16 t t.ip;
        t.ip <- (t.ip + d) land 0xffff;
        13
      | 0xe9 ->
        let d = fetch16 t in
        t.ip <- (t.ip + d) land 0xffff;
        7
      (* --- 186 확장. 8086 실칩은 undoc 미러로 응답하지만 ZZT(1991,
          Borland 계열 산출)가 push imm 등을 바로 쓴다(실측 @1010:06C2).
          8086 하드웨어 스위트로는 검증 불가 — 문서 계약 + 게임 실행으로
          증명한다. --- *)
      | 0x68 -> push16 t (fetch16 t); 10
      | 0x6a ->
        let d = fetch8 t in
        push16 t (if d >= 0x80 then d lor 0xff00 else d);
        9
      | 0x60 (* pusha *) ->
        atomic t (fun () ->
            let sp0 = t.regs.(4) in
            push16 t (reg16 t 0); push16 t (reg16 t 1);
            push16 t (reg16 t 2); push16 t (reg16 t 3);
            push16 t sp0; push16 t (reg16 t 5);
            push16 t (reg16 t 6); push16 t (reg16 t 7);
            36)
      | 0x61 (* popa. pusha 가 AX CX DX BX SP BP SI DI 순으로 밀었으니
                 되꺼내는 순서는 그 역순이고, SP 자리는 버린다(186 계약 —
                 SP 는 8 워드 pop 으로 저절로 제자리). *) ->
        let popa () =
          set_reg16 t 7 (pop16 t);   (* DI *)
          set_reg16 t 6 (pop16 t);   (* SI *)
          set_reg16 t 5 (pop16 t);   (* BP *)
          ignore (pop16 t);          (* pusha 가 민 SP — 버린다 *)
          set_reg16 t 3 (pop16 t);   (* BX *)
          set_reg16 t 2 (pop16 t);   (* DX *)
          set_reg16 t 1 (pop16 t);   (* CX *)
          set_reg16 t 0 (pop16 t);   (* AX *)
          25
        in
        (match t.model with
         | I80386 -> restore_sp_on_fault t popa
         | I8086 | I80186 | I80286 -> atomic t popa)
      | 0x69 | 0x6b (* imul r16, rm16, imm *) ->
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let a = op_read t 16 rm in
        let imm =
          if opcode = 0x69 then fetch16 t
          else begin
            let d = fetch8 t in
            if d >= 0x80 then d lor 0xff00 else d
          end
        in
        let sx v = if v >= 0x8000 then v - 0x10000 else v in
        let p = sx a * sx imm in
        set_reg16 t regf (p land 0xffff);
        t.cf <- p > 0x7fff || p < -0x8000;
        t.of_ <- p > 0x7fff || p < -0x8000;
        21
      | 0xc0 | 0xc1 (* shift rm, imm8 (186) *) ->
        let width = if opcode = 0xc0 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        shift_group t regf width rm (fetch8 t);
        6
      | 0xc8 (* enter: Intel 186 계약. nesting>0 은 디스플레이(프레임
               포인터 체인) 복사 — TP 런타임이 중첩 프레임으로 사용(실측). *) ->
        let size = fetch16 t in
        let nesting = fetch8 t land 0x1f in
        atomic t (fun () ->
            push16 t (reg16 t 5);
            let frame = t.regs.(4) in
            if nesting > 0 then begin
              for _ = 1 to nesting - 1 do
                t.regs.(5) <- (t.regs.(5) - 2) land 0xffff;
                push16 t (read16 t sreg_ss t.regs.(5))
              done;
              push16 t frame
            end;
            (* BP 는 프레임 바닥, SP 는 디스플레이를 민 뒤의 SP 에서 size 만큼
               아래 — size 가 지역변수 자리다(Intel: SP <- SP - Size). 프레임에서
               빼면 nesting>0 일 때 지역변수가 방금 민 디스플레이를 덮는다. *)
            set_reg16 t 5 frame;
            set_reg16 t 4 ((t.regs.(4) - size) land 0xffff);
            19)
      | 0xc9 (* leave *) ->
        atomic t (fun () ->
            set_reg16 t 4 (reg16 t 5);
            set_reg16 t 5 (pop16 t);
            8)
      (* --- hlt --- *)
      | 0xf4 -> t.halted <- true; 2
      (* --- push/pop 세그먼트 (0x0F pop cs 는 8086 에만 유효) --- *)
      | 0x06 -> push16 t (seg t 0); 10
      | 0x0e -> push16 t (seg t 1); 10
      | 0x16 -> push16 t (seg t 2); 10
      | 0x1e -> push16 t (seg t 3); 10
      | 0x07 -> set_seg t 0 (pop16 t); 8
      (* 286: 실모드 게임이 쓰는 두 바이트 명령은 CPU 판별용 SMSW 뿐이다.
         보호 모드로 가는 나머지(LMSW·LGDT·CLTS …)는 아직 없다. 386 의
         0x0F 확장은 [Cpu86_exec0f] 가 RFC 0001 3 단계로 하나씩 더한다. *)
      | 0x0f ->
        (match t.model with
         | I80286 ->
           let op2 = fetch8 t in
           if op2 = 0x01 then begin
             let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
             if regf = 4 then begin op_write t 16 rm msw_real_mode_286; 3 end
             else unsupported t ctx (Printf.sprintf "0f 01 /%d" regf)
           end
           else unsupported t ctx (Printf.sprintf "0f %02x" op2)
         | I80386 -> Cpu86_exec0f.dispatch t ctx
         | I8086 | I80186 -> set_seg t 1 (pop16 t); 8)
      | 0x17 -> set_seg t 2 (pop16 t); 8
      | 0x1f -> set_seg t 3 (pop16 t); 8
      (* --- 그룹1: ALU rm,imm (0x83 은 imm8 부호확장) --- *)
      | 0x80 | 0x81 | 0x82 | 0x83 ->
        let width = if opcode land 1 = 1 then 16 else 8 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let imm =
          if width = 8 then fetch8 t
          else if opcode = 0x83 then begin
            let d = fetch8 t in
            if d >= 0x80 then d lor 0xff00 else d
          end
          else fetch16 t
        in
        let a = op_read t width rm in
        let r = alu_result t regf a imm width in
        if regf <> 7 then op_write t width rm r;
        4
      (* --- test / xchg --- *)
      | 0x84 | 0x85 ->
        let width = if opcode = 0x84 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let a = op_read t width rm in
        let b = if width = 8 then reg8 t regf else reg16 t regf in
        ignore (alu_result t 4 a b width);
        3
      | 0xa8 ->
        let a = reg8 t 0 in
        ignore (alu_result t 4 a (fetch8 t) 8);
        4
      | 0xa9 ->
        let a = reg16 t 0 in
        ignore (alu_result t 4 a (fetch16 t) 16);
        4
      | 0x86 | 0x87 ->
        let width = if opcode = 0x86 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let a = op_read t width rm in
        let b = if width = 8 then reg8 t regf else reg16 t regf in
        op_write t width (Reg regf) a;
        op_write t width rm b;
        4
      | b when b >= 0x90 && b <= 0x97 ->
        (* xchg ax,r — 0x90 은 xchg ax,ax = NOP *)
        let n = opcode land 7 in
        if n <> 0 then begin
          let a = reg16 t 0 in
          set_reg16 t 0 (reg16 t n);
          set_reg16 t n a
        end;
        3
      (* --- lea / mov sreg / pop rm --- *)
      | 0x8d ->
        let _, regf, _, ea = decode_modrm t ~addr:ctx.addr in
        (match ea with
         | Some off -> set_reg16 t regf off
         | None -> (undefined_on_286 t true; unsupported t ctx "lea on register"));
        2
      (* mov 의 sreg 필드는 2비트다 — modrm 의 reg 필드 상위 비트는 무시
         된다(실칩 관측). land 3 을 빼면 regf>=4 인 인코딩에서 세그먼트
         배열 밖을 짚어 에뮬레이터가 죽는다. *)
      | 0x8c ->
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let sreg = sreg_field t regf ~load:false in
        op_write t 16 rm (seg t sreg);
        2
      | 0x8e ->
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        (* reg 필드를 먼저 따진다 — INT 6 이 피연산자 읽기의 INT 13 보다 앞선다. *)
        let sreg = sreg_field t regf ~load:true in
        set_seg t sreg (op_read t 16 rm);
        2
      | 0x8f ->
        (* 286 은 SP 를 올려 둔 뒤에 쓴다 — 쓰기가 세그먼트 끝을 넘어도 SP 는
           올라간 채로 INT 13 을 받는다(SingleStepTests/80286 8F). *)
        let pop_rm () =
          let _, regf, rm, _ = decode_pop_target t ~addr:ctx.addr ~bytes:2 in
          undefined_on_286 t (regf <> 0);
          op_write t 16 rm (pop16 t)
        in
        (match t.model with
         | I80386 -> restore_sp_on_fault t pop_rm
         | I8086 | I80186 | I80286 -> pop_rm ());
        8
      (* --- cbw / cwd --- *)
      | 0x98 ->
        let al = reg8 t 0 in
        set_reg16 t 0 (if al >= 0x80 then al lor 0xff00 else al);
        2
      | 0x99 ->
        let ax = reg16 t 0 in
        set_reg16 t 2 (if ax >= 0x8000 then 0xffff else 0);
        2
      (* --- call far imm / pushf / popf / sahf / lahf --- *)
      | 0x9a ->
        let off = fetch16 t in
        let s = fetch16 t in
        atomic t (fun () ->
            push16 t (seg t 1);
            push16 t t.ip;
            set_seg t 1 s;
            t.ip <- off;
            13)
      | 0x9c -> push16 t (flags t); 10
      | 0x9d -> set_flags t (pop16 t); 8
      | 0x9e ->
        let ah = reg8 t 4 in
        t.cf <- ah land 1 <> 0; t.pf <- ah land 4 <> 0;
        t.af <- ah land 0x10 <> 0; t.zf <- ah land 0x40 <> 0;
        t.sf <- ah land 0x80 <> 0;
        4
      | 0x9f ->
        let b bit = if bit then 1 else 0 in
        set_reg8 t 4
          (0x02 lor b t.cf lor (b t.pf lsl 2) lor (b t.af lsl 4)
             lor (b t.zf lsl 6) lor (b t.sf lsl 7));
        4
      (* --- LES/LDS r16, m32 (C4/C5) — 메모리의 far 포인터 로드. --- *)
      | 0xc4 | 0xc5 ->
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        check_span t rm far_pointer16_bytes;
        (match rm with
         | Mem (b, o) | Mem32 (b, o) ->
           let off = read16 t b o in
           let segv = read16 t b (o + 2) in
           set_reg16 t regf off;
           if opcode = 0xc4 then set_seg t 0 segv else set_seg t 3 segv
         | Reg _ -> (undefined_on_286 t true; unsupported t ctx "les/lds reg"));
        2
      (* --- accumulator 직접주소 mov (A0-A3) — TP 런타임 실측 요구. --- *)
      | 0xa0 | 0xa1 | 0xa2 | 0xa3 ->
        let opd = moffs t ctx.addr in
        let width = if opcode land 1 = 0 then 8 else 16 in
        if opcode < 0xa2 then op_write t width (Reg 0) (op_read t width opd)
        else op_write t width opd (op_read t width (Reg 0));
        8
      (* --- string ops (rep prefix 소비) --- *)
      | 0xa4 -> string_op t ctx 8 Movs; 9
      | 0xa5 -> string_op t ctx 16 Movs; 9
      | 0xa6 -> string_op t ctx 8 Cmps; 9
      | 0xa7 -> string_op t ctx 16 Cmps; 9
      | 0xaa -> string_op t ctx 8 Stos; 7
      | 0xab -> string_op t ctx 16 Stos; 7
      | 0xac -> string_op t ctx 8 Lods; 7
      | 0xad -> string_op t ctx 16 Lods; 7
      | 0xae -> string_op t ctx 8 Scas; 7
      | 0xaf -> string_op t ctx 16 Scas; 7
      (* --- ret / jmp far / mov rm,imm --- *)
      | 0xc2 ->
        let n = fetch16 t in
        t.ip <- pop16 t;
        t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
        10
      (* RETF — 서드파티 첫 요구(ZZT @1b21:002a). far call/인터럽트 스타일
         프레임에서 CS:IP 를 스택에서 되돌린다. *)
      | 0xcb ->
        atomic t (fun () ->
            t.ip <- pop16 t;
            set_seg t 1 (pop16 t);
            17)
      | 0xca ->
        let n = fetch16 t in
        atomic t (fun () ->
            t.ip <- pop16 t;
            set_seg t 1 (pop16 t);
            t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
            18)
      | 0xc3 -> t.ip <- pop16 t; 8
      | 0xc6 | 0xc7 ->
        let width = if opcode = 0xc6 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let v = if width = 8 then fetch8 t else fetch16 t in
        (* 286 은 즉치값까지 읽어 명령 길이를 정한 뒤에 INT 6 을 낸다 — 10
           바이트를 넘으면 INT 13 이 먼저다(SingleStepTests/80286 C7). *)
        undefined_on_286 t (regf <> 0);
        op_write t width rm v;
        4
      (* --- int / iret --- *)
      | 0xcc -> do_int t ctx 3; 25
      | 0xcd ->
        let n = fetch8 t in
        do_int t ctx n;
        25
      | 0xce -> if t.of_ then do_int t ctx 4 else (); 4
      | 0xcf ->
        atomic t (fun () ->
            t.ip <- pop16 t;
            set_seg t 1 (pop16 t);
            set_flags t (pop16 t);
            8)
      (* --- 그룹2: shift/rotate (count 1 또는 CL) --- *)
      | 0xd0 | 0xd1 | 0xd2 | 0xd3 ->
        let width = if opcode land 1 = 0 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        let count = if opcode < 0xd2 then 1 else reg8 t 1 (* CL *) in
        shift_group t regf width rm count;
        if opcode < 0xd2 then 4 else 8
      | 0xd4 -> aam t ctx
      | 0xd5 -> aad t
      (* --- xlat --- *)
      | 0xd7 (* xlat: AL <- [seg:BX+AL]. 오프셋은 세그먼트 안에서 16비트로
                 감긴다 — 마스크를 빼면 BX+AL 이 0xFFFF 를 넘을 때 옆
                 세그먼트를 읽는다. 세그먼트 override 도 먹는다. 67h 면
                 EBX+AL 이고 감기지 않는다. *) ->
        let sel = segment_for ctx.addr ~default:sreg_ds in
        let opd =
          if address32 ctx.addr then Mem32 (sel, (get32 t 3 + reg8 t 0) land offset32_mask)
          else Mem (sel, (reg16 t 3 + reg8 t 0) land 0xffff)
        in
        set_reg8 t 0 (op_read t 8 opd);
        7
      (* --- loopnz/loopz/loop/jcxz --- *)
      (* 카운터는 주소 크기를 따른다: 67h 면 ECX(JECXZ), 아니면 CX. *)
      | 0xe0 | 0xe1 | 0xe2 ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        let left =
          if address32 ctx.addr then begin
            let c = (get32 t 1 - 1) land offset32_mask in
            put32 t 1 c;
            c
          end else begin
            t.regs.(1) <- (t.regs.(1) - 1) land 0xffff;
            t.regs.(1)
          end
        in
        let take =
          match opcode with
          | 0xe0 -> left <> 0 && not t.zf
          | 0xe1 -> left <> 0 && t.zf
          | _ -> left <> 0
        in
        if take then t.ip <- (t.ip + d) land 0xffff;
        5
      | 0xe3 ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        let cx = if address32 ctx.addr then get32 t 1 else t.regs.(1) in
        if cx = 0 then t.ip <- (t.ip + d) land 0xffff;
        5
      (* --- in/out (워드는 port_in16/port_out16) --- *)
      | 0xe4 -> set_reg8 t 0 (t.port_in (fetch8 t)); 10
      | 0xe5 -> set_reg16 t 0 (port_in16 t (fetch8 t)); 10
      | 0xe6 -> t.port_out (fetch8 t) (reg8 t 0); 10
      | 0xe7 -> port_out16 t (fetch8 t) (reg16 t 0); 10
      | 0xec -> set_reg8 t 0 (t.port_in (reg16 t 2)); 8
      | 0xed -> set_reg16 t 0 (port_in16 t (reg16 t 2)); 8
      | 0xee -> t.port_out (reg16 t 2) (reg8 t 0); 8
      | 0xef -> port_out16 t (reg16 t 2) (reg16 t 0); 8
      (* --- jmp far imm --- *)
      | 0xea ->
        let off = fetch16 t in
        let s = fetch16 t in
        set_seg t 1 s;
        t.ip <- off;
        7
      (* --- flag ops --- *)
      | 0xf5 -> t.cf <- not t.cf; 2
      | 0xf8 -> t.cf <- false; 2
      | 0xf9 -> t.cf <- true; 2
      | 0xfa -> t.intf <- false; 2
      | 0xfb -> t.intf <- true; 2
      | 0xfc -> t.df <- false; 2
      | 0xfd -> t.df <- true; 2
      (* --- 그룹3: test/not/neg/mul/imul/div/idiv --- *)
      | 0xf6 | 0xf7 -> group3 t ctx
      (* --- 그룹 FE/FF: inc/dec rm, call/jmp rm, push rm --- *)
      | 0xfe | 0xff ->
        let width = if opcode = 0xfe then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        (match regf, opcode with
         | 0, _ (* inc rm — CF 유지 *) ->
           let saved_cf = t.cf in
           let r = alu_result t 0 (op_read t width rm) 1 width in
           t.cf <- saved_cf;
           op_write t width rm r
         | 1, _ (* dec rm *) ->
           let saved_cf = t.cf in
           let r = alu_result t 5 (op_read t width rm) 1 width in
           t.cf <- saved_cf;
           op_write t width rm r
         (* call near indirect: 피연산자를 먼저 읽고 그 다음에 민다.
            순서를 바꾸면 `call sp` 가 감소된 SP 로 뛴다(실칩과 다름). *)
         | 2, 0xff ->
           let target = op_read t 16 rm in
           push16 t t.ip;
           set_ip t target
         | 3, 0xff ->
           check_span t rm far_pointer16_bytes;
           (match rm with
            | Mem (b, o) | Mem32 (b, o) ->
              let off = read16 t b o in
              let sg = read16 t b (o + 2) in
              ignore
                (atomic t (fun () ->
                     push16 t (seg t 1);
                     push16 t t.ip;
                     set_seg t 1 sg;
                     t.ip <- off;
                     0))
            | Reg _ -> (undefined_on_286 t true; unsupported t ctx "call far reg"))
         | 4, 0xff -> set_ip t (op_read t 16 rm)
         | 5, 0xff ->
           check_span t rm far_pointer16_bytes;
           (match rm with
            | Mem (b, o) | Mem32 (b, o) ->
              let off = read16 t b o in
              set_seg t 1 (read16 t b (o + 2));
              t.ip <- off
            | Reg _ -> (undefined_on_286 t true; unsupported t ctx "jmp far reg"))
         (* /7 은 문서에 없지만 실칩은 /6 과 같은 push 로 응답한다.
            피연산자가 SP 면 8086 은 이미 감소된 SP 를 민다 — 0x54 의
            PUSH SP 와 같은 규칙이다. *)
         | (6 | 7), 0xff ->
           (match rm, t.model with
            | Reg 4, (I8086 | I80186) ->
              t.regs.(4) <- (t.regs.(4) - 2) land 0xffff;
              write16 t sreg_ss t.regs.(4) t.regs.(4)
            | Reg _, (I8086 | I80186 | I80286 | I80386)
            | Mem _, (I8086 | I80186 | I80286 | I80386)
            | Mem32 _, (I8086 | I80186 | I80286 | I80386) -> push16 t (op_read t 16 rm))
         | _ -> unsupported t ctx "group FE/FF");
        15
      (* --- 186 string I/O. rep 접두어가 그대로 먹는다. --- *)
      | 0x6c -> string_op t ctx 8 Ins; 14
      | 0x6d -> string_op t ctx 16 Ins; 14
      | 0x6e -> string_op t ctx 8 Outs; 14
      | 0x6f -> string_op t ctx 16 Outs; 14
      (* --- bound (186): 배열 인덱스가 [하한,상한] 밖이면 INT 5.
             Turbo Pascal 의 {$R+} 범위 검사가 이 명령을 낸다. --- *)
      | 0x62 ->
        let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
        check_span t rm bounds16_bytes;
        (match rm with
         | Mem (b, o) | Mem32 (b, o) ->
           let sx v = if v >= 0x8000 then v - 0x10000 else v in
           let idx = sx (reg16 t regf) in
           (* 386 은 두 경계를 다 읽고 견준다 — 상한이 세그먼트 끝을 넘으면
              하한 밖이어도 INT 13 이다(SingleStepTests/80386 62). 286 은
              하한 밖이면 상한을 읽지 않는다. *)
           let out_of_bounds =
             match t.model with
             | I80386 ->
               let lower = sx (read16 t b o) in
               let upper = sx (read16 t b (o + 2)) in
               idx < lower || idx > upper
             | I8086 | I80186 | I80286 ->
               idx < sx (read16 t b o) || idx > sx (read16 t b (o + 2))
           in
           if out_of_bounds then
             (* 186 은 다음 명령으로 돌아가는 트랩으로 둔다. 286 은 BOUND
                자신을 가리키는 결함이다(SingleStepTests/80286 62). *)
             (match t.model with
              | I80286 | I80386 -> fault vector_bound
              | I8086 | I80186 -> do_int t ctx vector_bound)
         | Reg _ -> (undefined_on_286 t true; unsupported t ctx "bound on register"));
        13
      (* --- wait: 코프로세서 대기. 8087 이 없으니 즉시 통과한다. --- *)
      | 0x9b -> 4
      (* --- salc (문서 외, 8086 부터 모든 실칩이 구현): AL = CF ? FF : 00 --- *)
      | 0xd6 -> set_reg8 t 0 (if t.cf then 0xff else 0x00); 2
      (* --- esc: 8087 코프로세서 명령. 이 기계는 8087 이 없다 —
             INT 11h 장비 워드의 코프로세서 비트도 0 이라 게스트는
             소프트웨어 부동소수 경로를 고른다. 코프로세서 없는 실기
             8086 은 피연산자만 읽고 아무 일도 하지 않는다. --- *)
      | b when b >= 0xd8 && b <= 0xdf ->
        let _, _, rm, _ = decode_modrm t ~addr:ctx.addr in
        (* 286 은 워드로 읽는다 — 오프셋 FFFF 면 INT 13(SingleStepTests/80286
           D8). *)
        (match rm with
         | Mem _ | Mem32 _ ->
           (match t.model with
            | I80286 | I80386 -> ignore (op_read t 16 rm)
            | I8086 | I80186 -> ignore (op_read t 8 rm))
         | Reg _ -> ());
        2
      (* --- 386 의 66h 형태([operand32_form])는 0xFF 를 넘어 여기에만 닿는다.
             나머지는 예외로 알린다 — 조용한 오동작 대신 하네스가 다음에
             구현할 명령을 읽는다. --- *)
      | _ ->
        if opcode land operand32_form <> 0 then Cpu86_exec32.dispatch t ctx
        else unsupported t ctx (Printf.sprintf "opcode %02x" opcode)
      end
      with Fault vector ->
        t.ip <- base_ip;
        (match t.int_hook with
         | Some f -> f vector
         | None ->
           raise (Unsupported (Printf.sprintf "fault %d (no hook) @%04x:%04x"
                                 vector (seg t 1) base_ip)));
        fault_cycles
    in
    t.cycles <- t.cycles + used_cycles;
    (* 단일 스텝 트랩: TF 가 켜진 채 명령이 끝나면 INT 1. 훅이 IVT 로
       전달하며 TF 를 지우므로 핸들러 안에서 재귀하지 않는다. *)
    if entry_tf && t.tf then
      (match t.int_hook with Some f -> f 1 | None -> ());
    used_cycles
  end
