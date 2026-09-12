(* 8086 코어. 명령 집합 전체 중 M0 범위만: 상태·modrm·ALU 8종·mov·
   inc/dec·push/pop·jcc/jmp·hlt. 미구현 opcode 는 Unsupported 로 죽는다 —
   조용한 오동작(0xFF 반환 등)이 아니라 예외가 하네스에 다음에 구현할
   명령을 알려준다.

   레지스터 번호는 인코딩 그대로 (mli 문서 참조). 세그먼트 기본값:
   BP 기반 유효주소는 SS, 나머지는 DS (8086 하드웨어 배선). *)

exception Unsupported of string

let f_carry = 0x001
let f_parity = 0x004
let f_aux = 0x010
let f_zero = 0x040
let f_sign = 0x080
let f_trap = 0x100
let f_interrupt = 0x200
let f_direction = 0x400
let f_overflow = 0x800

type t = {
  mutable regs : int array;  (** AX CX DX BX SP BP SI DI *)
  mutable segs : int array;  (** ES CS SS DS *)
  mutable ip : int;
  mutable cf : bool;
  mutable pf : bool;
  mutable af : bool;
  mutable zf : bool;
  mutable sf : bool;
  mutable tf : bool;
  mutable intf : bool;
  mutable df : bool;
  mutable of_ : bool;
  mutable halted : bool;
  mutable cycles : int;
  mutable int_hook : (int -> unit) option;
  read : int -> int;
  write : int -> int -> unit;
  port_in : int -> int;
  port_out : int -> int -> unit;
}

let physical ~seg ~off = ((seg lsl 4) + off) land 0xfffff

let create ~read ~write ~port_in ~port_out =
  {
    regs = Array.make 8 0;
    segs = Array.make 4 0;
    ip = 0;
    cf = false; pf = false; af = false; zf = false; sf = false;
    tf = false; intf = true; df = false; of_ = false;
    halted = false;
    cycles = 0;
    int_hook = None;
    read; write; port_in; port_out;
  }

let halted t = t.halted
let cycles t = t.cycles
let set_int_hook t f = t.int_hook <- Some f

let reg16 t n = t.regs.(n)
let set_reg16 t n v = t.regs.(n) <- v land 0xffff

let reg8 t n =
  let full = t.regs.(n land 3) in
  if n < 4 then full land 0xff else full lsr 8

let set_reg8 t n v =
  let b = v land 0xff in
  let i = n land 3 in
  t.regs.(i) <-
    (if n < 4 then (t.regs.(i) land 0xff00) lor b
     else (t.regs.(i) land 0x00ff) lor (b lsl 8))

let seg t n = t.segs.(n)
let set_seg t n v = t.segs.(n) <- v land 0xffff
let dump_ip t = t.ip
let set_ip t v = t.ip <- v land 0xffff

let flags t =
  (* 8086 의 FLAGS 워드: bit1 은 항상 1, bits12-15 은 읽으면 항상 1
     (실칩 SingleStepTests 실측 — 계산 비트만 돌려주면 상위 니블이 0
     로 나와 전 add 그룹이 fail). *)
  let b cond bit = if cond then bit else 0 in
  0xF002
  lor b t.cf f_carry lor b t.pf f_parity lor b t.af f_aux
  lor b t.zf f_zero lor b t.sf f_sign lor b t.tf f_trap
  lor b t.intf f_interrupt lor b t.df f_direction
  lor b t.of_ f_overflow

let set_flags t v =
  let test bit = v land bit <> 0 in
  t.cf <- test f_carry; t.pf <- test f_parity; t.af <- test f_aux;
  t.zf <- test f_zero; t.sf <- test f_sign; t.tf <- test f_trap;
  t.intf <- test f_interrupt; t.df <- test f_direction; t.of_ <- test f_overflow

(* 하위 8비트의 1 개수가 짝수면 PF. 8086 은 PF 만 8비트 폭으로 본다. *)
let parity_even v =
  let n = ref (v land 0xff) in
  let count = ref 0 in
  while !n <> 0 do
    count := !count + (!n land 1);
    n := !n lsr 1
  done;
  !count land 1 = 0

(* 피연산자 폭 — 8비트/16비트 양쪽 산술의 플래그 계산에 쓴다. *)
let alu_result t op a b width =
  let mask = if width = 8 then 0xff else 0xffff in
  let sign_bit = if width = 8 then 0x80 else 0x8000 in
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

(* ---------- 명령 페치와 피연산자 ---------- *)

let fetch8 t =
  let phys = physical ~seg:t.segs.(1) ~off:t.ip in
  t.ip <- (t.ip + 1) land 0xffff;
  t.read phys

let fetch16 t =
  let lo = fetch8 t in
  let hi = fetch8 t in
  lo lor (hi lsl 8)

type operand =
  | Reg of int       (** 레지스터 번호 (폭은 문맥) *)
  | Mem of int       (** 물리 주소 (세그먼트 적용 완료) *)

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
        | None -> if rm = 2 || rm = 3 || (rm = 6 && m <> 0) then 2 else 3
      in
      let off = (base + disp) land 0xffff in
      (Mem (physical ~seg:t.segs.(segsel) ~off:off), Some off)
    end
  in
  (m, regf, operand, ea)

let op_read t width = function
  | Reg n -> if width = 8 then reg8 t n else reg16 t n
  | Mem a -> if width = 8 then t.read a else t.read a lor (t.read (a + 1) lsl 8)

let op_write t width opd v =
  match opd with
  | Reg n -> if width = 8 then set_reg8 t n v else set_reg16 t n v
  | Mem a ->
    if width = 8 then t.write a v
    else begin
      t.write a (v land 0xff);
      t.write ((a + 1) land 0xfffff) (v lsr 8)
    end

let push16 t v =
  t.regs.(4) <- (t.regs.(4) - 2) land 0xffff;
  let a = physical ~seg:t.segs.(2) ~off:t.regs.(4) in
  t.write a (v land 0xff);
  t.write (a + 1) (v lsr 8)

let pop16 t =
  let a = physical ~seg:t.segs.(2) ~off:t.regs.(4) in
  let v = t.read a lor (t.read (a + 1) lsl 8) in
  t.regs.(4) <- (t.regs.(4) + 2) land 0xffff;
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

let seg_override_of = function
  | 0x26 -> Some 0 | 0x2e -> Some 1 | 0x36 -> Some 2 | 0x3e -> Some 3
  | _ -> None

(* ---------- step: 한 명령 ---------- *)

let step t =
  if t.halted then begin
    t.cycles <- t.cycles + 2;
    2
  end
  else begin
    let base_ip = t.ip in
    let used_cycles =
      (* prefix: 세그먼트 override 와 rep. [rep] 은 Some true(rep)/false(repnz)
         — string 명령만 소비하고 다른 명령에 붙으면 무시한다(8086 문서 동작). *)
      let ovr = ref None in
      let rep = ref None in
      let rec prefixes () =
        match fetch8 t with
        | 0xf0 -> prefixes ()                       (* lock *)
        | 0xf2 -> rep := Some false; prefixes ()    (* repnz *)
        | 0xf3 -> rep := Some true; prefixes ()     (* rep/repz *)
        | b ->
          (match seg_override_of b with
           | Some s -> ovr := Some s; prefixes ()
           | None -> b)
      in
      let opcode = prefixes () in
      let bad what =
        raise (Unsupported (Printf.sprintf "%s @%04x:%04x (opcode %02x)"
                  what (seg t 1) base_ip opcode))
      in
      let alu_group op =
        (* 00-3F ALU 8종의 여섯 형태. x6/x7 (push/pop 세그먼트 등) 는
           ALU 가 아니라 이 디스패치에 오지 않는다. *)
        match opcode land 7 with
        | 0 | 1 ->
          let width = if opcode land 1 = 0 then 8 else 16 in
          let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
          let a = op_read t width rm in
          let b = if width = 8 then reg8 t regf else reg16 t regf in
          let r = alu_result t op a b width in
          if op <> 7 then op_write t width rm r;
          3
        | 2 | 3 ->
          let width = if opcode land 1 = 0 then 8 else 16 in
          let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
          let a = if width = 8 then reg8 t regf else reg16 t regf in
          let b = op_read t width rm in
          let r = alu_result t op a b width in
          if op <> 7 then op_write t width (Reg regf) r;
          3
        | 4 | 5 ->
          let width = if opcode land 1 = 0 then 8 else 16 in
          let a = if width = 8 then reg8 t 0 else reg16 t 0 in
          let imm = if width = 8 then fetch8 t else fetch16 t in
          let r = alu_result t op a imm width in
          if op <> 7 then op_write t width (Reg 0) r;
          4
        | _ -> bad "alu form"
      in
      (* 그룹2 shift/rotate: regf 0=rol 1=ror 2=rcl 3=rcr 4=shl 5=shr
         6=shl(undoc sal) 7=sar. count 0 은 플래그도 건드리지 않는다.
         ZF/SF/PF 는 논리 shift 만 갱신(rotate 는 갱신 안 함 — 8086 규칙),
         OF 는 count=1 일 때만 정의된다. *)
      let shift_group regf width rm_op count =
        if count > 0 then begin
          let mask = if width = 8 then 0xff else 0xffff in
          let msb = if width = 8 then 0x80 else 0x8000 in
          let bits = width in
          let v = ref (op_read t width rm_op) in
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
          op_write t width rm_op !v;
          if regf >= 4 then begin
            t.zf <- !v = 0;
            t.sf <- !v land msb <> 0;
            t.pf <- parity_even !v
          end;
          if count = 1 then begin
            match regf with
            | 0 | 2 | 4 | 6 -> t.of_ <- t.cf <> (!v land msb <> 0)
            | 7 -> t.of_ <- false
            | _ ->
              (* ror/rcr/shr: 결과 최상위 두 비트가 다르면 OF. *)
              let b1 = !v land msb <> 0 in
              let b2 = !v land (msb lsr 1) <> 0 in
              t.of_ <- b1 <> b2
          end
        end
      in
      let do_int n =
        match t.int_hook with
        | Some f -> f n
        | None -> bad (Printf.sprintf "int %02x (no hook)" n)
      in
      (* string ops: DS:[SI] → ES:[DI], DF 가 방향. rep 은 CX 카운터 —
         cmps/scas 는 repz/repnz 의 ZF 판정으로 조기 종료한다. *)
      let string_op word unit_kind =
        let seg_si () =
          let s = match !ovr with Some s -> s | None -> 3 in
          physical ~seg:t.segs.(s) ~off:t.regs.(6)
        in
        let seg_di () = physical ~seg:t.segs.(0) ~off:t.regs.(7) in
        let step_regs () =
          let d = (if word then 2 else 1) * (if t.df then -1 else 1) in
          t.regs.(6) <- (t.regs.(6) + d) land 0xffff;
          t.regs.(7) <- (t.regs.(7) + d) land 0xffff
        in
        let run_once () =
          let width = if word then 16 else 8 in
          match unit_kind with
          | 0 (* movs *) ->
            let a = seg_si () in
            let v = op_read t width (Mem a) in
            op_write t width (Mem (seg_di ())) v
          | 1 (* cmps *) ->
            let a = alu_result t 7 (op_read t width (Mem (seg_si ())))
                        (op_read t width (Mem (seg_di ()))) width in
            ignore a
          | 2 (* stos *) ->
            op_write t width (Mem (seg_di ())) (if word then reg16 t 0 else reg8 t 0)
          | 3 (* lodsb/lodsw: lod *) ->
            let v = op_read t width (Mem (seg_si ())) in
            (if word then set_reg16 t 0 v else set_reg8 t 0 v)
          | _ (* scas *) ->
            let al = if word then reg16 t 0 else reg8 t 0 in
            let d = op_read t width (Mem (seg_di ())) in
            ignore (alu_result t 7 al d width)
        in
        match !rep with
        | None -> run_once (); step_regs ()
        | Some want_zf ->
          if t.regs.(1) = 0 then ()  (* CX=0: 실행 없음 *)
          else begin
            let continue_ = ref true in
            while !continue_ && t.regs.(1) <> 0 do
              run_once ();
              step_regs ();
              t.regs.(1) <- t.regs.(1) - 1;
              (* cmps/scas 만 조기 종료 판정 *)
              if unit_kind = 1 || unit_kind = 4 then
                continue_ := (t.zf = want_zf)
            done
          end
      in
      match opcode with
      (* --- ALU 8종: opcode lsr 3 = 연산, 하위 3비트 = 형태. --- *)
      | 0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 -> alu_group 0
      | 0x08 | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d -> alu_group 1
      | 0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 -> alu_group 2
      | 0x18 | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d -> alu_group 3
      | 0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 -> alu_group 4
      | 0x28 | 0x29 | 0x2a | 0x2b | 0x2c | 0x2d -> alu_group 5
      | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 -> alu_group 6
      | 0x38 | 0x39 | 0x3a | 0x3b | 0x3c | 0x3d -> alu_group 7
      (* --- mov --- *)
      | 0x88 | 0x89 ->
        let width = if opcode = 0x88 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
        let v = if width = 8 then reg8 t regf else reg16 t regf in
        op_write t width rm v;
        2
      | 0x8a | 0x8b ->
        let width = if opcode = 0x8a then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
        t.regs.(4) <- (t.regs.(4) - 2) land 0xffff;
        let a = physical ~seg:t.segs.(2) ~off:t.regs.(4) in
        t.write a (t.regs.(4) land 0xff);
        t.write (a + 1) (t.regs.(4) lsr 8);
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
        let sp0 = t.regs.(4) in
        push16 t (reg16 t 0); push16 t (reg16 t 1);
        push16 t (reg16 t 2); push16 t (reg16 t 3);
        push16 t sp0; push16 t (reg16 t 5);
        push16 t (reg16 t 6); push16 t (reg16 t 7);
        36
      | 0x61 (* popa — SP 는 버리고 원래 값 유지 *) ->
        ignore (pop16 t); ignore (pop16 t); ignore (pop16 t);
        ignore (pop16 t (* sp0 — 버림 *));
        let saved_sp = pop16 t in
        set_reg16 t 5 (pop16 t); set_reg16 t 6 (pop16 t);
        set_reg16 t 7 (pop16 t);
        set_reg16 t 0 (pop16 t);
        ignore saved_sp;
        t.regs.(4) <- t.regs.(4);  (* pop 순서: DI SI BP SP버림 BX DX CX AX *)
        25
      | 0x69 | 0x6b (* imul r16, rm16, imm *) ->
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
        shift_group regf width rm (fetch8 t);
        6
      | 0xc8 (* enter — M2c 시점에 프레임 포인터 체인이 필요하면 확장.
               ZZT 관측에서는 얕은 프레임(imm8=0)이 대부분. *) ->
        let _size = fetch16 t in
        let nesting = fetch8 t in
        push16 t (reg16 t 5);
        set_reg16 t 5 (reg16 t 4);
        if nesting > 0 then bad "enter nesting";
        19
      | 0xc9 (* leave *) ->
        set_reg16 t 4 (reg16 t 5);
        set_reg16 t 5 (pop16 t);
        8
      (* --- hlt --- *)
      | 0xf4 -> t.halted <- true; 2
      (* --- push/pop 세그먼트 (0x0F pop cs 는 8086 에만 유효) --- *)
      | 0x06 -> push16 t (seg t 0); 10
      | 0x0e -> push16 t (seg t 1); 10
      | 0x16 -> push16 t (seg t 2); 10
      | 0x1e -> push16 t (seg t 3); 10
      | 0x07 -> set_seg t 0 (pop16 t); 8
      | 0x0f -> set_seg t 1 (pop16 t); 8
      | 0x17 -> set_seg t 2 (pop16 t); 8
      | 0x1f -> set_seg t 3 (pop16 t); 8
      (* --- 그룹1: ALU rm,imm (0x83 은 imm8 부호확장) --- *)
      | 0x80 | 0x81 | 0x82 | 0x83 ->
        let width = if opcode land 1 = 1 then 16 else 8 in
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
        let _, regf, _, ea = decode_modrm t ~ovr:!ovr in
        (match ea with
         | Some off -> set_reg16 t regf off
         | None -> bad "lea on register");
        2
      | 0x8c ->
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
        op_write t 16 rm (seg t regf);
        2
      | 0x8e ->
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
        set_seg t regf (op_read t 16 rm);
        2
      | 0x8f ->
        let _, _, rm, _ = decode_modrm t ~ovr:!ovr in
        op_write t 16 rm (pop16 t);
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
        push16 t (seg t 1);
        push16 t t.ip;
        set_seg t 1 s;
        t.ip <- off;
        13
      | 0x9c -> push16 t ((flags t) lor 0xf002); 10
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
      (* --- string ops (rep prefix 소비) --- *)
      | 0xa4 -> string_op false 0; 9
      | 0xa5 -> string_op true 0; 9
      | 0xa6 -> string_op false 1; 9
      | 0xa7 -> string_op true 1; 9
      | 0xaa -> string_op false 2; 7
      | 0xab -> string_op true 2; 7
      | 0xac -> string_op false 3; 7
      | 0xad -> string_op true 3; 7
      | 0xae -> string_op false 4; 7
      | 0xaf -> string_op true 4; 7
      (* --- ret / jmp far / mov rm,imm --- *)
      | 0xc2 ->
        let n = fetch16 t in
        t.ip <- pop16 t;
        t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
        10
      (* RETF — 서드파티 첫 요구(ZZT @1b21:002a). far call/인터럽트 스타일
         프레임에서 CS:IP 를 스택에서 되돌린다. *)
      | 0xcb ->
        t.ip <- pop16 t;
        set_seg t 1 (pop16 t);
        17
      | 0xca ->
        let n = fetch16 t in
        t.ip <- pop16 t;
        set_seg t 1 (pop16 t);
        t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
        18
      | 0xc3 -> t.ip <- pop16 t; 8
      | 0xc6 | 0xc7 ->
        let width = if opcode = 0xc6 then 8 else 16 in
        let _, _, rm, _ = decode_modrm t ~ovr:!ovr in
        let v = if width = 8 then fetch8 t else fetch16 t in
        op_write t width rm v;
        4
      (* --- int / iret --- *)
      | 0xcc -> do_int 3; 25
      | 0xcd ->
        let n = fetch8 t in
        do_int n;
        25
      | 0xce -> if t.of_ then do_int 4 else (); 4
      | 0xcf ->
        t.ip <- pop16 t;
        set_seg t 1 (pop16 t);
        set_flags t (pop16 t);
        8
      (* --- 그룹2: shift/rotate (count 1 또는 CL) --- *)
      | 0xd0 | 0xd1 | 0xd2 | 0xd3 ->
        let width = if opcode land 1 = 0 then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
        let count = if opcode < 0xd2 then 1 else reg8 t 1 (* CL *) in
        shift_group regf width rm count;
        if opcode < 0xd2 then 4 else 8
      (* --- aam / aad (BCD 조정 — aam 은 십진 변환에 자주 쓰인다) --- *)
      | 0xd4 ->
        let base = fetch8 t in
        let al = reg8 t 0 in
        if base = 0 then do_int 0
        else begin
          set_reg8 t 4 (al / base);
          set_reg8 t 0 (al mod base);
          t.zf <- al mod base = 0 && al / base = 0;
          t.sf <- (al / base) land 0x80 <> 0;
          t.pf <- parity_even ((al / base) lor (al mod base))
        end;
        15
      | 0xd5 ->
        let ah = reg8 t 4 in
        let al = reg8 t 0 in
        let v = ah * 10 + al in
        set_reg16 t 0 v;
        t.sf <- v land 0x80 <> 0;
        t.zf <- v land 0xff = 0;
        t.pf <- parity_even v;
        10
      (* --- xlat --- *)
      | 0xd7 ->
        let a = physical ~seg:t.segs.(3) ~off:((reg16 t 3) + reg8 t 0) in
        set_reg8 t 0 (t.read a);
        7
      (* --- loopnz/loopz/loop/jcxz --- *)
      | 0xe0 | 0xe1 | 0xe2 ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        t.regs.(1) <- (t.regs.(1) - 1) land 0xffff;
        let take =
          match opcode with
          | 0xe0 -> t.regs.(1) <> 0 && not t.zf
          | 0xe1 -> t.regs.(1) <> 0 && t.zf
          | _ -> t.regs.(1) <> 0
        in
        if take then t.ip <- (t.ip + d) land 0xffff;
        5
      | 0xe3 ->
        let d = fetch8 t in
        let d = if d >= 0x80 then d - 0x100 else d in
        if t.regs.(1) = 0 then t.ip <- (t.ip + d) land 0xffff;
        5
      (* --- in/out --- *)
      | 0xe4 -> set_reg8 t 0 (t.port_in (fetch8 t)); 10
      | 0xe5 -> set_reg16 t 0 (t.port_in (fetch8 t)); 10
      | 0xe6 -> t.port_out (fetch8 t) (reg8 t 0); 10
      | 0xe7 -> t.port_out (fetch8 t) (reg16 t 0); 10
      | 0xec -> set_reg8 t 0 (t.port_in (reg16 t 2)); 8
      | 0xed -> set_reg16 t 0 (t.port_in (reg16 t 2)); 8
      | 0xee -> t.port_out (reg16 t 2) (reg8 t 0); 8
      | 0xef -> t.port_out (reg16 t 2) (reg16 t 0); 8
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
      | 0xf6 | 0xf7 ->
        let width = if opcode = 0xf6 then 8 else 16 in
        let mask = if width = 8 then 0xff else 0xffff in
        let msb = if width = 8 then 0x80 else 0x8000 in
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
           if divisor = 0 then do_int 0
           else begin
             let dividend =
               if width = 8 then reg16 t 0
               else ((reg16 t 2) lsl 16) lor (reg16 t 0)
             in
             if regf = 6 then begin
               let q = dividend / divisor and r = dividend mod divisor in
               if q > mask then do_int 0
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
               if q > hi || q < lo then do_int 0
               else begin
                 if width = 8 then begin
                   set_reg8 t 0 (q land 0xff); set_reg8 t 4 (r land 0xff)
                 end else begin
                   set_reg16 t 0 (q land 0xffff); set_reg16 t 2 (r land 0xffff)
                 end
               end
             end
           end
         | _ -> bad "group3");
        30
      (* --- 그룹 FE/FF: inc/dec rm, call/jmp rm, push rm --- *)
      | 0xfe | 0xff ->
        let width = if opcode = 0xfe then 8 else 16 in
        let _, regf, rm, _ = decode_modrm t ~ovr:!ovr in
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
         | 2, 0xff -> push16 t t.ip; set_ip t (op_read t 16 rm)
         | 3, 0xff ->
           (match rm with
            | Mem a ->
              push16 t (seg t 1);
              push16 t t.ip;
              set_seg t 1 (t.read (a + 2) lor (t.read (a + 3) lsl 8));
              t.ip <- t.read a lor (t.read (a + 1) lsl 8)
            | _ -> bad "call far reg")
         | 4, 0xff -> set_ip t (op_read t 16 rm)
         | 5, 0xff ->
           (match rm with
            | Mem a ->
              set_seg t 1 (t.read (a + 2) lor (t.read (a + 3) lsl 8));
              t.ip <- t.read a lor (t.read (a + 1) lsl 8)
            | _ -> bad "jmp far reg")
         | 6, 0xff -> push16 t (op_read t 16 rm)
         | _ -> bad "group FE/FF");
        15
      (* --- 나머지: BCD 조정(daa/das/aaa/aas), esc, wait 등. 예외로 알린다. --- *)
      | _ ->
        bad (Printf.sprintf "opcode %02x" opcode)
    in
    t.cycles <- t.cycles + used_cycles;
    used_cycles
  end
