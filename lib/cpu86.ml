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
    read; write; port_in; port_out;
  }

let halted t = t.halted
let cycles t = t.cycles

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
  let b cond bit = if cond then bit else 0 in
  b t.cf f_carry lor b t.pf f_parity lor b t.af f_aux
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
     (* add/sub 계열 OF: 같은 부호 피연산자의 결과 부호 반전. *)
     let ov =
       match op with
       | 0 | 2 ->
         let full = a + b + (if op = 2 && t.cf then 1 else 0) in
         ((a lxor (full land mask)) land (b lxor (full land mask)) land sign_bit) <> 0
       | _ ->
         let full = a - b - (if op = 3 && t.cf then 1 else 0) in
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

(* modrm 디코드. 반환: (mod, reg필드, 피연산자). 세그먼트 override 가
   있으면 [~ovr] 로, 없으면 기본 규칙(BP 기반 = SS, 나머지 = DS). *)
let decode_modrm t ~ovr =
  let byte = fetch8 t in
  let m = byte lsr 6 in
  let regf = (byte lsr 3) land 7 in
  let rm = byte land 7 in
  let operand =
    if m = 3 then Reg rm
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
      Mem (physical ~seg:t.segs.(segsel) ~off:((base + disp) land 0xffff))
    end
  in
  (m, regf, operand)

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
  | 0x26 -> Some 0 | 0x2e -> Some 1 | 0x36 -> Some 3 | 0x3e -> Some 2
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
      (* prefix: 세그먼트 override 만 M0. rep/lock 은 string 명령과 함께 M1. *)
      let ovr = ref None in
      let rec prefixes () =
        match fetch8 t with
        | 0xf0 | 0xf2 | 0xf3 ->
          (* lock/repnz/rep — string 명령 없이는 prefix 로 무의미하다.
             M0 은 소비만 하고 넘어간다 (페치 부작용 없음 — opcode 가 뒤에 온다). *)
          prefixes ()
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
          let _, regf, rm = decode_modrm t ~ovr:!ovr in
          let a = op_read t width rm in
          let b = if width = 8 then reg8 t regf else reg16 t regf in
          let r = alu_result t op a b width in
          if op <> 7 then op_write t width rm r;
          3
        | 2 | 3 ->
          let width = if opcode land 1 = 0 then 8 else 16 in
          let _, regf, rm = decode_modrm t ~ovr:!ovr in
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
        let _, regf, rm = decode_modrm t ~ovr:!ovr in
        let v = if width = 8 then reg8 t regf else reg16 t regf in
        op_write t width rm v;
        2
      | 0x8a | 0x8b ->
        let width = if opcode = 0x8a then 8 else 16 in
        let _, regf, rm = decode_modrm t ~ovr:!ovr in
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
      (* --- push/pop --- *)
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
      | 0xe9 ->
        let d = fetch16 t in
        t.ip <- (t.ip + d) land 0xffff;
        7
      (* --- hlt --- *)
      | 0xf4 -> t.halted <- true; 2
      (* --- 나머지: M1 이후. 예외로 하네스에 알린다. --- *)
      | _ ->
        bad (Printf.sprintf "opcode %02x" opcode)
    in
    t.cycles <- t.cycles + used_cycles;
    used_cycles
  end
