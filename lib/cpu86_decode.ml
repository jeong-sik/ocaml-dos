(* 명령 바이트와 피연산자: 페치, 접두, modrm, 메모리·포트·스택 접근. *)

open Cpu86_state
open Cpu86_fault

(* ---------- 명령 페치와 피연산자 ---------- *)

let fetch8 t =
  if t.ip = t.fetch_end then fault vector_segment_overrun;
  let phys = physical t ~seg:t.segs.(1) ~off:t.ip in
  t.ip <- (t.ip + 1) land 0xffff;
  t.read phys

let fetch16 t =
  let lo = fetch8 t in
  let hi = fetch8 t in
  lo lor (hi lsl 8)

type operand =
  | Reg of int       (** 레지스터 번호 (폭은 문맥) *)
  | Mem of int * int (** 세그먼트 물리 베이스, 세그먼트 안 16비트 오프셋.
                         워드 접근은 오프셋이 세그먼트 안에서 감긴다 —
                         물리 주소 하나만 들고 있으면 offset 0xFFFF 에서
                         옆 세그먼트를 읽는다(실칩 관측). *)

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
      (Mem (t.segs.(segsel) lsl 4, off), Some off)
    end
  in
  (m, regf, operand, ea)

(* 세그먼트 베이스 + 오프셋 → 물리 주소. 오프셋은 16비트에서 감긴다. *)
let at t base off = (base + (off land 0xffff)) land t.addr_mask

(* 286 실모드에서 오프셋 FFFF 의 워드는 세그먼트 끝을 넘는다. 8086 처럼
   오프셋 0 으로 감지 않고 INT 13 이며, 바이트는 하나도 오가지 않는다
   (SingleStepTests/80286: 워드 피연산자 파일마다 30-50 건). 두 바이트
   메모리 접근은 모두 이 둘을 지난다. *)
let overruns t off =
  match t.model with
  | I80286 -> off land 0xffff = 0xffff
  | I8086 | I80186 -> false

let check_word t off = if overruns t off then fault vector_segment_overrun

(* 낮은 바이트부터 읽는다. OCaml 은 [lor] 의 오른쪽을 먼저 계산해서(ocamlopt
   5.5.2 실측) 한 식으로 쓰면 o+1 을 먼저 읽는다. 286 은 홀수 워드를 o, o+1
   순서의 두 버스 사이클로 읽고(SingleStepTests/80286 8B 사이클 기록 1,910
   건), 읽기에 부수효과가 있는 장치는 그 순서를 본다 — VGA 래치는 마지막에
   읽은 바이트를 들고 있다. *)
let read16 t b o =
  check_word t o;
  let lo = t.read (at t b o) in
  let hi = t.read (at t b (o + 1)) in
  lo lor (hi lsl 8)

let write16 t b o v =
  check_word t o;
  t.write (at t b o) (v land 0xff);
  t.write (at t b (o + 1)) (v lsr 8)

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
   같이 쓴다. 읽기는 [read16] 처럼 p 부터다(SingleStepTests/80286 E5 2,540
   건). *)
let port_in16 t p =
  let lo = t.port_in p land 0xff in
  let hi = t.port_in ((p + 1) land 0xffff) land 0xff in
  lo lor (hi lsl 8)

let port_out16 t p v =
  t.port_out p (v land 0xff);
  t.port_out ((p + 1) land 0xffff) ((v lsr 8) land 0xff)

(* SP 는 쓰기가 끝난 뒤에 내린다 — 넘치면 SP 도 그대로다. *)
let push16 t v =
  let sp = (t.regs.(4) - 2) land 0xffff in
  write16 t (t.segs.(2) lsl 4) sp v;
  t.regs.(4) <- sp

let pop16 t =
  let v = read16 t (t.segs.(2) lsl 4) t.regs.(4) in
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

(* 접두: 세그먼트 override 와 rep. [rep] 은 Some true(rep)/false(repnz)
   — string 명령만 소비하고 다른 명령에 붙으면 무시한다(8086 문서 동작).
   같은 종류의 접두가 여럿이면 마지막 것이 남는다. *)
let rec prefixes_after t ~base_ip ovr rep =
  match fetch8 t with
  | 0xf0 -> prefixes_after t ~base_ip ovr rep            (* lock *)
  | 0xf2 -> prefixes_after t ~base_ip ovr (Some false)   (* repnz *)
  | 0xf3 -> prefixes_after t ~base_ip ovr (Some true)    (* rep/repz *)
  | b ->
    (match seg_override_of b with
     | Some s -> prefixes_after t ~base_ip (Some s) rep
     | None -> { base_ip; opcode = b; ovr; rep })

let decode_prefixes t ~base_ip = prefixes_after t ~base_ip None None
