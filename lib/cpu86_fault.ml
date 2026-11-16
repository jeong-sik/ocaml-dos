(* 결함과 인터럽트: 286 결함, 여러 번 접근하는 명령의 되돌림, INT 훅,
   나눗셈 오류. *)

open Cpu86_state

(* ---------- 286 결함 ---------- *)

(* 286 의 결함(fault). IP 는 그 명령의 첫 접두 바이트로 돌아가고 인터럽트
   [vector] 를 받는다. 레지스터는 던질 때의 값 그대로다: 접근은 바이트가
   오가기 전에 따지고, 여러 번 접근하는 스택 명령은 [atomic] 이 되돌린다.
   실칩이 먼저 고쳐 두는 레지스터(POP r/m 의 SP, 문자열 명령의 SI·DI·CX)는
   그대로 남는다. 386 은 그 둘도 명령 앞 값으로 되돌린다
   ([restore_sp_on_fault], [Cpu86_string]). step 밖으로 나가지 않는다. *)
exception Fault of int

let vector_divide = 0
let vector_bound = 5
let vector_invalid_opcode = 6
let vector_stack_fault = 12
let vector_segment_overrun = 13

let fault vector = raise (Fault vector)

(* 결함을 받는 비용. INT 3 과 같은 값을 쓴다. *)
let fault_cycles = 25

(* 286 이 정의하지 않은 인코딩은 INT 6 결함이다. [cond] 가 그 인코딩인지
   말한다. 8086·186 에서는 그냥 돌아온다 — 그 자리를 별칭으로 실행하거나
   (MOV sreg 의 reg 필드 상위 비트, POP/MOV r/m 의 남는 reg 필드) 호출한
   쪽이 Unsupported 로 알린다(SingleStepTests/80286 8C 8D 8E 8F C4 C5 C6 C7
   62 FF). step 안의 지역 함수로 두면 명령마다 클로저를 만든다. *)
let undefined_on_286 t cond =
  match t.model with
  | I80286 | I80386 -> if cond then fault vector_invalid_opcode
  | I8086 | I80186 -> ()

(* MOV sreg(8C·8E)의 reg 필드가 고르는 세그먼트. 8086·186 은 아래 두 비트만
   본다(실칩 관측) — 그대로 두면 regf>=4 가 세그먼트 배열 밖을 짚는다. 286 은
   4 이상이 INT 6 이고, 386 은 4·5 가 FS·GS 이고 6·7 이 INT 6 이다. CS 로
   싣기([load])는 286 부터 INT 6 이다. *)
let sreg_field t regf ~load =
  match t.model with
  | I8086 | I80186 -> regf land 3
  | I80286 -> undefined_on_286 t (regf >= 4 || (load && regf = 1)); regf
  | I80386 ->
    if regf >= 6 || (load && regf = 1) then fault vector_invalid_opcode;
    regf

(* 스택을 여러 번 건드리는 명령(PUSHA·POPA·ENTER·LEAVE·far CALL·RETF·
   IRET)은 286 에서 도중에 넘치면 시작 때의 범용·세그먼트 레지스터로
   돌아간다(SingleStepTests/80286 60 61 C8 C9). 다른 명령은 접근 전에
   따지고 쓰기 전에 읽으므로 넘칠 때 바뀐 것이 없다. 모든 명령의 레지스터를
   미리 떠 두면 삼국지3 30M 스텝이 1.7 초에서 2.0 초가 되어, 이 명령들만
   뜬다. [f] 는 그 명령의 몸이고 사이클 수를 돌려준다. *)
let atomic t f =
  match t.model with
  | I8086 | I80186 -> f ()
  | I80286 | I80386 ->
    Array.blit t.regs 0 t.undo_regs 0 8;
    Array.blit t.segs 0 t.undo_segs 0 4;
    (try f () with
     | Fault _ as e ->
       Array.blit t.undo_regs 0 t.regs 0 8;
       Array.blit t.undo_segs 0 t.segs 0 4;
       raise e)

(* 386 은 결함 난 POPA·POP r/m 의 SP 를 명령 앞 값으로 되돌린다. POPA 가
   이미 꺼낸 DI·SI·BP 는 그대로 둔다(SingleStepTests/80386 61 8F). 286 은
   POP r/m 의 SP 를 올려 둔 채 두고 POPA 는 [atomic] 으로 모두 되돌린다. *)
let restore_sp_on_fault t f =
  let sp = t.regs.(4) in
  try f () with Fault _ as e -> t.regs.(4) <- sp; raise e

let do_int t ctx n =
  match t.int_hook with
  | Some f -> f n
  | None -> unsupported t ctx (Printf.sprintf "int %02x (no hook)" n)

(* 8086·186 의 나눗셈 오류는 다음 명령으로 돌아가는 트랩이고, 286 부터는
   나눗셈 자신(접두 바이트 포함)으로 돌아가는 결함이다. *)
let divide_error t ctx =
  match t.model with
  | I80286 | I80386 -> fault vector_divide
  | I8086 | I80186 -> do_int t ctx vector_divide
