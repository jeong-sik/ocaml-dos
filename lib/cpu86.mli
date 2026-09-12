(** 8086 CPU 코어 — 해석형, 사이클 카운트 포함. (M1: 명령 집합 완성)

    메모리는 코어 밖에 있다: 생성 시 read/write 콜백을 받는다. 콜백은
    20비트 물리 주소(세그먼트*16 + 오프셋, 1MB wrap)로 불린다 — 장치
    배선(text VRAM 0xB8000, BIOS ROM 영역)은 콜백 쪽이 담당한다.

    결정론: 같은 상태 + 같은 메모리 + 같은 입력 = 같은 실행. 사이클은
    [cycles] 누적으로 하네스가 실기 타이밍과 대조한다.

    M1 범위: ALU 8종 전 형태, 그룹1(imm ALU)/그룹2(shift·rotate)/
    그룹3(test/not/neg/mul/imul/div/idiv)/그룹 FE·FF(inc/dec/call/
    jmp/push), mov 전형태(rm,imm · sreg · lea), xchg, string ops+
    rep, call/ret/jmp(far 포함), loop 계열, in/out, flag ops,
    pushf/popf/sahf/lahf, cbw/cwd/xlat/aam/aad, INT(호스트 훅).
    빠진 것: daa/das/aaa/aas(BCD), esc(D8-DF), wait — 만나면
    [Unsupported] 예외로 죽는 게 조용한 오동작보다 낫다 (Silent
    Failure 방지). INT 10h/21h 표면 구현은 Dos 머신 모듈이 소유한다. *)

exception Unsupported of string
(** 아직 구현되지 않은 명령을 만났다. 메시지는 명령 위치와 opcode.
    하네스가 이 예외를 보면 그 게임이 필요로 하는 다음 명령을 안다 —
    구현 우선순위의 관측 자료다. *)

type t

val create :
  read:(int -> int) ->
  write:(int -> int -> unit) ->
  port_in:(int -> int) ->
  port_out:(int -> int -> unit) ->
  t
(** 콜백은 물리 주소(0..0xFFFFF)를 받는다. 포트는 16비트 I/O 공간.
    하네스는 빈 콜백(fun _ -> 0xFF / fun _ _ -> ())으로 시작해 장치를
    붙여 나간다. *)

val step : t -> int
(** 한 명령을 실행하고 그 명령의 클럭 사이클을 반환. 8086 명령당
    사이클은 명령·피연산자 위치에 따라 2~50+ 이다(실효 주소 계산
    포함). M0 은 기본 사이클표의 근삿값을 쓴다. HLT 상태면 아무
    것도 하지 않고 2 를 반환한다. *)

(** {1 레지스터 접근 — 하네스 판정·시드용}

    레지스터 번호는 8086 인코딩 그대로: 16비트 0..7 = AX CX DX BX SP
    BP SI DI, 8비트 0..7 = AL CL DL BL AH CH DH BH. *)

val reg16 : t -> int -> int
val set_reg16 : t -> int -> int -> unit
val reg8 : t -> int -> int
val set_reg8 : t -> int -> int -> unit

val seg : t -> int -> int
(** 세그먼트 레지스터 0..3 = ES CS SS DS. *)

val set_seg : t -> int -> int -> unit

val dump_ip : t -> int
val set_ip : t -> int -> unit

val physical : seg:int -> off:int -> int
(** [~seg:s ~off:o] 물리 주소 = ((s lsl 4) + o) land 0xFFFFF. 1MB wrap
    은 8086 의 주소 버스가 20비트인 것의 재현 — A20 게이트는 M1. *)

val flags : t -> int
(** FLAGS word (bit0 CF, bit2 PF, bit4 AF, bit6 ZF, bit7 SF, bit8 TF,
    bit9 IF, bit10 DF, bit11 OF). *)

val set_flags : t -> int -> unit

val halted : t -> bool

(** HLT 해제 — 하드웨어 인터럽트 도착을 알릴 때(DOS idle 루프의 HLT 가
    타이머로 깨어나는 모델). 큐에 넣는 쪽이 deliver 후 호출한다. *)
val wake : t -> unit

val cycles : t -> int
(** 생성 이후 누적 사이클. *)

val set_int_hook : t -> (int -> unit) -> unit
(** INT n / INT3 을 만났을 때 부르는 콜백. DOS 표면(INT 10h/21h 등)의
    소유자가 심는다 — 코어는 벡터 번호만 넘기고 스택 조작을 하지
    않는다(호스트 서브루틴 모델: 훅이 레지스터를 세팅하면 INT 다음
    명령으로 그대로 계속). 훅이 없는 INT 는 [Unsupported] 이다. 실제
    IVT 로 가는 소프트웨어 인터럽트가 필요해지면 그때 계약을 늘린다. *)

(** {1 플래그 비트} *)

val f_carry : int
val f_parity : int
val f_aux : int
val f_zero : int
val f_sign : int
val f_trap : int
val f_interrupt : int
val f_direction : int
val f_overflow : int
