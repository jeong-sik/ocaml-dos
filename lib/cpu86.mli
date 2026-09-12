(** 8086 CPU 코어 — 해석형, 사이클 카운트 포함. (M0: 명령 집합 일부)

    메모리는 코어 밖에 있다: 생성 시 read/write 콜백을 받는다. 콜백은
    20비트 물리 주소(세그먼트*16 + 오프셋, 1MB wrap)로 불린다 — 장치
    배선(text VRAM 0xB8000, BIOS ROM 영역)은 콜백 쪽이 담당한다.

    결정론: 같은 상태 + 같은 메모리 + 같은 입력 = 같은 실행. 사이클은
    [cycles] 누적으로 하네스가 실기 타이밍과 대조한다.

    M0 범위: 레지스터/플래그 상태, modrm 디코딩, ALU 8종(rm,reg ·
    rm,imm · acc,imm), mov(reg/reg, reg/imm, rm), inc/dec, push/pop,
    jcc/jmp, hlt. 나머지 명령(string, mul/div, shift 그룹, call/ret,
    int, …)은 M1 이후 채운다 — 만나면 [ Unsupported ] 예외로 죽는
    게 조용한 오동작보다 낫다 (Silent Failure 방지 원칙). *)

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

val cycles : t -> int
(** 생성 이후 누적 사이클. *)

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
