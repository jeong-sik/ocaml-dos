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

(** 흉내낼 실리콘 세대. 8086 과 80186 은 같은 opcode 자리에 다른 명령을
    놓았다 — 0x60-0x6F(8086: jcc 거울 / 186: pusha·push imm·bound·ins),
    0xC0/0xC1·0xC8/0xC9(8086: ret/retf 거울 / 186: shift imm·enter·leave),
    그룹2 의 reg=6(8086: SETMO / 186: shl). 실칩 검증 스위트는 [I8086]
    으로 돈다.

    [I80286] 은 실모드에서 [I80186] 과 같은 명령을 돌리고, 프로그램이
    CPU 를 가르는 세 자리만 다르다: FLAGS bit12-15 를 읽으면 0, PUSH SP
    는 줄기 전 SP 를 민다, 0x0F 는 두 바이트 명령의 머리이고 그 가운데
    SMSW(0F 01 /4)만 있다 — 0xFFF0 을 돌려준다. 1990년대 DOS 게임은 286
    이상을 요구하고 이 자리로 확인한다. 나눗셈 오류(DIV·IDIV·AAM 0)는
    286 에서 결함이다 — INT 0 이 돌아갈 자리가 다음 명령이 아니라 그
    나눗셈 자신이다.

    [I80386] 은 286 의 실모드에 세 가지를 더한다: 범용 레지스터의 위 16
    비트({!reg32}), FS·GS(세그먼트 4·5, 접두 64h·65h, MOV sreg), 실모드에서도
    POPF·IRET 로 쓸 수 있는 FLAGS bit12-14(IOPL·NT). bit15 는 0 이다. 게임은
    bit12-14 가 남는지로 386 을 가른다. 명령 길이는 15 바이트까지다. 결함도
    286 과 다르다: 받지 않는 명령의 LOCK 은 INT 6, SS 로 세그먼트 끝을 넘으면
    INT 12, 결함 난 문자열 명령은 그 반복을 처음부터 다시 한다.

    66h 는 80386.csv 의 66 열이 고른 155 opcode 의 피연산자를 32 비트로
    바꾼다. 실칩이 설명서를 글자 그대로 읽은 것과
    다른 자리가 셋 있다. 세그먼트 레지스터의 32 비트 PUSH·POP 은 SP 를 4
    움직이고 워드 하나만 오간다. POPAD 는 ESP 의 위 16 비트를 스택의 ESP
    자리에서 가져온다. PUSHAD 는 EDI 부터 쓰고 세그먼트 끝을 넘는 첫 자리에서
    INT 12 다. EIP 가 FFFFh 를 넘는 RET·점프는 CS 한계 밖이라 INT 13 이다.

    67h 는 주소를 32 비트로 센다: modrm 의 SIB·disp32, A0h-A3h 의 moffs32,
    문자열 명령의 ESI·EDI·ECX, LOOP·JECXZ 의 ECX, XLAT 의 EBX 다. 실모드
    세그먼트 한계는 FFFFh 그대로라, 접근의 끝 바이트가 그 위면 SS 는 INT 12,
    나머지는 INT 13 이다. 설명서가 말하지 않는 규칙이 하나 있다: SIB 에
    인덱스가 없으면 배율이 베이스에 곱해진다. POP r/m 의 [esp+...] 는 pop 한
    뒤의 ESP 로 센다.

    0x0F 확장(RFC 0001 3 단계)은 실모드 데이터에 있는 형태가 다 있다: Jcc
    rel16·rel32, SETcc, MOVZX·MOVSX, FS·GS 의 PUSH·POP, BT·BTS·BTR·BTC,
    SHLD·SHRD, IMUL r,r/m, BSF·BSR, LSS·LFS·LGS, CLTS 다. 66h 는 SETcc 와 CLTS
    를 뺀 모두를 32 비트로 바꾼다. Jcc rel16 은 IP 를 16 비트에서 감고, rel32
    는 FFFFh 를 넘으면 INT 13 이다.
    설명서가 정하지 않은 자리에서 실칩은 이렇게 한다. 16 비트 SHLD·SHRD 의
    횟수가 17-31 이면 결과는 원본을 (횟수-16) 만큼 돌린 값이다. 횟수가 0 이어도
    메모리 피연산자를 읽는다. BSF·BSR 의 원본이 0 이면 대상은 그대로다.
    LOCK 은 메모리를 바꾸는 BTS·BTR·BTC 에만 붙고, 나머지는 INT 6 이다.
    80386.csv 의 lck 열은 BTR(0F B3)을 0 으로 적었지만 실칩은 LOCK BTR 을
    실행한다.
    근거는 SingleStepTests/80386 실모드다. *)
type model = I8086 | I80186 | I80286 | I80386

exception Unsupported of string
(** 아직 구현되지 않은 명령을 만났다. 메시지는 명령 위치와 opcode.
    하네스가 이 예외를 보면 그 게임이 필요로 하는 다음 명령을 안다 —
    구현 우선순위의 관측 자료다. *)

type t

val create :
  ?model:model ->
  read:(int -> int) ->
  write:(int -> int -> unit) ->
  port_in:(int -> int) ->
  port_out:(int -> int -> unit) ->
  unit ->
  t
(** 콜백은 물리 주소(0..0xFFFFF)를 받는다. 포트는 16비트 I/O 공간.
    하네스는 빈 콜백(fun _ -> 0xFF / fun _ _ -> ())으로 시작해 장치를
    붙여 나간다. 포트는 바이트 폭이다 — 워드 I/O 는 p 와 p+1 로 두 번
    부른다. [model] 기본값은 [I80186]. *)

val step : t -> int
(** 한 명령을 실행하고 그 명령의 클럭 사이클을 반환. 8086 명령당
    사이클은 명령·피연산자 위치에 따라 2~50+ 이다(실효 주소 계산
    포함). M0 은 기본 사이클표의 근삿값을 쓴다. HLT 상태면 아무
    것도 하지 않고 2 를 반환한다. *)

val idle : t -> int
(** Advance the clock by one two-cycle idle quantum without fetching an
    instruction or changing HLT. A machine uses this while a host service
    is suspended; hardware interrupts can still run guest handlers. *)

(** {1 레지스터 접근 — 하네스 판정·시드용}

    레지스터 번호는 8086 인코딩 그대로: 16비트 0..7 = AX CX DX BX SP
    BP SI DI, 8비트 0..7 = AL CL DL BL AH CH DH BH. *)

val reg16 : t -> int -> int
val set_reg16 : t -> int -> int -> unit
val reg8 : t -> int -> int
val set_reg8 : t -> int -> int -> unit

val reg32 : t -> int -> int
(** [I80386] 의 32 비트 레지스터 0..7 = EAX ECX EDX EBX ESP EBP ESI EDI. 16·8
    비트 쓰기는 위 16 비트를 그대로 둔다. 다른 모델에서는 [Invalid_argument]. *)

val set_reg32 : t -> int -> int -> unit
(** 아래 32 비트만 쓴다. 다른 모델에서는 [Invalid_argument]. *)

val seg : t -> int -> int
(** 세그먼트 레지스터 0..3 = ES CS SS DS, [I80386] 은 4·5 = FS GS 까지. 그
    밖의 번호는 [Invalid_argument]. *)

val set_seg : t -> int -> int -> unit

val dump_ip : t -> int
(** [I80386] 은 FFFF 의 마지막 바이트를 지난 뒤 10000h 를 돌려준다. EIP 가
    32 비트라서다. 다음 페치는 CS 한계 밖이라 INT 13 이다. *)

val set_ip : t -> int -> unit

val physical : t -> seg:int -> off:int -> int
(** [~seg:s ~off:o] 이 CPU 가 버스에 내는 주소 = (s lsl 4) + o 를 주소선
    수로 자른 값. 8086·186 은 20비트라 1MB 에서 감기고, 286 은 24비트라
    10FFEFh 까지 간다. 286 보드에서 1MB 에서 감는 것은 A20 게이트의 일이라
    메모리를 붙이는 쪽이 정한다. *)

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

(** {1 Saved state}

    What a machine snapshot needs from the CPU: the registers, the flags word,
    HLT and the cycle count the machine's clock derives from. The model and
    the memory, port and interrupt callbacks belong to whoever creates the
    CPU and are not part of it. *)

type saved = {
  saved_regs : int array;  (** 8 × 16-bit, AX CX DX BX SP BP SI DI *)
  saved_segs : int array;  (** 4 × segment, ES CS SS DS *)
  saved_ip : int;
  saved_flags : int;  (** as {!flags} returns it *)
  saved_halted : bool;
  saved_cycles : int;
}

val save_state : t -> saved
(** [I80386] 이면 [Invalid_argument]: 위 16 비트·FS·GS·IOPL·NT 를 담을 자리가
    아직 없다(RFC 0001 4 단계). 기계는 아직 286 으로 돈다. *)

val load_state : t -> saved -> unit
(** Values are masked as the setters mask them. [Invalid_argument] unless
    there are 8 registers and 4 segments. *)
