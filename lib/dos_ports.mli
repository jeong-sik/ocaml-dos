(** 게스트가 보는 하드웨어 포트 — PIT 타이머, PIC 마스크, VGA DAC,
    CRTC, PC 스피커, CGA 상태 레지스터.

    이 포트들이 무엇을 돌려주는지가 게임의 진행 조건이 된다. 고정값을
    돌려주면 상태 변화를 기다리는 루프가 영원히 안 풀린다 — CGA 재주사
    비트와 스피커 리프레시 비트가 그렇다(ZZT 실측).

    시간에 의존하는 포트(PIT 카운터, 재주사 비트)는 벽시계가 아니라
    CPU 누적 사이클로 답한다. 같은 이미지 + 같은 입력 = 같은 실행이라는
    결정론 계약을 포트도 지킨다. *)

type t

val cga_palette : (int * int * int) array
(** CGA 16색 — 속성 바이트의 하위 니블이 글자색, 상위 니블이 바탕색. *)

val create : mem:Bytes.t -> video:Dos_video.t -> t
(** [mem] 은 1MB 게스트 메모리 — CRTC 커서 레지스터를 BDA(0x450/0x451)
    와 같은 값으로 유지하는 데 쓴다. 커서의 진실 원천은 하나여야 한다.
    비디오 어댑터의 포트(시퀀서·그래픽 컨트롤러·속성)는 [video] 로
    넘긴다. *)

val set_now : t -> int -> unit
(** 현재 CPU 누적 사이클. 기계가 매 스텝 알려준다. *)

val port_in : t -> int -> int
val port_out : t -> int -> int -> unit

val cycles_per_tick : t -> int
(** PIT 채널 0 의 분주비가 정하는 IRQ0 간격(CPU 사이클). 기본 분주비
    65536 = 18.2Hz. 게임이 음악·부드러운 이동을 위해 이 값을 바꾼다. *)

val irq0_masked : t -> bool
(** PIC IMR(포트 0x21) 의 bit0. 마스크된 IRQ 는 전달하지 않는다. *)

val palette : t -> (int * int * int) array
(** 256색 DAC — 채널당 6비트. 포트 0x3C8/0x3C9 와 INT 10h AH=10h 이
    같은 배열을 쓴다. *)

val reset_dac : t -> unit
(** DAC 256 자리를 기본값으로, 쓰기·읽기 순번과 PEL 마스크(0x3C6)를
    처음 상태로. INT 10h AH=00h 가 모드를 세울 때 부른다 — 실기 BIOS 가
    그 순간 팔레트를 다시 싣는다. *)

val set_scancode : t -> int -> unit
(** 포트 0x60 이 돌려줄 마지막 스캔 코드. *)

val speaker_on : t -> bool
(** 포트 0x61 의 bit0·bit1 이 둘 다 켜졌다 — 게이트와 데이터가 열렸다.
    소리를 내지는 않고, 하네스가 관측만 한다. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
(** Every field but the RAM and the video adapter, which the machine writes.
    For {!Dos_snapshot}. *)

val read_state : Dos_snap_codec.reader -> t -> unit
(** Overwrites [t] with what {!write_state} wrote. Raises
    [Dos_snap_codec.Invalid] on a value out of range. *)
