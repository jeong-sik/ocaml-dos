(** 8254 PIT 카운터 하나. 지금은 채널 2(PC 스피커·지연 측정)가 쓴다.

    모드 0~5, 카운터 래치, 1·2바이트 접근, BCD 를 Intel 82C54 데이터시트
    (231244) 의 Mode Definitions·Read Operations 대로 따른다.

    시간은 PIT 틱(CLK 펄스)으로 받는다. 카운터는 틱마다 갱신하지 않고,
    마지막 사건(제어 워드, 카운트 쓰기, GATE 변화) 뒤 흐른 틱으로 그때그때
    값을 낸다. GATE 의 레벨은 이 모듈이 들고 있지 않다 — 포트 0x61 이
    진실 원천이고, 레벨이 바뀔 때 {!gate_edge} 로 알린다. *)

type t

val create : unit -> t
(** 데이터시트는 전원 직후 상태를 정하지 않는다. 이 기계는 모드 0, 하위·상위
    바이트 접근, 카운트 없음으로 시작한다 — OUT 이 낮아 부팅 직후 포트 0x61
    bit 5 가 0 이다. *)

val control : t -> tick:int -> int -> unit
(** 포트 0x43 에 쓴 제어 워드 가운데 이 카운터를 고른 것. bit 5-4 가 0 이면
    카운터 래치 명령이고, 아니면 접근 방식·모드·BCD 를 새로 정하고 카운터를
    멈춘다. *)

val write : t -> tick:int -> gate:bool -> int -> unit
(** 카운터 포트에 쓴 바이트. [gate] 는 지금 GATE 레벨이다. *)

val read : t -> tick:int -> int
(** 카운터 포트에서 읽은 바이트. 래치가 있으면 래치 값을 낸다. *)

val gate_edge : t -> tick:int -> high:bool -> unit
(** GATE 레벨이 바뀌었다. 같은 레벨로 다시 부르지 않는다. *)

val out : t -> tick:int -> bool
(** OUT 핀. *)

val next_rise : t -> tick:int -> int option
(** [tick] 뒤로 OUT 이 낮음에서 높음으로 바뀌는 첫 틱. 다음 사건(제어 워드,
    카운트 쓰기, GATE 변화)이 없다고 볼 때의 값이다. 그런 상승이 없으면
    [None] 이다 — 멈춘 카운터, GATE 가 낮은 셈, 끝난 원숏이 그렇다. 채널 0 의
    OUT 상승이 IRQ0 이다. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit

val read_state : Dos_snap_codec.reader -> t -> unit
(** Overwrites [t]. Raises [Dos_snap_codec.Invalid] on a value out of range,
    including a BCD count above 10000. *)
