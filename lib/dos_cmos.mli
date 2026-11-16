(** MC146818 실시간 시계와 CMOS RAM — 포트 70h(번지)와 71h(데이터).

    시각 레지스터(00h~09h, 세기 32h)는 기계의 시계에서 읽는다. INT 1Ah
    AH=02h·04h 와 같은 시계라서 두 길이 같은 시각을 본다. 레지스터 A 의
    UIP(bit7)는 초가 바뀌기 244µs 전에 켜지고 1984µs 뒤에 꺼진다. UIP 가
    꺼진 뒤 읽으면 바뀐 시각이 보인다. 이 타이밍은 86Box [src/nvr_at.c] 가
    옮겨 적은 데이터시트 문구와 그 갱신 타이머를 따른다.

    레지스터 B 는 쓸 수 있다. bit2(이진), bit1(24시간)이 시각을 읽는 꼴을
    바꾸고, bit7(SET)이 서면 UIP 가 켜지지 않는다. 레지스터 A 는 bit0~6 만
    쓸 수 있다. 그 밖의 쓰기는 버린다. 시각은 기계의 시계를 따르고, RAM
    표(메모리 크기, 체크섬)는 고정이다.
    레지스터 A 가 고른 주기(RS)가 끝날 때마다 레지스터 C 의 PF(bit6)가
    선다. 발진기 비트(DV)가 010 이 아니거나 RS 가 0 이면 서지 않는다.
    레지스터 B 의 PIE(bit6)가 켜져 있으면 IRQF(bit7)도 같이 보인다.
    레지스터 C 를 읽으면 둘 다 지워진다. AF·UF(알람·갱신 끝)는 만들지
    않아 늘 0 이다. 주기의 끝은 기계가 켜진 때부터 센다. RTC 인터럽트
    (IRQ8)는 아직 내지 않는다 — 받을 두 번째 8259 가 없다.

    번지 쓰기의 bit7 은 NMI 끄기라서 번지는 bit0~6 이다. *)

type wall_time = {
  year : int;
  month : int;
  day : int;
  weekday : int;  (** 0 이 일요일 *)
  hour : int;
  minute : int;
  second : int;
}

type t

val create : clock:(unit -> wall_time) -> t
(** [clock] 은 지금 시각이다. 시각 레지스터를 읽을 때마다 부른다. *)

val base_memory_kb : int
(** 기본 메모리(KB). CMOS 15h·16h 와 BDA 0x413 이 같은 값을 쓴다. *)

val select : t -> int -> unit
(** 포트 70h 쓰기. *)

val index : t -> int
(** 고른 번지(0~7Fh). *)

val read : t -> now:int -> cpu_hz:int -> int
(** 포트 71h 읽기. [now] 는 CPU 누적 사이클, [cpu_hz] 는 기계의 클록이다.
    초 경계는 [cpu_hz] 의 배수에 있다. *)

val write : t -> int -> now:int -> cpu_hz:int -> unit
(** 포트 71h 쓰기. 레지스터 A 를 바꾸면 그때까지 선 PF 는 남고, 새 주기는
    [now] 부터 센다. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
