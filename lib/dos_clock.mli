(** MS-DOS 4.0 의 시각·날짜 계산(src/BIOS/MSCLOCK.ASM, src/DOS/TIME.ASM).
    시각은 BIOS 틱에서, 날짜는 1980-01-01 부터 센 날 수에서 나온다.
    값은 [*_of_fields] 와 [*_of_ticks]·[*_of_days] 로만 만든다 — 범위
    밖의 시각·날짜는 이 타입으로 나타낼 수 없다. *)

type time = private { hour : int; minute : int; second : int; hundredths : int }
type date = private { year : int; month : int; day : int }

val time_of_ticks : int -> time
(** [TIM$READ]: 틱 → 시·분·초·1/100 초. 틱이 하루(0x1800B0)보다 작으면
    시는 0..23 이다. *)

val ticks_of_time : time -> int
(** [TIME_TO_TICKS]. [time_of_ticks] 와 짝이지만 내림 때문에 되돌리면
    한 틱 모자랄 수 있다(MS-DOS 와 같다). *)

val time_of_fields : hour:int -> minute:int -> second:int -> hundredths:int -> time option
(** [$SET_TIME] 의 검사. 시 24·분 60·초 60·1/100 100 이상이면 [None]. *)

val date_of_fields : year:int -> month:int -> day:int -> date option
(** [$SET_DATE]·[DODATE] 의 검사. 1980..2099 년, 그 달의 날 수 안. 윤년은
    (연도 − 1980) 이 4 의 배수인 해다. *)

val days_of_date : date -> int
(** 1980-01-01 이 0. *)

val date_of_days : int -> date

val weekday : int -> int
(** 0 이 일요일. 1980-01-01(0) 은 화요일(2). *)

val next_day : int -> int
(** 자정을 넘긴 날 수. 120 년(43830 일) 째에서는 그대로다 — MS-DOS 의
    [READTIME] 이 그 이상을 무시해 날짜가 2099-12-31 에 머문다. *)
