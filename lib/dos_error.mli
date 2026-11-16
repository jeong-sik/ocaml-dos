(** INT 21h 에러의 확장 정보 — MS-DOS 4.0 [MS_TABLE.ASM] 의
    [I21_MAP_E_TAB]·[ERR_TABLE_21], 번호는 [ERROR.INC]. *)

type error_class =
  | Out_of_resource | Temporary | Authorization | Internal | Hardware_failure
  | System_failure | Application | Not_found | Bad_format | Locked | Media
  | Already_exists | Unknown_class

type action = Retry | Delay_retry | User | Abort | Panic | Ignore | Interrupt_retry

type locus = Unknown_locus | Disk | Network | Serial_device | Memory

val class_code : error_class -> int
val action_code : action -> int
val locus_code : locus -> int

type info = { error_class : error_class; action : action; locus : locus option }
(** [locus = None] 은 표의 0FFh: 기능이 이미 정한 locus 를 그대로 둔다. *)

val info : int -> info option
(** 표에 없는 코드는 [None] — class·action·locus 를 그대로 둔다. *)

val returned_code : fn:int -> int -> int
(** 기능 [fn](AH)이 돌려줄 수 있는 코드가 아니면 그 기능 목록의 마지막
    코드. 목록이 없는 기능은 그대로. *)
