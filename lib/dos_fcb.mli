(** FCB 필드 배치와 레코드 번호 (MS-DOS 4.0 CPMFCB.INC, FCBIO2.ASM).
    주소는 모두 물리 주소다. *)

val drive : int
val extent : int
val record_size : int
val file_size : int
val next_record : int
val random_record : int
val default_record_size : int
val wide_record_limit : int

val locate : Dos_state.t -> int -> int
(** DS:DX 가 가리키는 자리에서 보통 FCB 의 시작을 찾는다 — 확장 FCB 면
    머리 7 바이트를 건너뛴다 (GetExtended). *)

val current : Dos_state.t -> int -> int
(** 블록과 다음 레코드가 가리키는 레코드 번호 (GetExtent) *)

val set_current : Dos_state.t -> int -> int -> unit
(** SetExtent *)

val record_size_of : Dos_state.t -> int -> int
(** 레코드 크기. 0 이면 128 로 고쳐 쓴다 (GetRecSize) *)

val random : Dos_state.t -> int -> record_size:int -> int
(** 랜덤 레코드. 레코드가 64 바이트 이상이면 3 바이트, 아니면 4 바이트 (GetRR) *)

val store_random : Dos_state.t -> int -> int -> fourth:bool -> unit
(** 랜덤 레코드의 아래 3 바이트를 쓰고, [fourth] 면 넷째 바이트도 쓴다 *)

val records_for : record_size:int -> int -> int
(** 바이트 수를 레코드 수로, 남는 바이트는 한 레코드로 올린다 *)
