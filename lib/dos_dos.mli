(** DOS 표면 — INT 21h 와 INT 20h.

    파일은 하네스가 마운트한 것만 보인다. 쓰기는 메모리 사본에 쌓였다가
    닫을 때 마운트 표로 돌아간다 — 같은 세션 안에서 저장하고 다시 여는
    것이 그래서 된다. 호스트 디스크의 원본은 바뀌지 않는다. *)

val conv_mem_top : int
(** 할당 가능한 마지막 세그먼트 — 비디오 메모리 바로 앞. *)

val service : Dos_state.t -> unit
(** AH 에 맞는 INT 21h 기능을 실행한다. *)

val terminate : Dos_state.t -> unit
(** INT 20h — RET 로 돌아온 프로그램의 종료. *)

val init_memory : Dos_state.t -> psp_seg:int -> unit
(** 적재 직후의 메모리 소유 상태. 실기처럼 프로그램이 남은 전부를
    가진다 — AH=4Ah 로 줄여야 AH=48h 이 성공한다. *)

val largest_free : Dos_state.t -> int
(** 지금 한 번에 줄 수 있는 가장 큰 덩어리(파라그래프). *)

val matches_pattern : pattern:string -> name:string -> bool
(** DOS 8.3 와일드카드. '*' 는 그 칸의 남은 자리를 '?' 로 채운다. *)
