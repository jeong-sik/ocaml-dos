(** LIM EMS 4.0 의 expanded memory 자체 — 핸들, 논리 페이지, 프레임에
    겹친 페이지, 핸들별로 저장된 매핑. 상태는 [Dos_state] 의 [ems_pages]·
    [ems_mapped]·[ems_saved] 에 있다. INT 67h 의 레지스터와 게스트 배열은
    [Dos_ems] 가 다룬다. *)

val page_frame : int
(** 물리 페이지 넷이 겹쳐지는 첫 세그먼트(0xD000). *)

val page_size : int
(** 페이지 하나의 단락 수(0x400). *)

val frame_pages : int

val page_bytes : int
(** 논리 페이지 하나의 크기(16KB). 아직 안 건드린 페이지는 빈 [Bytes]. *)

val total_pages : int
(** 전체 페이지 수(2048, 32MB). *)

val os_handle : int
(** 운영체제 핸들(0). 드라이버가 설치될 때부터 열려 있고 닫히지 않는다.
    이 드라이버에서는 페이지가 없다. *)

val first_handle : int
(** 할당되는 첫 핸들(1). *)

val max_handle : int
(** 할당되는 마지막 핸들(254). 1..254 가 다 쓰이면 할당이 85h 다. *)

val unmap_page : int
(** 논리 페이지 FFFFh: 그 물리 페이지를 비운다(Function 5·17). *)

val boot_handles : unit -> (int, Bytes.t array) Hashtbl.t
(** 드라이버가 설치된 직후의 핸들 표: 페이지 없는 운영체제 핸들 하나. *)

(** LIM 상태 코드. 이름 옆이 AH 값이다. *)
type status =
  | Invalid_handle                               (** 83h *)
  | Undefined_function                           (** 84h *)
  | No_more_handles                              (** 85h *)
  | Map_still_saved                              (** 86h *)
  | More_than_total                              (** 87h *)
  | More_than_free                               (** 88h *)
  | Zero_pages                                   (** 89h *)
  | Logical_out_of_range                         (** 8Ah *)
  | Physical_out_of_range                        (** 8Bh *)
  | Map_already_saved                            (** 8Dh *)
  | No_saved_map                                 (** 8Eh *)
  | Invalid_subfunction                          (** 8Fh *)

val status_code : status -> int

val free_pages : Dos_state.t -> int

val target : Dos_state.t -> handle:int -> log:int -> ((int * int) option, status) result
(** 핸들 [handle] 의 논리 페이지 [log] 를 겹칠 수 있으면 [Some (handle, log)],
    [log] 가 FFFFh 면 [None](비우기 — 핸들은 보지 않는다). *)

val show : Dos_state.t -> int -> (int * int) option -> unit
(** [show t phys shown]: 물리 페이지 [phys] 에 [shown] 을 겹치거나 비운다.
    [phys] 는 [frame_pages] 보다 작고, [shown] 은 [target] 을 지난 값이다. *)

val allocate : Dos_state.t -> count:int -> zero_ok:bool -> (int, status) result
(** 새 핸들. [zero_ok] 는 Function 27 처럼 0 페이지를 받을지다. *)

val release : Dos_state.t -> int -> (unit, status) result
val reallocate : Dos_state.t -> int -> count:int -> (unit, status) result
val save_map : Dos_state.t -> int -> (unit, status) result
val restore_map : Dos_state.t -> int -> (unit, status) result
