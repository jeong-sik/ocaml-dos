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
  | Undefined_attribute                          (** 90h *)
  | Not_supported                                (** 91h *)
  | Moved_over_source
      (** 92h. 같은 핸들 안에서 겹치는 옮기기다. 옮기기는 끝났고, 원본
          일부가 덮였다는 알림이다 *)
  | Past_handle_pages                            (** 93h *)
  | Conventional_overlaps_expanded               (** 94h *)
  | Offset_past_page                             (** 95h *)
  | Longer_than_1mb                              (** 96h *)
  | Exchange_overlap                             (** 97h *)
  | Undefined_memory_type                        (** 98h *)
  | Name_not_found                               (** A0h *)
  | Name_taken                                   (** A1h: 다른 핸들이 이미 가진 이름(53h) *)
  | Null_name                                    (** A1h: 모두 NUL 인 이름으로 찾음(54h) *)
  | Wraps_1mb                                    (** A2h *)
  | Corrupt_map                                  (** A3h *)

val status_code : status -> int

val free_pages : Dos_state.t -> int

val frame_addr : int -> int
(** 물리 페이지의 첫 물리 주소. *)

val peek_byte : Dos_state.t -> int -> int -> int
(** [peek_byte t h x]: 핸들 [h] 의 논리 페이지를 이어 붙였을 때 [x] 번째
    바이트. 프레임에 떠 있는 페이지는 프레임에서 읽는다. [x] 는 핸들의
    페이지 안이어야 한다. *)

val poke_byte : Dos_state.t -> int -> int -> int -> unit
(** [peek_byte] 의 쓰기. 페이지가 떠 있으면 떠 있는 창 모두에 쓴다. *)

val target : Dos_state.t -> handle:int -> log:int -> ((int * int) option, status) result
(** 핸들 [handle] 의 논리 페이지 [log] 를 겹칠 수 있으면 [Some (handle, log)],
    [log] 가 FFFFh 면 [None](비우기 — 핸들은 보지 않는다). *)

val show : Dos_state.t -> int -> (int * int) option -> unit
(** [show t phys shown]: 물리 페이지 [phys] 에 [shown] 을 겹치거나 비운다.
    [phys] 는 [frame_pages] 보다 작고, [shown] 은 [target] 을 지난 값이다. *)

val physical_of_segment : int -> int option
(** 매핑 가능 세그먼트(D000h·D400h·D800h·DC00h)면 그 물리 페이지. *)

val allocate : Dos_state.t -> count:int -> zero_ok:bool -> (int, status) result
(** 새 핸들. [zero_ok] 는 Function 27 처럼 0 페이지를 받을지다. *)

val release : Dos_state.t -> int -> (unit, status) result
val reallocate : Dos_state.t -> int -> count:int -> (unit, status) result
val save_map : Dos_state.t -> int -> (unit, status) result
val restore_map : Dos_state.t -> int -> (unit, status) result

val name_bytes : int
(** 핸들 이름의 길이(8). *)

val name : Dos_state.t -> int -> string
(** 핸들의 이름. 이름이 없으면(없는 핸들도) NUL 여덟. *)

val set_name : Dos_state.t -> int -> string -> (unit, status) result
(** 이름(앞 여덟 바이트, 짧으면 NUL 로 채운다)을 붙인다. 모두 NUL 이면
    이름을 뗀다. 다른 핸들의 이름과 여덟 바이트가 같으면 [Name_taken] 이고
    아무것도 바뀌지 않는다. *)

val find_name : Dos_state.t -> string -> (int, status) result
(** 이름으로 핸들을 찾는다. 모두 NUL 이면 [Null_name], 없으면
    [Name_not_found]. *)

val names_ok : Dos_state.t -> bool
(** [ems_names] 가 있는 핸들만 가리키고, 이름이 모두 여덟 바이트·모두 NUL
    아님·서로 다름인지. 스냅숏이 저장과 복원 때 본다. *)
