(** LIM EMS 4.0 스텁 — INT 67h.

    실기 386 의 EMM386/QEMM 자리. 드라이버 알아보기는 INT 21h AH=35h
    AL=67h 로 벡터를 읽어 그 세그먼트 +0x0A 의 이름 칸({"EMMXXXX0"})을
    보는 관례를 따른다(Dos_bios.install 이 심는다). 프레임은 0xD000-
    0xEFFF 넷, 페이지 내용은 호스트 배열에 둔다. *)

val page_frame : int
(** 물리 페이지 넷이 겹쳐지는 첫 세그먼트(0xD000). *)

val frame_pages : int

val page_bytes : int
(** 논리 페이지 하나의 크기(16KB). 아직 안 건드린 페이지는 빈 [Bytes]. *)

val first_handle : int
(** 할당되는 첫 핸들(1). 0 은 운영체제 몫이다. *)

val max_handle : int
(** 할당되는 마지막 핸들(254). 1..254 가 다 쓰이면 AH=43h 가 85h 로 답한다. *)

val emm_name : string
val emm_version : int

val service : Dos_state.t -> unit
(** INT 67h — AH 40h 상태, 41h 프레임, 42h 페이지 수, 43h 할당,
    44h 매핑, 45h 해제, 46h 버전, 58h 매핑 가능 주소. 그 외는 LIM
    오류 코드로 답한다. *)
