(** LIM EMS 4.0 드라이버 — INT 67h.

    실기 386 의 EMM386/QEMM 자리. 드라이버 알아보기는 INT 21h AH=35h
    AL=67h 로 벡터를 읽어 그 세그먼트 +0x0A 의 이름 칸({"EMMXXXX0"})을
    보는 관례를 따른다(Dos_bios.install 이 심는다). 프레임은 0xD000-
    0xEFFF 넷, 페이지 내용은 호스트 배열에 둔다.

    아래 상수와 [boot_handles] 는 [Dos_ems_pages] 의 것을 그대로 낸다.
    스냅숏·BIOS·기계가 모델 모듈을 따로 알지 않아도 된다. *)

val page_frame : int
(** 물리 페이지 넷이 겹쳐지는 첫 세그먼트(0xD000). *)

val frame_pages : int

val page_bytes : int
(** 논리 페이지 하나의 크기(16KB). 아직 안 건드린 페이지는 빈 [Bytes]. *)

val os_handle : int
(** 운영체제 핸들(0). 드라이버가 설치될 때부터 열려 있고 닫히지 않는다.
    이 드라이버에서는 페이지가 없다. *)

val max_handle : int
(** 할당되는 마지막 핸들(254). 1..254 가 다 쓰이면 AH=43h 가 85h 로 답한다. *)

val emm_name : string
val emm_version : int

val boot_handles : unit -> (int, Bytes.t array) Hashtbl.t
(** 드라이버가 설치된 직후의 핸들 표: 페이지 없는 운영체제 핸들 하나. *)

val service : Dos_state.t -> unit
(** INT 67h — AH 40h 상태, 41h 프레임, 42h 페이지 수, 43h 할당,
    44h 매핑(BX=FFFFh 는 비우기), 45h 해제, 46h 버전, 47h·48h 매핑
    저장·복원, 4Bh 핸들 수, 4Ch 핸들의 페이지 수, 4Dh 모든 핸들의 페이지
    수, 4Eh 매핑 배열 읽기·쓰기, 4Fh 부분 매핑 배열, 50h 여러 장 매핑,
    51h 재할당, 57h 메모리 영역 옮기기·바꾸기, 58h 매핑 가능 주소, 59h
    하드웨어 정보, 5Ah 0 페이지도 받는 할당. 그 외는 LIM 상태 코드로
    답한다. *)
