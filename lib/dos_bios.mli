(** BIOS 표면 — ROM 코드, 인터럽트 벡터표, BIOS 데이터 영역, 그리고
    INT 10h(비디오)·11h(장비)·12h(메모리)·16h(키보드)·1Ah(시각)·
    33h(마우스) 서비스. *)

val host_served : int list
(** 호스트가 구현을 갖고 있는 벡터. 이 벡터들은 IVT 에 실주소가 필요하다
    — 인터럽트를 IVT 직독으로 부르는 런타임이 있다. 스텁은 "int 실벡터
    +0x80; iret" 이고, 그 사설 벡터를 호스트 훅이 받는다. *)

val install : Dos_state.t -> unit
(** ROM·IVT·BDA 를 실기 부팅 상태로 채운다. *)

val ivt_is_own_stub : Dos_state.t -> int -> bool
(** 그 벡터가 아직 우리 스텁을 가리키는가 — 게스트가 가로챘으면 false. *)

val service : Dos_state.t -> int -> unit
(** 벡터 번호에 맞는 BIOS 서비스를 실행한다. 모르는 벡터는 아무 일도
    하지 않는다(실기의 IRET 스텁과 같다). *)
