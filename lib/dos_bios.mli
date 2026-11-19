(** BIOS 표면 — ROM 코드, 인터럽트 벡터표, BIOS 데이터 영역, 그리고
    INT 10h(비디오)·11h(장비)·12h(메모리)·15h(시스템)·16h(키보드)·1Ah(시각)·
    33h(마우스) 서비스. *)

type bios_service =
  | Keyboard_irq | Rtc_irq | Video | Equipment | Memory_size | System | Keyboard | Time_of_day
  | Mouse
type host_vector =
  | Bios of bios_service
  | Dos_divide_overflow
  | Dos_terminate
  | Dos_call
  | Dos_keep
  | Multiplex
  | Ems

val host_vector : int -> host_vector option
(** 호스트가 구현을 갖고 있는 벡터. [install] 이 깔 스텁과
    [Dos_machine] 이 부를 서비스가 모두 이 함수에서 나온다. 이 벡터들은
    IVT 에 실주소가 필요하다 — 인터럽트를 IVT 직독으로 부르는 런타임이
    있다. 스텁은 "int 실벡터+0x80; iret" 이고, 그 사설 벡터를 호스트 훅이
    받는다. EMS(67h)는 디바이스 헤더 0 번지의 스텁이다. *)

val rom_seg : int
(** F000h, the segment of every ROM routine and stub. *)

val xms_entry_off : int
(** Where the XMS control function starts in the ROM segment; INT 2Fh
    AX=4310h hands out F000:[xms_entry_off]. *)

val xms_control_id : int
(** 43h: the service id {!private_service_vector} returns for the XMS
    control function's stub. It is not a vector: IVT[43h] keeps the
    graphics font pointer. *)

val install : Dos_state.t -> unit
(** ROM·IVT·BDA 를 실기 부팅 상태로 채운다. *)

val ivt_is_own_stub : Dos_state.t -> int -> bool
(** 그 벡터가 아직 우리 스텁을 가리키는가 — 게스트가 가로챘으면 false. *)

val ivt_is_iret_stub : Dos_state.t -> int -> bool
(** 그 벡터가 ROM 의 IRET 스텁을 가리키는가 — 이 기계가 구현하지 않은 벡터의
    부팅 값이다. *)

val private_service_vector : Dos_state.t -> int -> int option
(** The real vector only when the private INT was issued by its ROM stub.
    This entry invokes the host service without consulting a hooked IVT. *)

val read_key : Dos_state.t -> Dos_state.service_result
(** Complete a blocking BIOS key read, or retain the request without
    changing AX when the ring is empty. *)

val resume_wait : Dos_state.t -> Dos_state.service_result
(** INT 15h AH=86h 를 다시 본다: 끝 깃발(40:A1h bit 7)이 섰으면 CF 를 지우고
    끝, 아니면 계속 기다린다. 기계가 [Bios_wait] 을 이어 갈 때 부른다. *)

val service : Dos_state.t -> bios_service -> Dos_state.service_result
(** BIOS 서비스 하나를 실행한다. *)

val country : Dos_country.layout
(** 나라별 표의 자리. [install] 이 ROM 에 {!Dos_country.image} 를 싣는다. *)

val set_ticks_from_clock : Dos_state.t -> unit
(** BIOS 틱(0x46C)을 지금 시각의 시·분·초로 채우고 자정 넘김(0x470)을
    지운다. 부팅 때 BIOS 가 RTC 를 읽어 하는 일이다(SeaBIOS
    [clock_setup]). *)
