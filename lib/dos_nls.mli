(** 나라별 INT 21h 서비스 — MS-DOS 4.0 [GETSET.ASM]. 미국(1), 코드 페이지
    437 하나다. *)

val international : Dos_state.t -> unit
(** AH=38h: 나라 정보 24 바이트 얻기, 나라 바꾸기. *)

val lead_byte_table : Dos_state.t -> unit
(** AH=63h: AL=0 이면 DS:SI 에 DBCS 표(범위 없음). *)

val extended_country : Dos_state.t -> unit
(** AH=65h: AL=01h..07h 표 얻기, 20h..23h 대문자·YES/NO. *)

val code_page : Dos_state.t -> unit
(** AH=66h: AL=1 이면 BX·DX 에 코드 페이지. 바꾸기는 거절이다. *)
