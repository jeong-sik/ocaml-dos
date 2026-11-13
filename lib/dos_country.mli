(** 미국(국가 1), 코드 페이지 437 — MS-DOS 4.0
    커널 기본표 [src/DOS/DOSMES.ASM] 의 [COUNTRY_CDPG]·[UCASE_TAB]. *)

val country_code : int
(** 1(미국). *)

val upcase : int -> int
(** a-z 와, 80h..A7h 는 [UCASE_TAB] 를 따른다. 그 밖은 그대로다. *)

val info : case_map:int * int -> string
(** INT 21h AH=38h 가 DS:DX 에 쓰는 24 바이트([DOSCNTRY.INC]
    [OLD_COUNTRY_SIZE]). [case_map] 은 대문자 루틴의 (세그먼트, 오프셋). *)

val case_map_code : at:int -> string
(** 오프셋 [at] 에 둘 대문자 루틴과 그 뒤의 128 바이트 표(80h..FFh).
    FAR CALL 로 부르고, AL 이 80h 이상이면 대문자로 바꾼다. 다른 레지스터와
    플래그 밖의 상태는 그대로다. *)
