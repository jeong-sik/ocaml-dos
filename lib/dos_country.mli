(** 미국(국가 1), 코드 페이지 437 — MS-DOS 4.0
    커널 기본표 [src/DOS/DOSMES.ASM] 의 [COUNTRY_CDPG] 와 그 표들. *)

val country_code : int
(** 1(미국). *)

val code_page : int
(** 437. 지금 코드 페이지와 시스템 코드 페이지가 같다. *)

val upcase : int -> int
(** a-z 와, 80h..A7h 는 [UCASE_TAB] 를 따른다. 그 밖은 그대로다. *)

val collate : int -> int
(** [COLLATE_TAB] 의 정렬 값. *)

type answer = No | Yes | Neither

val answer : int -> answer
(** INT 21h AX=6523h: Y·y 는 [Yes], N·n 은 [No], 그 밖은 [Neither]. *)

(** ROM 안 표들의 자리. 오프셋은 모두 [segment] 안이다. *)
type layout = {
  segment : int;
  case_map : int;
  ucase : int;
  file_ucase : int;
  file_chars : int;
  collate : int;
  dbcs : int;
}

(** INT 21h AH=65h 가 far 포인터로 주는 표. *)
type table = Ucase | File_ucase | File_chars | Collate | Dbcs

val table_of_id : int -> table option
(** 65h 의 AL(2, 4, 5, 6, 7). 그 밖은 [None]. *)

val table_offset : layout -> table -> int
(** 표의 길이 워드 자리. *)

val country_info_id : int
(** 65h AL=01h. *)

val dbcs_ranges : layout -> int
(** INT 21h AX=6300h 가 DS:SI 로 주는 자리 — DBCS 표의 길이 워드 다음. *)

val info : layout -> string
(** INT 21h AH=38h 가 DS:DX 에 쓰는 24 바이트([DOSCNTRY.INC]
    [OLD_COUNTRY_SIZE]). *)

val ext_info : layout -> string
(** INT 21h AX=6501h 가 길이 워드 뒤에 쓰는 38 바이트([NEW_COUNTRY_SIZE]):
    나라, 코드 페이지, {!info}, 예약 10 바이트. *)

val image : segment:int -> at:int -> layout * string
(** 오프셋 [at] 부터 둘 바이트: 대문자 루틴([MAP_CASE] 처럼 FAR CALL, AL 이
    80h 이상이면 바꾼다), [UCASE_TAB], [FILE_UCASE_TAB], [FILE_CHAR_TAB],
    [COLLATE_TAB], [DBCS_TAB]. *)
