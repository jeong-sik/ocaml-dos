(** 화면을 바깥이 읽을 수 있는 모양으로 바꾼다 — 판정용 텍스트와 픽셀.
    순수 함수다: 게스트 메모리와 팔레트만 읽고 아무 것도 바꾸지 않는다. *)

val cp437 : string array
(** 코드 페이지 437 의 256 글자를 UTF-8 로. 0x00-0x1F 와 0x7F 는 제어
    문자가 아니라 그림 문자로 읽는다. *)

val text_ascii : Bytes.t -> cols:int -> string
(** 텍스트 VRAM 을 개행 포함 ASCII 그리드로. ASCII 밖 글자는 공백이
    된다 — 사람이 눈으로 훑기 좋다. *)

val text_utf8 : Bytes.t -> cols:int -> string
(** 같은 화면을 CP437 그대로 UTF-8 로. 테두리·하트·화살표가 살아 있어
    기계 판정에 쓴다. *)

val rgb_text : Bytes.t -> cols:int -> string
(** 텍스트 화면을 RGB 로. 8x8 글리프를 세로로 두 배 늘려 25 줄이 400
    줄이 된다. 너비는 [cols] * 8. *)

val rgb_cga4 : Bytes.t -> mode:int -> color_select:int -> string
(** CGA 320x200 4색(모드 4·5). 네 색은 포트 0x3D9 가 고른다. *)

val rgb_cga2 : Bytes.t -> string
(** CGA 640x200 2색(모드 6). *)

val rgb_planar :
  Bytes.t array -> w:int -> h:int -> attr:int array ->
  pal:(int * int * int) array -> string
(** EGA/VGA 16색. 평면 넷의 같은 비트가 색 번호를 만들고, [attr] 이
    그것을 DAC 자리로 옮긴다. *)

val rgb_vga13 : Bytes.t -> (int * int * int) array -> string
(** VGA 13h 화면(0xA0000 선형)을 320x200 RGB 로. *)
