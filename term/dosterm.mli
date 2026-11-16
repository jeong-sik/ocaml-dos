(** 터미널과 DOS 기계 사이의 순수한 변환. 터미널이 보낸 바이트를 사건으로
    나누고, 프레임을 Kitty 그래픽 이스케이프로 만들고, 터미널 픽셀을 마우스
    드라이버 좌표로 옮긴다. 입출력과 시간은 [bin/dosplay.ml] 이 맡는다.

    키는 Kitty 키보드 프로토콜의 disambiguate 단계(flags=1)를 가정한다:
    Esc·Ctrl·Alt 조합과 기능 키가 [CSI … u] / [CSI … ~] / [CSI 1;m A] 꼴로
    온다. 그 프로토콜을 모르는 터미널의 옛 꼴(SS3, ESC+글자, C0 제어 문자)도
    받는다. 마우스는 SGR 픽셀 모드(1006·1016)다. *)

type mouse = {
  x : int;  (** 텍스트 영역 왼쪽 위에서 잰 픽셀, 0부터 *)
  y : int;
  code : int;  (** SGR 버튼 코드: 아래 두 비트가 버튼, 32 는 움직임, 64 는 휠 *)
  released : bool;  (** 끝 글자가 [m] 이다 *)
}

type event =
  | Key of int  (** BIOS 워드 — (스캔 코드 lsl 8) lor ASCII *)
  | Unmapped of string  (** 이 기계가 넣을 워드가 없는 키. 받은 바이트 그대로 *)
  | Mouse of mouse
  | Text_area of { width : int; height : int }  (** [CSI 14 t] 의 답, 픽셀 *)
  | Cells of { cols : int; rows : int }  (** [CSI 18 t] 의 답 *)
  | Quit  (** F12 — 실행기를 끝낸다 *)

val parse : string -> event list * string
(** 지금까지 받은 바이트를 사건으로 나눈다. 끝에서 끊긴 이스케이프는 사건
    으로 만들지 않고 둘째 값으로 돌려준다 — 다음에 받은 바이트 앞에 붙여
    다시 부른다. *)

val lone_escape : string -> event list
(** [parse] 가 남긴 꼬리가 ESC 한 바이트면 Esc 키다. Kitty 프로토콜을 모르는
    터미널은 Esc 를 그렇게 보내므로, 실행기는 꼬리가 한동안 그대로일 때 이것을
    부른다. 다른 꼬리는 [[]] 다. *)

val buttons : int -> mouse -> int
(** INT 33h 버튼 비트(bit0 왼쪽, bit1 오른쪽, bit2 가운데)를 마우스 사건
    하나만큼 바꾼다. 움직임과 휠은 버튼을 바꾸지 않는다. *)

val mouse_space : mode:int -> frame_width:int -> int * int
(** 마우스 드라이버의 가상 화면 크기. DOSBox-X [src/ints/mouse.cpp]
    [Mouse_AfterNewVideoMode] 를 따른다: 텍스트는 (열×8, 200), 4·5·6·0Dh·
    0Eh·13h 는 640x200, 0Fh·10h 는 640x350, 11h·12h 와 그 밖은 640x480.
    텍스트의 열 수는 프레임 너비 / 8 이다. *)

val to_guest : box:int * int -> space:int * int -> int * int -> int * int
(** 화면 상자([box], 픽셀) 안의 점을 가상 화면 좌표로. 상자 밖은 가장자리로
    자른다. *)

val fit : cols:int -> rows:int -> cell_w:int -> cell_h:int -> int * int
(** 터미널([cols]×[rows] 칸, 칸 하나는 [cell_w]×[cell_h] 픽셀)에 들어가는
    가장 큰 4:3 상자의 칸 수. 둘 다 1 이상이다. *)

val base64 : string -> string
(** RFC 4648 base64, 줄바꿈 없음. *)

val image :
  id:int -> width:int -> height:int -> cols:int -> rows:int -> string -> string
(** RGB 바이트([width]×[height]×3)를 보내고 커서 자리에 [cols]×[rows] 칸
    크기로 보이게 하는 Kitty 그래픽 이스케이프들. 커서는 움직이지 않고
    (C=1), 터미널은 답하지 않는다(q=2). base64 를 4096 바이트씩 나눠
    보낸다. *)

val delete_image : int -> string
(** 그 id 의 그림과 자리를 지우고 데이터도 버린다. *)
