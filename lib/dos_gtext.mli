(** 그래픽 모드에서 BIOS 가 글자를 그리고 창을 올리는 법.

    글자 칸은 가로 8 점, 세로 [Dos_video.char_height] 점이다. 행과 열은
    BIOS 의 글자 좌표다. 점은 [Dos_video.put_pixel] 로 찍으므로 색의 bit7
    은 256색이 아닌 모드에서 XOR 이다. 화면 밖 점은 찍지 않는다. *)

val draw :
  Dos_video.t -> glyph:(int -> int) -> col:int -> row:int -> fg:int -> bg:int -> unit
(** [glyph line] 은 글자의 한 줄(bit7 이 왼쪽 점)이다. 켜진 점은 [fg],
    꺼진 점은 [bg] 로 찍는다. 바탕을 [0x80] 으로 주면 XOR 0 이라 바탕이
    그대로 남는다. *)

val scroll :
  Dos_video.t -> up:bool -> lines:int -> top:int -> left:int -> bottom:int -> right:int ->
  fill:int -> unit
(** INT 10h AH=06h·07h 의 그래픽 판. 창을 [lines] 행 올리거나 내리고 빈
    행을 [fill] 색으로 채운다. [lines] 가 0 이거나 창보다 크면 창 전체를
    지운다. 채우는 색은 XOR 하지 않는다. *)
