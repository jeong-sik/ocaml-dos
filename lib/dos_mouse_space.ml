(* 마우스 드라이버의 가상 화면 — 비디오 모드마다 좌표가 닿는 넓이와 한
   칸의 크기. 게임은 INT 33h 가 준 좌표를 이 넓이로 나눠 화면 자리를 얻는다.
   80열 텍스트는 640x200 이라 열 = x/8, 13h 는 640x200 이라 화소 = x/2 다.

   표는 CuteMouse ctmouse.asm(github.com/davidebreso/ctmouse 0dd8f32)
   setupvideo 의 것이다:

     모드        화면       칸
     0, 1        640x200   16x8   너비 = BDA 열 수 × 16
     2, 3, 7     640x200    8x8   너비 = BDA 열 수 × 8
     4, 5        640x200    2x1
     6           640x200    1x1
     0Dh         640x200    2x1
     0Eh         640x200    1x1
     0Fh, 10h    640x350    1x1
     11h, 12h    640x480    1x1
     13h         640x200    2x1
     그 밖       640x200    1x1

   텍스트 높이는 (BDA 마지막 줄 + 1) × 8 이고, 마지막 줄이 0 이면(그 바이트가
   없던 옛 기계) 200 이다. DOSBox-X mouse.cpp 는 40열 너비를 320 으로, 모드
   4·5 의 칸을 1 로 둔다. 40열 칸이 16 이면 너비는 640 이어야 40칸이 되니
   CuteMouse 를 따른다. *)

type t = { width : int; height : int; cell_w : int; cell_h : int }

let graphics_width = 640
let cga_height = 200
let ega_height = 350
let vga_height = 480
let text_cell_h = 8
let wide_text_cell_w = 16  (* 40열: 글자 하나가 640 의 16 *)
let text_cell_w = 8

let bda_mode = 0x449
let bda_columns = 0x44A
let bda_last_row = 0x484

let text ~columns ~last_row ~cell_w =
  let height = if last_row = 0 then cga_height else (last_row + 1) * text_cell_h in
  { width = columns * cell_w; height; cell_w; cell_h = text_cell_h }

let graphics ~height ~cell_w = { width = graphics_width; height; cell_w; cell_h = 1 }

let of_mode ~mode ~columns ~last_row =
  match mode with
  | 0x00 | 0x01 -> text ~columns ~last_row ~cell_w:wide_text_cell_w
  | 0x02 | 0x03 | 0x07 -> text ~columns ~last_row ~cell_w:text_cell_w
  | 0x04 | 0x05 | 0x0D | 0x13 -> graphics ~height:cga_height ~cell_w:2
  | 0x06 | 0x0E -> graphics ~height:cga_height ~cell_w:1
  | 0x0F | 0x10 -> graphics ~height:ega_height ~cell_w:1
  | 0x11 | 0x12 -> graphics ~height:vga_height ~cell_w:1
  | _ -> (* CuteMouse 의 "other": 표에 없는 모드 *) graphics ~height:cga_height ~cell_w:1

(* 지금 BDA 가 적은 모드. 드라이버가 INT 10h 모드 세우기를 가로채 다시 읽는다. *)
let current mem =
  of_mode ~mode:(Bytes.get_uint8 mem bda_mode)
    ~columns:(Bytes.get_uint16_le mem bda_columns)
    ~last_row:(Bytes.get_uint8 mem bda_last_row)

let range_x t = (0, t.width - 1)
let range_y t = (0, t.height - 1)
let center t = (t.width / 2, t.height / 2)

(* 게임에 알리는 좌표는 칸의 왼쪽 위로 내린다(RBIL INT 33/000C: "In text
   modes, the row and column will be reported as a multiple of the
   character cell size"). 칸은 2 의 거듭제곱이다. *)
let cell_x t x = x land lnot (t.cell_w - 1)
let cell_y t y = y land lnot (t.cell_h - 1)
