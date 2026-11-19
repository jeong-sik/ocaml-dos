(* The mouse driver draws its cursor into video memory. See
   dos_mouse_cursor.mli for the rules and their source. *)

open Dos_state

let cursor_cols = 16
let leftmost_bit = 1 lsl (cursor_cols - 1)

(* DOSBox-X DrawCursor: "pixel = pixel ^ 0x0F" *)
let cursor_xor_color = 0x0F

let word_sign_bit = 0x8000
let word_span = 0x10000

(* DOSBox-X mouse.cpp defaultScreenMask and defaultCursorMask (GPL-2; the
   values only). The screen mask keeps an arrow-shaped hole open and the
   cursor mask lights the arrow inside it. *)
let default_screen_mask =
  [| 0x3FFF; 0x1FFF; 0x0FFF; 0x07FF; 0x03FF; 0x01FF; 0x00FF; 0x007F;
     0x003F; 0x001F; 0x01FF; 0x00FF; 0x30FF; 0xF87F; 0xF87F; 0xFCFF |]

let default_cursor_mask =
  [| 0x0000; 0x4000; 0x6000; 0x7000; 0x7800; 0x7C00; 0x7E00; 0x7F00;
     0x7F80; 0x7C00; 0x6C00; 0x4600; 0x0600; 0x0300; 0x0300; 0x0000 |]

(* INT 33h AX=09h takes the hot spot in BX and CX as words; a negative hot
   spot puts the cursor right of or below the mouse. *)
let signed_word v = if v land word_sign_bit <> 0 then v - word_span else v

let restore t =
  let m = t.mouse in
  (match m.mouse_drawn with
   | Not_drawn -> ()
   | Drawn_text { page; col; row; under } -> write_cell_on t ~page row col under
   | Drawn_graphics { left; top; width; height; under } ->
     for r = 0 to height - 1 do
       for c = 0 to width - 1 do
         Dos_video.put_pixel t.video ~x:(left + c) ~y:(top + r) ~color:under.((r * width) + c)
       done
     done);
  m.mouse_drawn <- Not_drawn

let draw_text t (space : Dos_mouse_space.t) =
  let m = t.mouse in
  match m.mouse_text_cursor with
  | Hardware_text _ -> ()
  | Software_text { screen_mask; cursor_mask } ->
    let col = m.mouse_x / space.cell_w and row = m.mouse_y / space.cell_h in
    (* 07h·08h may let the mouse past the last cell; nothing to draw there *)
    if col < cols t && row < rows t then begin
      let page = active_page t in
      let under = read_cell_on t ~page row col in
      write_cell_on t ~page row col ((under land screen_mask) lxor cursor_mask);
      m.mouse_drawn <- Drawn_text { page; col; row; under }
    end

let draw_graphics t (space : Dos_mouse_space.t) =
  let m = t.mouse in
  let hot_x, hot_y, screen_mask, cursor_mask =
    match m.mouse_graphics_cursor with
    | Default_arrow ->
      let hx, hy = default_hot_spot in
      (hx, hy, default_screen_mask, default_cursor_mask)
    | Custom_cursor { hot_x; hot_y; screen_mask; cursor_mask } ->
      (signed_word hot_x, signed_word hot_y, screen_mask, cursor_mask)
  in
  let screen_w, screen_h = Dos_video.dims t.video in
  let x0 = (m.mouse_x / space.cell_w) - hot_x and y0 = (m.mouse_y / space.cell_h) - hot_y in
  let left = max 0 x0 and top = max 0 y0 in
  let right = min (screen_w - 1) (x0 + cursor_cols - 1)
  and bottom = min (screen_h - 1) (y0 + cursor_rows - 1) in
  if left <= right && top <= bottom then begin
    let width = right - left + 1 and height = bottom - top + 1 in
    let under = Array.make (width * height) 0 in
    for y = top to bottom do
      let row = y - y0 in
      for x = left to right do
        let bit = leftmost_bit lsr (x - x0) in
        let below = Dos_video.get_pixel t.video ~x ~y in
        under.(((y - top) * width) + (x - left)) <- below;
        let kept = if screen_mask.(row) land bit <> 0 then below else 0 in
        let color = if cursor_mask.(row) land bit <> 0 then kept lxor cursor_xor_color else kept in
        Dos_video.put_pixel t.video ~x ~y ~color
      done
    done;
    m.mouse_drawn <- Drawn_graphics { left; top; width; height; under }
  end

let draw t =
  let m = t.mouse in
  if m.mouse_present && m.mouse_cursor_flag = 0 then begin
    restore t;
    let space = Dos_mouse_space.current t.mem in
    match Dos_video.kind t.video with
    | Dos_video.Text -> draw_text t space
    | Dos_video.Cga4 | Dos_video.Cga2 | Dos_video.Planar | Dos_video.Linear256 ->
      draw_graphics t space
  end
