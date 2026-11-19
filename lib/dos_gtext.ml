(* 그래픽 모드의 BIOS 글자. 계약은 dos_gtext.mli. *)

let cell_width = 8
let leftmost_dot = 0x80

(* 채우기는 XOR 하지 않는다 — bit7 을 떼고 찍는다. 256색 모드는 bit7 도
   색이다. *)
let plain_fill video fill =
  match Dos_video.kind video with
  | Dos_video.Linear256 -> fill
  | Dos_video.Text | Dos_video.Cga4 | Dos_video.Cga2 | Dos_video.Planar ->
    fill land lnot Dos_video.xor_pixel

let put video ~x ~y color =
  let w, h = Dos_video.dims video in
  if x >= 0 && x < w && y >= 0 && y < h then Dos_video.put_pixel video ~x ~y ~color

let draw video ~glyph ~col ~row ~fg ~bg =
  let height = Dos_video.char_height video in
  let x0 = col * cell_width and y0 = row * height in
  for line = 0 to height - 1 do
    let bits = glyph line in
    for dot = 0 to cell_width - 1 do
      let on = bits land (leftmost_dot lsr dot) <> 0 in
      put video ~x:(x0 + dot) ~y:(y0 + line) (if on then fg else bg)
    done
  done

let scroll video ~up ~lines ~top ~left ~bottom ~right ~fill =
  let height = Dos_video.char_height video in
  let bottom = min bottom (Dos_video.text_rows video - 1) in
  let right = min right (Dos_video.text_cols video - 1) in
  if top <= bottom && left <= right then begin
    let span = bottom - top + 1 in
    let lines = if lines = 0 || lines > span then span else lines in
    let fill = plain_fill video fill in
    let x0 = left * cell_width and x1 = ((right + 1) * cell_width) - 1 in
    let each_dot row f =
      for line = 0 to height - 1 do
        for x = x0 to x1 do
          f ~x ~y:((row * height) + line)
        done
      done
    in
    let copy ~src ~dst =
      let shift = (src - dst) * height in
      each_dot dst (fun ~x ~y ->
          let w, h = Dos_video.dims video in
          if x < w && y + shift < h then
            put video ~x ~y (plain_fill video (Dos_video.get_pixel video ~x ~y:(y + shift))))
    in
    let clear row = each_dot row (fun ~x ~y -> put video ~x ~y fill) in
    if up then
      for row = top to bottom do
        if row + lines > bottom then clear row else copy ~src:(row + lines) ~dst:row
      done
    else
      for row = bottom downto top do
        if row - lines < top then clear row else copy ~src:(row - lines) ~dst:row
      done
  end
