(* 화면을 바깥이 읽을 수 있는 모양으로 바꾼다 — 판정용 텍스트와 픽셀.
   순수 함수다: 게스트 메모리와 팔레트만 읽고 아무 것도 바꾸지 않는다. *)

let vram_base = 0xB8000
let rows = 25
let glyph_h = 8                      (* 폰트 한 글자의 줄 수 *)
let glyph_scale = 2                  (* 세로로 두 배 — 25 줄이 400 줄이 된다 *)

(* CGA 16색 — 속성 바이트의 하위 니블이 글자색, 상위 니블이 바탕색. *)
let cga_palette = Dos_ports.cga_palette

(* 코드 페이지 437 의 글자를 UTF-8 로. 0x00-0x1F 와 0x7F 는 제어 문자가
   아니라 그림 문자로 읽는다 — DOS 화면에서 그 자리는 하트·화살표이지
   줄바꿈이 아니다(ZZT 는 0x01·0x02 를 등장인물로 쓴다). *)
let cp437 = [|
  "\032"; "\226\152\186"; "\226\152\187"; "\226\153\165"; "\226\153\166"; "\226\153\163"; "\226\153\160"; "\226\128\162";
  "\226\151\152"; "\226\151\139"; "\226\151\153"; "\226\153\130"; "\226\153\128"; "\226\153\170"; "\226\153\171"; "\226\152\188";
  "\226\150\186"; "\226\151\132"; "\226\134\149"; "\226\128\188"; "\194\182"; "\194\167"; "\226\150\172"; "\226\134\168";
  "\226\134\145"; "\226\134\147"; "\226\134\146"; "\226\134\144"; "\226\136\159"; "\226\134\148"; "\226\150\178"; "\226\150\188";
  "\032"; "\033"; "\034"; "\035"; "\036"; "\037"; "\038"; "\039";
  "\040"; "\041"; "\042"; "\043"; "\044"; "\045"; "\046"; "\047";
  "\048"; "\049"; "\050"; "\051"; "\052"; "\053"; "\054"; "\055";
  "\056"; "\057"; "\058"; "\059"; "\060"; "\061"; "\062"; "\063";
  "\064"; "\065"; "\066"; "\067"; "\068"; "\069"; "\070"; "\071";
  "\072"; "\073"; "\074"; "\075"; "\076"; "\077"; "\078"; "\079";
  "\080"; "\081"; "\082"; "\083"; "\084"; "\085"; "\086"; "\087";
  "\088"; "\089"; "\090"; "\091"; "\092"; "\093"; "\094"; "\095";
  "\096"; "\097"; "\098"; "\099"; "\100"; "\101"; "\102"; "\103";
  "\104"; "\105"; "\106"; "\107"; "\108"; "\109"; "\110"; "\111";
  "\112"; "\113"; "\114"; "\115"; "\116"; "\117"; "\118"; "\119";
  "\120"; "\121"; "\122"; "\123"; "\124"; "\125"; "\126"; "\226\140\130";
  "\195\135"; "\195\188"; "\195\169"; "\195\162"; "\195\164"; "\195\160"; "\195\165"; "\195\167";
  "\195\170"; "\195\171"; "\195\168"; "\195\175"; "\195\174"; "\195\172"; "\195\132"; "\195\133";
  "\195\137"; "\195\166"; "\195\134"; "\195\180"; "\195\182"; "\195\178"; "\195\187"; "\195\185";
  "\195\191"; "\195\150"; "\195\156"; "\194\162"; "\194\163"; "\194\165"; "\226\130\167"; "\198\146";
  "\195\161"; "\195\173"; "\195\179"; "\195\186"; "\195\177"; "\195\145"; "\194\170"; "\194\186";
  "\194\191"; "\226\140\144"; "\194\172"; "\194\189"; "\194\188"; "\194\161"; "\194\171"; "\194\187";
  "\226\150\145"; "\226\150\146"; "\226\150\147"; "\226\148\130"; "\226\148\164"; "\226\149\161"; "\226\149\162"; "\226\149\150";
  "\226\149\149"; "\226\149\163"; "\226\149\145"; "\226\149\151"; "\226\149\157"; "\226\149\156"; "\226\149\155"; "\226\148\144";
  "\226\148\148"; "\226\148\180"; "\226\148\172"; "\226\148\156"; "\226\148\128"; "\226\148\188"; "\226\149\158"; "\226\149\159";
  "\226\149\154"; "\226\149\148"; "\226\149\169"; "\226\149\166"; "\226\149\160"; "\226\149\144"; "\226\149\172"; "\226\149\167";
  "\226\149\168"; "\226\149\164"; "\226\149\165"; "\226\149\153"; "\226\149\152"; "\226\149\146"; "\226\149\147"; "\226\149\171";
  "\226\149\170"; "\226\148\152"; "\226\148\140"; "\226\150\136"; "\226\150\132"; "\226\150\140"; "\226\150\144"; "\226\150\128";
  "\206\177"; "\195\159"; "\206\147"; "\207\128"; "\206\163"; "\207\131"; "\194\181"; "\207\132";
  "\206\166"; "\206\152"; "\206\169"; "\206\180"; "\226\136\158"; "\207\134"; "\206\181"; "\226\136\169";
  "\226\137\161"; "\194\177"; "\226\137\165"; "\226\137\164"; "\226\140\160"; "\226\140\161"; "\195\183"; "\226\137\136";
  "\194\176"; "\226\136\153"; "\194\183"; "\226\136\154"; "\226\129\191"; "\194\178"; "\226\150\160"; "\194\160"
|]

let text_grid mem ~cols ~render =
  let b = Buffer.create (cols * rows * 2) in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let ch = Char.code (Bytes.get mem (vram_base + (((r * cols) + c) * 2))) in
      Buffer.add_string b (render ch)
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let text_ascii mem ~cols =
  text_grid mem ~cols ~render:(fun ch ->
      if ch >= 32 && ch < 127 then String.make 1 (Char.chr ch) else " ")

let text_utf8 mem ~cols = text_grid mem ~cols ~render:(fun ch -> cp437.(ch))

(* 6비트 DAC 값을 8비트로. 팔레트 배열은 바깥에서도 만질 수 있으니
   범위를 벗어난 값은 자른다 — 그림을 그리다 기계를 죽일 일은 아니다. *)
let dac8 v = (max 0 (min 63 v) * 255) / 63

let rgb_of_dac pal i =
  let r, g, b = pal.(i land 0xff) in
  (dac8 r, dac8 g, dac8 b)

let blit img ~width ~x ~y (r, g, b) =
  let i = (((y * width) + x) * 3) in
  Bytes.set img i (Char.chr r);
  Bytes.set img (i + 1) (Char.chr g);
  Bytes.set img (i + 2) (Char.chr b)

(* 텍스트: 셀마다 8x8 글리프를 세로로 두 줄씩 찍는다. 한 번만 그리면
   위쪽 절반만 차고 아래가 검게 남는다. 폰트는 1비트/행 — 최상위 비트가
   왼쪽이다. 바탕색은 상위 니블의 아래 세 비트만 쓴다. 네 번째 비트는
   깜빡임 속성이지 색이 아니다. *)
let rgb_text mem ~cols =
  let width = cols * 8 in
  let height = rows * glyph_h * glyph_scale in
  let img = Bytes.make (width * height * 3) '\000' in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let cell = vram_base + (((r * cols) + c) * 2) in
      let ch = Char.code (Bytes.get mem cell) in
      let attr = Char.code (Bytes.get mem (cell + 1)) in
      let fg = cga_palette.(attr land 0x0f) in
      let bg = cga_palette.((attr lsr 4) land 0x07) in
      let glyph = Font8x8.glyph ch in
      for gy = 0 to glyph_h - 1 do
        let bits = glyph.(gy) in
        for dup = 0 to glyph_scale - 1 do
          let y = (r * glyph_h * glyph_scale) + (gy * glyph_scale) + dup in
          for gx = 0 to 7 do
            let on = bits land (0x80 lsr gx) <> 0 in
            blit img ~width ~x:((c * 8) + gx) ~y (if on then fg else bg)
          done
        done
      done
    done
  done;
  Bytes.to_string img

(* CGA 그래픽은 짝수 줄과 홀수 줄이 8KB 떨어져 있다. *)
let cga_row mem y ~bytes_per_row =
  vram_base + (if y land 1 = 0 then 0 else 0x2000) + ((y / 2) * bytes_per_row)
  |> fun a -> (mem, a)

(* 모드 4/5 의 네 가지 색은 고정 팔레트 셋 중 하나에서 온다. 어느 것인지
   는 포트 0x3D9 가 정한다: bit5 가 팔레트, bit4 가 밝기, 하위 니블이
   바탕색. 모드 5 는 bit5 와 상관없이 청록·빨강·흰색을 쓴다. *)
let cga4_colors ~mode ~color_select =
  let bg = color_select land 0x0f in
  let bright = if color_select land 0x10 <> 0 then 8 else 0 in
  if mode = 5 then [| bg; 3 + bright; 4 + bright; 7 + bright |]
  else if color_select land 0x20 <> 0 then
    [| bg; 3 + bright; 5 + bright; 7 + bright |]
  else [| bg; 2 + bright; 4 + bright; 6 + bright |]

let rgb_cga4 mem ~mode ~color_select =
  let width = 320 and height = 200 in
  let colors = cga4_colors ~mode ~color_select in
  let img = Bytes.make (width * height * 3) '\000' in
  for y = 0 to height - 1 do
    let mem, row = cga_row mem y ~bytes_per_row:80 in
    for x = 0 to width - 1 do
      let byte = Char.code (Bytes.get mem (row + (x / 4))) in
      let idx = (byte lsr ((3 - (x mod 4)) * 2)) land 3 in
      blit img ~width ~x ~y cga_palette.(colors.(idx) land 0x0f)
    done
  done;
  Bytes.to_string img

let rgb_cga2 mem =
  let width = 640 and height = 200 in
  let img = Bytes.make (width * height * 3) '\000' in
  for y = 0 to height - 1 do
    let mem, row = cga_row mem y ~bytes_per_row:80 in
    for x = 0 to width - 1 do
      let byte = Char.code (Bytes.get mem (row + (x / 8))) in
      let on = (byte lsr (7 - (x mod 8))) land 1 = 1 in
      blit img ~width ~x ~y cga_palette.(if on then 15 else 0)
    done
  done;
  Bytes.to_string img

(* EGA/VGA 16색: 평면 넷의 같은 비트를 모아 색 번호를 만들고, 속성
   팔레트가 그것을 DAC 자리로 옮긴다. *)
let rgb_planar planes ~w ~h ~attr ~pal =
  let img = Bytes.make (w * h * 3) '\000' in
  let bytes_per_row = w / 8 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let off = ((y * bytes_per_row) + (x / 8)) land 0xffff in
      let bit = 7 - (x mod 8) in
      let idx = ref 0 in
      for p = 0 to Array.length planes - 1 do
        if (Char.code (Bytes.get planes.(p) off) lsr bit) land 1 = 1 then
          idx := !idx lor (1 lsl p)
      done;
      blit img ~width:w ~x ~y (rgb_of_dac pal attr.(!idx land 0x0f))
    done
  done;
  Bytes.to_string img

(* VGA 13h: 0xA0000 선형 320x200, DAC 은 채널당 6비트. *)
let rgb_vga13 mem pal =
  let width = 320 and height = 200 in
  let img = Bytes.make (width * height * 3) '\000' in
  for i = 0 to (width * height) - 1 do
    let r, g, b = rgb_of_dac pal (Char.code (Bytes.get mem (0xA0000 + i))) in
    Bytes.set img (i * 3) (Char.chr r);
    Bytes.set img ((i * 3) + 1) (Char.chr g);
    Bytes.set img ((i * 3) + 2) (Char.chr b)
  done;
  Bytes.to_string img
