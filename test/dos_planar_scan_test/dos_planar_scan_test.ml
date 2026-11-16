(* EGA/VGA 16색 화면이 CRTC 를 따른다 — 시작 주소(페이지 넘김·스크롤),
   행 간격, 크기 레지스터. 게스트가 쓰는 길로만 확인한다: INT 10h, CRTC
   포트, 0xA0000 쓰기. 판정은 화면에 보이는 점, 프레임 크기와 RGB,
   포트에서 읽은 값이다. 모드별 CRTC 부팅값은 SeaBIOS
   vgasrc/stdvgamodes.c 의 표다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got=%d (0x%x) want=%d (0x%x)\n%!" name got got want want
  end

let check_dims name got (w, h) =
  check (name ^ " width") (fst got) w;
  check (name ^ " height") (snd got) h

(* ---------- 아주 작은 어셈블러 ---------- *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax n = "\xb8" ^ w n
let mov_bx n = "\xbb" ^ w n
let mov_cx n = "\xb9" ^ w n
let mov_dx n = "\xba" ^ w n
let mov_al n = "\xb0" ^ b n
let int_ n = "\xcd" ^ b n
let out_dx_ax = "\xef"
let out_dx_al = "\xee"
let in_al_dx = "\xec"
let quit = mov_ax 0x4c00 ^ int_ 0x21
let set_mode m = mov_ax m ^ int_ 0x10

(* ES = 0xA000 *)
let es_gfx = mov_ax 0xA000 ^ "\x8e\xc0"
let store_es off = "\x26\xa2" ^ w off            (* mov es:[off], al *)
let store_al off = "\xa2" ^ w off                (* mov [off], al *)

let reg port index value =
  mov_dx port ^ mov_ax (((value land 0xff) lsl 8) lor (index land 0xff)) ^ out_dx_ax

let seq = reg 0x3C4
let crtc = reg 0x3D4

let read_reg port index dst =
  mov_dx port ^ mov_al index ^ out_dx_al ^ mov_dx (port + 1) ^ in_al_dx ^ store_al dst

let map_mask m = seq 0x02 m
let put_es off v = mov_al v ^ store_es off
let start_at a = crtc 0x0C (a lsr 8) ^ crtc 0x0D a

(* 0x11 bit7 을 내려 0-7 번 레지스터의 쓰기 막음을 푼다. *)
let unprotect = crtc 0x11 0x0E

let psp_base = 0x1000 * 16
let peek m off = Dos_machine.mem_read m (psp_base + off)

let run_com code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (code ^ quit);
  Dos_machine.run m ~max_steps:100_000;
  if not (Dos_machine.exited m) then begin
    incr failed;
    prerr_endline "FAIL the program did not end"
  end;
  m

let px m x y = Dos_machine.pixel m ~x ~y

(* PPM 의 (x, y) 점. 머리글은 줄바꿈 세 개로 끝난다. *)
let rgb_at m x y =
  let ppm = Dos_machine.frame_ppm m in
  let rec skip i n = if n = 0 then i else skip (String.index_from ppm i '\n' + 1) (n - 1) in
  let body = skip 0 3 in
  let width, _ = Dos_machine.frame_dims m in
  let i = body + (((y * width) + x) * 3) in
  let c k = Char.code ppm.[i + k] in
  (c 0, c 1, c 2)

let check_rgb name got want =
  let r, g, b = got and wr, wg, wb = want in
  check (name ^ " R") r wr;
  check (name ^ " G") g wg;
  check (name ^ " B") b wb

(* ---------- 모드를 세운 직후 ---------- *)

let test_boot_tables () =
  List.iter
    (fun (mode, dims, offset) ->
      let m =
        run_com
          (set_mode mode ^ read_reg 0x3D4 0x13 0x2000 ^ read_reg 0x3D4 0x17 0x2001
           ^ read_reg 0x3D4 0x0C 0x2002 ^ read_reg 0x3D4 0x0D 0x2003)
      in
      let name = Printf.sprintf "mode %02Xh" mode in
      check_dims name (Dos_machine.frame_dims m) dims;
      check (name ^ " offset") (peek m 0x2000) offset;
      check (name ^ " byte mode") (peek m 0x2001) 0xE3;
      check (name ^ " start high") (peek m 0x2002) 0;
      check (name ^ " start low") (peek m 0x2003) 0)
    [ (0x0D, (320, 200), 0x14); (0x0E, (640, 200), 0x28);
      (0x0F, (640, 350), 0x28); (0x10, (640, 350), 0x28);
      (0x11, (640, 480), 0x28); (0x12, (640, 480), 0x28) ]

(* ---------- 시작 주소 ---------- *)

let page_one = 0x2000

let test_page_flip () =
  let draw = set_mode 0x0D ^ es_gfx ^ put_es page_one 0xFF in
  let before = run_com draw in
  check "page 1 is off screen" (px before 0 0) 0;
  let after = run_com (draw ^ start_at page_one) in
  check "start 2000h shows page 1" (px after 0 0) 15;
  check "its eighth dot" (px after 7 0) 15;
  check "the next byte" (px after 8 0) 0;
  check_rgb "the frame shows page 1" (rgb_at after 0 0) (255, 255, 255);
  check_rgb "the frame's next byte" (rgb_at after 8 0) (0, 0, 0);
  if Dos_machine.screen_digest before = Dos_machine.screen_digest after then begin
    incr failed;
    prerr_endline "FAIL the digest follows the start address"
  end

(* 점 하나를 평면 하나에만 써도 지문이 바뀐다. *)
let test_digest_sees_each_plane () =
  let blank = Dos_machine.screen_digest (run_com (set_mode 0x0D)) in
  for p = 0 to 3 do
    let m = run_com (set_mode 0x0D ^ es_gfx ^ map_mask (1 lsl p) ^ put_es 0 0x80) in
    check (Printf.sprintf "plane %d alone" p) (px m 0 0) (1 lsl p);
    if Dos_machine.screen_digest m = blank then begin
      incr failed;
      Printf.eprintf "FAIL the digest misses plane %d\n%!" p
    end
  done

let test_scroll_one_byte () =
  let m = run_com (set_mode 0x0D ^ es_gfx ^ put_es 1 0x80 ^ start_at 1) in
  check "byte 1 is at the left edge" (px m 0 0) 15;
  check "only its top bit is set" (px m 1 0) 0

let test_start_wraps () =
  let m = run_com (set_mode 0x0D ^ es_gfx ^ put_es 0 0xFF ^ start_at 0xFFFF) in
  check "byte FFFFh first" (px m 0 0) 0;
  check "then byte 0" (px m 8 0) 15

(* ---------- 행 간격 ---------- *)

let test_row_stride () =
  let draw =
    set_mode 0x0D ^ es_gfx ^ map_mask 0x01 ^ put_es 40 0xFF
    ^ map_mask 0x0F ^ put_es 80 0xFF
  in
  let narrow = run_com draw in
  check "40 bytes a row: row 1" (px narrow 0 1) 1;
  check "40 bytes a row: row 2" (px narrow 0 2) 15;
  let wide = run_com (draw ^ crtc 0x13 0x28) in
  check "80 bytes a row: row 1" (px wide 0 1) 15;
  check_rgb "80 bytes a row: frame row 1" (rgb_at wide 0 1) (255, 255, 255)

(* ---------- 크기 ---------- *)

let test_sizes () =
  let tall = run_com (set_mode 0x12 ^ crtc 0x12 0x5D) in
  check_dims "12h with 350 lines" (Dos_machine.frame_dims tall) (640, 350);
  let single = run_com (set_mode 0x0D ^ crtc 0x09 0x40) in
  check_dims "0Dh without doubling" (Dos_machine.frame_dims single) (320, 400);
  let wide = run_com (set_mode 0x0D ^ unprotect ^ crtc 0x01 0x4F) in
  check_dims "0Dh with 80 characters" (Dos_machine.frame_dims wide) (640, 200);
  let locked = run_com (set_mode 0x0D ^ crtc 0x01 0x4F) in
  check_dims "0Dh with the width locked" (Dos_machine.frame_dims locked) (320, 200)

(* BIOS 점 찍기는 모드표의 줄 너비(0Dh 는 40 바이트)를 쓴다. 화면 너비를
   80 글자로 넓혀도 (0,1) 은 바이트 40 이고, 행 간격은 그대로라 화면에서도
   (0,1) 에 보인다. *)
let test_bios_pixel () =
  let m =
    run_com
      (set_mode 0x0D ^ unprotect ^ crtc 0x01 0x4F
       ^ mov_ax 0x0C09 ^ mov_cx 0 ^ mov_dx 1 ^ int_ 0x10
       ^ mov_ax 0x0D00 ^ mov_cx 0 ^ mov_dx 1 ^ int_ 0x10 ^ store_al 0x2000)
  in
  check "BIOS read pixel" (peek m 0x2000) 9;
  check "BIOS pixel on screen" (px m 0 1) 9;
  check "not at byte 80" (px m 0 2) 0

(* ---------- 속성 팔레트 ---------- *)

(* 색 번호 1 을 DAC 3Fh 로 옮기고 그 DAC 을 (10,20,30) 으로 둔다. *)
let test_attribute_palette () =
  let m =
    run_com
      (set_mode 0x0D
       ^ mov_ax 0x1000 ^ mov_bx 0x3F01 ^ int_ 0x10
       ^ mov_ax 0x1010 ^ mov_bx 0x003F ^ mov_dx 0x0A00 ^ mov_cx 0x141E ^ int_ 0x10
       ^ es_gfx ^ map_mask 0x01 ^ put_es 0 0x80)
  in
  check "colour number" (px m 0 0) 1;
  check_rgb "through the attribute palette" (rgb_at m 0 0) (40, 80, 121)

(* ---------- 텍스트는 그대로 ---------- *)

let test_text_after_graphics () =
  let m = run_com (set_mode 0x0D ^ start_at page_one ^ crtc 0x13 0x28 ^ set_mode 0x03) in
  check_dims "text frame" (Dos_machine.frame_dims m) (640, 400)

let () =
  test_boot_tables ();
  test_page_flip ();
  test_digest_sees_each_plane ();
  test_scroll_one_byte ();
  test_start_wraps ();
  test_row_stride ();
  test_sizes ();
  test_bios_pixel ();
  test_attribute_palette ();
  test_text_after_graphics ();
  if !failed = 0 then print_endline "dos planar scan: all passed"
  else begin
    Printf.eprintf "dos planar scan: %d failures\n%!" !failed;
    exit 1
  end
