(* 그래픽 모드 회귀 — CGA 4색·2색, EGA/VGA 16색 평면, 40열 텍스트.
   게스트가 실제로 쓰는 길로만 확인한다: INT 10h, 그래픽 컨트롤러 포트,
   0xA0000 읽기-쓰기. 판정은 화면에서 읽은 값과 그려진 픽셀이다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got=%d (0x%x) want=%d (0x%x)\n%!" name got got want
      want
  end

let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s:\n  got=%S\n  want=%S\n%!" name got want
  end

let check_true name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* ---------- 아주 작은 어셈블러 ---------- *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ah n = "\xb4" ^ b n
let mov_al n = "\xb0" ^ b n
let mov_ax n = "\xb8" ^ w n
let mov_bx n = "\xbb" ^ w n
let mov_cx n = "\xb9" ^ w n
let mov_dx n = "\xba" ^ w n
let int_ n = "\xcd" ^ b n
let out_dx_ax = "\xef"
let store_ax a = "\xa3" ^ w a
let quit = mov_ax 0x4c00 ^ int_ 0x21

(* ES = 0xA000 — 그래픽 창 *)
let es_gfx = mov_ax 0xA000 ^ "\x8e\xc0"
let load_latch off = "\x26\xa0" ^ w off          (* mov al, es:[off] *)
let store_es off = "\x26\xa2" ^ w off            (* mov es:[off], al *)

(* 그래픽 컨트롤러와 시퀀서는 index 포트와 data 포트가 나란히 있다.
   워드 출력 하나가 둘을 한 번에 채운다 — 실기 코드의 관용구다. *)
let idx_data index value =
  mov_ax (((value land 0xff) lsl 8) lor (index land 0xff)) ^ out_dx_ax

let gc_port = mov_dx 0x3CE
let seq_port = mov_dx 0x3C4

let set_mode m = mov_ax m ^ int_ 0x10

let assemble code_of data =
  let off = 0x100 + String.length (code_of 0) in
  code_of off ^ data

let psp_base = 0x1000 * 16
let scratch = 0x2000
let peek m off = Dos_machine.mem_read m (psp_base + off)

let run_com ?(steps = 500_000) code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:steps;
  m

let rgb_at m ~x ~y =
  let width, _ = Dos_machine.frame_dims m in
  let f = Dos_machine.frame_rgb m in
  let i = (((y * width) + x) * 3) in
  (Char.code f.[i], Char.code f.[i + 1], Char.code f.[i + 2])

(* ---------- EGA/VGA 16색 평면 ---------- *)

let test_bios_pixel_mode_0d () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x0D
           ^ mov_ah 0x0C ^ mov_al 9 ^ mov_bx 0 ^ mov_cx 17 ^ mov_dx 5
           ^ int_ 0x10
           ^ mov_ah 0x0D ^ mov_bx 0 ^ mov_cx 17 ^ mov_dx 5 ^ int_ 0x10
           ^ store_ax scratch ^ quit)
         "")
  in
  check "모드 0Dh" (Dos_machine.video_mode m) 0x0D;
  check_true "0Dh 크기" (Dos_machine.frame_dims m = (320, 200));
  check "AH=0Dh 로 되읽은 색" (peek m scratch) 9;
  check "화면에서 읽은 색" (Dos_machine.pixel m ~x:17 ~y:5) 9;
  (* 색 9 는 기본 팔레트에서 밝은 파랑 *)
  check_true "그려진 색" (rgb_at m ~x:17 ~y:5 = (0x55, 0x55, 0xFF));
  check "옆 점은 그대로" (Dos_machine.pixel m ~x:16 ~y:5) 0

(* 실기 EGA 코드의 관용구: set/reset 에 색을 넣고 비트 마스크로 점 하나를
   고른 뒤, 한 번 읽어 래치를 채우고 같은 자리에 쓴다. 래치를 안 채우면
   나머지 일곱 점이 지워진다. *)
let test_set_reset_bit_mask () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x0D
           (* 먼저 첫 바이트의 여덟 점을 색 7 로 채운다: 마스크 전체 *)
           ^ gc_port ^ idx_data 0 7 ^ idx_data 1 0x0F ^ idx_data 8 0xFF
           ^ es_gfx ^ load_latch 0 ^ store_es 0
           (* 그 다음 x=3 한 점만 색 5 로 *)
           ^ gc_port ^ idx_data 0 5 ^ idx_data 8 0x10
           ^ load_latch 0 ^ store_es 0 ^ quit)
         "")
  in
  check "마스크로 고른 점" (Dos_machine.pixel m ~x:3 ~y:0) 5;
  check "왼쪽 이웃은 보존" (Dos_machine.pixel m ~x:2 ~y:0) 7;
  check "오른쪽 이웃은 보존" (Dos_machine.pixel m ~x:4 ~y:0) 7;
  check "여덟 번째 점도 보존" (Dos_machine.pixel m ~x:7 ~y:0) 7

(* 쓰기 모드 1 은 래치를 그대로 옮긴다 — 화면 조각을 옆으로 복사하는 길. *)
let test_write_mode_1_copies_latches () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x0D
           ^ gc_port ^ idx_data 0 6 ^ idx_data 1 0x0F ^ idx_data 8 0xFF
           ^ es_gfx ^ load_latch 0 ^ store_es 0   (* 첫 바이트를 색 6 으로 *)
           ^ gc_port ^ idx_data 5 1                      (* 쓰기 모드 1 *)
           ^ load_latch 0 ^ store_es 40            (* 래치를 다음 줄로 *)
           ^ quit)
         "")
  in
  check "원본" (Dos_machine.pixel m ~x:0 ~y:0) 6;
  check "래치 복사본" (Dos_machine.pixel m ~x:0 ~y:1) 6;
  check "복사본의 마지막 점" (Dos_machine.pixel m ~x:7 ~y:1) 6

(* 시퀀서의 평면 마스크가 막은 평면에는 아무 것도 안 들어간다. *)
let test_map_mask_blocks_planes () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x0D
           ^ gc_port ^ idx_data 0 0x0F ^ idx_data 1 0x0F ^ idx_data 8 0xFF
           ^ seq_port ^ idx_data 2 0x03          (* 평면 0,1 만 쓰기 허용 *)
           ^ es_gfx ^ load_latch 0 ^ store_es 0
           ^ quit)
         "")
  in
  (* 색 15 를 쓰려 했지만 평면 2,3 이 막혀 3 만 남는다 *)
  check "막힌 평면은 0" (Dos_machine.pixel m ~x:0 ~y:0) 3

let test_attribute_palette_remaps_color () =
  let m =
    run_com
      (assemble
         (fun off ->
           set_mode 0x0D
           ^ mov_ah 0x0C ^ mov_al 1 ^ mov_bx 0 ^ mov_cx 0 ^ mov_dx 0
           ^ int_ 0x10
           (* 색 1 을 DAC 4(빨강) 자리로 옮긴다: AX=1000, BL=색, BH=자리 *)
           ^ mov_ax 0x1000 ^ mov_bx 0x0401 ^ int_ 0x10
           ^ (ignore off; quit))
         "")
  in
  check "점의 색 번호는 그대로" (Dos_machine.pixel m ~x:0 ~y:0) 1;
  check_true "그려지는 색은 바뀐다" (rgb_at m ~x:0 ~y:0 = (0xAA, 0x00, 0x00))

let test_mode_12_dims_and_corner () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x12
           ^ mov_ah 0x0C ^ mov_al 15 ^ mov_bx 0 ^ mov_cx 639 ^ mov_dx 479
           ^ int_ 0x10 ^ quit)
         "")
  in
  check_true "12h 크기" (Dos_machine.frame_dims m = (640, 480));
  check "오른쪽 아래 모서리" (Dos_machine.pixel m ~x:639 ~y:479) 15;
  check_true "모서리가 흰색" (rgb_at m ~x:639 ~y:479 = (0xFF, 0xFF, 0xFF))

let test_mode_set_clears_planes () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x0D
           ^ mov_ah 0x0C ^ mov_al 12 ^ mov_bx 0 ^ mov_cx 3 ^ mov_dx 3
           ^ int_ 0x10
           ^ set_mode 0x0D                       (* 다시 세우면 지워진다 *)
           ^ quit)
         "")
  in
  check "모드를 다시 세우면 평면이 비워진다" (Dos_machine.pixel m ~x:3 ~y:3) 0

(* DAC 쓰기: 포트 0x3C8 에 자리, 0x3C9 에 R·G·B 차례로 *)
let dac_set idx (r, g, bl) =
  mov_dx 0x3C8 ^ mov_al idx ^ "\xee"
  ^ mov_dx 0x3C9 ^ mov_al r ^ "\xee" ^ mov_al g ^ "\xee" ^ mov_al bl ^ "\xee"

(* 실기 BIOS 는 모드를 세울 때 DAC 을 기본값으로 다시 싣는다. 앞
   프로그램이 페이드아웃으로 DAC 을 검게 두고 끝나도, 다음 프로그램이
   모드만 세우면 기본색이 보인다(삼국지3 카피프로텍션 화면이 이 길이다).
   AL bit7(화면 보존)도 팔레트 싣기는 막지 않는다. *)
let test_mode_set_reloads_dac () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x12
           ^ dac_set 15 (0, 0, 0)                (* 색 15 를 검게 *)
           ^ mov_dx 0x3C6 ^ mov_al 0x00 ^ "\xee" (* PEL 마스크도 닫는다 *)
           ^ set_mode 0x92                       (* 12h, 화면 보존 *)
           ^ mov_ah 0x0C ^ mov_al 15 ^ mov_bx 0 ^ mov_cx 10 ^ mov_dx 10
           ^ int_ 0x10
           ^ mov_dx 0x3C6 ^ "\xec" ^ store_ax scratch
           ^ quit)
         "")
  in
  check "점의 색 번호" (Dos_machine.pixel m ~x:10 ~y:10) 15;
  check_true "모드를 세우면 색 15 는 다시 흰색"
    (rgb_at m ~x:10 ~y:10 = (0xFF, 0xFF, 0xFF));
  check "PEL 마스크도 처음 값" (peek m scratch land 0xff) 0xFF

(* BDA 0x489 bit3 은 "기본 팔레트 싣기 금지" 다. 켜져 있으면 모드를
   세워도 게스트가 쓴 DAC 이 남는다. *)
let test_mode_set_keeps_dac_when_inhibited () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x12
           ^ dac_set 15 (63, 0, 0)               (* 색 15 를 빨강으로 *)
           ^ mov_ax 0x0040 ^ "\x8e\xc0"          (* es = 0040h *)
           ^ "\x26\x80\x0e\x89\x00\x08"         (* or byte es:[89h],08h *)
           ^ set_mode 0x12
           ^ mov_ah 0x0C ^ mov_al 15 ^ mov_bx 0 ^ mov_cx 10 ^ mov_dx 10
           ^ int_ 0x10 ^ quit)
         "")
  in
  check_true "금지 비트가 켜지면 게스트의 DAC 이 남는다"
    (rgb_at m ~x:10 ~y:10 = (0xFF, 0x00, 0x00))

(* 텍스트 화면의 글자도 IBM 순서로 그린다 — 최상위 비트가 왼쪽 점.
   '/' 는 맨 윗줄이 오른쪽, 일곱째 줄이 맨 왼쪽에 점이 있다. *)
let test_text_glyph_not_mirrored () =
  let m =
    run_com
      (assemble (fun off -> set_mode 0x03 ^ mov_ah 0x09 ^ mov_dx off
                             ^ int_ 0x21 ^ quit) "/$")
  in
  let lit x y = rgb_at m ~x ~y <> (0, 0, 0) in
  (* 한 글리프 줄이 화면 두 줄이다: 글리프 6 번 줄은 y=12,13 *)
  check_true "'/' 일곱째 줄의 맨 왼쪽 점이 켜진다" (lit 0 12);
  check_true "'/' 맨 윗줄의 맨 왼쪽 점은 꺼져 있다" (not (lit 0 0));
  check_true "'/' 맨 윗줄은 오른쪽에 점" (lit 5 0 || lit 6 0)

(* ---------- CGA ---------- *)

let test_cga_mode_4 () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x04
           ^ mov_ah 0x0C ^ mov_al 2 ^ mov_bx 0 ^ mov_cx 9 ^ mov_dx 1
           ^ int_ 0x10
           (* 포트 0x3D9: 팔레트 1(청록·자홍·흰색) + 밝기 *)
           ^ mov_dx 0x3D9 ^ mov_al 0x30 ^ "\xee"
           ^ quit)
         "")
  in
  check_true "4 번 모드 크기" (Dos_machine.frame_dims m = (320, 200));
  check "홀수 줄의 점" (Dos_machine.pixel m ~x:9 ~y:1) 2;
  (* 팔레트 1 + 밝기에서 색 번호 2 는 밝은 자홍 *)
  check_true "CGA 팔레트가 색을 정한다"
    (rgb_at m ~x:9 ~y:1 = (0xFF, 0x55, 0xFF));
  check "이웃은 바탕색" (Dos_machine.pixel m ~x:8 ~y:1) 0

let test_cga_mode_6 () =
  let m =
    run_com
      (assemble
         (fun _ ->
           set_mode 0x06
           ^ mov_ah 0x0C ^ mov_al 1 ^ mov_bx 0 ^ mov_cx 100 ^ mov_dx 3
           ^ int_ 0x10 ^ quit)
         "")
  in
  check_true "6 번 모드 크기" (Dos_machine.frame_dims m = (640, 200));
  check "켜진 점" (Dos_machine.pixel m ~x:100 ~y:3) 1;
  check_true "흰색" (rgb_at m ~x:100 ~y:3 = (0xFF, 0xFF, 0xFF));
  check "꺼진 점" (Dos_machine.pixel m ~x:101 ~y:3) 0

(* ---------- 40 열 텍스트 ---------- *)

let test_forty_column_text () =
  let text = String.make 45 'A' ^ "$" in
  let m =
    run_com
      (assemble
         (fun off -> set_mode 0x00 ^ mov_ah 0x09 ^ mov_dx off ^ int_ 0x21
                     ^ quit)
         text)
  in
  check_true "0 번 모드 크기" (Dos_machine.frame_dims m = (320, 400));
  let lines = String.split_on_char '\n' (Dos_machine.screen_text m) in
  check "한 줄은 40 칸" (String.length (List.hd lines)) 40;
  check_s "첫 줄이 꽉 찬다" (List.nth lines 0) (String.make 40 'A');
  check_s "나머지는 다음 줄로"
    (List.nth lines 1) (String.make 5 'A' ^ String.make 35 ' ')

let () =
  test_bios_pixel_mode_0d ();
  test_set_reset_bit_mask ();
  test_write_mode_1_copies_latches ();
  test_map_mask_blocks_planes ();
  test_attribute_palette_remaps_color ();
  test_mode_12_dims_and_corner ();
  test_mode_set_clears_planes ();
  test_mode_set_reloads_dac ();
  test_mode_set_keeps_dac_when_inhibited ();
  test_text_glyph_not_mirrored ();
  test_cga_mode_4 ();
  test_cga_mode_6 ();
  test_forty_column_text ();
  if !failed = 0 then print_endline "dos 그래픽: all passed"
  else begin
    Printf.eprintf "dos 그래픽: %d failures\n%!" !failed;
    exit 1
  end
