(* 텍스트 화면이 CRTC 를 따른다 — 시작 주소(0Ch/0Dh), 행 간격(13h), 주소
   크기(17h). 게스트가 쓰는 길로만 바꾼다: CRTC 포트와 INT 10h AH=00h.
   판정은 화면 글자(screen_text), 프레임 RGB, 화면 지문이다. 모드별 CRTC
   부팅값은 SeaBIOS vgasrc/stdvgamodes.c 의 crtc_01·crtc_03·crtc_07 이다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got=%d (0x%x) want=%d (0x%x)\n%!" name got got want want
  end

let check_bool name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* ---------- 아주 작은 어셈블러 ---------- *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax n = "\xb8" ^ w n
let mov_dx n = "\xba" ^ w n
let int_ n = "\xcd" ^ b n
let out_dx_ax = "\xef"
let quit = mov_ax 0x4c00 ^ int_ 0x21
let set_mode m = mov_ax m ^ int_ 0x10
let crtc index value = mov_dx 0x3D4 ^ mov_ax (((value land 0xff) lsl 8) lor index) ^ out_dx_ax
let start_at a = crtc 0x0C (a lsr 8) ^ crtc 0x0D a

let text_base = 0xB8000
let page_bytes = 0x1000                   (* 80x25 한 장, 4KB *)
let cells_80 = 80 * 25

let run ?(before = fun _ -> ()) code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (code ^ quit);
  before m;
  Dos_machine.run m ~max_steps:100_000;
  check_bool "program exited" (Dos_machine.exited m);
  m

let poke_cell m addr ch attr =
  Dos_machine.mem_write m addr (Char.code ch);
  Dos_machine.mem_write m (addr + 1) attr

let video m = m.Dos_state.video
let reg m i = Dos_video.crtc (video m) i

(* screen_text 의 [row] 행 [col] 열 글자 *)
let char_at m ~row ~col =
  (List.nth (String.split_on_char '\n' (Dos_machine.screen_text m)) row).[col]

let check_char name m ~row ~col want =
  let got = char_at m ~row ~col in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: (%d,%d) = %C want %C\n%!" name row col got want
  end

(* 부팅 직후: 모드 3 의 표(crtc_03) — 시작 주소 0, 행 간격 28h, word 주소 *)
let test_boot () =
  let m = Dos_machine.create () in
  check "boot 0Ch" (reg m 0x0C) 0;
  check "boot 0Dh" (reg m 0x0D) 0;
  check "boot 13h" (reg m 0x13) 0x28;
  check "boot 14h" (reg m 0x14) 0x1F;
  check "boot 17h" (reg m 0x17) 0xA3;
  check "boot cell (1,2)" (Dos_video.text_cell (video m) ~row:1 ~col:2) (text_base + ((80 + 2) * 2))

(* 시작 주소를 바꾸면 화면이 그 자리부터 보인다 *)
let test_start () =
  let before m =
    poke_cell m text_base 'Q' 0x07;
    poke_cell m (text_base + 2) 'R' 0x07;
    poke_cell m (text_base + page_bytes) 'P' 0x07
  in
  let m = run (start_at 0x0800) ~before in
  check_char "start 800h: 1 쪽 첫 칸" m ~row:0 ~col:0 'P';
  let m = run (start_at 0x0001) ~before in
  check_char "start 1: 한 칸 밀림" m ~row:0 ~col:0 'R';
  let m = run "" ~before in
  check_char "start 0: 0 쪽" m ~row:0 ~col:0 'Q'

(* 13h 를 바꾸면 행이 그만큼 건너뛴다(28h → 50h 는 한 행 160 글자) *)
let test_offset () =
  let m =
    run (crtc 0x13 0x50) ~before:(fun m ->
        poke_cell m (text_base + (160 * 2)) 'S' 0x07;
        poke_cell m (text_base + (80 * 2)) 'T' 0x07)
  in
  check_char "13h 50h: 1 행 = 글자 160" m ~row:1 ~col:0 'S';
  let m = run "" ~before:(fun m -> poke_cell m (text_base + (80 * 2)) 'T' 0x07) in
  check_char "13h 28h: 1 행 = 글자 80" m ~row:1 ~col:0 'T'

(* 같은 글자를 0 쪽에 두고 시작 0 으로 본 그림과, 1 쪽에 두고 시작 800h
   로 본 그림이 같다. 지문도 같다. 메모리가 같아도 시작이 다르면 지문이
   다르다. *)
let test_frame_and_digest () =
  let fill m base =
    for i = 0 to cells_80 - 1 do
      poke_cell m (base + (i * 2)) (Char.chr (0x20 + (i mod 95))) ((i * 7) land 0x7F)
    done
  in
  let page0 = run "" ~before:(fun m -> fill m text_base) in
  let page1 =
    run (start_at 0x0800) ~before:(fun m ->
        fill m (text_base + page_bytes);
        for i = 0 to cells_80 - 1 do poke_cell m (text_base + (i * 2)) ' ' 0x07 done)
  in
  check_bool "1 쪽 그림 = 0 쪽 그림"
    (Dos_machine.frame_rgb page0 = Dos_machine.frame_rgb page1);
  check "1 쪽 지문 = 0 쪽 지문" (Dos_machine.screen_digest page1) (Dos_machine.screen_digest page0);
  let same_mem = run (start_at 0x0800) ~before:(fun m -> fill m text_base) in
  check_bool "시작만 달라도 지문이 바뀐다"
    (Dos_machine.screen_digest same_mem <> Dos_machine.screen_digest page0);
  (* 마지막 칸(24 행 79 열)의 속성만 바꿔도 지문이 바뀐다 *)
  let last = text_base + page_bytes + (((24 * 80) + 79) * 2) + 1 in
  let d0 = Dos_machine.screen_digest page1 in
  Dos_machine.mem_write page1 last 0x4E;
  check_bool "마지막 칸 속성도 지문에" (Dos_machine.screen_digest page1 <> d0)

(* 창 끝을 넘으면 0xB8000 으로 돈다: 시작 3FFFh 의 둘째 칸은 0 번 칸 *)
let test_wrap () =
  let m =
    run (start_at 0x3FFF) ~before:(fun m ->
        poke_cell m (text_base + 0x7FFE) 'W' 0x07;
        poke_cell m text_base 'X' 0x07)
  in
  check_char "창 마지막 칸" m ~row:0 ~col:0 'W';
  check_char "창 끝 다음은 0 번 칸" m ~row:0 ~col:1 'X';
  check "자리는 창 안" (Dos_video.text_cell (video m) ~row:24 ~col:79) (text_base + ((0x3FFF + (24 * 80) + 79) * 2 land 0x7FFF))

(* 17h bit6(byte 주소)이면 MA 가 그대로 바이트 자리다 — 둘째 칸이 0 번 칸,
   셋째 칸이 1 번 칸을 본다. *)
let test_byte_mode () =
  let m =
    run (crtc 0x17 0xE3) ~before:(fun m ->
        poke_cell m text_base 'A' 0x07;
        poke_cell m (text_base + 2) 'B' 0x07)
  in
  check_char "byte 주소: 1 열 = 0 번 칸" m ~row:0 ~col:1 'A';
  check_char "byte 주소: 2 열 = 1 번 칸" m ~row:0 ~col:2 'B'

(* 모드를 세우면 그 모드의 표가 다시 실린다 *)
let test_mode_set () =
  let m = run (start_at 0x0800 ^ crtc 0x13 0x50 ^ set_mode 0x0003) in
  check "모드 3: 시작 0" (Dos_video.text_cell (video m) ~row:0 ~col:0) text_base;
  check "모드 3: 13h" (reg m 0x13) 0x28;
  let m = run (set_mode 0x000D ^ start_at 0x2000 ^ set_mode 0x0003) in
  check "0Dh 에서 돌아온 모드 3: 시작 0" (reg m 0x0C) 0;
  let m = run (set_mode 0x0001) in
  check "모드 1: 13h" (reg m 0x13) 0x14;
  check "모드 1: (1,0) = 글자 40" (Dos_video.text_cell (video m) ~row:1 ~col:0) (text_base + 80);
  check "모드 1: 40 열" (String.length (List.hd (String.split_on_char '\n' (Dos_machine.screen_text m)))) 40;
  List.iter
    (fun (mode, r13, r14) ->
      let m = run (set_mode mode) in
      check (Printf.sprintf "모드 %d: 13h" mode) (reg m 0x13) r13;
      check (Printf.sprintf "모드 %d: 14h" mode) (reg m 0x14) r14;
      check (Printf.sprintf "모드 %d: 09h" mode) (reg m 0x09) 0x4F)
    [ (0, 0x14, 0x1F); (2, 0x28, 0x1F); (7, 0x28, 0x0F) ]

let save_exn m =
  match Dos_snapshot.save m with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let test_snapshot () =
  let m = run (start_at 0x0800) ~before:(fun m -> poke_cell m (text_base + page_bytes) 'P' 0x07) in
  match Dos_snapshot.restore (save_exn m) with
  | Error e -> check_bool ("restore: " ^ Dos_snapshot.error_to_string e) false
  | Ok r -> check_char "복원한 기계도 1 쪽을 본다" r ~row:0 ~col:0 'P'

let () =
  test_boot ();
  test_start ();
  test_offset ();
  test_frame_and_digest ();
  test_wrap ();
  test_byte_mode ();
  test_mode_set ();
  test_snapshot ();
  if !failed = 0 then print_endline "dos text scan: all passed"
  else begin
    Printf.eprintf "dos text scan: %d failures\n%!" !failed;
    exit 1
  end
