(* 텍스트 쪽(page) — INT 10h AH=05h(보이는 쪽), BH 를 받는 AH=02h·03h·08h·
   09h·0Ah·13h, 보이는 쪽을 쓰는 AH=0Eh·06h·07h 와 INT 21h 출력, 모드
   세우기의 쪽 정보. 규칙은 SeaBIOS vgasrc/vgabios.c(set_active_page,
   set_cursor_pos, get_cursor_pos, verify_scroll, calc_page_size)다. *)

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

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let int10 ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x10"
let dos_putc ch = "\xb8\x00\x02\xba" ^ w16 (Char.code ch) ^ "\xcd\x21"
let store_ax a = "\xa3" ^ w16 a
let store_dx a = "\x89\x16" ^ w16 a
let mov_bp n = "\xbd" ^ w16 n
let es_is_cs = "\x0e\x07"
let crtc index value = "\xba\xd4\x03\xb8" ^ w16 (((value land 0xff) lsl 8) lor index) ^ "\xef"
let exit_0 = "\xb8\x00\x4c\xcd\x21"

let select_page p = int10 (0x0500 lor p)
let set_cursor ~page ~row ~col = int10 0x0200 ~bx:(page lsl 8) ~dx:((row lsl 8) lor col)

let psp = 0x10000
let out = 0x300
let text_base = 0xB8000
let page_bytes = 0x1000
let bda_page_size = 0x44C
let bda_page_start = 0x44E
let bda_cursor = 0x450
let bda_cursor_type = 0x460
let bda_active_page = 0x462

let run ?(before = fun _ -> ()) code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (code ^ exit_0);
  before m;
  Dos_machine.run m ~max_steps:200_000;
  check_bool "program exited" (Dos_machine.exited m);
  m

let rd m a = Dos_machine.mem_read m a
let rd16 m a = rd m a lor (rd m (a + 1) lsl 8)
let peek16 m off = rd16 m (psp + off)
let reg m i = Dos_video.crtc m.Dos_state.video i
let crtc_start m = (reg m 0x0C lsl 8) lor reg m 0x0D
let crtc_cursor m = (reg m 0x0E lsl 8) lor reg m 0x0F
let cell ~page ~row ~col = text_base + (page * page_bytes) + (((row * 80) + col) * 2)
let poke_char m a ch = Dos_machine.mem_write m a (Char.code ch)
let char_of m a = Char.chr (rd m a)
let check_char name m a want =
  let got = char_of m a in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: %C want %C\n%!" name got want
  end

(* 모드를 세우면 쪽 크기(0x44C)가 모드를 따르고, 0 쪽을 보이며, 커서
   여덟 칸이 0 이고, 텍스트는 쪽 여덟 장을 모두 지운다 *)
let test_mode_set () =
  List.iter
    (fun (mode, size) ->
      let m = run (int10 mode) in
      check (Printf.sprintf "모드 %02Xh 쪽 크기" mode) (rd16 m bda_page_size) size)
    [ (0x03, 0x1000); (0x01, 0x0800); (0x04, 0x4000); (0x06, 0x4000);
      (0x0D, 0x2000); (0x0E, 0x4000); (0x10, 0x8000); (0x12, 0xA000); (0x13, 0x2000) ];
  let dirty m =
    Dos_machine.mem_write m bda_active_page 3;
    Dos_machine.mem_write m bda_page_start 0x00;
    Dos_machine.mem_write m (bda_page_start + 1) 0x30;
    Dos_machine.mem_write m (bda_cursor + 14) 9;
    Dos_machine.mem_write m (bda_cursor + 15) 9;
    poke_char m (cell ~page:7 ~row:24 ~col:79) 'Z'
  in
  let m = run (int10 0x0003) ~before:dirty in
  check "모드 3: 보이는 쪽 0" (rd m bda_active_page) 0;
  check "모드 3: 쪽 시작 0" (rd16 m bda_page_start) 0;
  check "모드 3: 7 쪽 커서 0" (rd16 m (bda_cursor + 14)) 0;
  check_char "모드 3: 7 쪽 끝 칸도 지운다" m (cell ~page:7 ~row:24 ~col:79) ' ';
  check "모드 3: 7 쪽 끝 속성 07h" (rd m (cell ~page:7 ~row:24 ~col:79 + 1)) 0x07;
  let m = run (int10 0x0083) ~before:dirty in
  check_char "AL bit 7: 지우지 않는다" m (cell ~page:7 ~row:24 ~col:79) 'Z'

(* AH=05h: CRTC 시작 주소, BDA 0x462·0x44E, 그 쪽의 커서 *)
let test_select () =
  let m =
    run (set_cursor ~page:1 ~row:3 ~col:5 ^ select_page 1)
      ~before:(fun m -> poke_char m (cell ~page:1 ~row:0 ~col:0) 'P')
  in
  check "1 쪽: 0x462" (rd m bda_active_page) 1;
  check "1 쪽: 0x44E" (rd16 m bda_page_start) page_bytes;
  check "1 쪽: CRTC 시작(글자)" (crtc_start m) (page_bytes / 2);
  check "1 쪽: CRTC 커서 = 쪽 처음 + 3 행 5 열" (crtc_cursor m) ((page_bytes / 2) + (3 * 80) + 5);
  check_char "1 쪽: 화면 첫 칸" m (Dos_video.text_cell m.Dos_state.video ~row:0 ~col:0) 'P';
  check_bool "1 쪽: screen_text" ((Dos_machine.screen_text m).[0] = 'P');
  let m = run (select_page 2 ^ select_page 8) in
  check "쪽 8 은 무시: 0x462" (rd m bda_active_page) 2;
  check "쪽 8 은 무시: CRTC" (crtc_start m) page_bytes;
  let m = run (int10 0x0001 ^ select_page 1) in
  check "모드 1: 1 쪽 시작 = 800h 바이트" (rd16 m bda_page_start) 0x800;
  check "모드 1: CRTC 400h" (crtc_start m) 0x400;
  let m = run (int10 0x000D ^ select_page 1) in
  check "모드 0Dh: CRTC = 2000h(평면 바이트)" (crtc_start m) 0x2000;
  let m = run (int10 0x0013 ^ select_page 1) in
  check "모드 13h: CRTC = 800h" (crtc_start m) 0x800;
  let m = run (int10 0x0004 ^ select_page 1) in
  check "모드 4: CRTC = 2000h" (crtc_start m) 0x2000;
  let m = run (int10 0x0006 ^ select_page 1) in
  check "모드 6: CRTC = 4000h" (crtc_start m) 0x4000;
  (* 12h 의 7 쪽은 40960 × 7 바이트라 B800 세그먼트(64KB)를 넘는다. 오프셋이
     16비트에서 돌아 6000h 가 되고, 글자도 0xBE000 에 앉는다 — 0xFE000 의
     ROM 을 덮지 않는다. *)
  let rom = 0xFE000 in
  let m =
    run (int10 0x0012 ^ select_page 7 ^ int10 0x0941 ~bx:0x0707 ~cx:1)
      ~before:(fun m -> Dos_machine.mem_write m rom 0x5A)
  in
  check "12h 7 쪽: 0x44E" (rd16 m bda_page_start) 0x6000;
  check "12h 7 쪽: CRTC" (crtc_start m) 0x6000;
  check_char "12h 7 쪽: 글자는 B800:6000" m (text_base + 0x6000) 'A';
  check "12h 7 쪽: ROM 그대로" (rd m rom) 0x5A

(* 쪽마다 커서가 따로 있다. 보이는 쪽의 커서만 CRTC 에 간다. 게스트가
   CRTC 커서를 직접 쓰면 보이는 쪽의 칸이 바뀐다. *)
let test_cursor () =
  let m =
    run
      (String.concat ""
         [ set_cursor ~page:0 ~row:1 ~col:2;
           set_cursor ~page:1 ~row:3 ~col:5;
           int10 0x0300 ~bx:0x0100; store_dx out;
           int10 0x0300 ~bx:0x0000; store_dx (out + 2);
           set_cursor ~page:8 ~row:7 ~col:7;
           int10 0x0300 ~bx:0x0800 ~dx:0x1234; store_dx (out + 4) ])
  in
  check "1 쪽 커서 BDA" (rd16 m (bda_cursor + 2)) 0x0305;
  check "0 쪽 커서 BDA" (rd16 m bda_cursor) 0x0102;
  check "AH=03h BH=1" (peek16 m out) 0x0305;
  check "AH=03h BH=0" (peek16 m (out + 2)) 0x0102;
  check "AH=03h BH=8 → 0" (peek16 m (out + 4)) 0;
  check "BH=1 은 CRTC 를 안 바꾼다" (crtc_cursor m) (80 + 2);
  check "BH=8 은 커서 종류(0x460)를 안 덮는다" (rd16 m bda_cursor_type) 0x0607;
  (* 1 쪽을 보인 뒤 CRTC 커서를 1 쪽 2 행 7 열로 직접 *)
  let linear = (page_bytes / 2) + (2 * 80) + 7 in
  let m =
    run
      (String.concat ""
         [ set_cursor ~page:0 ~row:1 ~col:2; select_page 1;
           crtc 0x0E (linear lsr 8); crtc 0x0F (linear land 0xff) ])
  in
  check "직접 쓴 커서 → 1 쪽 칸" (rd16 m (bda_cursor + 2)) 0x0207;
  check "0 쪽 칸은 그대로" (rd16 m bda_cursor) 0x0102;
  (* 보이는 쪽 번호가 0-7 밖이면 맞출 칸이 없다 — 0x460 을 덮지 않는다 *)
  let m =
    run (crtc 0x0E 0x01 ^ crtc 0x0F 0x23)
      ~before:(fun m -> Dos_machine.mem_write m bda_active_page 8)
  in
  check "쪽 8 에서 CRTC 커서: 0x460 그대로" (rd16 m bda_cursor_type) 0x0607

(* BH 를 받는 글자 함수 *)
let test_char_functions () =
  let str = 0x280 in
  let m =
    run
      ~before:(fun m ->
          List.iteri (fun i c -> Dos_machine.mem_write m (psp + str + i) (Char.code c)) [ 'H'; 'I' ];
          poke_char m (cell ~page:0 ~row:4 ~col:1) 'o';
          Dos_machine.mem_write m (cell ~page:2 ~row:5 ~col:0 + 1) 0x5A)
      (String.concat ""
         [ set_cursor ~page:0 ~row:0 ~col:0;
           set_cursor ~page:2 ~row:4 ~col:1;
           int10 0x0941 ~bx:0x021E ~cx:3;                     (* 'A' 세 번, 속성 1Eh *)
           set_cursor ~page:2 ~row:5 ~col:0;
           int10 0x0A42 ~bx:0x0200 ~cx:1;                     (* 'B', 속성은 둔다 *)
           set_cursor ~page:2 ~row:4 ~col:2;
           int10 0x0800 ~bx:0x0200; store_ax out;
           es_is_cs; mov_bp str; int10 0x1301 ~bx:0x0370 ~cx:2 ~dx:0x0200;
           es_is_cs; mov_bp str; int10 0x1300 ~bx:0x0470 ~cx:2 ~dx:0x0600 ])
  in
  check_char "AH=09h 2 쪽" m (cell ~page:2 ~row:4 ~col:1) 'A';
  check_char "AH=09h 2 쪽 셋째" m (cell ~page:2 ~row:4 ~col:3) 'A';
  check "AH=09h 속성" (rd m (cell ~page:2 ~row:4 ~col:1 + 1)) 0x1E;
  check_char "AH=09h 은 0 쪽을 안 건드린다" m (cell ~page:0 ~row:4 ~col:1) 'o';
  check_char "AH=0Ah 2 쪽" m (cell ~page:2 ~row:5 ~col:0) 'B';
  check "AH=0Ah 은 화면의 속성을 둔다" (rd m (cell ~page:2 ~row:5 ~col:0 + 1)) 0x5A;
  check "AH=08h 2 쪽 (4,2) = 1E41h" (peek16 m out) 0x1E41;
  check_char "AH=13h 3 쪽" m (cell ~page:3 ~row:2 ~col:0) 'H';
  check_char "AH=13h 3 쪽 둘째" m (cell ~page:3 ~row:2 ~col:1) 'I';
  check "AH=13h AL=1: 3 쪽 커서가 끝으로" (rd16 m (bda_cursor + 6)) 0x0202;
  check_char "AH=13h AL=0: 4 쪽에 쓴다" m (cell ~page:4 ~row:6 ~col:0) 'H';
  check "AH=13h AL=0: 4 쪽 커서는 그대로" (rd16 m (bda_cursor + 8)) 0;
  check "0 쪽 커서는 그대로" (rd16 m bda_cursor) 0

(* teletype 과 INT 21h 출력은 보이는 쪽에, 줄이 넘치면 그 쪽만 올린다 *)
let test_active_page_output () =
  let m =
    run
      ~before:(fun m ->
          poke_char m (cell ~page:0 ~row:0 ~col:0) 'K';
          poke_char m (cell ~page:1 ~row:1 ~col:0) 'L')
      (String.concat ""
         [ select_page 1; set_cursor ~page:1 ~row:0 ~col:3;
           int10 0x0E58; dos_putc 'Y';
           set_cursor ~page:1 ~row:24 ~col:0; int10 0x0E0A ])
  in
  check_char "0 쪽은 그대로" m (cell ~page:0 ~row:0 ~col:0) 'K';
  check_char "1 쪽이 한 줄 올라갔다" m (cell ~page:1 ~row:0 ~col:0) 'L';
  check "1 쪽 커서 = 24 행" (rd16 m (bda_cursor + 2)) 0x1800;
  let m =
    run (select_page 1 ^ set_cursor ~page:1 ~row:0 ~col:3 ^ int10 0x0E58 ^ dos_putc 'Y')
      ~before:(fun m -> poke_char m (cell ~page:0 ~row:0 ~col:3) 'o')
  in
  check_char "AH=0Eh → 1 쪽" m (cell ~page:1 ~row:0 ~col:3) 'X';
  check_char "INT 21h AH=02h → 1 쪽" m (cell ~page:1 ~row:0 ~col:4) 'Y';
  check_char "0 쪽 (0,3) 그대로" m (cell ~page:0 ~row:0 ~col:3) 'o'

(* AH=06h/07h 의 창은 화면 안으로 잘린다 — DH=DL=FFh 여도 다음 쪽을
   덮지 않는다 *)
let test_scroll_clamp () =
  List.iter
    (fun ah ->
      let m =
        run (int10 ((ah lsl 8) lor 1) ~bx:0x1700 ~cx:0 ~dx:0xFFFF)
          ~before:(fun m ->
              poke_char m (cell ~page:1 ~row:0 ~col:0) 'P';
              poke_char m (cell ~page:0 ~row:24 ~col:79) 'E')
      in
      check_char (Printf.sprintf "AH=%02Xh: 1 쪽 그대로" ah) m (cell ~page:1 ~row:0 ~col:0) 'P';
      check (Printf.sprintf "AH=%02Xh: 화면 끝까지 스크롤" ah)
        (rd m (cell ~page:0 ~row:(if ah = 6 then 24 else 0) ~col:79 + 1)) 0x17)
    [ 6; 7 ];
  (* AH=06h 은 보이는 쪽을 스크롤한다 *)
  let m =
    run (select_page 1 ^ int10 0x0600 ~bx:0x1F00 ~cx:0 ~dx:0x184F)
      ~before:(fun m ->
          poke_char m (cell ~page:0 ~row:3 ~col:3) 'K';
          poke_char m (cell ~page:1 ~row:3 ~col:3) 'L')
  in
  check_char "1 쪽을 지웠다" m (cell ~page:1 ~row:3 ~col:3) ' ';
  check "1 쪽 속성 1Fh" (rd m (cell ~page:1 ~row:3 ~col:3 + 1)) 0x1F;
  check_char "0 쪽은 그대로" m (cell ~page:0 ~row:3 ~col:3) 'K'

let () =
  test_mode_set ();
  test_select ();
  test_cursor ();
  test_char_functions ();
  test_active_page_output ();
  test_scroll_clamp ();
  if !failed = 0 then print_endline "dos text page: all passed"
  else begin
    Printf.eprintf "dos text page: %d failures\n%!" !failed;
    exit 1
  end
