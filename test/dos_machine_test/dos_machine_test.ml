(* Dos 머신 M2a 테스트: hello COM(teletype + 4Ch 종료)이 텍스트 VRAM 에
   글자를 쓰고 종료하는 전 경로. 하네스 데모 바이트와 동일한 이미지를
   생성한다. *)

let hello_com () =
  let msg = "Hello, DOS!\000" in
    Bytes.to_string
    (Bytes.concat Bytes.empty
       (List.map Bytes.of_string
          [ "\xb4\x0e";          (* mov ah,0Eh *)
            "\xb3\x07";          (* mov bl,07h — 전경 백색 *)
            "\xbe\x15\x01";     (* mov si,0x115 (msg) *)
            "\xac";               (* lodsb (0x107) *)
            "\x08\xc0";          (* or al,al *)
            "\x74\x04";          (* jz done(0x110) *)
            "\xcd\x10";          (* int 10h *)
            "\xeb\xf7";          (* jmp 0x107 *)
            "\xb8\x00\x4c";     (* mov ax,4C00h *)
            "\xcd\x21";          (* int 21h *)
            msg ]))

let failed = ref 0
let checkb name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let contains_sub hay needle =
  let n = String.length needle and m = String.length hay in
  let rec at i j = j = n || (hay.[i + j] = needle.[j] && at i (j + 1)) in
  let rec go i = i + n <= m && (at i 0 || go (i + 1)) in
  go 0

let () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (hello_com ());
  Dos_machine.run m ~max_steps:1000;
  checkb "hello exited" (Dos_machine.exited m) true;
  if not (Dos_machine.exited m) then
    Printf.eprintf "  (screen so far: %S)\n%!" (Dos_machine.screen_text m);
  checkb "hello exit code 0" (Dos_machine.exit_code m = 0) true;
  let txt = Dos_machine.screen_text m in
  checkb "hello text on screen" (contains_sub txt "Hello, DOS!") true;
  let rgb = Dos_machine.frame_rgb m in
  let nonblack = ref 0 in
  String.iteri
    (fun i c ->
      if i mod 3 = 0
         && (Char.code c > 8 || Char.code rgb.[i + 1] > 8
             || Char.code rgb.[i + 2] > 8)
      then incr nonblack)
    rgb;
  if !nonblack < 100 then begin
    incr failed;
    Printf.eprintf "FAIL hello renders: nonblack=%d\n%!" !nonblack
  end;
  (* RET 로 종료하는 프로그램: push 0 아닌 call 없이 RET — 스택 top 의
     0x0000 으로 돌아가 PSP INT 20h 를 만난다 (0xC3). *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m "\xc3";
  Dos_machine.run m ~max_steps:100;
  checkb "ret exits via PSP int20" (Dos_machine.exited m) true;
  (* 키 입력: push_key 후 INT 16h AH=0 로 읽어 teletype 찍기.
     mov ah,0 / int 16 / mov ah,0E / int 10 / mov ax,4C00 / int 21 *)
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    ("\xb4\x00\xcd\x16\xb4\x0e\xcd\x10\xb8\x00\x4c\xcd\x21" ^ "Q");
  (* push_key 계약: (스캔<<8)|ASCII 워드 — Q 스캔코드 0x10 *)
  Dos_machine.push_key m ((0x10 lsl 8) lor Char.code 'Q');
  Dos_machine.run m ~max_steps:200;
  checkb "int16 key read" (contains_sub (Dos_machine.screen_text m) "Q") true;
  checkb "int16 program exited" (Dos_machine.exited m) true;
  (* ROM 8x8 글꼴: IBM BIOS 표준 자리 F000:FA6E. 그래픽 모드에서 글자를
     직접 찍는 프로그램이 BIOS 를 안 거치고 이 표를 읽는다 — 삼국지3
     MAIN.EXE 의 글자 루틴이 es=0F000h, si=0FA6Eh+ch*8 (실측). 게스트가
     표를 읽어 종료 코드로 돌려주는 COM 으로 증명한다. *)
  let rom_font_com ?(row = 0) ch =
    let off = 0xFA6E + (ch * 8) + row in
    Bytes.to_string
      (Bytes.concat Bytes.empty
         (List.map Bytes.of_string
            [ "\xb8\x00\xf0";            (* mov ax,0F000h *)
              "\x8e\xc0";                (* mov es,ax *)
              Printf.sprintf "\xbb%c%c"  (* mov bx,글리프 오프셋 *)
                (Char.chr (off land 0xff)) (Char.chr (off lsr 8));
              "\x26\x8a\x07";            (* mov al,es:[bx] *)
              "\xb4\x4c";                (* mov ah,4Ch *)
              "\xcd\x21" ]))             (* int 21h *)
  in
  let check_rom_glyph ch name =
    let m = Dos_machine.create () in
    Dos_machine.load_com m (rom_font_com ch);
    Dos_machine.run m ~max_steps:100;
    checkb (name ^ " exits") (Dos_machine.exited m) true;
    checkb (name ^ " byte at F000:FA6E")
      (Dos_machine.exit_code m = (Font8x8.glyph ch).(0))
      true
  in
  check_rom_glyph 65 "rom font 'A'";
  check_rom_glyph 0x5F "rom font '_'";
  (* 표는 IBM 순서여야 한다 — 최상위 비트가 왼쪽 점. 좌우가 비대칭인
     '/' 로 잰다: 맨 윗줄은 오른쪽, 일곱째 줄은 맨 왼쪽 점이 켜진다.
     font8x8 원본(최하위 비트가 왼쪽)을 그대로 실으면 0x60·0x01 이 나오고
     게임이 찍는 글자가 좌우로 뒤집힌다(삼국지3 카피프로텍션 실측). *)
  let rom_byte ch row =
    let m = Dos_machine.create () in
    Dos_machine.load_com m (rom_font_com ~row ch);
    Dos_machine.run m ~max_steps:100;
    Dos_machine.exit_code m
  in
  checkb "rom font '/' top row is on the right" (rom_byte 0x2F 0 = 0x06) true;
  checkb "rom font '/' row 6 is the leftmost dot" (rom_byte 0x2F 6 = 0x80) true;
  (* mem_write: mem_read 의 쓰기 짝 — 하네스의 RAM 주입(관측/실험). *)
  let mw = Dos_machine.create () in
  Dos_machine.mem_write mw 0x500 0xA5;
  checkb "mem_write roundtrip" (Dos_machine.mem_read mw 0x500 = 0xA5) true;
  Dos_machine.mem_write mw 0x500 0x1FF;
  checkb "mem_write masks to a byte" (Dos_machine.mem_read mw 0x500 = 0xFF) true;
  (* INT 33h — 마우스 벡터가 호스트 핸들러로 산다: 기본 부착 감지와
     set_mouse 주입 좌표 조회를 게스트에서 확인한다. *)
  let m1 = Dos_machine.create () in
  Dos_machine.load_com m1 "\xb8\x00\x00\xcd\x33\xb4\x4c\xcd\x21";
  Dos_machine.run m1 ~max_steps:1000;
  checkb "int33 reset reports installed"
    (Dos_machine.exit_code m1 = 0xFF) true;
  let m2 = Dos_machine.create () in
  Dos_machine.load_com m2 "\xb8\x03\x00\xcd\x33\x89\xc8\xb4\x4c\xcd\x21";
  Dos_machine.set_mouse m2 ~x:100 ~y:50 ~buttons:1;
  Dos_machine.run m2 ~max_steps:1000;
  checkb "int33 position returns injected x"
    (Dos_machine.exit_code m2 = 100) true;
  (* INT 21h AH=08 확장키 2바이트 계약 — 실기 DOS 는 방향키를 첫 읽기에
     0x00, 다음 읽기에 스캔코드로 내준다. MSC getch 기반 게임 입력
     (삼국지3 전투 배치의 커서 이동)의 전제. COM 은 게임과 같은
     "0 이면 다시 읽기" 루프로 검증한다. *)
  let rdloop = "\xb4\x08\xcd\x21\x3c\x00\x74\xfa\xb4\x4c\xcd\x21" in
  let m3 = Dos_machine.create () in
  Dos_machine.load_com m3 rdloop;
  ignore (Dos_machine.run_with_keys m3 ~max_steps:4000 ~keys:[{ word = 0x5000; not_before = 0 }]);
  checkb "int21 ah=08 delivers ext scan on 2nd read"
    (Dos_machine.exit_code m3 = 0x50) true;
  let m5 = Dos_machine.create () in
  Dos_machine.load_com m5 rdloop;
  ignore (Dos_machine.run_with_keys m5 ~max_steps:4000 ~keys:[{ word = 0x1c0d; not_before = 0 }]);
  checkb "int21 ah=08 plain key reads ascii"
    (Dos_machine.exit_code m5 = 0x0d) true;
  if !failed = 0 then print_endline "dos machine M2a: all passed"
  else begin
    Printf.eprintf "dos machine M2a: %d failures\n%!" !failed;
    exit 1
  end
