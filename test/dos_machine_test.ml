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
  let rom_font_com ch =
    let off = 0xFA6E + (ch * 8) in
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
  if !failed = 0 then print_endline "dos machine M2a: all passed"
  else begin
    Printf.eprintf "dos machine M2a: %d failures\n%!" !failed;
    exit 1
  end
