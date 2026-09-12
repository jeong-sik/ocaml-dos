(* DOS 부트 하네스 — COM 이미지(또는 내장 데모)를 머신에 싣고 실행해
   텍스트 화면을 PPM 으로 덤프한다. 판정 재료: exit_code, screen_text,
   nonblack 픽셀 수. *)

(* 내장 데모: mov ah,0E / mov si,msg / lodsb / or al,al / jz done /
   int 10 / jmp / done: mov ax,4C00 / int 21 — "Hello, DOS!" 를
   teletype 로 찍고 AH=4Ch 로 종료한다. org 0x100 어셈블 결과. *)
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

let () =
  let com = ref "" and demo = ref "" and steps = ref 100000 and out = ref "/tmp/dosboot" in
  Arg.parse
    [ ("--com", Arg.Set_string com, "PATH  COM 이미지 실행");
      ("--demo", Arg.Set_string demo, "NAME  내장 데모 (hello)");
      ("--steps", Arg.Int (fun n -> steps := n), "N  최대 명령 수");
      ("--out", Arg.Set_string out, "PREFIX  PPM 덤프 접두어") ]
    (fun _ -> ()) "dosboot — DOS COM 실행 하네스";
  let image =
    if !demo = "hello" then hello_com ()
    else if !com <> "" then begin
      let ic = open_in_bin !com in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic; s
    end else begin
      prerr_endline "need --demo hello or --com PATH"; exit 2
    end
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m image;
  (try Dos_machine.run m ~max_steps:!steps
   with Cpu86.Unsupported msg ->
     Printf.eprintf "UNSUPPORTED: %s\n%!" msg);
  Printf.printf "exited=%b code=%d halted=%b\n%!"
    (Dos_machine.exited m) (Dos_machine.exit_code m) (Dos_machine.halted m);
  print_string (Dos_machine.screen_text m);
  let rgb = Dos_machine.frame_rgb m in
  let nonblack = ref 0 in
  String.iteri (fun i c ->
      if i mod 3 = 0 && (Char.code c > 8
                         || Char.code rgb.[i+1] > 8 || Char.code rgb.[i+2] > 8)
      then incr nonblack) rgb;
  Printf.printf "nonblack=%d\n" !nonblack;
  let oc = open_out_bin (!out ^ ".ppm") in
  Printf.fprintf oc "P6\n640 400\n255\n%s" rgb;
  close_out oc;
  Printf.printf "wrote %s.ppm\n" !out
