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
  let com = ref "" and exe = ref "" and demo = ref "" and steps = ref 100000
  and out = ref "/tmp/dosboot" and mounts = ref [] and trace = ref 0
  and keys = ref "" in
  Arg.parse
    [ ("--com", Arg.Set_string com, "PATH  COM 이미지 실행");
      ("--exe", Arg.Set_string exe, "PATH  MZ EXE 이미지 실행");
      ("--mount", Arg.String (fun m -> mounts := m :: !mounts),
       "NAME=PATH  게스트에 파일 마운트 (INT 21h open 대상)");
      ("--demo", Arg.Set_string demo, "NAME  내장 데모 (hello)");
      ("--steps", Arg.Int (fun n -> steps := n), "N  최대 명령 수");
      ("--trace", Arg.Int (fun n -> trace := n), "N  N 스텝마다 CS:IP 추적");
      ("--out", Arg.Set_string out, "PREFIX  PPM 덤프 접두어");
      ("--keys", Arg.Set_string keys,
       "HEX,HEX..  실행 전 주입할 키 — (스캔<<8)|ASCII 워드 (예: 1c0d)") ]
    (fun _ -> ()) "dosboot — DOS COM 실행 하네스";
  let m = Dos_machine.create () in
  (* 키 자동 투입기(keybot): --keys 를 미리 밀어넣지 않고, 게임이
     INT 16h AH=00(블로킹 읽기) 로 굶주리는 순간에 하나씩 넣는다.
     AH=00 가 AX=0 으로 즉시 복귀하면 TP ReadKey 는 Break 신호로
     해석해 무한 재시도하기 때문(ZZT 실측) — 굶주림이 진짜 입력 대기. *)
  let keyq = Queue.create () in
  String.split_on_char ',' !keys
  |> List.iter (fun h ->
         if h <> "" then
           match int_of_string_opt ("0x" ^ h) with
           | Some w -> Queue.push w keyq
           | None -> prerr_endline ("bad key " ^ h ^ " (want hex like 1c0d)"));
  List.iter (fun spec ->
      match String.index_opt spec '=' with
      | Some i ->
        let name = String.sub spec 0 i in
        let path = String.sub spec (i + 1) (String.length spec - i - 1) in
        let ic = open_in_bin path in
        let data = really_input_string ic (in_channel_length ic) in
        close_in ic;
        Dos_machine.mount_file m name data
      | None -> prerr_endline ("bad --mount " ^ spec ^ " (want NAME=PATH)"))
    (List.rev !mounts);
  let image =
    if !demo = "hello" then hello_com ()
    else if !com <> "" then begin
      let ic = open_in_bin !com in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic; s
    end else if !exe = "" then begin
      prerr_endline "need --demo hello or --com PATH or --exe PATH"; exit 2
    end
    else ""
  in
  if !exe <> "" then begin
    let ic = open_in_bin !exe in
    let img = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Dos_machine.load_exe m img
  end
  else Dos_machine.load_com m image;
  (try
     let n = ref 0 and starve = ref 0 in
     while (not (Dos_machine.exited m))
           && not (Cpu86.halted (Dos_machine.cpu_of m))
           && !n < !steps do
       if !trace > 0 && !n mod !trace = 0 then begin
         let c = Dos_machine.cpu_of m in
         let pc = ((Cpu86.seg c 1 lsl 4) + Cpu86.dump_ip c) land 0xfffff in
         Printf.eprintf "T %07d cs=%04x ip=%04x op=%02x sp=%04x ds=%04x es=%04x bx=%04x dx=%04x\n%!"
           !n (Cpu86.seg c 1) (Cpu86.dump_ip c)
           (Dos_machine.mem_read m pc) (Cpu86.reg16 c 4)
           (Cpu86.seg c 3) (Cpu86.seg c 0) (Cpu86.reg16 c 3) (Cpu86.reg16 c 2)
       end;
       ignore (Dos_machine.step m);
       incr n;
       if Dos_machine.kbd_waiting m then begin
         incr starve;
         if !starve >= 2000 && not (Queue.is_empty keyq) then begin
           Dos_machine.push_key m (Queue.pop keyq);
           starve := 0
         end
       end
       else starve := 0
     done
   with Cpu86.Unsupported msg ->
     Printf.eprintf "UNSUPPORTED: %s\n%!" msg);
  Printf.printf "exited=%b code=%d halted=%b cs=%04x ip=%04x\n%!"
    (Dos_machine.exited m) (Dos_machine.exit_code m) (Dos_machine.halted m)
    (Cpu86.seg (Dos_machine.cpu_of m) 1) (Cpu86.dump_ip (Dos_machine.cpu_of m));
  print_string (Dos_machine.screen_text m);
  let rgb = Dos_machine.frame_rgb m in
  let nonblack = ref 0 in
  String.iteri (fun i c ->
      if i mod 3 = 0 && (Char.code c > 8
                         || Char.code rgb.[i+1] > 8 || Char.code rgb.[i+2] > 8)
      then incr nonblack) rgb;
  Printf.printf "nonblack=%d\n" !nonblack;
  (try
     let env = Sys.getenv "MEM_DUMP" in
     let c = String.index env ',' in
     let hx x =
       if String.length x > 2 && String.sub x 0 2 = "0x" then int_of_string x
       else int_of_string ("0x" ^ x) in
     let a0 = hx (String.sub env 0 c) in
     let n = hx (String.sub env (c + 1) (String.length env - c - 1)) in
     for row = 0 to (n - 1) / 16 do
       Printf.eprintf "mem %05x:" (a0 + row * 16);
       for j = 0 to 15 do
         Printf.eprintf " %02x" (Dos_machine.mem_read m ((a0 + row * 16 + j) land 0xfffff))
       done;
       Printf.eprintf "\n%!"
     done
   with Not_found -> ());
  let w, h = Dos_machine.frame_dims m in
  let oc = open_out_bin (!out ^ ".ppm") in
  Printf.fprintf oc "P6\n%d %d\n255\n%s" w h rgb;
  close_out oc;
  Printf.printf "wrote %s.ppm\n" !out
