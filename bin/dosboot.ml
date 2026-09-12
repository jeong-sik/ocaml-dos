(* DOS 부트 하네스 — COM/EXE 이미지를 기계에 싣고 돌려 화면을 내놓는다.
   판정 재료: exit_code, 화면 텍스트, 검지 않은 픽셀 수, PPM 파일.

   진단은 전부 여기에 있다. 라이브러리는 환경 변수를 읽지 않는다 —
   읽으면 같은 입력이 같은 실행이라는 계약이 깨지고 테스트가 흔들린다. *)

(* 내장 데모: "Hello, DOS!" 를 teletype 으로 찍고 AH=4Ch 로 끝낸다.
   org 0x100 어셈블 결과. *)
let hello_com () =
  String.concat ""
    [ "\xb4\x0e";        (* mov ah,0Eh *)
      "\xb3\x07";        (* mov bl,07h — 글자색 흰색 *)
      "\xbe\x15\x01";    (* mov si,0x115 (msg) *)
      "\xac";            (* lodsb (0x107) *)
      "\x08\xc0";        (* or al,al *)
      "\x74\x04";        (* jz done(0x110) *)
      "\xcd\x10";        (* int 10h *)
      "\xeb\xf7";        (* jmp 0x107 *)
      "\xb8\x00\x4c";    (* mov ax,4C00h *)
      "\xcd\x21";        (* int 21h *)
      "Hello, DOS!\000" ]

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let parse_keys spec =
  List.filter_map
    (fun h ->
      if h = "" then None
      else begin
        let body, at =
          match String.index_opt h '@' with
          | Some i ->
            (String.sub h 0 i,
             int_of_string (String.sub h (i + 1) (String.length h - i - 1)))
          | None -> (h, 0)
        in
        match int_of_string_opt ("0x" ^ body) with
        | Some w -> Some { Dos_machine.word = w; not_before = at }
        | None ->
          prerr_endline
            ("bad key " ^ h ^ " (want hex[@step] like 1c0d or 4d00@40000)");
          None
      end)
    (String.split_on_char ',' spec)

let parse_pair spec what =
  match String.index_opt spec '=' with
  | Some i ->
    Some
      (String.sub spec 0 i,
       String.sub spec (i + 1) (String.length spec - i - 1))
  | None ->
    prerr_endline ("bad --" ^ what ^ " " ^ spec ^ " (want NAME=PATH)");
    None

let hex s =
  let s = if String.length s > 2 && String.sub s 0 2 = "0x" then s else "0x" ^ s in
  int_of_string s

let () =
  let com = ref "" and exe = ref "" and demo = ref "" and steps = ref 100000
  and out = ref "/tmp/dosboot" and mounts = ref [] and trace = ref 0
  and keys = ref "" and typed = ref "" and utf8 = ref false
  and dump = ref "" and int_trace = ref false and saves = ref []
  and mouse = ref "" and clock = ref "" in
  Arg.parse
    [ ("--com", Arg.Set_string com, "PATH  COM 이미지 실행");
      ("--exe", Arg.Set_string exe, "PATH  MZ EXE 이미지 실행");
      ("--demo", Arg.Set_string demo, "NAME  내장 데모 (hello)");
      ("--mount", Arg.String (fun m -> mounts := m :: !mounts),
       "NAME=PATH  게스트에 파일 마운트 (INT 21h open 대상)");
      ("--save", Arg.String (fun m -> saves := m :: !saves),
       "NAME=PATH  끝난 뒤 게스트가 쓴 파일을 호스트로 꺼낸다");
      ("--steps", Arg.Int (fun n -> steps := n), "N  최대 명령 수");
      ("--keys", Arg.Set_string keys,
       "HEX[@STEP],..  키 워드 — (스캔 lsl 8) lor ASCII. @STEP 는 그 스텝 \
        전에는 넣지 않는다는 예약 (예: 4d00@5000000)");
      ("--type", Arg.Set_string typed,
       "TEXT  글자를 차례로 입력 (US 자판 스캔 코드)");
      ("--mouse", Arg.Set_string mouse, "X,Y,BUTTONS  마우스를 붙이고 위치를 준다");
      ("--clock", Arg.Set_string clock,
       "Y-M-D,H:M:S  게스트가 보는 기준시각 (기본 1990-1-1,8:0:0)");
      ("--out", Arg.Set_string out, "PREFIX  PPM 덤프 접두어");
      ("--utf8", Arg.Set utf8, "  화면을 코드 페이지 437 그대로 출력");
      ("--trace", Arg.Int (fun n -> trace := n), "N  N 스텝마다 CS:IP 추적");
      ("--int-trace", Arg.Set int_trace, "  INT 명령을 만날 때마다 기록");
      ("--dump", Arg.Set_string dump, "ADDR,LEN  끝난 뒤 물리 메모리 덤프") ]
    (fun _ -> ())
    "dosboot — DOS 이미지 실행 하네스";
  let m = Dos_machine.create () in
  (match String.split_on_char ',' !clock with
   | [ date; time ] ->
     (match (String.split_on_char '-' date, String.split_on_char ':' time) with
      | [ y; mo; d ], [ h; mi; s ] ->
        Dos_machine.set_clock m ~year:(int_of_string y) ~month:(int_of_string mo)
          ~day:(int_of_string d) ~hour:(int_of_string h)
          ~minute:(int_of_string mi) ~second:(int_of_string s)
      | _ -> prerr_endline "bad --clock (want Y-M-D,H:M:S)")
   | _ -> if !clock <> "" then prerr_endline "bad --clock (want Y-M-D,H:M:S)");
  (match String.split_on_char ',' !mouse with
   | [ x; y; b ] ->
     Dos_machine.attach_mouse m;
     Dos_machine.set_mouse m ~x:(int_of_string x) ~y:(int_of_string y)
       ~buttons:(int_of_string b)
   | _ -> if !mouse <> "" then prerr_endline "bad --mouse (want X,Y,BUTTONS)");
  List.iter
    (fun spec ->
      match parse_pair spec "mount" with
      | Some (name, path) -> Dos_machine.mount_file m name (read_file path)
      | None -> ())
    (List.rev !mounts);
  if !demo = "hello" then Dos_machine.load_com m (hello_com ())
  else if !com <> "" then Dos_machine.load_com m (read_file !com)
  else if !exe <> "" then Dos_machine.load_exe m (read_file !exe)
  else begin
    prerr_endline "need --demo hello or --com PATH or --exe PATH";
    exit 2
  end;
  String.iter (fun c -> Dos_machine.push_ascii m c) !typed;
  let on_step mm n =
    let c = Dos_machine.cpu_of mm in
    let pc = ((Cpu86.seg c 1 lsl 4) + Cpu86.dump_ip c) land 0xfffff in
    let op = Dos_machine.mem_read mm pc in
    if !trace > 0 && n mod !trace = 0 then
      Printf.eprintf
        "T %07d cs=%04x ip=%04x pc=%05x op=%02x sp=%04x ss=%04x ds=%04x \
         es=%04x bx=%04x dx=%04x\n%!"
        n (Cpu86.seg c 1) (Cpu86.dump_ip c) pc op (Cpu86.reg16 c 4)
        (Cpu86.seg c 2) (Cpu86.seg c 3) (Cpu86.seg c 0) (Cpu86.reg16 c 3)
        (Cpu86.reg16 c 2);
    if !int_trace && op = 0xCD then
      Printf.eprintf
        "INT %02x ah=%02x al=%02x @%04x:%04x bx=%04x cx=%04x dx=%04x ds=%04x\n%!"
        (Dos_machine.mem_read mm (pc + 1))
        (Cpu86.reg8 c 4) (Cpu86.reg8 c 0) (Cpu86.seg c 1) (Cpu86.dump_ip c)
        (Cpu86.reg16 c 3) (Cpu86.reg16 c 1) (Cpu86.reg16 c 2) (Cpu86.seg c 3)
  in
  let on_key w n = Printf.eprintf "KEY %04x @step %d\n%!" w n in
  (try
     ignore
       (Dos_machine.run_with_keys ~on_step ~on_key m ~max_steps:!steps
          ~keys:(parse_keys !keys))
   with Cpu86.Unsupported msg -> Printf.eprintf "UNSUPPORTED: %s\n%!" msg);
  let c = Dos_machine.cpu_of m in
  Printf.printf "exited=%b code=%d halted=%b cs=%04x ip=%04x ticks=%d\n%!"
    (Dos_machine.exited m) (Dos_machine.exit_code m) (Dos_machine.halted m)
    (Cpu86.seg c 1) (Cpu86.dump_ip c) (Dos_machine.tick_count m);
  print_string
    (if !utf8 then Dos_machine.screen_text_utf8 m else Dos_machine.screen_text m);
  let rgb = Dos_machine.frame_rgb m in
  let nonblack = ref 0 in
  String.iteri
    (fun i ch ->
      if i mod 3 = 0
         && (Char.code ch > 8 || Char.code rgb.[i + 1] > 8
             || Char.code rgb.[i + 2] > 8)
      then incr nonblack)
    rgb;
  Printf.printf "nonblack=%d\n" !nonblack;
  (match String.split_on_char ',' !dump with
   | [ a; n ] when !dump <> "" ->
     let a0 = hex a and len = hex n in
     for row = 0 to (len - 1) / 16 do
       Printf.eprintf "mem %05x:" (a0 + (row * 16));
       for j = 0 to 15 do
         Printf.eprintf " %02x"
           (Dos_machine.mem_read m ((a0 + (row * 16) + j) land 0xfffff))
       done;
       Printf.eprintf "\n%!"
     done
   | _ -> if !dump <> "" then prerr_endline "bad --dump (want ADDR,LEN)");
  List.iter
    (fun spec ->
      match parse_pair spec "save" with
      | Some (name, path) ->
        (match Dos_machine.read_mounted m name with
         | Some data ->
           let oc = open_out_bin path in
           output_string oc data;
           close_out oc;
           Printf.printf "saved %s -> %s (%d bytes)\n" name path
             (String.length data)
         | None -> Printf.eprintf "no such guest file: %s\n%!" name)
      | None -> ())
    (List.rev !saves);
  let oc = open_out_bin (!out ^ ".ppm") in
  output_string oc (Dos_machine.frame_ppm m);
  close_out oc;
  Printf.printf "wrote %s.ppm\n" !out
