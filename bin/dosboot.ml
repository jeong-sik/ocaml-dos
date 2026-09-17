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

(* EGA 16색 데모: 모드 0Dh 를 세우고 320x200 을 스무 칸짜리 색 띠
   열여섯으로 채운다. 픽셀마다 INT 10h AH=0Ch 을 부르므로 명령이 오십만
   개 넘게 든다 — `--steps 2000000` 쯤 줘야 끝까지 간다. *)
let ega_com () =
  String.concat ""
    [ "\xb8\x0d\x00";      (* mov ax,000Dh *)
      "\xcd\x10";           (* int 10h *)
      "\x31\xd2";           (* xor dx,dx        — y *)
      "\x31\xc9";           (* xor cx,cx        — x *)
      "\x89\xc8";           (* mov ax,cx *)
      "\xb3\x14";           (* mov bl,20 *)
      "\xf6\xf3";           (* div bl           — al = x/20 = 색 *)
      "\xb4\x0c";           (* mov ah,0Ch *)
      "\xb7\x00";           (* mov bh,0 *)
      "\xcd\x10";           (* int 10h *)
      "\x41";                 (* inc cx *)
      "\x81\xf9\x40\x01";  (* cmp cx,320 *)
      "\x72\xed";           (* jb  x 루프 *)
      "\x42";                 (* inc dx *)
      "\x81\xfa\xc8\x00";  (* cmp dx,200 *)
      "\x72\xe4";           (* jb  y 루프 *)
      "\xb8\x00\x4c";      (* mov ax,4C00h *)
      "\xcd\x21" ]           (* int 21h *)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* 정시 주입 — 게스트가 키를 "묻는" 방식이 무엇이든 상관없이 그 스텝에
   키를 링에 넣는다. INT 16h 를 안 부르고 BIOS 링(0x40:0x1A/0x1C)을
   직접 폴링하는 게임은 굶주림 신호를 주지 않아 --keys 로는 못 넣는다
   (삼국지3 "아무 키나" 대기 실측). *)
let parse_feed spec =
  List.filter_map
    (fun h ->
      if h = "" then None
      else
        match String.index_opt h '@' with
        | None ->
          prerr_endline ("bad feed " ^ h ^ " (want hex@step like 1c0d@4000000)");
          None
        | Some i ->
          let body = String.sub h 0 i in
          let at =
            int_of_string_opt (String.sub h (i + 1) (String.length h - i - 1))
          in
          match (int_of_string_opt ("0x" ^ body), at) with
          | Some w, Some n -> Some (w, n)
          | _ ->
            prerr_endline ("bad feed " ^ h ^ " (want hex@step like 1c0d@4000000)");
            None)
    (String.split_on_char ',' spec)

(* --feed-sync: 스텝을 안 적는다 — 게임이 키를 기다리는 순간에 맞춰
   하나씩 넣는다. INT 16h 폴링의 굶주림(kbd_waiting) 상승에 다음 키를
   주고, INT 16h 없이 BDA 링을 직접 폴링하는 게임(삼국지3 프로텍션의
   0x25a3)을 위해 링이 비어 있으면 주기적으로도 넣는다. 스텝 하드코딩
   --feed 는 렌더 속도·플래그 수정에 따라 전부 어긋난다(실측). *)
let parse_feed_sync spec =
  List.filter_map
    (fun h ->
      if h = "" then None
      else
        match int_of_string_opt ("0x" ^ h) with
        | Some w -> Some w
        | None ->
          prerr_endline ("bad feed-sync " ^ h ^ " (want hex like 1c0d)");
          None)
    (String.split_on_char ',' spec)

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
  and glyph_max = ref 0 and glyph_rec = ref 0
  and mouse = ref "" and clock = ref "" and feed = ref "" in
  let feed_sync = ref "" in
  Arg.parse
    [ ("--com", Arg.Set_string com, "PATH  COM 이미지 실행");
      ("--exe", Arg.Set_string exe, "PATH  MZ EXE 이미지 실행");
      ("--demo", Arg.Set_string demo,
       "NAME  내장 데모 — hello(텍스트) 또는 ega(모드 0Dh 색 띠, \
        --steps 2000000 필요)");
      ("--mount", Arg.String (fun m -> mounts := m :: !mounts),
       "NAME=PATH  게스트에 파일 마운트 (INT 21h open 대상)");
      ("--save", Arg.String (fun m -> saves := m :: !saves),
       "NAME=PATH  끝난 뒤 게스트가 쓴 파일을 호스트로 꺼낸다");
      ("--steps", Arg.Int (fun n -> steps := n), "N  최대 명령 수");
      ("--feed", Arg.Set_string feed,
       "HEX@STEP,..  그 스텝에 무조건 키를 넣는다 — 굶주림 대기 없이");
      ("--feed-sync", Arg.Set_string feed_sync,
       "HEX,HEX,..  게스트가 키를 기다리는 순간에 하나씩 넣는다");
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
      ("--glyph-trace", Arg.String (fun s ->
         match String.split_on_char ',' s with
         | [ mx; rec_ ] ->
           (match int_of_string_opt mx, int_of_string_opt rec_ with
            | Some a, Some b -> glyph_max := a; glyph_rec := b
            | _ -> prerr_endline "bad --glyph-trace (want MAX,RECSIZE)")
         | _ -> prerr_endline "bad --glyph-trace (want MAX,RECSIZE)"),
       "  INT 21h AH=42 seek 중 폰트 글리프 요청(offset = idx*RECSIZE+base 패턴) 기록");
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
  else if !demo = "ega" then Dos_machine.load_com m (ega_com ())
  else if !com <> "" then Dos_machine.load_com m (read_file !com)
  else if !exe <> "" then Dos_machine.load_exe m (read_file !exe)
  else begin
    prerr_endline "need --demo hello|ega or --com PATH or --exe PATH";
    exit 2
  end;
  String.iter (fun c -> Dos_machine.push_ascii m c) !typed;
  let feeds = Queue.create () in
  List.iter (fun (w, at) -> Queue.push (w, at) feeds) (parse_feed !feed);
  let sync_feeds = Queue.create () in
  List.iter (fun w -> Queue.push w sync_feeds) (parse_feed_sync !feed_sync);
  let feed_armed = ref true in
  let feed_at mm n =
    (match Queue.peek_opt feeds with
     | Some (w, at) when n >= at ->
       ignore (Queue.pop feeds);
       Dos_machine.push_key mm w;
       Printf.eprintf "FEED %04x @step %d\n%!" w n
     | _ -> ());
    (* 대기 동기화: 굶주림 상승에 한 개, 폴밋은 링이 빈 때만 *)
    (match Queue.peek_opt sync_feeds with
     | Some w when !feed_armed && Dos_machine.kbd_waiting mm ->
       ignore (Queue.pop sync_feeds);
       Dos_machine.push_key mm w;
       feed_armed := false;
       Printf.eprintf "FEED-SYNC %04x @step %d\n%!" w n
     | _ -> ());
    (match Queue.peek_opt sync_feeds with
     | Some w when n > 0 && n mod 2_000_000 = 0
                    && not (Dos_state.key_pending mm) ->
       ignore (Queue.pop sync_feeds);
       Dos_machine.push_key mm w;
       Printf.eprintf "FEED-POLL %04x @step %d\n%!" w n
     | _ -> ());
    if not (Dos_machine.kbd_waiting mm) then feed_armed := true
  in
  let on_step mm n =
    feed_at mm n;
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
        (Cpu86.reg16 c 3) (Cpu86.reg16 c 1) (Cpu86.reg16 c 2) (Cpu86.seg c 3);
    (* --glyph-trace MAX,RECSIZE: INT 21h AH=42(seek) 호출 중 오프셋이
       idx*RECSIZE+2 패턴(삼국지3 all_font.16p: 레코드 30B, 코드 2B)이면
       화면이 그리는 글리프 요청으로 보고 기록한다. 삼국지3 진단 실측:
       폰트는 파일에서 직접 seek+read 하므로 이 지점이 곧 화면 텍스트. *)
    (if !glyph_max > 0 && op = 0xCD
       && Dos_machine.mem_read mm (pc + 1) = 0x21
       && Cpu86.reg8 c 4 = 0x42 then begin
       let off = (Cpu86.reg16 c 1 lsl 16) + Cpu86.reg16 c 2 in
       if off >= 2 && off <= !glyph_max && (off - 2) mod !glyph_rec = 0 then
         Printf.eprintf "GLYPHSEEK off=%d idx=%d\n%!" off ((off - 2) / !glyph_rec)
     end);
  in
  let on_key w n = Printf.eprintf "KEY %04x @step %d\n%!" w n in
  (try
     ignore
       (Dos_machine.run_with_keys ~on_step ~on_key m ~max_steps:!steps
          ~keys:(parse_keys !keys))
   with Cpu86.Unsupported msg -> Printf.eprintf "UNSUPPORTED: %s\n%!" msg);
  let c = Dos_machine.cpu_of m in
  let fw, fh = Dos_machine.frame_dims m in
  Printf.printf "mode=%02xh frame=%dx%d\n%!" (Dos_machine.video_mode m) fw fh;
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
