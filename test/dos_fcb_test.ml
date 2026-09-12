(* FCB 파일 표면: INT 21h AH=0Fh(open) + AH=14h(순차 읽기) → DTA.
   구식 API 를 쓰는 80년대~90년대 초 게임(코에이 계열)을 위한 층. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end
let checkb name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let make_exe ~image ~ip ~cs ~ss ~sp relocs =
  let b = Buffer.create (32 + String.length image) in
  let w v = Buffer.add_char b (Char.chr (v land 0xff));
            Buffer.add_char b (Char.chr ((v lsr 8) land 0xff)) in
  Buffer.add_string b "MZ"; w 0; w 1; w (List.length relocs); w 2;
  w 0; w 0; w ss; w sp; w 0; w ip; w cs; w 0x1C; w 0;
  List.iter (fun (o, sg) -> w o; w sg) relocs;
  if relocs = [] then Buffer.add_string b "\x00\x00\x00\x00";
  Buffer.add_string b image;
  Buffer.contents b

(* 코드 길이 0x14 — FCB 는 그 뒤. open 실패(AL=FF)면 read 도 FCB 없음
   → AL=FF → exit FF. 성공만 exit 0. *)
let prog () =
  let fcb = "\x00TEST    DAT" ^ String.make 21 '\x00' in
  "\x0e\x1f"
  ^ "\xba\x14\x00" ^ "\xb4\x0f\xcd\x21"   (* mov dx,14; open *)
  ^ "\xba\x14\x00" ^ "\xb4\x14\xcd\x21"   (* mov dx,14; 순차 읽기 *)
  ^ "\xb4\x4c\xcd\x21"                        (* exit(AL) *)
  ^ fcb

let () =
  let exe = make_exe ~image:(prog ()) ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE [] in
  let m = Dos_machine.create () in
  Dos_machine.mount_file m "TEST.DAT"
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" ^ String.make 200 'z');
  Dos_machine.load_exe m exe;
  Dos_machine.run m ~max_steps:500;
  checkb "fcb exited" (Dos_machine.exited m) true;
  check "fcb exit 0 (open+read ok)" (Dos_machine.exit_code m) 0;
  (* DTA(기본 0x80) 에 첫 레코드 128 바이트 *)
  check "fcb dta A" (Dos_machine.mem_read m 0x80) (Char.code 'A');
  check "fcb dta B" (Dos_machine.mem_read m 0x81) (Char.code 'B');
  (* 파일 크기 가 FCB+0x10 dword 에 *)
  check "fcb size lo" (Dos_machine.mem_read m (0x10000 + 0x14 + 0x10)) 226;

  (* 없는 파일: open AL=FF → fail 경로 exit 1 *)
  let m = Dos_machine.create () in
  Dos_machine.load_exe m exe;
  Dos_machine.run m ~max_steps:500;
  check "fcb missing exit FF" (Dos_machine.exit_code m) 0xFF;

  if !failed = 0 then print_endline "dos FCB: all passed"
  else begin
    Printf.eprintf "dos FCB: %d failures\n%!" !failed;
    exit 1
  end
