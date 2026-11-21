(* COMMAND.COM /C — 디스크에 COMMAND.COM 이 없을 때 AH=4Bh 가 "/C 프로그램
   인자들" 을 받아 그 프로그램을 자식으로 돌린다. Borland C 의 system() 이
   COMSPEC 의 COMMAND.COM 을 이렇게 부른다. 첫 낱말이 COPY 면 내장 COPY 를
   호스트가 한다(아래 「내장 COPY」 절).

   부모 COM 하나가 EXEC 를 부르고 CF·AX·4Dh 결과를 자기 이미지에 남긴다.
   자식은 PSP 의 명령줄 꼬리 16 바이트와 환경 세그먼트를 2000:0000 에
   남기고 코드 37h(COM)·38h(EXE)로 끝난다. 부모의 PSP 는 1000h 다. *)

let mount m name data =
  match Dos_machine.mount_file m name data with
  | Ok () -> ()
  | Error e -> failwith ("mount " ^ e)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end
let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %S want %S\n%!" name got want
  end

let w16s v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

let r_ax = 0x140                   (* EXEC 직후 AX (CF=1 이면 DOS 오류 번호) *)
let r_cf = 0x142                   (* EXEC 직후 CF *)
let r_code = 0x144                 (* 4Dh 의 AX *)
let name_at = 0x150
let epb_at = 0x170
let tail_at = 0x180
let parent_psp = 0x10000
let child_out = 0x20000            (* 자식이 남기는 자리: 2000:0000 *)

(* 자식: PSP:80h 부터 16 바이트를 2000:0 에, 환경 세그먼트를 2000:20h 에
   남기고 [code] 로 끝난다. COM 과 EXE 가 같은 바이트다(DS=PSP 로 시작). *)
let child_code code =
  "\xfc"                           (* cld *)
  ^ "\xb8\x00\x20\x8e\xc0"         (* mov ax,2000h; mov es,ax *)
  ^ "\xbe\x80\x00"                 (* mov si,0080h *)
  ^ "\x31\xff"                     (* xor di,di *)
  ^ "\xb9\x10\x00"                 (* mov cx,0010h *)
  ^ "\xf3\xa4"                     (* rep movsb *)
  ^ "\xa1\x2c\x00"                 (* mov ax,[002Ch] *)
  ^ "\x26\xa3\x20\x00"             (* mov es:[0020h],ax *)
  ^ "\xb8" ^ String.make 1 (Char.chr code) ^ "\x4c\xcd\x21" (* mov ax,4Cxxh; int 21h *)

let w16 b v =
  Buffer.add_char b (Char.chr (v land 0xff));
  Buffer.add_char b (Char.chr ((v lsr 8) land 0xff))

(* 최소 MZ 헤더 + 이미지, 재배치 없음 — dos_exec_test 와 같은 모양. *)
let exe_of image =
  let b = Buffer.create 64 in
  let total = 32 + String.length image in
  Buffer.add_string b "MZ";
  w16 b (total mod 512);
  w16 b ((total + 511) / 512);
  w16 b 0;
  w16 b 2;                         (* 헤더 2 문단 *)
  w16 b 0; w16 b 0;                (* minalloc maxalloc *)
  w16 b 0; w16 b 0xFFFE;           (* ss sp *)
  w16 b 0;
  w16 b 0; w16 b 0;                (* ip cs *)
  w16 b 0x1C; w16 b 0;
  Buffer.add_string b "\x00\x00\x00\x00";
  Buffer.add_string b image;
  Buffer.contents b

let child_com = child_code 0x37
let child_exe = exe_of (child_code 0x38)

(* EXEC 앞에 돌 코드 덩어리. 코드 첫 3 바이트의 jmp 가 [pre_at] 으로 건너
   뛰고, 덩어리 끝의 jmp 가 본 코드로 돌아온다. [code] 와 [data] 는 덩어리
   바로 뒤에 놓일 데이터의 주소 [data_at] 을 받아 코드 바이트와 데이터
   바이트를 돌려준다. *)
let pre_at = 0x1B0
let main_at = 0x103               (* 덩어리가 있으면 본 코드는 jmp 뒤에서 시작한다 *)

let pre_block ~code ~data =
  let len = String.length (code ~data_at:0) in
  let data_at = pre_at + len + 3 in
  code ~data_at ^ "\xe9" ^ w16s (main_at - data_at) ^ data ~data_at

(* 부모 COM: 0x20 문단만 남기고 EXEC [name] 을 부른다. [tail] 은 EPB 의 꼬리
   글(None 이면 꼬리 포인터 0:0)이다. [pre] 가 있으면 EXEC 앞에 그 덩어리를
   돌린다. 이미지는 0x100 바이트 — 코드 뒤에 결과 칸, 이름, EPB, 꼬리가
   온다. *)
let parent ?(name = "C:\\COMMAND.COM") ?pre ~tail () =
  let main =
    "\xb4\x4a\xbb\x20\x00\xcd\x21"           (* mov ah,4Ah; mov bx,0020h; int 21h *)
    ^ "\xb8\x00\x4b"                         (* mov ax,4B00h *)
    ^ "\xba" ^ w16s name_at                  (* mov dx,NAME *)
    ^ "\xbb" ^ w16s epb_at                   (* mov bx,EPB *)
    ^ "\xcd\x21"                             (* int 21h *)
    ^ "\xa3" ^ w16s r_ax                     (* mov [R_AX],ax *)
    ^ "\x9c\x58\x25\x01\x00"                 (* pushf; pop ax; and ax,1 *)
    ^ "\xa3" ^ w16s r_cf                     (* mov [R_CF],ax *)
    ^ "\xb4\x4d\xcd\x21"                     (* mov ah,4Dh; int 21h *)
    ^ "\xa3" ^ w16s r_code                   (* mov [R_CODE],ax *)
    ^ "\xb8\x00\x4c\xcd\x21"                 (* mov ax,4C00h; int 21h *)
  in
  let code =
    match pre with
    | None -> main
    | Some _ -> "\xe9" ^ w16s (pre_at - main_at) ^ main
  in
  let img = Bytes.make 0x100 '\x00' in
  let put off s = Bytes.blit_string s 0 img (off - 0x100) (String.length s) in
  put 0x100 code;
  Option.iter (put pre_at) pre;
  put name_at (name ^ "\x00");
  (match tail with
   | None -> put epb_at (w16s 0 ^ w16s 0 ^ w16s 0)
   | Some text ->
     put epb_at (w16s 0 ^ w16s tail_at ^ w16s 0x1000);
     put tail_at (String.make 1 (Char.chr (String.length text)) ^ text ^ "\r"));
  Bytes.to_string img

let word m a = Dos_machine.mem_read m a lor (Dos_machine.mem_read m (a + 1) lsl 8)
let bytes_at m a n = String.init n (fun i -> Char.chr (Dos_machine.mem_read m (a + i)))

let run ?name ?(files = []) ?pre tail =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> mount m n d) files;
  Dos_machine.load_com m (parent ?name ?pre ~tail ());
  Dos_machine.run m ~max_steps:200_000;
  check "machine exits" (if Dos_machine.exited m then 1 else 0) 1;
  m

let root_list = "COMSPEC=C:\\COMMAND.COM\x00\x00"
let env_tail name = "\x01\x00" ^ name ^ "\x00"

(* 자식이 돌고 4Dh 가 [code] 를 줬는지, 꼬리와 환경 이름이 맞는지 본다 *)
let expect_child what m ~code ~tail ~env_name =
  check (what ^ ": EXEC CF") (word m (parent_psp + r_cf)) 0;
  check (what ^ ": 4Dh") (word m (parent_psp + r_code)) code;
  let want_tail = String.make 1 (Char.chr (String.length tail)) ^ tail ^ "\r" in
  check_s (what ^ ": child tail") (bytes_at m child_out (String.length want_tail)) want_tail;
  let env = word m (child_out + 0x20) * 16 in
  let want_env = root_list ^ env_tail env_name in
  check_s (what ^ ": child environment") (bytes_at m env (String.length want_env)) want_env

let expect_error what m code =
  check (what ^ ": EXEC CF") (word m (parent_psp + r_cf)) 1;
  check (what ^ ": EXEC error") (word m (parent_psp + r_ax)) code;
  check (what ^ ": child did not run") (word m (child_out + 0x20)) 0

let invalid_function = 1
let file_not_found = 2

let () =
  (* 1) 프로그램과 인자: 인자 글은 프로그램 낱말 바로 뒤부터라 앞 공백이
     남는다. 환경의 프로그램 이름은 C: 부터 쓴 전체 경로다. *)
  let m = run ~files:[ ("CHILD.COM", child_com) ] (Some "/C CHILD ARG1 ARG2") in
  expect_child "program with arguments" m ~code:0x37 ~tail:" ARG1 ARG2"
    ~env_name:"C:\\CHILD.COM";
  (* 2) 스위치와 낱말은 대소문자·앞뒤 공백을 가리지 않는다. 인자가 없으면
     꼬리는 비어 있다. *)
  let m = run ~files:[ ("CHILD.COM", child_com) ] (Some "  /c   child") in
  expect_child "lower case, blanks" m ~code:0x37 ~tail:"" ~env_name:"C:\\CHILD.COM";
  let m = run ~files:[ ("CHILD.COM", child_com) ] (Some "/CCHILD") in
  expect_child "no blank after the switch" m ~code:0x37 ~tail:"" ~env_name:"C:\\CHILD.COM"

let () =
  (* 3) 확장자가 없으면 .COM 을 먼저, 없으면 .EXE. 있으면 그 파일이다. *)
  let both = [ ("CHILD.COM", child_com); ("CHILD.EXE", child_exe) ] in
  let m = run ~files:both (Some "/C CHILD") in
  expect_child ".COM before .EXE" m ~code:0x37 ~tail:"" ~env_name:"C:\\CHILD.COM";
  let m = run ~files:[ ("CHILD.EXE", child_exe) ] (Some "/C CHILD") in
  expect_child ".EXE when no .COM" m ~code:0x38 ~tail:"" ~env_name:"C:\\CHILD.EXE";
  let m = run ~files:both (Some "/C child.exe /X") in
  expect_child "explicit .EXE" m ~code:0x38 ~tail:" /X" ~env_name:"C:\\CHILD.EXE"

let () =
  (* 4) 하위 폴더의 프로그램 *)
  let m = run ~files:[ ("DATA\\TOOL.COM", child_com) ] (Some "/C DATA\\TOOL A") in
  expect_child "program in a folder" m ~code:0x37 ~tail:" A" ~env_name:"C:\\DATA\\TOOL.COM"

let () =
  (* 5) 지원 밖: 내장 명령은 파일이 없으니 오류 2, 리디렉션·파이프·배치·
     실행 파일이 아닌 확장자·스위치 없는 꼬리는 오류 1. 자식은 돌지 않는다. *)
  let files =
    [ ("CHILD.COM", child_com); ("GAME.BAT", "ECHO\r\n"); ("README.TXT", "text") ]
  in
  List.iter
    (fun (what, tail, code) -> expect_error what (run ~files tail) code)
    [ ("internal command", Some "/C DIR", file_not_found);
      ("missing program", Some "/C NOPE.EXE", file_not_found);
      ("redirect out", Some "/C CHILD > NUL", invalid_function);
      ("redirect in", Some "/C CHILD < IN", invalid_function);
      ("pipe", Some "/C CHILD | MORE", invalid_function);
      ("batch file by name", Some "/C GAME.BAT", invalid_function);
      ("batch file without extension", Some "/C GAME", invalid_function);
      ("not a program", Some "/C README.TXT", invalid_function);
      ("no switch", Some "CHILD", invalid_function);
      ("other switch letter", Some "/K CHILD", invalid_function);
      ("switch letter without slash", Some "XC CHILD", invalid_function);
      ("switch only", Some "/C", invalid_function);
      ("switch and blanks", Some "/C   ", invalid_function);
      ("no tail at all", None, invalid_function) ]

let () =
  (* 6) 디스크에 COMMAND.COM 이 있으면 그 파일을 돌린다 — 호스트가 대신하지
     않는다. 이름이 COMMAND.COM 이 아닌 없는 파일은 그대로 오류 2. *)
  let command = "\xb8\x66\x4c\xcd\x21" in          (* mov ax,4C66h; int 21h *)
  let m =
    run ~files:[ ("COMMAND.COM", command); ("CHILD.COM", child_com) ] (Some "/C CHILD")
  in
  check "mounted COMMAND.COM: EXEC CF" (word m (parent_psp + r_cf)) 0;
  check "mounted COMMAND.COM runs" (word m (parent_psp + r_code)) 0x66;
  check "mounted COMMAND.COM: no child" (word m (child_out + 0x20)) 0;
  let m =
    run ~name:"C:\\NOPE.COM" ~files:[ ("CHILD.COM", child_com) ] (Some "/C CHILD")
  in
  expect_error "another missing file" m file_not_found

(* ---------- 내장 COPY ----------

   디스크에 COMMAND.COM 이 없을 때 첫 낱말이 COPY 면 호스트가 복사한다.
   EXEC 는 성공하고(CF=0) 종료 코드는 0 이며 자식은 돌지 않는다. 결과는
   마운트 표의 파일 전부와 화면 글로 본다. 지원 밖인 줄은 파일을 하나도
   건드리지 않고 오류 1 로 답한다. *)

let table m =
  List.map (fun n -> (n, Option.get (Dos_machine.read_mounted m n))) (Dos_machine.mounted_names m)

let show_table t = String.concat " " (List.map (fun (n, d) -> Printf.sprintf "%s=%d:%S" n (String.length d) (if String.length d > 8 then String.sub d 0 8 else d)) t)

let check_table what m want =
  let want = List.sort compare want and got = table m in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: files\n  got  %s\n  want %s\n%!" what (show_table got) (show_table want)
  end

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do decr n done;
  String.sub s 0 !n

(* 화면의 빈 줄을 뺀 글줄들. 줄 끝 공백만 뗀다 — 앞 공백은 센 수의 자리다. *)
let screen_lines m =
  List.filter (( <> ) "") (List.map rtrim (String.split_on_char '\n' (Dos_machine.screen_text m)))

let check_lines what m want =
  let got = screen_lines m in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: screen\n  got  [%s]\n  want [%s]\n%!" what
      (String.concat "|" got) (String.concat "|" want)
  end

let copied n = Printf.sprintf "%9d File(s) copied" n

(* 복사가 끝났다: EXEC 성공, 종료 코드 0, 자식은 안 돌았다 *)
let expect_copy what m =
  check (what ^ ": EXEC CF") (word m (parent_psp + r_cf)) 0;
  check (what ^ ": 4Dh") (word m (parent_psp + r_code)) 0;
  check (what ^ ": child did not run") (word m (child_out + 0x20)) 0

(* 지원 밖: 오류 1, 파일은 그대로, 화면에 글이 없다 *)
let expect_unsupported ?pre ?(created = []) ~files what tail =
  let m = run ~files ?pre tail in
  expect_error what m invalid_function;
  check_table what m (created @ files);
  check_lines what m []

let a_sr1 = ("A.SR1", "aaa")
let b_sr1 = ("B.SR1", "bbbb")

let () =
  (* 1) 와일드카드 원본·와일드카드 대상: 이름을 한 줄씩 보이고, 이미 있는
     대상은 덮어쓰고, 원본과 다른 파일은 건드리지 않는다. 낱말은 소문자여도
     같다. *)
  let others = [ a_sr1; b_sr1; ("C.TXT", "c") ] in
  let m = run ~files:(("B.SR", "old") :: others) (Some "/C copy *.SR1 *.SR") in
  expect_copy "wildcard" m;
  check_table "wildcard" m (("A.SR", "aaa") :: ("B.SR", "bbbb") :: others);
  check_lines "wildcard" m [ "A.SR1"; "B.SR1"; copied 2 ]

let () =
  (* 2) 파일 하나: 이름은 보이지 않고 센 수만 나온다. 대상이 이미 있어도
     묻지 않는다. *)
  let m = run ~files:[ a_sr1; ("Z.SR", "old") ] (Some "/C COPY A.SR1 Z.SR") in
  expect_copy "single file" m;
  check_table "single file" m [ a_sr1; ("Z.SR", "aaa") ];
  check_lines "single file" m [ copied 1 ];
  (* 대상이 없는 인자 하나는 현재 디렉터리로 복사한다 — 다른 디렉터리의
     원본이면 된다 *)
  let m = run ~files:[ ("DATA\\X.SR1", "xx") ] (Some "/C COPY DATA\\X.SR1") in
  expect_copy "no destination" m;
  check_table "no destination" m [ ("DATA\\X.SR1", "xx"); ("X.SR1", "xx") ];
  check_lines "no destination" m [ copied 1 ]

let () =
  (* 3) 대상이 디렉터리면 그 안에 같은 이름으로. 끝의 \ 는 있어도 없어도 같다. *)
  let files = [ a_sr1; b_sr1; ("SAVE\\KEEP.TXT", "k") ] in
  List.iter
    (fun (what, tail) ->
      let m = run ~files (Some tail) in
      expect_copy what m;
      check_table what m (("SAVE\\A.SR1", "aaa") :: ("SAVE\\B.SR1", "bbbb") :: files);
      check_lines what m [ "A.SR1"; "B.SR1"; copied 2 ])
    [ ("directory", "/C COPY *.SR1 SAVE"); ("directory with a backslash", "/C COPY *.SR1 SAVE\\") ];
  (* 디렉터리 안의 와일드카드 대상, 그리고 앞 경로가 붙은 원본 이름은 입력한
     그대로 보인다 *)
  let m = run ~files:[ ("DATA\\X.SR1", "xx"); ("SAVE\\KEEP.TXT", "k") ] (Some "/C COPY data\\*.sr1 save\\*.bak") in
  expect_copy "path both sides" m;
  check_table "path both sides" m
    [ ("DATA\\X.SR1", "xx"); ("SAVE\\KEEP.TXT", "k"); ("SAVE\\X.BAK", "xx") ];
  check_lines "path both sides" m [ "DATA\\X.SR1"; copied 1 ]

let () =
  (* 4) 대상 틀: `?` 칸은 원본 글자, 글자 칸은 그 글자, `*` 는 그 칸의 남은
     자리를 `?` 로 채운다. 확장자 칸이 비면 확장자가 없고, 원본에 글자가
     없는 칸은 그냥 빠진다. *)
  let src = [ ("AB.TXT", "ab"); ("C.TXT", "c") ] in
  let m = run ~files:src (Some "/C COPY *.TXT N*.BIN") in
  expect_copy "letter and star" m;
  check_table "letter and star" m (("NB.BIN", "ab") :: ("N.BIN", "c") :: src);
  let m = run ~files:src (Some "/C COPY AB.TXT *.") in
  expect_copy "empty extension" m;
  check_table "empty extension" m (("AB", "ab") :: src);
  let m = run ~files:src (Some "/C COPY C.TXT D*") in
  expect_copy "star without extension" m;
  check_table "star without extension" m (("D", "c") :: src);
  let m = run ~files:src (Some "/C COPY AB.TXT ?X.*") in
  expect_copy "question mark" m;
  check_table "question mark" m (("AX.TXT", "ab") :: src)

let () =
  (* 5) 원본이 없다: 메시지, 센 수 0, EXEC 는 성공하고 파일은 안 생긴다 *)
  List.iter
    (fun (what, tail, shown) ->
      let m = run ~files:[ a_sr1 ] (Some tail) in
      expect_copy what m;
      check_table what m [ a_sr1 ];
      check_lines what m [ "File not found - " ^ shown; copied 0 ])
    [ ("missing file", "/C COPY nope.txt X", "NOPE.TXT");
      ("missing wildcard", "/C COPY *.ZZZ *.OUT", "*.ZZZ") ]

let () =
  (* 6) 원본이 곧 대상이면 그 파일에서 멈춘다. 이름은 먼저 보이고, 뒤 파일은
     건드리지 않는다. *)
  let files = [ a_sr1; b_sr1 ] in
  List.iter
    (fun (what, tail, lines) ->
      let m = run ~files (Some tail) in
      expect_copy what m;
      check_table what m files;
      check_lines what m lines)
    [ ("same file", "/C COPY A.SR1 A.SR1", [ "File cannot be copied onto itself"; copied 0 ]);
      ("same file by wildcard", "/C COPY *.SR1 *.SR1",
       [ "A.SR1"; "File cannot be copied onto itself"; copied 0 ]);
      ("no destination, same directory", "/C COPY A.SR1",
       [ "File cannot be copied onto itself"; copied 0 ]) ]

let () =
  (* 7) 빈 원본: 대상은 지워지고(이미 있던 것도) 센 수에 들지 않는다 *)
  let m =
    run ~files:[ ("E.SR1", ""); ("N.SR1", "n"); ("E.SR", "old") ] (Some "/C COPY *.SR1 *.SR")
  in
  expect_copy "empty source" m;
  check_table "empty source" m [ ("E.SR1", ""); ("N.SR1", "n"); ("N.SR", "n") ];
  check_lines "empty source" m [ "E.SR1"; "N.SR1"; copied 1 ]

let () =
  (* 8) 디스크: 파일 하나가 빈 자리(4096 클러스터 × 2048 바이트)를 넘으면 그
     대상을 지우고 멈춘다. 빈 자리에 꼭 맞는 파일은 복사된다. 멈추기 전에
     복사한 것은 남는다. *)
  let room = 4096 * 2048 in
  let fits = String.make room 'f' and over = String.make (room + 1) 'o' in
  let m = run ~files:[ ("A.SR1", "a"); ("B.SR1", fits) ] (Some "/C COPY *.SR1 *.SR") in
  expect_copy "fits the disk" m;
  check_lines "fits the disk" m [ "A.SR1"; "B.SR1"; copied 2 ];
  check "fits the disk: copied" (match Dos_machine.read_mounted m "B.SR" with Some d -> String.length d | None -> -1) room;
  let m =
    run ~files:[ ("A.SR1", "a"); ("B.SR1", over); ("C.SR1", "c"); ("B.SR", "old") ]
      (Some "/C COPY *.SR1 *.SR")
  in
  expect_copy "over the disk" m;
  check_table "over the disk" m [ ("A.SR1", "a"); ("A.SR", "a"); ("B.SR1", over); ("C.SR1", "c") ];
  check_lines "over the disk" m [ "A.SR1"; "B.SR1"; "Insufficient disk space"; copied 1 ]

let () =
  (* 9) 내장 명령은 같은 이름의 파일보다 먼저다. COPY.COM 이 있어도 호스트의
     COPY 를 하고 그 프로그램은 돌지 않는다. COPY 로 시작할 뿐인 낱말과 다른
     내장 명령은 전과 같다. *)
  let files = [ a_sr1; ("COPY.COM", child_com) ] in
  let m = run ~files (Some "/C copy A.SR1 B.SR1") in
  expect_copy "internal before file" m;
  check_table "internal before file" m (("B.SR1", "aaa") :: files);
  expect_error "word that only starts with COPY" (run ~files (Some "/C COPYX A.SR1 B")) file_not_found;
  expect_error "other internal command" (run ~files (Some "/C DEL A.SR1")) file_not_found

let () =
  (* 10) 지원 밖: 오류 1, 파일은 그대로 *)
  let files = [ a_sr1; b_sr1; ("DATA\\X.TXT", "x"); ("A\\K.TXT", "k") ] in
  List.iter
    (fun (what, tail) -> expect_unsupported ~files what (Some tail))
    [ ("concatenation by plus", "/C COPY A.SR1+B.SR1 C");
      ("switch", "/C COPY /Y A.SR1 B");
      ("switch after", "/C COPY A.SR1 B /V");
      ("comma", "/C COPY A.SR1,B.SR1 C");
      ("no argument", "/C COPY");
      ("three arguments", "/C COPY A.SR1 B.SR1 C");
      ("wildcards to one file", "/C COPY *.SR1 OUT");
      ("source is a directory", "/C COPY DATA *.OUT");
      ("device as destination", "/C COPY A.SR1 NUL");
      ("device as source", "/C COPY CON X");
      ("device with an extension", "/C COPY A.SR1 NUL.TXT");
      ("missing destination directory", "/C COPY A.SR1 NODIR\\X");
      ("missing source directory", "/C COPY NODIR\\*.TXT X");
      ("trailing backslash on a file", "/C COPY A.SR1 B.SR1\\");
      ("name that is a directory", "/C COPY A.SR1 *.");
      ("destination re-enters the sources", "/C COPY *.SR1 B*.SR1");
      ("not a DOS name", "/C COPY A.SR1 B:C");
      ("redirection", "/C COPY A.SR1 B > NUL") ]

let open_text = "OPEN.TXT"

(* 파일을 만들어 글을 쓰고 닫지 않은 채 EXEC 로 간다 *)
let pre_open =
  pre_block
    ~code:(fun ~data_at ->
      "\xb4\x3c\x31\xc9"                    (* mov ah,3Ch; xor cx,cx *)
      ^ "\xba" ^ w16s data_at               (* mov dx,NAME *)
      ^ "\xcd\x21\x89\xc3"                  (* int 21h; mov bx,ax *)
      ^ "\xb4\x40\xb9\x02\x00"              (* mov ah,40h; mov cx,2 *)
      ^ "\xba" ^ w16s (data_at + String.length open_text + 1) (* mov dx,TEXT *)
      ^ "\xcd\x21")                         (* int 21h *)
    ~data:(fun ~data_at:_ -> open_text ^ "\x00hi")

(* 파일을 만들어 표준 출력 자리에 얹는다(AH=46h) *)
let pre_redirect =
  pre_block
    ~code:(fun ~data_at ->
      "\xb4\x3c\x31\xc9"                    (* mov ah,3Ch; xor cx,cx *)
      ^ "\xba" ^ w16s data_at               (* mov dx,NAME *)
      ^ "\xcd\x21\x89\xc3"                  (* int 21h; mov bx,ax *)
      ^ "\xb4\x46\xb9\x01\x00\xcd\x21")     (* mov ah,46h; mov cx,1; int 21h *)
    ~data:(fun ~data_at:_ -> "OUT.TXT\x00")

(* 자식(CHILD.COM, 종료 코드 37h)을 먼저 EXEC 로 돌린다. 그 뒤 4Dh 가 이
   코드를 들고 있다. 이 덩어리가 메모리를 줄이는 것도 본 코드와 같다. *)
let pre_child =
  let name = "C:\\COMMAND.COM\x00" and tail = "\x08/C CHILD\r" in
  pre_block
    ~code:(fun ~data_at ->
      let epb_data_at = data_at + String.length name in
      "\xb4\x4a\xbb\x20\x00\xcd\x21"       (* mov ah,4Ah; mov bx,0020h; int 21h *)
      ^ "\xb8\x00\x4b"                         (* mov ax,4B00h *)
      ^ "\xba" ^ w16s data_at                  (* mov dx,NAME *)
      ^ "\xbb" ^ w16s epb_data_at              (* mov bx,EPB *)
      ^ "\xcd\x21")                            (* int 21h *)
    ~data:(fun ~data_at ->
      let epb_len = 14 in
      let tail_at = data_at + String.length name + epb_len in
      name ^ w16s 0 ^ w16s tail_at ^ w16s 0x1000 ^ String.make (epb_len - 6) '\x00' ^ tail)

let () =
  (* 11) 열려 있는 파일은 원본으로도 대상으로도 안 쓴다 — 닫기 전의 쓰기가
     호스트 표에 없다. 표준 출력이 콘솔이 아니어도 안 한다. 연 파일은 표에
     빈 내용으로 보인다. *)
  let files = [ a_sr1 ] and created = [ (open_text, "") ] in
  expect_unsupported ~pre:pre_open ~created ~files "open source" (Some "/C COPY OPEN.TXT X.TXT");
  expect_unsupported ~pre:pre_open ~created ~files "open source by wildcard"
    (Some "/C COPY OPEN.* *.OUT");
  expect_unsupported ~pre:pre_open ~created ~files "open destination" (Some "/C COPY A.SR1 OPEN.TXT");
  expect_unsupported ~pre:pre_redirect ~created:[ ("OUT.TXT", "") ] ~files "stdout is a file"
    (Some "/C COPY A.SR1 B");
  (* 12) 내장 COPY 의 종료 코드는 0 이다 — 직전 자식의 코드(37h)가 남지 않는다 *)
  let m =
    run ~files:[ a_sr1; ("CHILD.COM", child_com) ] ~pre:pre_child (Some "/C COPY A.SR1 B.SR1")
  in
  check "after a child: the child ran" (if word m (child_out + 0x20) <> 0 then 1 else 0) 1;
  check "after a child: EXEC CF" (word m (parent_psp + r_cf)) 0;
  check "after a child: 4Dh" (word m (parent_psp + r_code)) 0;
  check_table "after a child" m [ a_sr1; ("B.SR1", "aaa"); ("CHILD.COM", child_com) ]

let () =
  if !failed = 0 then print_endline "dos command com: all passed"
  else begin
    Printf.eprintf "dos command com: %d failures\n%!" !failed;
    exit 1
  end
