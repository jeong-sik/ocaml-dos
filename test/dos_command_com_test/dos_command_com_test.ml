(* COMMAND.COM /C — 디스크에 COMMAND.COM 이 없을 때 AH=4Bh 가 "/C 프로그램
   인자들" 을 받아 그 프로그램을 자식으로 돌린다. Borland C 의 system() 이
   COMSPEC 의 COMMAND.COM 을 이렇게 부른다.

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

(* 부모 COM: 0x20 문단만 남기고 EXEC [name] 을 부른다. [tail] 은 EPB 의 꼬리
   글(None 이면 꼬리 포인터 0:0)이다. 이미지는 0x100 바이트 — 코드 뒤에
   결과 칸, 이름, EPB, 꼬리가 온다. *)
let parent ?(name = "C:\\COMMAND.COM") ~tail () =
  let code =
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
  let img = Bytes.make 0x100 '\x00' in
  let put off s = Bytes.blit_string s 0 img (off - 0x100) (String.length s) in
  put 0x100 code;
  put name_at (name ^ "\x00");
  (match tail with
   | None -> put epb_at (w16s 0 ^ w16s 0 ^ w16s 0)
   | Some text ->
     put epb_at (w16s 0 ^ w16s tail_at ^ w16s 0x1000);
     put tail_at (String.make 1 (Char.chr (String.length text)) ^ text ^ "\r"));
  Bytes.to_string img

let word m a = Dos_machine.mem_read m a lor (Dos_machine.mem_read m (a + 1) lsl 8)
let bytes_at m a n = String.init n (fun i -> Char.chr (Dos_machine.mem_read m (a + i)))

let run ?name ?(files = []) tail =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> mount m n d) files;
  Dos_machine.load_com m (parent ?name ~tail ());
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

let () =
  if !failed = 0 then print_endline "dos command com: all passed"
  else begin
    Printf.eprintf "dos command com: %d failures\n%!" !failed;
    exit 1
  end
