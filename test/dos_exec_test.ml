(* AH=4Bh EXEC — 자식 COM/EXE 실행, TSR 상주, 종료 코드 전파.

   계약: 부모는 AH=4Ah 로 제 블록을 줄인 뒤 자식을 띄운다(실기 관례,
   KOEI.COM 로더가 정확히 이 순서를 쓴다). 자식의 종료 코드는 AH=4Dh
   로, EXEC 에서 돌아온 직후의 AX 로도 읽힌다. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let w16 b v =
  Buffer.add_char b (Char.chr (v land 0xff));
  Buffer.add_char b (Char.chr ((v lsr 8) land 0xff))

(* 최소 MZ 헤더 + 이미지 — dos_exe_test 와 같은 모양. *)
let make_exe ~image ~ip ~cs ~ss ~sp relocs =
  let b = Buffer.create (32 + String.length image) in
  Buffer.add_string b "MZ";
  let total = String.length image in
  w16 b ((total - 32) mod 512);
  w16 b ((total + 511) / 512);
  w16 b (List.length relocs);
  w16 b 2;                                  (* header paras = 32 바이트 *)
  w16 b 0; w16 b 0;                         (* minalloc/maxalloc *)
  w16 b ss; w16 b sp;
  w16 b 0;                                  (* checksum *)
  w16 b ip; w16 b cs;
  w16 b 0x1C; w16 b 0;
  List.iter (fun (o, sg) -> w16 b o; w16 b sg) relocs;
  if relocs = [] then Buffer.add_string b "\x00\x00\x00\x00";
  Buffer.add_string b image;
  Buffer.contents b

(* 부모 COM 조립기: 코드 길이를 먼저 계산해 파일명·EPB 오프셋을 유도한다.
   계약: [shrink] paras 로 줄이고, [child1]/[child2] 이름을 차례로
   EXEC 한 뒤 [fin] 시퀀스로 끝낸다. *)
let build_parent ~(shrink : int) ~child1 ~child2 ~(fin : string) =
  let code_len =
    7                                     (* 4A: mov ah,4A; mov bx,shrink; int 21 *)
    + 11                                  (* 4B: mov ax,4B00; mov dx,..; mov bx,..; int 21 *)
    + (if child2 = "" then 0 else 11)
    + String.length fin
  in
  let n1 = 0x100 + code_len in
  let n2 = n1 + String.length child1 + 1 in
  let epb_off = n2 + String.length child2 + 1 in
  let b = Buffer.create 64 in
  Buffer.add_string b "\xb4\x4a";         (* mov ah,4Ah *)
  Buffer.add_string b "\xbb"; w16 b shrink;  (* mov bx,shrink — ES 는 PSP *)
  Buffer.add_string b "\xcd\x21";
  let emit_exec dx =
    Buffer.add_string b "\xb8\x00\x4b";   (* mov ax,4B00h *)
    Buffer.add_string b "\xba"; w16 b dx;
    Buffer.add_string b "\xbb"; w16 b epb_off;
    Buffer.add_string b "\xcd\x21"
  in
  emit_exec n1;
  if child2 <> "" then emit_exec n2;
  Buffer.add_string b fin;
  let code = Buffer.contents b in
  assert (String.length code = code_len);
  ( code ^ child1 ^ "\x00"
    ^ (if child2 = "" then "" else child2 ^ "\x00")
    ^ String.make 14 '\x00'               (* EPB: env 0, 꼬리 0:0, FCB 0:0 *)
  )

let () =
  (* 1) COM 자식: 종료 코드 42 가 부모의 4C 로 흘러간다 *)
  let child = "\xb0\x2a\xb4\x4c\xcd\x21" in   (* mov al,42; 4C; int 21 *)
  let parent =
    build_parent ~shrink:0x20 ~child1:"CHILD.COM" ~child2:""
      ~fin:"\xb4\x4d\xcd\x21\xb4\x4c\xcd\x21"
  in
  let m = Dos_machine.create () in
  Dos_machine.mount_file m "CHILD.COM" child;
  Dos_machine.load_com m parent;
  Dos_machine.run m ~max_steps:5000;
  check "com child code" (Dos_machine.exit_code m) 42;
  check "machine exits" (if Dos_machine.exited m then 1 else 0) 1

let () =
  (* 2) TSR 자식(INT 27h) 뒤 다음 자식은 상주 블록 위에 실린다.
       배치 유도: 부모 루트 블록 4A 로 0x20 paras(이미지 전체보다 커야
       한다 — 작게 부르면 자식 PSP 가 부모 이미지 꼬리를 밟는다) →
       자식1(first fit) 0x1020. TSR 가 dx=0x120 을 남기면 블록
       (0x1020, 0x13) — (0x120+15)/16+1 = 19 = 0x13 paras. 자식2 는
       0x1033 에 실린다.
       자식2 는 자기 CS 를 DS:[0x200] 에 찍고 코드 5 로 끝난다. *)
  let tsr = "\xba\x20\x01\xcd\x27" in          (* mov dx,0x120; int 27h *)
  let child2 =
    "\x8c\xc8"                                  (* mov ax,cs *)
    ^ "\xa3\x00\x02"                            (* mov [0x200],ax *)
    ^ "\xb0\x05\xb4\x4c\xcd\x21"                (* mov al,5; 4C; int 21 *)
  in
  let parent =
    build_parent ~shrink:0x20 ~child1:"TSR.COM" ~child2:"CHILD2.COM"
      ~fin:"\xb4\x4d\xcd\x21\xb4\x4c\xcd\x21"
  in
  let m = Dos_machine.create () in
  Dos_machine.mount_file m "TSR.COM" tsr;
  Dos_machine.mount_file m "CHILD2.COM" child2;
  Dos_machine.load_com m parent;
  Dos_machine.run m ~max_steps:8000;
  check "tsr parent exits" (if Dos_machine.exited m then 1 else 0) 1;
  check "child2 code" (Dos_machine.exit_code m) 5;
  let child2_cs =
    Dos_machine.mem_read m 0x10530
    lor (Dos_machine.mem_read m 0x10531 lsl 8)
  in
  check "child2 placed above tsr" child2_cs 0x1033

let () =
  (* 3) MZ 자식: 재배치 없는 EXE 를 띄워 코드 33 으로 끝나게 한다 *)
  let child_img = "\xb0\x21\xb4\x4c\xcd\x21\x00\x00" in
  let exe = make_exe ~image:child_img ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE [] in
  let parent =
    build_parent ~shrink:0x20 ~child1:"CHILD.EXE" ~child2:""
      ~fin:"\xb4\x4d\xcd\x21\xb4\x4c\xcd\x21"
  in
  let m = Dos_machine.create () in
  Dos_machine.mount_file m "CHILD.EXE" exe;
  Dos_machine.load_com m parent;
  Dos_machine.run m ~max_steps:8000;
  check "exe child code" (Dos_machine.exit_code m) 33

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos EXEC: all passed\n"
