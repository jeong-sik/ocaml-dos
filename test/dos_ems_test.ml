(* EMS 4.0 스텁 테스트 — 게스트가 INT 21h AH=35h AL=67h 로 드라이버를
   알아보고(INT 67h 세그먼트+0x0A 의 이름), 프레임을 얻고, 페이지를
   할당·매핑해 프레임에 쓴 값이 논리 페이지를 따라 다니는지까지 본다.
   삼국지3 런타임이 데이터 버퍼를 얻는 길 그대로다. *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ah n = "\xb4" ^ b n
let int_ n = "\xcd" ^ b n
let quit = "\xb8" ^ w 0x4C00 ^ int_ 0x21

let psp_base = 0x1000 * 16
let peek m off = Dos_machine.mem_read m (psp_base + off)
let peek16 m off = peek m off lor (peek m (off + 1) lsl 8)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got=%d (0x%x) want=%d (0x%x)\n%!" name got got want
      want
  end

let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s:\n  got=%S\n  want=%S\n%!" name got want
  end

let () =
  let m = Dos_machine.create () in
  let code =
    (* 1. INT 67h 벡터 세그먼트 +0x0A 의 이름 여덟 바이트를 옮겨 온다 *)
    "\xb8" ^ w 0x3567 ^ int_ 0x21
    ^ "\x8c\xc0"                                  (* mov ax,es *)
    ^ "\x8e\xd8"                                  (* mov ds,ax *)
    ^ "\xbe\x0a\x00"                              (* mov si,0x0A *)
    ^ "\xb8" ^ w 0x1000 ^ "\x8e\xc0"              (* mov es,0x1000 *)
    ^ "\xbf" ^ w 0x2000                           (* mov di,0x2000 *)
    ^ "\xb9" ^ w 8 ^ "\xfc\xf3\xa4"               (* mov cx,8; cld; rep movsb *)
    (* 이후 DS 도 PSP 로 *)
    ^ "\x8c\xc0" ^ "\x8e\xd8"                     (* mov ax,es; mov ds,ax *)
    (* 2. 프레임 세그먼트 *)
    ^ mov_ah 0x41 ^ int_ 0x67
    ^ "\x89\x1e" ^ w 0x2008                       (* [0x2008]=bx *)
    (* 3. 페이지 둘 할당 → 핸들 (LIM: DX=요청 페이지 수, 돌려받는 것도 DX) *)
    ^ "\xba" ^ w 2 ^ mov_ah 0x43 ^ int_ 0x67
    ^ "\x89\x16" ^ w 0x200A                       (* [0x200A]=dx *)
    (* 4. 물리 0 에 논리 0 매핑, 프레임에 표식 *)
    ^ "\xb8" ^ w 0x4400 ^ "\x2b\xdb" ^ int_ 0x67  (* ax=4400, bx=0 *)
    ^ "\xb8" ^ w 0xD000 ^ "\x8e\xc0"              (* mov es,0xD000 *)
    ^ "\x26\xc6\x06" ^ w 0x100 ^ b 0xA5           (* mov es:[100],0A5h *)
    (* 5. 논리 1 로 바꾸면 그 자리는 0 *)
    ^ "\xb8" ^ w 0x4400 ^ "\xbb" ^ w 1 ^ int_ 0x67
    ^ "\x26\x8a\x06" ^ w 0x100                    (* mov al,es:[100] *)
    ^ "\xa2" ^ w 0x200C
    (* 6. 논리 0 로 돌아오면 표식이 살아 있다 *)
    ^ "\xb8" ^ w 0x4400 ^ "\x2b\xdb" ^ int_ 0x67
    ^ "\x26\x8a\x06" ^ w 0x100
    ^ "\xa2" ^ w 0x200D
    ^ quit
  in
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:400;
  if not (Dos_machine.exited m) then begin
    incr failed;
    Printf.eprintf "FAIL EMS 프로그램이 안 끝났다\n%!"
  end;
  let name = String.init 8 (fun i -> Char.chr (peek m (0x2000 + i))) in
  check_s "드라이버 이름 EMMXXXX0" name "EMMXXXX0";
  check "프레임 세그먼트 0xD000" (peek16 m 0x2008) 0xD000;
  check "핸들 발급" (if peek16 m 0x200A >= 1 then 1 else 0) 1;
  check "논리 1 은 백지" (peek m 0x200C) 0;
  check "논리 0 표식 보존" (peek m 0x200D) 0xA5;
  if !failed = 0 then print_endline "dos ems: all passed"
  else begin
    Printf.eprintf "dos ems: %d failures\n%!" !failed;
    exit 1
  end
