(* DOS/BIOS 표면 회귀 — 실제 COM 프로그램을 기계에 태워 결과를 읽는다.
   게스트가 보는 것만 증거로 쓴다: 화면, 게스트 메모리, 마운트 표. *)

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

let check_true name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* ---------- 아주 작은 어셈블러 ---------- *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ah n = "\xb4" ^ b n
let mov_al n = "\xb0" ^ b n
let mov_ax n = "\xb8" ^ w n
let mov_bx n = "\xbb" ^ w n
let mov_bl n = "\xb3" ^ b n
let mov_cx n = "\xb9" ^ w n
let mov_dx n = "\xba" ^ w n
let mov_bx_ax = "\x89\xc3"
let int_ n = "\xcd" ^ b n
let store_ax a = "\xa3" ^ w a
let store_cx a = "\x89\x0e" ^ w a
let store_dx a = "\x89\x16" ^ w a
let mov_ax_es = "\x8c\xc0"
let mov_ax_ds = "\x8c\xd8"
let store_bx a = "\x89\x1e" ^ w a
let sbb_ax_ax = "\x19\xc0"
let sbb_cx_cx = "\x19\xc9"
let quit = mov_ax 0x4c00 ^ int_ 0x21

(* COM 은 0x100 에 실린다. 데이터를 코드 뒤에 붙이고, 코드가 쓰는
   데이터 오프셋은 코드 길이에서 나온다 — 인코딩 길이가 고정이라
   더미 오프셋으로 한 번 재면 된다. *)
let assemble code_of data =
  let off = 0x100 + String.length (code_of 0) in
  code_of off ^ data

let psp_base = 0x1000 * 16
let scratch = 0x2000
let peek m off = Dos_machine.mem_read m (psp_base + off)
let peek16 m off = peek m off lor (peek m (off + 1) lsl 8)

let run_com ?(steps = 500_000) ?(mounts = []) code =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> Dos_machine.mount_file m n d) mounts;
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:steps;
  m

let screen_line m n =
  let s = Dos_machine.screen_text m in
  List.nth (String.split_on_char '\n' s) n

let trim_right s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do decr n done;
  String.sub s 0 !n

(* ---------- INT 21h ---------- *)

let test_print_string () =
  let m =
    run_com
      (assemble
         (fun off -> mov_ah 0x09 ^ mov_dx off ^ int_ 0x21 ^ quit)
         "HELLO$")
  in
  check_s "AH=09 문자열 출력" (trim_right (screen_line m 0)) "HELLO"

let test_version () =
  let m =
    run_com (assemble (fun _ -> mov_ah 0x30 ^ int_ 0x21 ^ store_ax scratch ^ quit) "")
  in
  (* AL=주 버전 5, AH=부 버전 0 *)
  check "AH=30 DOS 버전" (peek16 m scratch) 0x0005

let test_clock_is_deterministic () =
  let code =
    assemble (fun _ -> mov_ah 0x2C ^ int_ 0x21 ^ store_cx scratch
                       ^ store_dx (scratch + 2) ^ quit) ""
  in
  let m = Dos_machine.create () in
  Dos_machine.set_clock m ~year:1992 ~month:3 ~day:4 ~hour:13 ~minute:45
    ~second:7;
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:100_000;
  check "AH=2C 시" (peek m (scratch + 1)) 13;
  check "AH=2C 분" (peek m scratch) 45;
  check "AH=2C 초" (peek m (scratch + 3)) 7;
  (* 같은 프로그램을 다시 태우면 같은 값이 나와야 한다 *)
  let m2 = Dos_machine.create () in
  Dos_machine.set_clock m2 ~year:1992 ~month:3 ~day:4 ~hour:13 ~minute:45
    ~second:7;
  Dos_machine.load_com m2 code;
  Dos_machine.run m2 ~max_steps:100_000;
  check "AH=2C 재현" (peek16 m2 scratch) (peek16 m scratch)

let test_date () =
  let code =
    assemble (fun _ -> mov_ah 0x2A ^ int_ 0x21 ^ store_cx scratch
                       ^ store_dx (scratch + 2) ^ store_ax (scratch + 4) ^ quit) ""
  in
  let m = Dos_machine.create () in
  Dos_machine.set_clock m ~year:1992 ~month:3 ~day:4 ~hour:0 ~minute:0
    ~second:0;
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:100_000;
  check "AH=2A 연" (peek16 m scratch) 1992;
  check "AH=2A 월" (peek m (scratch + 3)) 3;
  check "AH=2A 일" (peek m (scratch + 2)) 4;
  (* 1992-03-04 는 수요일 *)
  check "AH=2A 요일" (peek m (scratch + 4)) 3

(* 실기 DOS 는 .EXE/.COM 에 남은 메모리를 통째로 준다. 먼저 제 블록을
   줄여야(AH=4Ah) 할당(AH=48h)이 성공한다 — Turbo Pascal 이 그렇게 한다. *)
let test_memory_allocation () =
  let m =
    run_com
      (assemble
         (fun _ ->
           mov_ah 0x48 ^ mov_bx 0x0100 ^ int_ 0x21 ^ sbb_ax_ax
           ^ store_ax scratch                      (* 실패면 FFFF *)
           ^ mov_ah 0x4A ^ mov_bx 0x0100 ^ int_ 0x21
           ^ mov_ah 0x48 ^ mov_bx 0x0020 ^ int_ 0x21
           ^ store_ax (scratch + 2) ^ sbb_cx_cx ^ store_cx (scratch + 4)
           ^ quit)
         "")
  in
  check "줄이기 전 할당은 실패" (peek16 m scratch) 0xFFFF;
  check "줄인 뒤 할당은 성공" (peek16 m (scratch + 4)) 0x0000;
  check_true "받은 세그먼트는 프로그램 뒤"
    (peek16 m (scratch + 2) >= 0x1100 && peek16 m (scratch + 2) < 0xA000)

let test_file_round_trip () =
  let name = "T.DAT\000" in
  let payload = "XYZ" in
  let m =
    run_com
      (assemble
         (fun off ->
           let name_off = off and data_off = off + String.length name in
           mov_ah 0x3C ^ mov_cx 0 ^ mov_dx name_off ^ int_ 0x21 ^ mov_bx_ax
           ^ mov_ah 0x40 ^ mov_cx 3 ^ mov_dx data_off ^ int_ 0x21
           ^ mov_ah 0x3E ^ int_ 0x21
           ^ mov_ah 0x3D ^ mov_al 0 ^ mov_dx name_off ^ int_ 0x21 ^ mov_bx_ax
           ^ mov_ah 0x3F ^ mov_cx 3 ^ mov_dx scratch ^ int_ 0x21
           ^ store_ax (scratch + 8)
           ^ mov_ah 0x3E ^ int_ 0x21 ^ quit)
         (name ^ payload))
  in
  check "읽은 바이트 수" (peek16 m (scratch + 8)) 3;
  let got = String.init 3 (fun i -> Char.chr (peek m (scratch + i))) in
  check_s "파일 왕복" got payload;
  check_s "호스트가 꺼낸 내용"
    (match Dos_machine.read_mounted m "T.DAT" with Some s -> s | None -> "")
    payload

let test_find_first_next () =
  let pattern = "*.TXT\000" in
  let m =
    run_com
      ~mounts:[ ("AB.TXT", "aa"); ("CD.TXT", "bbb"); ("EF.DAT", "c") ]
      (assemble
         (fun off ->
           mov_ah 0x1A ^ mov_dx scratch ^ int_ 0x21      (* DTA *)
           ^ mov_ah 0x4E ^ mov_cx 0 ^ mov_dx off ^ int_ 0x21
           ^ sbb_ax_ax ^ store_ax (scratch + 0x40)
           ^ mov_ah 0x4F ^ int_ 0x21 ^ sbb_cx_cx ^ store_cx (scratch + 0x42)
           ^ quit)
         pattern)
  in
  check "findfirst 성공" (peek16 m (scratch + 0x40)) 0x0000;
  check "findnext 성공" (peek16 m (scratch + 0x42)) 0x0000;
  (* 두 번째 이름이 DTA 에 남아 있다 — 정렬이라 CD.TXT *)
  let name =
    let b = Buffer.create 12 in
    let i = ref 0 in
    while peek m (scratch + 0x1E + !i) <> 0 && !i < 12 do
      Buffer.add_char b (Char.chr (peek m (scratch + 0x1E + !i)));
      incr i
    done;
    Buffer.contents b
  in
  check_s "findnext 이름" name "CD.TXT";
  check_true "와일드카드가 확장자를 가린다"
    (Dos_dos.matches_pattern ~pattern:"*.TXT" ~name:"AB.TXT"
     && not (Dos_dos.matches_pattern ~pattern:"*.TXT" ~name:"EF.DAT"))

(* AH=0Ch 은 키 버퍼를 비운 뒤 AL 이 가리키는 입력 기능을 실제로 부른다.
   비우기만 하고 끝내면 게스트는 오지 않을 글자를 기다린다. *)
let test_flush_then_input () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    (assemble
       (fun _ -> mov_ax 0x0C08 ^ int_ 0x21 ^ store_ax scratch ^ quit) "");
  Dos_machine.type_string m "ab";
  Dos_machine.run m ~max_steps:100_000;
  (* 버퍼를 비웠으니 뒤이은 읽기는 굶는다 — AL=0. 비우기만 했다면 AL 에
     기능 번호 8 이 그대로 남는다. *)
  check "AH=0C 가 하위 기능을 부른다" (peek m scratch) 0;
  check_true "버퍼가 비었다" (Dos_machine.kbd_waiting m)

(* dup 은 같은 열린 파일을 가리킨다 — 두 핸들이 위치를 나눠 쓴다. *)
let test_dup_shares_position () =
  let name = "D.DAT\000" in
  let m =
    run_com
      (assemble
         (fun off ->
           let name_off = off and a_off = off + String.length name in
           let b_off = a_off + 2 in
           mov_ah 0x3C ^ mov_cx 0 ^ mov_dx name_off ^ int_ 0x21 ^ mov_bx_ax
           ^ mov_ah 0x40 ^ mov_cx 2 ^ mov_dx a_off ^ int_ 0x21
           ^ mov_ah 0x45 ^ int_ 0x21 ^ mov_bx_ax      (* dup — BX=새 핸들 *)
           ^ mov_ah 0x40 ^ mov_cx 2 ^ mov_dx b_off ^ int_ 0x21
           ^ mov_ah 0x3E ^ int_ 0x21 ^ quit)
         (name ^ "AB" ^ "CD"))
  in
  check_s "dup 이 위치를 이어 쓴다"
    (match Dos_machine.read_mounted m "D.DAT" with Some s -> s | None -> "")
    "ABCD"

(* 이름으로 키를 부르는 표. 하네스가 16진수 워드를 손으로 쓰지 않아도
   되고, 자리 없는 글자는 조용히 통과하지 않고 거절당한다. *)
let test_key_names () =
  let ok name want =
    match Dos_machine.key_of_string name with
    | Ok w -> check ("key_of_string " ^ name) w want
    | Error e -> incr failed; Printf.eprintf "FAIL %s: %s\n%!" name e
  in
  ok "up" 0x4800;
  ok "RIGHT" 0x4D00;
  ok "enter" 0x1C0D;
  ok "return" 0x1C0D;
  ok "esc" 0x011B;
  ok "space" 0x3920;
  ok "f10" 0x4400;
  ok "a" 0x1E61;
  ok "1" 0x0231;
  check_true "모르는 이름은 거절"
    (Result.is_error (Dos_machine.key_of_string "hyperspace"));
  (* US 자판에 자리가 없는 글자를 0 번 스캔 코드로 넣으면 게스트가 엉뚱한
     키로 읽는다 — 조용히 통과시키지 않는다. *)
  check_true "자리 없는 글자는 거절"
    (Result.is_error (Dos_machine.key_of_string "~"));
  check_s "이름으로 되돌리기" (Dos_machine.key_to_string 0x4800) "up";
  check_s "글자로 되돌리기" (Dos_machine.key_to_string 0x1E61) "a"

(* 이름으로 부른 키가 게스트에게 실제로 닿는가 *)
let test_named_key_reaches_guest () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    (assemble
       (fun _ -> mov_ah 0 ^ int_ 0x16 ^ store_ax scratch ^ quit) "");
  (match Dos_machine.key_of_string "up" with
   | Ok w -> Dos_machine.push_key m w
   | Error e -> failwith e);
  Dos_machine.run m ~max_steps:100_000;
  check "게스트가 읽은 워드" (peek16 m scratch) 0x4800

(* 화면 지문과 입력 요구 계수 — 하네스가 "게스트가 반응을 끝냈나" 를
   판정하는 두 신호다. 굶주림 래치는 "지금 기다리는가" 만 답하므로,
   한 구간 안에서 물었다가 받아간 것을 보려면 계수가 필요하다. *)
let test_screen_digest_follows_the_screen () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    (assemble
       (fun off -> mov_ah 0x09 ^ mov_dx off ^ int_ 0x21 ^ "\xeb\xfe") "Z$");
  let blank = Dos_machine.screen_digest m in
  Dos_machine.run m ~max_steps:100_000;
  let written = Dos_machine.screen_digest m in
  check_true "글자가 찍히면 지문이 바뀐다" (written <> blank);
  (* 프로그램은 이제 제자리를 돌 뿐이다 — 화면이 안 변하니 지문도 같다 *)
  ignore (Dos_machine.run_until m ~max_steps:200_000 ~stop:(fun _ -> false) : int);
  check "화면이 안 변하면 지문도 같다" (Dos_machine.screen_digest m) written

let test_input_requests_count_empty_reads () =
  let poll_twice =
    assemble (fun _ -> mov_ah 0x01 ^ int_ 0x16 ^ mov_ah 0x01 ^ int_ 0x16 ^ quit) ""
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m poll_twice;
  check "시작은 0" (Dos_machine.input_requests m) 0;
  Dos_machine.run m ~max_steps:100_000;
  check "빈 링을 두 번 물었다" (Dos_machine.input_requests m) 2;
  (* 링에 키가 있으면 굶지 않는다. AH=01 은 들여다볼 뿐 꺼내지 않으므로
     두 번째 폴링도 같은 키를 본다 — 그래서 둘 다 안 센다. *)
  let m2 = Dos_machine.create () in
  Dos_machine.load_com m2 poll_twice;
  Dos_machine.type_string m2 "a";
  Dos_machine.run m2 ~max_steps:100_000;
  check "키가 있으면 안 센다" (Dos_machine.input_requests m2) 0

(* 키보드를 직접 읽는 게임은 INT 16h 를 부르지 않고 BDA 링의 머리
   (0040:001A)를 꼬리(0040:001C)와 견준다 — 삼국지3 의 카피프로텍션과
   이름 입력이 그렇다(로고 뒤로 INT 16h 가 한 번도 안 불린다). 머리를
   읽는 것도 키를 묻는 것이므로, 빈 링이면 INT 16h 와 똑같이 센다. *)
let test_direct_ring_poll_counts_as_request () =
  let read_head_twice =
    (* mov ax,0040h; mov ds,ax; (mov ax,[001Ah]; cmp ax,[001Ch]) x2 *)
    let peek = "\xa1\x1a\x00\x3b\x06\x1c\x00" in
    assemble (fun _ -> "\xb8\x40\x00\x8e\xd8" ^ peek ^ peek ^ quit) ""
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m read_head_twice;
  Dos_machine.run m ~max_steps:100_000;
  check "빈 링의 머리를 두 번 읽었다" (Dos_machine.input_requests m) 2;
  check_true "직접 폴링도 기다림으로 보인다" (Dos_machine.kbd_waiting m);
  let m2 = Dos_machine.create () in
  Dos_machine.load_com m2 read_head_twice;
  Dos_machine.type_string m2 "a";
  Dos_machine.run m2 ~max_steps:100_000;
  check "키가 있으면 안 센다" (Dos_machine.input_requests m2) 0;
  check_true "키가 있으면 기다리지 않는다" (not (Dos_machine.kbd_waiting m2))

(* AH=01h 의 답은 ZF 로 돌아간다 — 스텁의 iret 이 프레임의 플래그를
   되돌리므로 프레임 워드를 고쳐야 호출자의 je/jne 가 본다. 삼국지3
   메뉴 대기 루프가 je 를 항상 탄 채 키를 못 받은 실측을 고정한다. *)
let test_int16_ah01_reports_key_via_zf () =
  let probe =
    assemble
      (fun _ ->
         mov_ah 0x01 ^ int_ 0x16
         ^ "\x75\x05"  (* jne have — 키가 있으면 ZF=0 *)
         ^ "\xb8\x00\x00"  (* mov ax,0 — 없음 경로 *)
         ^ "\xeb\x03"  (* jmp done *)
         ^ "\xb8\x01\x00"  (* have: mov ax,1 *)
         ^ store_ax scratch ^ quit)
      ""
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m probe;
  Dos_machine.run m ~max_steps:100_000;
  check "빈 링이면 je 를 탄다(0)" (peek16 m scratch) 0;
  let m2 = Dos_machine.create () in
  Dos_machine.load_com m2 probe;
  Dos_machine.type_string m2 "a";
  Dos_machine.run m2 ~max_steps:100_000;
  check "키가 있으면 jne 를 탄다(1)" (peek16 m2 scratch) 1

(* 같은 이치로 INT 21h 의 성공/실패는 CF 로 돌아간다. 파일 열기
   (AH=3Dh) 하나로 양쪽을 다 본다 — 실제 게스트 호출 경로(스텁
   iret)를 그대로 탄다. *)
let test_int21_cf_reaches_the_caller () =
  let probe name =
    assemble
      (fun off ->
         mov_ah 0x3D ^ mov_al 0 ^ mov_dx off
         ^ int_ 0x21
         ^ "\x73\x05"  (* jnc have — 성공이면 CF=0 *)
         ^ "\xb8\x00\x00"  (* mov ax,0 — 실패 경로 *)
         ^ "\xeb\x03"  (* jmp done *)
         ^ "\xb8\x01\x00"  (* have: mov ax,1 *)
         ^ store_ax scratch ^ quit)
      name
  in
  let m = run_com ~mounts:[ ("Z.TXT", "z") ] (probe "QQ$\x00") in
  check "없는 파일이면 CF=1(jnc 안 탐)" (peek16 m scratch) 0;
  let m2 = run_com ~mounts:[ ("Z.TXT", "z") ] (probe "Z.TXT\x00") in
  check "있는 파일이면 CF=0(jnc 탐)" (peek16 m2 scratch) 1

let test_console_scrolls () =
  (* 화면이 넘치면 위로 밀려야 한다. 마지막 칸에 붙들어 두면 출력이
     통째로 사라진다. 30 줄을 찍고 첫 줄이 사라졌는지 본다. *)
  let line n = Printf.sprintf "L%02d\r\n" n in
  let text = String.concat "" (List.init 30 line) in
  let m =
    run_com
      (assemble
         (fun off -> mov_ah 0x09 ^ mov_dx off ^ int_ 0x21 ^ quit)
         (text ^ "$"))
  in
  check_s "첫 줄은 밀려 나갔다" (trim_right (screen_line m 0)) "L06";
  check_s "마지막 줄" (trim_right (screen_line m 23)) "L29"

(* ---------- INT 10h ---------- *)

let test_video_write_char () =
  let m =
    run_com
      (assemble
         (fun _ ->
           mov_ah 0x02 ^ mov_bx 0 ^ "\xb6\x02" (* mov dh,2 *)
           ^ "\xb2\x05" (* mov dl,5 *) ^ int_ 0x10
           ^ mov_ah 0x09 ^ mov_al (Char.code 'X') ^ mov_bl 0x07 ^ mov_cx 5
           ^ int_ 0x10 ^ quit)
         "")
  in
  check_s "AH=09 반복 쓰기" (trim_right (screen_line m 2)) "     XXXXX"

let test_video_scroll_clears () =
  let m =
    run_com
      (assemble
         (fun off ->
           mov_ah 0x09 ^ mov_dx off ^ int_ 0x21
           (* AL=0 은 창 전체 지우기 *)
           ^ mov_ah 0x06 ^ mov_al 0 ^ "\xb7\x07" (* mov bh,7 *)
           ^ mov_cx 0 ^ mov_dx 0x184F ^ int_ 0x10 ^ quit)
         "WIPE ME$")
  in
  check_s "AH=06 전체 지우기" (trim_right (screen_line m 0)) ""

(* ---------- 키보드 ---------- *)

let test_typed_keys_read_back () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    (assemble
       (fun _ ->
         mov_ah 0 ^ int_ 0x16 ^ store_ax scratch
         ^ mov_ah 0 ^ int_ 0x16 ^ store_ax (scratch + 2) ^ quit)
       "");
  Dos_machine.type_string m "ab";
  Dos_machine.run m ~max_steps:100_000;
  check "첫 키 ASCII" (peek m scratch) (Char.code 'a');
  check "둘째 키 ASCII" (peek m (scratch + 2)) (Char.code 'b');
  check "첫 키 스캔 코드" (peek m (scratch + 1)) 0x1E

(* ---------- 장치 포트 ---------- *)

let test_dac_ports () =
  let m =
    run_com
      (assemble
         (fun _ ->
           mov_dx 0x3C8 ^ mov_al 5 ^ "\xee"          (* out dx,al *)
           ^ mov_dx 0x3C9 ^ mov_al 0x11 ^ "\xee"
           ^ mov_al 0x22 ^ "\xee" ^ mov_al 0x33 ^ "\xee"
           (* INT 10h AX=1015 로 되읽는다: DH=R CH=G CL=B *)
           ^ mov_ax 0x1015 ^ mov_bx 5 ^ int_ 0x10
           ^ store_cx scratch ^ store_dx (scratch + 2) ^ quit)
         "")
  in
  check "DAC 초록" (peek m (scratch + 1)) 0x22;
  check "DAC 파랑" (peek m scratch) 0x33;
  check "DAC 빨강" (peek m (scratch + 3)) 0x11

let test_word_port_in_reads_two_bytes () =
  let m =
    run_com
      (assemble
         (fun _ -> mov_dx 0x1234 ^ "\xed" (* in ax,dx *) ^ store_ax scratch
                   ^ quit)
         "")
  in
  (* 아무도 없는 포트는 양쪽 다 0xFF 를 돌려준다 — 한 번만 읽으면
     상위 바이트가 0 이 된다. *)
  check "워드 IN 은 포트 두 개를 읽는다" (peek16 m scratch) 0xFFFF

(* ---------- 타이머 ---------- *)

let spin_program = "\xfb\xeb\xfe"          (* sti; jmp $ *)
let spin_no_int = "\xfa\xeb\xfe"           (* cli; jmp $ *)

let test_timer_ticks () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m spin_program;
  ignore (Dos_machine.run_until m ~max_steps:3_000_000 ~stop:(fun _ -> false));
  check_true "타이머가 돈다" (Dos_machine.tick_count m > 0)

let test_cli_holds_the_tick () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m spin_no_int;
  ignore (Dos_machine.run_until m ~max_steps:3_000_000 ~stop:(fun _ -> false));
  check "인터럽트를 막으면 틱이 안 들어온다" (Dos_machine.tick_count m) 0

let test_pit_divisor_speeds_up_ticks () =
  (* 채널 0 을 분주비 0x1000 으로 — 기본 65536 보다 16 배 빠르다. *)
  let prog =
    assemble
      (fun _ ->
        mov_dx 0x43 ^ mov_al 0x36 ^ "\xee"
        ^ mov_dx 0x40 ^ mov_al 0x00 ^ "\xee" ^ mov_al 0x10 ^ "\xee"
        ^ "\xfb\xeb\xfe")
      ""
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m prog;
  ignore (Dos_machine.run_until m ~max_steps:3_000_000 ~stop:(fun _ -> false));
  let fast = Dos_machine.tick_count m in
  let m2 = Dos_machine.create () in
  Dos_machine.load_com m2 spin_program;
  ignore (Dos_machine.run_until m2 ~max_steps:3_000_000 ~stop:(fun _ -> false));
  let normal = Dos_machine.tick_count m2 in
  check_true
    (Printf.sprintf "분주비를 줄이면 틱이 빨라진다 (fast=%d normal=%d)" fast
       normal)
    (fast > normal * 4)

(* ---------- 화면 읽기 ---------- *)

let test_cp437_text () =
  let m =
    run_com
      (assemble
         (fun _ ->
           mov_ah 0x02 ^ mov_bx 0 ^ "\xb6\x00\xb2\x00" ^ int_ 0x10
           ^ mov_ah 0x09 ^ mov_al 0x02 ^ mov_bl 0x07 ^ mov_cx 1 ^ int_ 0x10
           ^ quit)
         "")
  in
  let line = List.hd (String.split_on_char '\n' (Dos_machine.screen_text_utf8 m)) in
  check_true "CP437 그림 문자가 산다"
    (String.length line > 0 && String.sub line 0 3 = "\xe2\x98\xbb")

(* 벡터 조회·DTA 조회는 결과를 ES:BX 로 돌려주고 DS 를 보존해야 한다.
   삼국지3 MAIN 의 그리기 루프가 DS 를 자기 데이터로 쓴 채 AH=35h 를
   부르는데, DS 가 갈리면 이후 그리기가 엉뚱한 세그먼트로 향한다(실측). *)
let test_get_vector_preserves_ds () =
  let m =
    run_com
      (assemble
         (fun _ ->
            mov_al 0x08 ^ mov_ah 0x35 ^ int_ 0x21 ^ mov_ax_es
            ^ store_ax scratch ^ store_bx (scratch + 2)
            ^ mov_ax_ds ^ store_ax (scratch + 4) ^ quit)
         "")
  in
  check "AH=35 ES=INT8 세그먼트(ROM)" (peek16 m scratch) 0xF000;
  check "AH=35 BX=INT8 오프셋" (peek16 m (scratch + 2)) 0x0040;
  check "AH=35 DS 보존" (peek16 m (scratch + 4)) 0x1000

let test_get_dta_preserves_ds () =
  let m =
    run_com
      (assemble
         (fun _ ->
            mov_ah 0x2F ^ int_ 0x21 ^ mov_ax_es ^ store_ax scratch
            ^ store_bx (scratch + 2) ^ mov_ax_ds ^ store_ax (scratch + 4)
            ^ quit)
         "")
  in
  (* ES:BX 의 평탄 주소가 PSP:0x80(기본 DTA) 이면 된다 — 세그먼트·
     오프셋을 어떻게 나눠 돌려주는지는 계약 밖이다. *)
  let flat = (peek16 m scratch lsl 4) + peek16 m (scratch + 2) in
  check "AH=2F ES:BX=기본 DTA" flat (psp_base + 0x80);
  check "AH=2F DS 보존" (peek16 m (scratch + 4)) 0x1000

let () =
  test_print_string ();
  test_version ();
  test_get_vector_preserves_ds ();
  test_get_dta_preserves_ds ();
  test_clock_is_deterministic ();
  test_date ();
  test_memory_allocation ();
  test_file_round_trip ();
  test_find_first_next ();
  test_flush_then_input ();
  test_key_names ();
  test_screen_digest_follows_the_screen ();
  test_input_requests_count_empty_reads ();
  test_direct_ring_poll_counts_as_request ();
  test_int16_ah01_reports_key_via_zf ();
  test_int21_cf_reaches_the_caller ();
  test_named_key_reaches_guest ();
  test_dup_shares_position ();
  test_console_scrolls ();
  test_video_write_char ();
  test_video_scroll_clears ();
  test_typed_keys_read_back ();
  test_dac_ports ();
  test_word_port_in_reads_two_bytes ();
  test_timer_ticks ();
  test_cli_holds_the_tick ();
  test_pit_divisor_speeds_up_ticks ();
  test_cp437_text ();
  if !failed = 0 then print_endline "dos 표면: all passed"
  else begin
    Printf.eprintf "dos 표면: %d failures\n%!" !failed;
    exit 1
  end
