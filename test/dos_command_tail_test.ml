(* 루트 프로그램의 명령줄 꼬리 — PSP:80h 길이, 81h 부터 글자, 그 뒤 0Dh
   (RBIL INT 21/26 PSP 표, MS-DOS 4.0 COMMAND TCODE.ASM COMTAIL). *)

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

let exit0 = "\xb8\x00\x4c\xcd\x21"                     (* mov ax,4C00h; int 21h *)

let tail_of m =
  let psp = Dos_machine.psp_seg_of m * 16 in
  let n = Dos_machine.mem_read m (psp + 0x80) in
  (n, String.init (n + 1) (fun i -> Char.chr (Dos_machine.mem_read m (psp + 0x81 + i))))

(* MZ: 헤더 32 바이트, 재배치 없음 *)
let exe_of code =
  let total = 32 + String.length code in
  let b = Buffer.create total in
  let w v = Buffer.add_char b (Char.chr (v land 0xff)); Buffer.add_char b (Char.chr (v lsr 8)) in
  Buffer.add_string b "MZ"; w (total mod 512); w ((total + 511) / 512); w 0; w 2;
  w 0; w 0; w 0; w 0xFFFE; w 0; w 0; w 0; w 0x1C; w 0;
  Buffer.add_string b "\x00\x00\x00\x00";
  Buffer.add_string b code;
  Buffer.contents b

let raises f = try f (); false with Invalid_argument _ -> true

let () =
  let m = Dos_machine.create () in
  Dos_machine.load_com ~tail:" a v" m exit0;
  let n, s = tail_of m in
  check "COM tail length" n 4;
  check_s "COM tail and CR" s " a v\r";

  let m = Dos_machine.create () in
  Dos_machine.load_exe ~tail:" /x" m (exe_of exit0);
  let n, s = tail_of m in
  check "EXE tail length" n 3;
  check_s "EXE tail and CR" s " /x\r";
  Dos_machine.run m ~max_steps:1000;
  check "EXE with a tail runs" (Dos_machine.exit_code m) 0;

  let m = Dos_machine.create () in
  Dos_machine.load_com m exit0;
  let n, s = tail_of m in
  check "no tail: length 0" n 0;
  check_s "no tail: CR" s "\r";

  (* 81h..FFh 에 CR 까지: 126 바이트는 들어가고 127 은 거절 *)
  let full = String.make 126 'x' in
  let m = Dos_machine.create () in
  Dos_machine.load_com ~tail:full m exit0;
  let n, s = tail_of m in
  check "126-byte tail" n 126;
  check "CR at FFh" (Char.code s.[126]) 0x0D;
  let m = Dos_machine.create () in
  check "127-byte tail refused"
    (if raises (fun () -> Dos_machine.load_com ~tail:(full ^ "x") m exit0) then 1 else 0) 1;
  check "refused tail loads nothing" (Dos_machine.mem_read m 0x10100) 0;
  check "CR inside refused"
    (if raises (fun () -> Dos_machine.load_com ~tail:" a\rb" (Dos_machine.create ()) exit0)
     then 1 else 0) 1;
  check "EXE: 127-byte tail refused"
    (if raises (fun () -> Dos_machine.load_exe ~tail:(full ^ "x") (Dos_machine.create ()) (exe_of exit0))
     then 1 else 0) 1;

  (* 프로그램이 꼬리를 읽는다: 첫 글자를 종료 코드로 *)
  let first_char = "\xa0\x82\x00\xb4\x4c\xcd\x21" in    (* mov al,[0082h]; 4Ch *)
  let m = Dos_machine.create () in
  Dos_machine.load_com ~tail:" q" m first_char;
  Dos_machine.run m ~max_steps:1000;
  check "guest reads its tail" (Dos_machine.exit_code m) (Char.code 'q');

  if !failed = 0 then print_endline "dos command tail: all passed"
  else begin
    Printf.eprintf "dos command tail: %d failures\n%!" !failed;
    exit 1
  end
