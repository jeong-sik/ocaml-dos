(* INT 21h AH=29h — MS-DOS 4.0 FCB.ASM MakeFcb, RBIL INT 21/29 *)

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

let only_c n = n = 3

(* [s] 를 오프셋 [at] 에 두고 [al] 로 파싱한다. 문자열 밖은 0(NUL — 이름
   끝). [fcb] 는 처음 FCB 내용. 돌려주는 값: AL, 다음 오프셋(돌려받은 그대로),
   FCB. *)
let run ?(at = 0) ?(fcb = String.make 16 '\x00') al s =
  let b = Bytes.of_string fcb in
  let read off =
    let i = (off - at) land 0xFFFF in
    if i < String.length s then Char.code s.[i] else 0
  in
  let write i v = Bytes.set b i (Char.chr v) in
  let r =
    Dos_fcb_parse.parse (Dos_fcb_parse.options_of_al al) ~drive_exists:only_c ~read ~write at
  in
  (r.al, r.next, Bytes.to_string b)

let fcb drive name ext = String.make 1 (Char.chr drive) ^ name ^ ext ^ "\x00\x00\x00\x00"

let case name ?at ?fcb al s ~al:want_al ~next ~want =
  let got_al, got_next, got = run ?at ?fcb al s in
  check (name ^ ": AL") got_al want_al;
  (* [next] 는 [s] 안의 자리다. 돌려받는 값은 16 비트 오프셋이다. *)
  check (name ^ ": next") got_next ((Option.value at ~default:0 + next) land 0xFFFF);
  check_s (name ^ ": FCB") got want

let () =
  case "plain" 0x01 "  game.exe rest" ~al:0 ~next:10 ~want:(fcb 0 "GAME    " "EXE");
  case "drive c" 0x01 "c:foo.bat" ~al:0 ~next:9 ~want:(fcb 3 "FOO     " "BAT");
  case "drive alone" 0x01 "C:" ~al:0 ~next:2 ~want:(fcb 3 "        " "   ");
  case "missing drive" 0x01 "a:x" ~al:0xFF ~next:3 ~want:(fcb 1 "X       " "   ");
  (* 틀린 드라이브는 와일드카드보다 앞선다: DL=FFh 에 OR 1 해도 FFh *)
  case "missing drive with wildcards" 0x01 "a:*.*" ~al:0xFF ~next:5
    ~want:(fcb 1 "????????" "???");
  case "digit drive" 0x01 "1:x" ~al:0xFF ~next:3 ~want:(fcb 0xF1 "X       " "   ");
  (* 드라이브 글자 뒤에 ':' 가 없으면 이름이다 *)
  case "letter without colon" 0x01 "c.x" ~al:0 ~next:3 ~want:(fcb 0 "C       " "X  ");
  (* '*' 는 남은 칸을 '?' 로, 그 뒤 글자는 버린다 *)
  case "star" 0x01 "*.t?t" ~al:1 ~next:5 ~want:(fcb 0 "????????" "T?T");
  case "star mid" 0x01 "ab*cd.e" ~al:1 ~next:7 ~want:(fcb 0 "AB??????" "E  ");
  (* 칸을 넘는 글자는 버리면서 끝 문자까지 읽는다 *)
  case "long name" 0x01 "verylongname.text" ~al:0 ~next:17 ~want:(fcb 0 "VERYLONG" "TEX");
  (* 칸이 찬 뒤의 '?' 는 와일드카드로 치지 않는다 *)
  case "question past the field" 0x01 "abcdefghi?" ~al:0 ~next:10 ~want:(fcb 0 "ABCDEFGH" "   ");
  (* 끝 문자 *)
  case "bracket ends" 0x01 "foo[bar" ~al:0 ~next:3 ~want:(fcb 0 "FOO     " "   ");
  case "slash ends" 0x01 "foo/x" ~al:0 ~next:3 ~want:(fcb 0 "FOO     " "   ");
  case "second dot ends" 0x01 "a.b.c" ~al:0 ~next:3 ~want:(fcb 0 "A       " "B  ");
  case "tab skipped" 0x00 "\tfoo" ~al:0 ~next:4 ~want:(fcb 0 "FOO     " "   ");
  (* bit 0: 공백 뒤 구분자 하나를 건너뛴다. 없으면 구분자가 이름을 끝낸다 *)
  case "separator skipped" 0x01 " , foo" ~al:0 ~next:6 ~want:(fcb 0 "FOO     " "   ");
  case "separator stops" 0x00 " , foo" ~al:0 ~next:1 ~want:(fcb 0 "        " "   ");
  case "only one separator" 0x01 ",,foo" ~al:0 ~next:1 ~want:(fcb 0 "        " "   ");
  (* 남기기 비트: 없는 칸은 그대로, 뒤 4 바이트는 늘 0 *)
  let old = "\x05OLDNAME OLD\xff\xff\xff\xff" in
  case "keep all" ~fcb:old 0x0E "" ~al:0 ~next:0 ~want:(fcb 5 "OLDNAME " "OLD");
  case "keep name, new extension" ~fcb:old 0x0E ".txt" ~al:0 ~next:4 ~want:(fcb 5 "OLDNAME " "TXT");
  (* '.' 뒤는 글자가 없어도 확장자를 공백으로 덮는다 *)
  case "dot without extension" ~fcb:old 0x0E "new." ~al:0 ~next:4 ~want:(fcb 5 "NEW     " "   ");
  case "keep nothing" ~fcb:old 0x00 "" ~al:0 ~next:0 ~want:(fcb 0 "        " "   ");
  (* 대문자: a-z 와 437 표(80h..A7h), 그 위는 그대로 *)
  case "437 upper case" 0x00 "\x81\x84\xa4\xe1" ~al:0 ~next:4
    ~want:(fcb 0 "\x9a\x8e\xa5\xe1    " "   ");
  (* SI 는 16 비트에서 돈다 *)
  case "offset wraps" ~at:0xFFFE 0x00 "ab.c" ~al:0 ~next:4 ~want:(fcb 0 "AB      " "C  ")

(* 기계 위: DS:SI 의 글자열을 ES:DI 의 FCB 로. AL 과 SI 를 돌려받고 DI 는
   그대로다. *)
let () =
  let text_at = 0x180 and fcb_at = 0x1A0 and out_at = 0x1C0 in
  let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff)) in
  let code =
    "\xb8\x01\x29"                       (* mov ax,2901h *)
    ^ "\xbe" ^ w text_at                 (* mov si,text *)
    ^ "\xbf" ^ w fcb_at                  (* mov di,fcb *)
    ^ "\xcd\x21"
    ^ "\xa2" ^ w out_at                  (* mov [out],al *)
    ^ "\x89\x36" ^ w (out_at + 2)        (* mov [out+2],si *)
    ^ "\x89\x3e" ^ w (out_at + 4)        (* mov [out+4],di *)
    ^ "\xb8\x00\x4c\xcd\x21"
  in
  let text = " c:report.t* tail\x0d" in
  let image =
    code ^ String.make (text_at - 0x100 - String.length code) '\x00' ^ text
  in
  let m = Dos_machine.create () in
  Dos_machine.load_com m image;
  Dos_machine.run m ~max_steps:1000;
  let base = 0x1000 * 16 in
  let byte a = Dos_machine.mem_read m (base + a) in
  let word a = byte a lor (byte (a + 1) lsl 8) in
  check "machine: AL" (byte out_at) 1;
  check "machine: SI after the name" (word (out_at + 2)) (text_at + 12);
  check "machine: DI kept" (word (out_at + 4)) fcb_at;
  check_s "machine: FCB" (String.init 16 (fun i -> Char.chr (byte (fcb_at + i))))
    (fcb 3 "REPORT  " "T??")

let () =
  if !failed = 0 then print_endline "dos parse fcb: all passed"
  else begin
    Printf.eprintf "dos parse fcb: %d failures\n%!" !failed;
    exit 1
  end
