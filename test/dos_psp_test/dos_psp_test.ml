(* PSP 칸 — 0Ah-15h(IVT 22h·23h·24h 사본), 16h(부모), 38h(이전 PSP),
   50h(INT 21h; RETF). 자식이 끝나면 IVT 22h-24h 를 그 자식 PSP 의 사본으로
   되돌린다(MS-DOS 4.0 PROC.ASM $ABORT). *)

let mount m name data =
  match Dos_machine.mount_file m name data with
  | Ok () -> ()
  | Error e -> failwith ("mount " ^ e)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04X want %04X\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let exit_with code = "\xb8" ^ w16 (0x4C00 lor code) ^ "\xcd\x21"

(* 루트 COM 의 PSP 는 1000h 다. 부모가 20h 문단으로 줄이면 첫 자식의
   환경 블록이 1020h(1 문단)에 오고 자식 PSP 는 1021h 다 — dos_exec_test
   의 배치와 같다. *)
let root_psp = 0x10000
let child_psp = 0x10210
let root_seg = 0x1000

let rd m a = Dos_machine.mem_read m a
let rd16 m a = rd m a lor (rd m (a + 1) lsl 8)
let poke16 m a v = Dos_machine.mem_write m a (v land 0xff); Dos_machine.mem_write m (a + 1) (v lsr 8)

let ivt v = v * 4
(* 부팅 뒤 IVT 22h-24h 에 심는 표식(오프셋, 세그먼트) *)
let marks = [ (0x22, 0x1111, 0x2222); (0x23, 0x3333, 0x4444); (0x24, 0x5555, 0x6666) ]
let plant m = List.iter (fun (v, o, s) -> poke16 m (ivt v) o; poke16 m (ivt v + 2) s) marks

let check_saved name m psp =
  List.iteri (fun i (v, o, s) ->
      checki (Printf.sprintf "%s: %02Xh 오프셋" name v) (rd16 m (psp + 0x0A + (i * 4))) o;
      checki (Printf.sprintf "%s: %02Xh 세그먼트" name v) (rd16 m (psp + 0x0C + (i * 4))) s)
    marks

let check_ivt name m =
  List.iter (fun (v, o, s) ->
      checki (Printf.sprintf "%s: IVT %02Xh 오프셋" name v) (rd16 m (ivt v)) o;
      checki (Printf.sprintf "%s: IVT %02Xh 세그먼트" name v) (rd16 m (ivt v + 2)) s)
    marks

let run_root ?(files = []) code ~steps =
  let m = Dos_machine.create () in
  plant m;
  List.iter (fun (n, c) -> mount m n c) files;
  Dos_machine.load_com m code;
  Dos_machine.run m ~max_steps:steps;
  checki "program exited" (if Dos_machine.exited m then 1 else 0) 1;
  m

(* 부모: AH=4Ah 로 20h 문단만 남기고 CHILD.COM 을 EXEC 한 뒤 4Dh 코드로
   끝난다. 이름과 EPB 는 코드 바로 뒤다. *)
let parent =
  let code_len = 7 + 11 + 8 in
  let name = 0x100 + code_len in
  let epb = name + String.length "CHILD.COM" + 1 in
  let code =
    "\xb4\x4a\xbb" ^ w16 0x20 ^ "\xcd\x21"
    ^ "\xb8\x00\x4b\xba" ^ w16 name ^ "\xbb" ^ w16 epb ^ "\xcd\x21"
    ^ "\xb4\x4d\xcd\x21\xb4\x4c\xcd\x21"
  in
  assert (String.length code = code_len);
  code ^ "CHILD.COM\x00" ^ String.make 14 '\x00'

let test_root () =
  (* far call PSP:0050 으로 AH=30h 를 부른 뒤 AL 을 [0300h] 에 둔다 *)
  let code =
    "\xb4\x30\x9a\x50\x00" ^ w16 root_seg ^ "\xa2\x00\x03"
    ^ "\xb4\x30\xcd\x21\xa2\x01\x03" ^ exit_with 0
  in
  let m = run_root code ~steps:2000 in
  check_saved "루트 PSP" m root_psp;
  checki "루트 16h 부모 = 자기" (rd16 m (root_psp + 0x16)) root_seg;
  checki "38h 이전 PSP 아래" (rd16 m (root_psp + 0x38)) 0xFFFF;
  checki "38h 이전 PSP 위" (rd16 m (root_psp + 0x3A)) 0xFFFF;
  checki "50h INT" (rd m (root_psp + 0x50)) 0xCD;
  checki "51h 21h" (rd m (root_psp + 0x51)) 0x21;
  checki "52h RETF" (rd m (root_psp + 0x52)) 0xCB;
  checki "PSP:0050 far call 과 INT 21h 가 같은 AL" (rd m (root_psp + 0x300)) (rd m (root_psp + 0x301));
  checki "AH=30h 가 실제로 돌았다(AL ≠ 30h)" (if rd m (root_psp + 0x300) = 0x30 then 1 else 0) 0

(* AH=25h AL=vec 으로 DS:DX = seg:off 를 건다 *)
let set_vec vec ~seg ~off = "\xb8" ^ w16 seg ^ "\x8e\xd8\xba" ^ w16 off ^ "\xb8" ^ w16 (0x2500 lor vec) ^ "\xcd\x21"

let test_child () =
  (* 자식: 23h·24h 를 바꾸고, 제 PSP 의 23h 사본(0Eh)을 7777:8888 로 고친 뒤
     끝난다. MS-DOS 는 PSP 사본을 IVT 로 되돌리므로 23h 는 7777:8888 이 된다. *)
  let child =
    set_vec 0x24 ~seg:0xAAAA ~off:0xBBBB ^ set_vec 0x23 ~seg:0xCCCC ~off:0xDDDD
    ^ "\x0e\x1f"                                   (* push cs; pop ds *)
    ^ "\xc7\x06\x0e\x00" ^ w16 0x8888 ^ "\xc7\x06\x10\x00" ^ w16 0x7777
    ^ exit_with 3
  in
  let m = run_root ~files:[ ("CHILD.COM", child) ] parent ~steps:5000 in
  checki "자식 코드" (Dos_machine.exit_code m) 3;
  checki "자식 16h 부모" (rd16 m (child_psp + 0x16)) root_seg;
  checki "자식 22h 사본 오프셋" (rd16 m (child_psp + 0x0A)) 0x1111;
  checki "자식 24h 사본 세그먼트" (rd16 m (child_psp + 0x14)) 0x6666;
  checki "IVT 24h 오프셋 되돌림" (rd16 m (ivt 0x24)) 0x5555;
  checki "IVT 24h 세그먼트 되돌림" (rd16 m (ivt 0x24 + 2)) 0x6666;
  checki "IVT 23h = 고친 PSP 사본 오프셋" (rd16 m (ivt 0x23)) 0x8888;
  checki "IVT 23h = 고친 PSP 사본 세그먼트" (rd16 m (ivt 0x23 + 2)) 0x7777;
  checki "IVT 22h 그대로" (rd16 m (ivt 0x22)) 0x1111

let test_tsr () =
  (* TSR 도 같다: 09h 는 남고 24h 는 되돌아간다 *)
  let tsr = set_vec 0x09 ~seg:0x1234 ~off:0x5678 ^ set_vec 0x24 ~seg:0xAAAA ~off:0xBBBB ^ "\xba\x00\x02\xcd\x27" in
  let m = run_root ~files:[ ("CHILD.COM", tsr) ] parent ~steps:5000 in
  check_ivt "TSR 뒤" m;
  checki "TSR 뒤 IVT 09h 는 남는다" (rd16 m (ivt 0x09)) 0x5678

(* ---- AH=55h 와, EXEC 를 거치지 않는 자식 ----
   Beauty and the Beast 데모(DEMO.COM)는 로더다. AH=55h 로 PSP 를 만들고,
   그 PSP 의 종료 주소(0Ah)를 제 이어하기 주소로 고치고, AH=50h 로 그 PSP 를
   현재로 옮긴 뒤 자식 코드를 돌린다. 자식이 끝나면 DOS 가 이어하기 주소로
   돌려준다(MS-DOS 4.0 MISC.ASM $Dup_PDB·SETMEM, CTRLC.ASM reset_environment).
   아래 프로그램은 nasm -f bin 으로 어셈블한 것이다. *)
let new_psp = 0x30000
let new_seg = 0x3000

(* 55h: DX=3000h SI=4000h. 새 PSP 는 현재 PSP 의 100h 바이트 사본이다. 커맨드라인 82h 에
   'Q' 를 미리 써 두고, 55h 뒤 현재 PSP 와 DTA 를 읽는다. *)
let create_code =
  "\xc6\x06\x82\x00\x51"
  ^ "\xba\x00\x30" ^ "\xbe\x00\x40" ^ "\xb4\x55" ^ "\xcd\x21"     (* mov dx,3000h; mov si,4000h; mov ah,55h; int 21h *)
  ^ "\xb4\x51" ^ "\xcd\x21" ^ "\x89\x1e\x00\x03"                     (* 51h: [0300] = 현재 PSP *)
  ^ "\xb4\x2f" ^ "\xcd\x21" ^ "\x89\x1e\x02\x03" ^ "\x8c\x06\x04\x03"  (* 2Fh: [0302] = DTA 오프셋, [0304] = 세그먼트 *)
  ^ "\xbb\x00\x10" ^ "\xb4\x50" ^ "\xcd\x21"                          (* 루트 PSP 를 돌려준다 *)
  ^ exit_with 0

let test_create_psp () =
  let m = run_root create_code ~steps:2000 in
  checki "새 PSP 의 INT 20h" (rd16 m new_psp) 0x20CD;
  checki "새 PSP 02h = SI" (rd16 m (new_psp + 0x02)) 0x4000;
  check_saved "새 PSP" m new_psp;
  checki "새 PSP 16h 부모 = 만든 쪽" (rd16 m (new_psp + 0x16)) root_seg;
  checki "새 PSP 38h 이전 PSP" (rd16 m (new_psp + 0x38)) 0xFFFF;
  checki "새 PSP 50h INT" (rd m (new_psp + 0x50)) 0xCD;
  checki "새 PSP 52h RETF" (rd m (new_psp + 0x52)) 0xCB;
  checki "새 PSP 2Ch 환경은 사본" (rd16 m (new_psp + 0x2C)) (rd16 m (root_psp + 0x2C));
  checki "새 PSP 82h 커맨드라인은 사본" (rd m (new_psp + 0x82)) 0x51;
  checki "55h 뒤 현재 PSP" (rd16 m (root_psp + 0x300)) new_seg;
  (* 2Fh 는 물리 주소를 seg:off 로 나눠 준다(1008:0000): 루트 PSP:0080 과 같은 자리다 *)
  checki "55h 는 DTA 를 안 바꾼다" ((rd16 m (root_psp + 0x304) lsl 4) + rd16 m (root_psp + 0x302)) (root_psp + 0x80)

(* 자식 PSP(3000h)를 만들고 종료 주소를 handler 로 고친 뒤 현재로 옮겨 7 로 끝낸다.
   handler: 4Dh 코드를 [0300], 현재 PSP 를 [0302] 에 두고 9 로 끝난다. *)
let term_code =
  "\xba\x00\x30" ^ "\xbe\x00\x40" ^ "\xb4\x55" ^ "\xcd\x21"
  ^ "\xb8\x00\x30" ^ "\x8e\xc0"
  ^ "\x26\xc7\x06\x0a\x00\x28\x01"                                  (* mov word [es:0Ah],handler(0128h) *)
  ^ "\x8c\xc8" ^ "\x26\xa3\x0c\x00"                                  (* mov [es:0Ch],cs *)
  ^ "\xbb\x00\x30" ^ "\xb4\x50" ^ "\xcd\x21"                        (* 자식을 현재 PSP 로 *)
  ^ "\xb8\x07\x4c" ^ "\xcd\x21"                                       (* 자식이 7 로 끝난다 *)
  ^ "\xb4\x4d" ^ "\xcd\x21" ^ "\xa3\x00\x03"                          (* handler: [0300] = 4Dh *)
  ^ "\xb4\x51" ^ "\xcd\x21" ^ "\x89\x1e\x02\x03"                     (* [0302] = 현재 PSP *)
  ^ "\xb8\x09\x4c" ^ "\xcd\x21"

let handler_off = 0x0128

let test_foreign_terminate () =
  let m = run_root term_code ~steps:2000 in
  checki "이어하기 주소에서 4Dh 가 자식 코드를 준다" (rd m (root_psp + 0x300)) 7;
  checki "끝난 뒤 현재 PSP 는 부모" (rd16 m (root_psp + 0x302)) root_seg;
  checki "루트가 9 로 끝난다" (Dos_machine.exit_code m) 9;
  checki "IVT 22h = 자식 PSP 의 종료 주소(오프셋)" (rd16 m (ivt 0x22)) handler_off;
  checki "IVT 22h = 자식 PSP 의 종료 주소(세그먼트)" (rd16 m (ivt 0x22 + 2)) root_seg;
  checki "IVT 23h 는 자식 PSP 사본(오프셋)" (rd16 m (ivt 0x23)) 0x3333;
  checki "IVT 24h 는 자식 PSP 사본(세그먼트)" (rd16 m (ivt 0x24 + 2)) 0x6666

(* 상주 종료(31h, DX=20h)로 끝나면 PSP 가 든 블록 A(100h 문단)가 20h 문단으로 준다.
   A 위에 블록 B 가 있어, 30h 문단짜리가 아닌 C0h 문단을 first fit 으로 달라고 하면
   줄어든 A 의 꼬리가 나온다: AX = A + 20h. *)
let tsr_code =
  "\xb4\x4a" ^ "\xbb\x20\x00" ^ "\xcd\x21"                            (* 이 COM 위를 내놓는다 *)
  ^ "\xb4\x48" ^ "\xbb\x00\x01" ^ "\xcd\x21" ^ "\xa3\x10\x03"       (* 블록 A = 100h 문단, [0310] *)
  ^ "\xb4\x48" ^ "\xbb\x10\x00" ^ "\xcd\x21"                          (* 블록 B 가 그 위 *)
  ^ "\x8b\x16\x10\x03" ^ "\x89\xd6" ^ "\x81\xc6\x00\x01" ^ "\xb4\x55" ^ "\xcd\x21"  (* 55h: DX=A SI=A+100h *)
  ^ "\x8e\x06\x10\x03"
  ^ "\x26\xc7\x06\x0a\x00\x47\x01"                                  (* 종료 주소 = handler(0147h) *)
  ^ "\x8c\xc8" ^ "\x26\xa3\x0c\x00"
  ^ "\x8b\x1e\x10\x03" ^ "\xb4\x50" ^ "\xcd\x21"                      (* A 의 PSP 를 현재로 *)
  ^ "\xb8\x00\x31" ^ "\xba\x20\x00" ^ "\xcd\x21"                      (* 31h: 20h 문단을 남긴다 *)
  ^ "\xb4\x4d" ^ "\xcd\x21" ^ "\xa3\x00\x03"                          (* handler: [0300] = 4Dh *)
  ^ "\xb4\x48" ^ "\xbb\xc0\x00" ^ "\xcd\x21" ^ "\xa3\x12\x03"       (* C0h 문단: [0312] *)
  ^ exit_with 0

let test_foreign_tsr () =
  let m = run_root tsr_code ~steps:5000 in
  let a = rd16 m (root_psp + 0x310) in
  checki "블록 A 는 1020h 위 어딘가" (if a > root_seg then 1 else 0) 1;
  checki "상주 종료 뒤 first fit 은 줄어든 A 의 꼬리" (rd16 m (root_psp + 0x312)) (a + 0x20);
  checki "4Dh 코드" (rd m (root_psp + 0x300)) 0

(* 종료 주소가 기본 IRET 스텁이면(로더가 안 고친 자식) 셸로 돌아가는 것이니 기계가 끝난다 *)
let test_foreign_default_exit () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    ("\xba\x00\x30" ^ "\xbe\x00\x40" ^ "\xb4\x55" ^ "\xcd\x21"
     ^ "\xbb\x00\x30" ^ "\xb4\x50" ^ "\xcd\x21" ^ exit_with 9);
  Dos_machine.run m ~max_steps:2000;
  checki "기본 스텁이면 기계가 끝난다" (if Dos_machine.exited m then 1 else 0) 1;
  checki "종료 코드" (Dos_machine.exit_code m) 9

let () =
  test_root ();
  test_child ();
  test_tsr ();
  test_create_psp ();
  test_foreign_terminate ();
  test_foreign_tsr ();
  test_foreign_default_exit ();
  if !failed = 0 then print_endline "dos psp: all passed"
  else begin
    Printf.eprintf "dos psp: %d failures\n%!" !failed;
    exit 1
  end
