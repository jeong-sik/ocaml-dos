(* PSP 칸 — 0Ah-15h(IVT 22h·23h·24h 사본), 16h(부모), 38h(이전 PSP),
   50h(INT 21h; RETF). 자식이 끝나면 IVT 22h-24h 를 그 자식 PSP 의 사본으로
   되돌린다(MS-DOS 4.0 PROC.ASM $ABORT). *)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04X want %04X\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let exit_with code = "\xb8" ^ w16 (0x4C00 lor code) ^ "\xcd\x21"

(* 루트 COM 의 PSP 는 1000h, 첫 자식(부모가 20h 문단으로 줄인 뒤)은
   1020h 다 — dos_exec_test 의 배치와 같다. *)
let root_psp = 0x10000
let child_psp = 0x10200
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
  List.iter (fun (n, c) -> Dos_machine.mount_file m n c) files;
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

let () =
  test_root ();
  test_child ();
  test_tsr ();
  if !failed = 0 then print_endline "dos psp: all passed"
  else begin
    Printf.eprintf "dos psp: %d failures\n%!" !failed;
    exit 1
  end
