(* 회색 합산 — INT 10h AH=12h BL=33h(켜고 끄기), AX=101Bh(DAC 한 덩이를
   회색으로), 깃발이 서 있을 때 AX=1010h·1012h 와 모드 세우기. 회색은
   (77 R + 151 G + 28 B + 80h) >> 8 이다(SeaBIOS·DOSBox-staging). *)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let int10 ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x10"
let es_is_cs = "\x0e\x07"
let store_al a = "\xa2" ^ w16 a
let exit_0 = "\xb8\x00\x4c\xcd\x21"
let gray_on = int10 0x1200 ~bx:0x33
let gray_off = int10 0x1201 ~bx:0x33
let set_one idx (r, g, b) = int10 0x1010 ~bx:idx ~dx:(r lsl 8) ~cx:((g lsl 8) lor b)

let psp = 0x10000
let out = 0x300
let table = 0x280
let bda_vga_flags = 0x489
let mono_bit = 0x04

let run ?(data = []) ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= table);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  List.iter (fun (off, bytes) ->
      List.iteri (fun i v -> Dos_machine.mem_write m (psp + off + i) v) bytes) data;
  before m;
  Dos_machine.run m ~max_steps:50_000;
  check "program exited" (Dos_machine.exited m);
  m

let dac m i = (Dos_ports.palette m.Dos_state.ports).(i)
let gray v = (v, v, v)
let show (r, g, b) = Printf.sprintf "(%d,%d,%d)" r g b
let check_dac name m i want =
  let got = dac m i in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: DAC %d = %s want %s\n%!" name i (show got) (show want)
  end

(* 77·63 + 80h = 4979 → 19, 151·63 + 80h = 9641 → 37, 28·63 + 80h = 1892 → 7 *)
let red = (63, 0, 0) and green = (0, 63, 0) and blue = (0, 0, 63)
let red_gray = gray 19 and green_gray = gray 37 and blue_gray = gray 7
let near_white_hi = (63, 63, 60) and near_white_lo = (63, 63, 58)

let test_switch () =
  let m =
    run (String.concat ""
           [ gray_on; store_al out; gray_off; store_al (out + 1);
             int10 0x1202 ~bx:0x33; store_al (out + 2); exit_0 ])
      ~before:(fun m -> Dos_machine.mem_write m bda_vga_flags 0x09)
  in
  checki "33h AL=0 → AL=12h" (Dos_machine.mem_read m (psp + out)) 0x12;
  checki "33h AL=1 → AL=12h" (Dos_machine.mem_read m (psp + out + 1)) 0x12;
  checki "33h AL=2 → AL=0" (Dos_machine.mem_read m (psp + out + 2)) 0;
  checki "33h AL=1 뒤 0x489: bit 1 만 끔" (Dos_machine.mem_read m bda_vga_flags) 0x09;
  let m = run (gray_on ^ exit_0) ~before:(fun m -> Dos_machine.mem_write m bda_vga_flags 0x09) in
  checki "33h AL=0 뒤 0x489: bit 1 만 켬" (Dos_machine.mem_read m bda_vga_flags) 0x0B

(* 깃발이 서 있으면 1010h·1012h 가 회색으로 싣는다 *)
let test_load_with_flag () =
  let m =
    run ~data:[ (table, [ 63; 0; 0; 0; 63; 0 ]) ]
      (String.concat ""
         [ gray_on; set_one 5 red; set_one 6 blue; set_one 11 near_white_hi; set_one 12 near_white_lo;
           es_is_cs; int10 0x1012 ~bx:7 ~cx:2 ~dx:table;
           gray_off; set_one 9 red; exit_0 ])
  in
  check_dac "1010h 빨강" m 5 red_gray;
  check_dac "1010h 파랑" m 6 blue_gray;
  check_dac "1012h 빨강" m 7 red_gray;
  check_dac "1012h 초록" m 8 green_gray;
  check_dac "깃발을 끄면 그대로" m 9 red;
  (* 무게 하나가 1 만 달라도 이 둘 가운데 하나가 바뀐다: 16172 → 63, 16116 → 62 *)
  check_dac "(63,63,60) → 63" m 11 (gray 63);
  check_dac "(63,63,58) → 62" m 12 (gray 62);
  (* 흑백 모니터 비트(bit 2)도 합산한다 — DOSBox-staging 의 0x06 마스크 *)
  let m =
    run (set_one 5 green ^ exit_0)
      ~before:(fun m -> Dos_machine.mem_write m bda_vga_flags mono_bit)
  in
  check_dac "흑백 모니터 비트" m 5 green_gray;
  (* 포트로 쓴 색은 BIOS 를 거치지 않아 그대로다 *)
  let port_red = "\xba" ^ w16 0x3C8 ^ "\xb0\x0a\xee\x42\xb0\x3f\xee\xb0\x00\xee\xee" in
  let m = run (gray_on ^ port_red ^ exit_0) in
  check_dac "포트 3C9h 는 합산하지 않는다" m 10 red

(* 101Bh 는 깃발과 상관없이 BX 부터 CX 자리를 바꾸고, 끝에서 0 으로 돈다 *)
let test_sum_block () =
  let colors = List.concat (List.init 10 (fun _ -> [ 63; 0; 0 ])) in
  let m =
    run ~data:[ (table, colors) ]
      (String.concat ""
         [ es_is_cs; int10 0x1012 ~bx:250 ~cx:10 ~dx:table;  (* 250..255, 0..3 *)
           set_one 4 red; set_one 249 red;
           int10 0x101B ~bx:250 ~cx:10; exit_0 ])
  in
  check_dac "101Bh 첫 자리" m 250 red_gray;
  check_dac "101Bh 255" m 255 red_gray;
  check_dac "101Bh 0 으로 돈다" m 0 red_gray;
  check_dac "101Bh 마지막(3)" m 3 red_gray;
  check_dac "101Bh 뒤 자리(4)는 그대로" m 4 red;
  check_dac "101Bh 앞 자리(249)는 그대로" m 249 red;
  let m = run (set_one 7 blue ^ set_one 200 green ^ int10 0x101B ~bx:0 ~cx:0x300 ^ exit_0) in
  check_dac "CX 가 DAC 보다 크면 256 자리 모두" m 7 blue_gray;
  check_dac "CX 가 DAC 보다 크면 256 자리 모두(200)" m 200 green_gray;
  check_dac "흰색은 63" m 15 (gray 63)

(* 모드 세우기: 기본 팔레트를 싣고 깃발이 서 있으면 256 자리를 합산한다.
   팔레트 싣기를 껐으면 DAC 을 두고 합산도 하지 않는다. *)
let test_mode_set () =
  let m = run (gray_on ^ int10 0x0013 ^ exit_0) in
  (* 기본 DAC 1 = (0,0,42): 28·42 + 80h = 1304 → 5 *)
  check_dac "모드 13h: 기본 파랑이 회색" m 1 (gray 5);
  check_dac "모드 13h: 검정은 검정" m 0 (gray 0);
  let f = Dos_machine.frame_rgb m in
  check "그림도 회색(점 0 = DAC 0)" (f.[0] = f.[1] && f.[1] = f.[2]);
  let m = run (set_one 1 red ^ int10 0x1201 ~bx:0x31 ^ gray_on ^ int10 0x0013 ^ exit_0) in
  check_dac "팔레트 싣기를 껐으면 합산도 없다" m 1 red;
  (* 흑백 모니터 비트만으로는 모드 세우기가 합산하지 않는다(bit 1 만 본다) *)
  let m = run (int10 0x0013 ^ exit_0) ~before:(fun m -> Dos_machine.mem_write m bda_vga_flags mono_bit) in
  check_dac "흑백 비트만: 모드 세우기는 기본색" m 1 (0, 0, 42)

let () =
  test_switch ();
  test_load_with_flag ();
  test_sum_block ();
  test_mode_set ();
  if !failed = 0 then print_endline "dos vga gray: all passed"
  else begin
    Printf.eprintf "dos vga gray: %d failures\n%!" !failed;
    exit 1
  end
