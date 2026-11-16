(* INT 10h AH=12h BL=30h·31h·34h·36h(VGA 대체 선택). 받으면 AL=12h, AL 이
   범위 밖이면 AL=0(RBIL INT 10/AH=12h BL=30h~36h). BDA 비트 자리는
   DOSBox-staging int10.cpp 를 따른다. 36h 와 시퀀서 1 번 bit 5 는 화면을
   DAC 0 번 색 한 가지로 만든다. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got 0x%x want 0x%x\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let int10 ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx ^ "\xcd\x10"
let store_al a = "\xa2" ^ w16 a
let alt ~bl al = int10 ((0x12 lsl 8) lor al) ~bx:bl
let exit_0 = "\xb8\x00\x4c\xcd\x21"
(* [flag] 가 0 이 아니게 될 때까지 돈다 *)
let wait_for flag = "\xa0" ^ w16 flag ^ "\x08\xc0\x74\xf9"

let psp = 0x10000
let out = 0x300
let flag_a = 0x2F0
let flag_b = 0x2F2

let bda_ega_misc = 0x487
let bda_switches = 0x488
let bda_vga_flags = 0x489
let done_ = 0x12

let peek m off = Dos_machine.mem_read m (psp + off)
let bda m a = Dos_machine.mem_read m a

let boot ?(before = fun _ -> ()) code =
  assert (0x100 + String.length code <= flag_a);
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  before m;
  m

let run_to_exit m =
  Dos_machine.run m ~max_steps:50_000;
  check "program exited" (Dos_machine.exited m)

(* AL 이 범위 안이면 12h, 밖이면 0 *)
let test_answers () =
  let calls =
    [ (0x30, 0); (0x30, 2); (0x30, 3); (0x31, 0); (0x31, 1); (0x31, 2);
      (0x34, 0); (0x34, 1); (0x34, 2); (0x36, 0); (0x36, 1); (0x36, 2) ]
  in
  let code =
    String.concat ""
      (List.mapi (fun i (bl, al) -> alt ~bl al ^ store_al (out + i)) calls
       @ [ alt ~bl:0x36 0; alt ~bl:0x37 5; store_al (out + 16); exit_0 ])
  in
  let m = boot code in
  run_to_exit m;
  List.iteri
    (fun i (bl, al) ->
      let limit = if bl = 0x30 then 2 else 1 in
      checki (Printf.sprintf "BL=%02Xh AL=%d" bl al) (peek m (out + i))
        (if al > limit then 0 else done_))
    calls;
  checki "모르는 BL=37h 는 AL 을 둔다" (peek m (out + 16)) 0x05

(* 30h: 0x489 bit 7·4 와 0x488 아래 니블 *)
let test_scan_lines () =
  let m =
    boot
      (String.concat ""
         [ alt ~bl:0x30 0; wait_for flag_a;
           alt ~bl:0x30 1; wait_for flag_b;
           alt ~bl:0x30 2; exit_0 ])
      ~before:(fun m ->
          Dos_machine.mem_write m bda_switches 0x60;
          Dos_machine.mem_write m bda_vga_flags 0x09)
  in
  Dos_machine.run m ~max_steps:5000;
  checki "200 줄: 0x489 = 89h(bit 7, 다른 비트 둠)" (bda m bda_vga_flags) 0x89;
  checki "200 줄: 0x488 = 68h(위 니블 둠)" (bda m bda_switches) 0x68;
  Dos_machine.mem_write m (psp + flag_a) 1;
  Dos_machine.run m ~max_steps:5000;
  checki "350 줄: 0x489 bit 7·4 둘 다 끔" (bda m bda_vga_flags) 0x09;
  checki "350 줄: 0x488 = 69h" (bda m bda_switches) 0x69;
  Dos_machine.mem_write m (psp + flag_b) 1;
  run_to_exit m;
  checki "400 줄: 0x489 bit 4" (bda m bda_vga_flags) 0x19;
  checki "400 줄: 0x488 = 69h" (bda m bda_switches) 0x69

(* 31h 가 끄면 모드 세우기가 DAC 을 두고, 켜면 기본값으로 되돌린다 *)
let test_palette_loading () =
  let red = int10 0x1010 ~bx:0 ~dx:(63 lsl 8) ~cx:0 in
  let m =
    boot
      (String.concat ""
         [ red; alt ~bl:0x31 1; int10 0x0013; wait_for flag_a;
           alt ~bl:0x31 0; int10 0x0013; exit_0 ])
  in
  Dos_machine.run m ~max_steps:20_000;
  check "31h AL=1: 0x489 bit 3" (bda m bda_vga_flags land 0x08 <> 0);
  check "31h AL=1: 모드 13h 뒤에도 DAC 0 은 빨강"
    ((Dos_machine.frame_rgb m).[0] = '\xff');
  Dos_machine.mem_write m (psp + flag_a) 1;
  run_to_exit m;
  check "31h AL=0: bit 3 을 끈다" (bda m bda_vga_flags land 0x08 = 0);
  check "31h AL=0: 모드 13h 가 DAC 0 을 검정으로" ((Dos_machine.frame_rgb m).[0] = '\x00')

let test_cursor_emulation () =
  let m =
    boot (alt ~bl:0x34 1 ^ wait_for flag_a ^ alt ~bl:0x34 0 ^ exit_0)
      ~before:(fun m -> Dos_machine.mem_write m bda_ega_misc 0x60)
  in
  Dos_machine.run m ~max_steps:5000;
  checki "34h AL=1: 0x487 bit 0 만 켠다" (bda m bda_ega_misc) 0x61;
  Dos_machine.mem_write m (psp + flag_a) 1;
  run_to_exit m;
  checki "34h AL=0: bit 0 만 끈다" (bda m bda_ega_misc) 0x60

let solid frame (r, g, b) =
  let ok = ref true in
  String.iteri
    (fun i c ->
      let want = match i mod 3 with 0 -> r | 1 -> g | _ -> b in
      if Char.code c <> want then ok := false)
    frame;
  !ok

(* 36h 로 화면을 끄면 그림은 DAC 0 번 색 한 가지다. 모드를 세우면 켜진다. *)
let test_blank () =
  let red0 = int10 0x1010 ~bx:0 ~dx:(63 lsl 8) ~cx:0 in
  let m =
    boot
      (String.concat ""
         [ red0; alt ~bl:0x36 1; wait_for flag_a;
           alt ~bl:0x36 0; wait_for flag_b;
           alt ~bl:0x36 1; int10 0x0012; exit_0 ])
  in
  Dos_machine.mem_write m 0xB8000 (Char.code 'A');
  Dos_machine.mem_write m 0xB8001 0x07;
  Dos_machine.run m ~max_steps:5000;
  let w, h = Dos_machine.frame_dims m in
  let f = Dos_machine.frame_rgb m in
  check "36h AL=1: 그림 크기는 그대로" (String.length f = w * h * 3);
  check "36h AL=1: 화면 전체가 DAC 0(빨강)" (solid f (0xff, 0, 0));
  Dos_machine.mem_write m (psp + flag_a) 1;
  Dos_machine.run m ~max_steps:5000;
  check "36h AL=0: 다시 그린다" (not (solid (Dos_machine.frame_rgb m) (0xff, 0, 0)));
  Dos_machine.mem_write m (psp + flag_b) 1;
  run_to_exit m;
  check "모드 세우기가 화면을 다시 켠다" (not (Dos_video.screen_blanked m.Dos_state.video))

let test_port_blank () =
  let seq_off = "\xba" ^ w16 0x3C4 ^ "\xb0\x01\xee\x42\xec\x0c\x20\xee" in
  let m = boot (int10 0x0013 ^ seq_off ^ exit_0) in
  run_to_exit m;
  check "포트로 끈 화면도 DAC 0 한 가지(검정)" (solid (Dos_machine.frame_rgb m) (0, 0, 0));
  check "포트로 끈 비트를 BIOS 도 본다" (Dos_video.screen_blanked m.Dos_state.video)

let save_exn m =
  match Dos_snapshot.save m with
  | Ok s -> s
  | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let test_snapshot () =
  let m = boot (alt ~bl:0x36 1 ^ exit_0) in
  run_to_exit m;
  match Dos_snapshot.restore (save_exn m) with
  | Error e -> check ("restore: " ^ Dos_snapshot.error_to_string e) false
  | Ok r -> check "끈 화면이 복원된다" (Dos_video.screen_blanked r.Dos_state.video)

let () =
  test_answers ();
  test_scan_lines ();
  test_palette_loading ();
  test_cursor_emulation ();
  test_blank ();
  test_port_blank ();
  test_snapshot ();
  if !failed = 0 then print_endline "dos vga alt select: all passed"
  else begin
    Printf.eprintf "dos vga alt select: %d failures\n%!" !failed;
    exit 1
  end
