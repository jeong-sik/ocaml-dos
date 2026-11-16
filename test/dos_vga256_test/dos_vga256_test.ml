(* 256색 VGA — 13h 의 chain-4 와 그것을 끈 Mode X. 게스트가 쓰는 길로만
   확인한다: INT 10h, 시퀀서·그래픽 컨트롤러·CRTC 포트, 0xA0000 읽기-쓰기.
   판정은 화면에 보이는 점, 프레임 크기, 포트에서 읽은 값이다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got=%d (0x%x) want=%d (0x%x)\n%!" name got got want want
  end

(* ---------- 아주 작은 어셈블러 ---------- *)

let b n = String.make 1 (Char.chr (n land 0xff))
let w n = b n ^ b (n lsr 8)
let mov_ax n = "\xb8" ^ w n
let mov_cx n = "\xb9" ^ w n
let mov_dx n = "\xba" ^ w n
let mov_al n = "\xb0" ^ b n
let int_ n = "\xcd" ^ b n
let out_dx_ax = "\xef"
let out_dx_al = "\xee"
let in_al_dx = "\xec"
let quit = mov_ax 0x4c00 ^ int_ 0x21
let set_mode m = mov_ax m ^ int_ 0x10

(* ES = 0xA000 *)
let es_gfx = mov_ax 0xA000 ^ "\x8e\xc0"
let store_es off = "\x26\xa2" ^ w off            (* mov es:[off], al *)
let load_es off = "\x26\xa0" ^ w off             (* mov al, es:[off] *)
let store_al off = "\xa2" ^ w off                (* mov [off], al *)

(* index 포트와 data 포트가 나란히 있는 장치에 워드 하나로 둘 다 쓴다. *)
let reg port index value =
  mov_dx port ^ mov_ax (((value land 0xff) lsl 8) lor (index land 0xff)) ^ out_dx_ax

let seq = reg 0x3C4
let gc = reg 0x3CE
let crtc = reg 0x3D4

(* 포트 [port] 의 [index] 레지스터를 읽어 [dst] 에 둔다. *)
let read_reg port index dst =
  mov_dx port ^ mov_al index ^ out_dx_al ^ mov_dx (port + 1) ^ in_al_dx ^ store_al dst

let map_mask m = seq 0x02 m
let put_es off color = mov_al color ^ store_es off

(* chain-4 를 끄고 CRTC 를 byte 모드로 — Abrash 의 Mode X 가 쓰는 세 줄. *)
let unchain = seq 0x04 0x06 ^ crtc 0x14 0x00 ^ crtc 0x17 0xE3

let psp_base = 0x1000 * 16
let peek m off = Dos_machine.mem_read m (psp_base + off)

let run_com code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m (code ^ quit);
  Dos_machine.run m ~max_steps:100_000;
  if not (Dos_machine.exited m) then begin
    incr failed;
    prerr_endline "FAIL the program did not end"
  end;
  m

let px m x y = Dos_machine.pixel m ~x ~y

(* ---------- 13h ---------- *)

let test_chained () =
  let m =
    run_com
      (set_mode 0x13 ^ es_gfx ^ put_es 0x0005 5 ^ put_es 0x0141 7
       ^ read_reg 0x3C4 0x04 0x2000
       ^ load_es 0x0005 ^ store_al 0x2001 ^ load_es 0x0004 ^ store_al 0x2002)
  in
  check "13h: reading byte 5" (peek m 0x2001) 5;
  check "13h: reading byte 4" (peek m 0x2002) 0;
  check "13h: (5,0)" (px m 5 0) 5;
  check "13h: (1,1) is byte 321" (px m 1 1) 7;
  check "13h: its neighbour" (px m 0 1) 0;
  check "13h: memory mode has chain-4" (peek m 0x2000) 0x0E;
  let fw, fh = Dos_machine.frame_dims m in
  check "13h: width" fw 320;
  check "13h: height" fh 200

let test_bios_pixel () =
  (* AH=0Ch AL=09h CX=3 DX=2, then AH=0Dh into [2000h] *)
  let m =
    run_com
      (set_mode 0x13 ^ mov_ax 0x0C09 ^ mov_cx 3 ^ mov_dx 2 ^ int_ 0x10
       ^ mov_ax 0x0D00 ^ mov_cx 3 ^ mov_dx 2 ^ int_ 0x10 ^ store_al 0x2000)
  in
  check "BIOS put pixel" (px m 3 2) 9;
  check "BIOS read pixel" (peek m 0x2000) 9

(* ---------- Mode X ---------- *)

let test_unchained_writes () =
  let m =
    run_com
      (set_mode 0x13 ^ unchain ^ es_gfx
       ^ map_mask 0x02 ^ put_es 0x0000 9
       ^ map_mask 0x0F ^ put_es 0x0001 3
       ^ put_es 0x0050 6)
  in
  check "plane 1 is the second dot" (px m 1 0) 9;
  check "the masked planes stay" (px m 0 0) 0;
  check "the third dot stays" (px m 2 0) 0;
  check "one byte, four planes: first" (px m 4 0) 3;
  check "one byte, four planes: last" (px m 7 0) 3;
  check "a row is 80 bytes" (px m 3 1) 6

let test_latch_copy () =
  let m =
    run_com
      (set_mode 0x13 ^ unchain ^ es_gfx
       ^ map_mask 0x01 ^ put_es 0 1 ^ map_mask 0x02 ^ put_es 0 2
       ^ map_mask 0x04 ^ put_es 0 3 ^ map_mask 0x08 ^ put_es 0 4
       ^ map_mask 0x0F ^ load_es 0 ^ gc 0x05 0x41 ^ put_es 0x0050 0xFF)
  in
  for x = 0 to 3 do
    check (Printf.sprintf "write mode 1 copies dot %d" x) (px m x 1) (x + 1)
  done

let test_page_flip () =
  let draw = set_mode 0x13 ^ unchain ^ es_gfx ^ map_mask 0x0F ^ put_es 0x4000 8 in
  let before = run_com draw in
  check "page 2 is off screen" (px before 0 0) 0;
  let after = run_com (draw ^ crtc 0x0C 0x40 ^ crtc 0x0D 0x00) in
  check "start address 4000h shows page 2" (px after 0 0) 8;
  check "and its fourth dot" (px after 3 0) 8;
  if Dos_machine.screen_digest before = Dos_machine.screen_digest after then begin
    incr failed;
    prerr_endline "FAIL the digest follows the start address"
  end

(* Abrash, Graphics Programming Black Book ch. 47: Set320x240Mode. *)
let mode_x_240 =
  crtc 0x11 0x0E    (* unprotect 0-7 *)
  ^ crtc 0x06 0x0D ^ crtc 0x07 0x3E ^ crtc 0x09 0x41 ^ crtc 0x10 0xEA
  ^ crtc 0x11 0xAC ^ crtc 0x12 0xDF ^ crtc 0x14 0x00 ^ crtc 0x15 0xE7
  ^ crtc 0x16 0x06 ^ crtc 0x17 0xE3

let test_320x240 () =
  let m = run_com (set_mode 0x13 ^ seq 0x04 0x06 ^ mode_x_240 ^ es_gfx
                   ^ map_mask 0x0F ^ put_es (239 * 80) 2) in
  let fw, fh = Dos_machine.frame_dims m in
  check "Mode X width" fw 320;
  check "Mode X height" fh 240;
  check "the last row is drawn" (px m 0 239) 2

let test_protect () =
  let m =
    run_com
      (set_mode 0x13
       ^ crtc 0x01 0x59         (* protected: ignored *)
       ^ crtc 0x07 0x00         (* protected: only bit 4 goes *)
       ^ read_reg 0x3D4 0x01 0x2000 ^ read_reg 0x3D4 0x07 0x2001
       ^ crtc 0x07 0x10         (* and comes back *)
       ^ read_reg 0x3D4 0x07 0x2002)
  in
  check "13h protects 01h" (peek m 0x2000) 0x4F;
  check "07h keeps all but bit 4" (peek m 0x2001) 0x0F;
  check "07h bit 4 is set while protected" (peek m 0x2002) 0x1F;
  let fw, _ = Dos_machine.frame_dims m in
  check "width unchanged" fw 320;
  let m = run_com (set_mode 0x13 ^ crtc 0x11 0x0E ^ crtc 0x01 0x59) in
  let fw, _ = Dos_machine.frame_dims m in
  check "unprotected 01h makes 360 wide" fw 360

(* 09h: 아래 다섯 비트 + 1 줄이 한 행이고, bit7 이 서 있으면 두 배다.
   0 이면 400 줄이 400 행이다(320x400). *)
let test_scan_lines () =
  List.iter
    (fun (v, want) ->
      let m = run_com (set_mode 0x13 ^ crtc 0x09 v) in
      let _, fh = Dos_machine.frame_dims m in
      check (Printf.sprintf "09h=%02X height" v) fh want)
    [ (0x41, 200); (0x40, 400); (0x80, 200); (0x81, 100) ]

(* 13h 로 쓴 바이트는 평면 (a & 3) 의 (a & ~3) 자리에 앉아 있다. chain-4 를
   끄면 그 자리가 평면별로 보인다: byte 5 는 평면 1 의 4 번, 곧 (17, 0). *)
let test_chained_then_unchained () =
  let m = run_com (set_mode 0x13 ^ es_gfx ^ put_es 0x0005 5 ^ unchain) in
  check "chained byte 5 seen unchained" (px m 17 0) 5;
  check "not where it was" (px m 5 0) 0

let test_snapshot () =
  let m =
    run_com
      (set_mode 0x13 ^ seq 0x04 0x06 ^ mode_x_240 ^ es_gfx ^ map_mask 0x0F
       ^ put_es 0x4000 8 ^ crtc 0x0C 0x40)
  in
  match Dos_snapshot.save m with
  | Error e ->
    incr failed;
    prerr_endline ("FAIL save: " ^ Dos_snapshot.save_error_to_string e)
  | Ok s ->
    (match Dos_snapshot.restore s with
     | Error e ->
       incr failed;
       prerr_endline ("FAIL restore: " ^ Dos_snapshot.error_to_string e)
     | Ok r ->
       if Dos_machine.frame_rgb r <> Dos_machine.frame_rgb m then begin
         incr failed;
         prerr_endline "FAIL the restored frame differs"
       end;
       check "restored page 2" (px r 0 0) 8;
       (match Dos_snapshot.save r with
        | Ok s' when s' = s -> ()
        | Ok _ | Error _ ->
          incr failed;
          prerr_endline "FAIL a restored machine saves the same bytes"))

let () =
  test_chained ();
  test_bios_pixel ();
  test_unchained_writes ();
  test_latch_copy ();
  test_page_flip ();
  test_320x240 ();
  test_protect ();
  test_scan_lines ();
  test_chained_then_unchained ();
  test_snapshot ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos vga256: all passed"
