(* split screen: line compare(CRTC 18h, 07h bit4 = 8번째 비트, 09h bit6 =
   9번째 비트)가 정한 행부터 CRTC 는 시작 주소 대신 주소 0 에서 다시 읽는다.
   행 번호는 (line compare + 1) / 행당 줄 수다(dosbox-staging 와 같은 식).
   게임은 위쪽을 스크롤하면서 아래 점수판을 붙박이로 둘 때 쓴다. *)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let check_bool name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end

let video () = Dos_video.create ~mem:(Bytes.make 0x100000 '\000')

let crtc_index = 0x3D4
let crtc_data = 0x3D5
let vga_base = 0xA0000

let write_crtc v index value =
  ignore (Dos_video.port_out v crtc_index index);
  ignore (Dos_video.port_out v crtc_data value)

(* line compare 의 세 조각을 한 번에 쓴다. 07h 와 09h 는 다른 비트를 지킨다. *)
let set_line_compare v lc =
  write_crtc v 0x18 (lc land 0xFF);
  let o = Dos_video.crtc v 0x07 in
  write_crtc v 0x07 (if lc land 0x100 <> 0 then o lor 0x10 else o land lnot 0x10);
  let m = Dos_video.crtc v 0x09 in
  write_crtc v 0x09 (if lc land 0x200 <> 0 then m lor 0x40 else m land lnot 0x40)

let set_start v ma =
  write_crtc v 0x0C (ma lsr 8);
  write_crtc v 0x0D (ma land 0xFF)

(* ---------- 256색(13h): 행 r 의 x = 10 에 r + 1 을 둔다 ---------- *)
let line_bytes_256 = 320
let marker_x = 10
let rows_256 = 200

let fill_256 v =
  for r = 0 to rows_256 - 1 do
    Dos_video.mem_write v (vga_base + (r * line_bytes_256) + marker_x) (r + 1)
  done

let pixel256 v y = Dos_video.screen_pixel v ~x:marker_x ~y
let frame256 v y = Char.code (Bytes.get (Dos_video.screen256 v) ((y * 320) + marker_x))

let both name v y want =
  checki (name ^ " (screen_pixel)") (pixel256 v y) want;
  checki (name ^ " (screen256)") (frame256 v y) want

let () =
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  fill_256 v;
  (* 부팅값: line compare 는 3FFh 라 나뉘지 않는다 *)
  both "13h boot, row 0" v 0 1;
  both "13h boot, row 199" v 199 200;
  (* 시작 주소 1600(= 메모리 20행)로 스크롤: 화면 행 y 는 메모리 행 y + 20 *)
  set_start v 1600;
  both "scrolled, row 0" v 0 21;
  both "scrolled, row 150" v 150 171;
  (* line compare 300, 행당 2줄 -> 301 / 2 = 150 행부터 주소 0 에서 다시 읽는다 *)
  set_line_compare v 300;
  both "split at 300, row 0 (scrolled)" v 0 21;
  both "split at 300, row 149 (scrolled)" v 149 170;
  both "split at 300, row 150 (panel row 0)" v 150 1;
  both "split at 300, row 151 (panel row 1)" v 151 2;
  both "split at 300, row 199 (panel row 49)" v 199 50;
  (* 홀수 299 도 같은 행이다: (299 + 1) / 2 = 150 *)
  set_line_compare v 299;
  both "split at 299, row 149" v 149 170;
  both "split at 299, row 150" v 150 1;
  (* 302 는 151 행부터다 *)
  set_line_compare v 302;
  both "split at 302, row 150 (still scrolled)" v 150 171;
  both "split at 302, row 151 (panel row 0)" v 151 1;
  (* 9번째 비트(09h bit6)가 서면 값이 512 를 넘어 화면 밖이다: 나뉘지 않는다 *)
  set_line_compare v 0x220;
  both "line compare 544 is below the screen" v 150 171;
  (* 8번째 비트(07h bit4)가 서야 300 이다: 낮은 8비트만으로는 44 다 *)
  set_line_compare v 44;
  both "line compare 44, row 21 (scrolled)" v 21 42;
  both "line compare 44, row 22 (panel row 0)" v 22 1;
  (* 시작 주소를 바꿔도 나뉜 화면은 계속 주소 0 이다 *)
  set_line_compare v 300;
  set_start v 3200;
  both "rescrolled, row 0" v 0 41;
  both "rescrolled, row 150 (panel row 0)" v 150 1

(* 지문은 나뉜 화면이 보여 주는 메모리만 본다 *)
let () =
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  fill_256 v;
  set_start v 1600;
  set_line_compare v 300;
  let base = Dos_video.screen_digest v in
  (* 메모리 행 3 은 나뉜 화면(행 153)에 보인다 *)
  Dos_video.mem_write v (vga_base + (3 * line_bytes_256) + marker_x) 99;
  check_bool "digest follows the panel" (Dos_video.screen_digest v <> base) true;
  Dos_video.mem_write v (vga_base + (3 * line_bytes_256) + marker_x) 4;
  checki "digest back" (Dos_video.screen_digest v) base;
  (* 메모리 행 180 은 스크롤 화면(20..169)과 나뉜 화면(0..49) 어디에도 안 보인다 *)
  Dos_video.mem_write v (vga_base + (180 * line_bytes_256) + marker_x) 99;
  checki "digest ignores a hidden row" (Dos_video.screen_digest v) base

(* ---------- 16색(12h, 640x480, 행당 1줄) ---------- *)
let line_bytes_16 = 80

let () =
  let v = video () in
  Dos_video.set_mode v 0x12 ~clear:true;
  (* 메모리 행 0 과 1 의 첫 바이트에 0x80 (x = 0 이 색 15, 맵 마스크는 부팅값) *)
  Dos_video.mem_write v (vga_base + 0) 0x80;
  Dos_video.mem_write v (vga_base + line_bytes_16) 0x80;
  let px y = Dos_video.screen_pixel v ~x:0 ~y in
  let frame y = Char.code (Bytes.get (Dos_video.screen16 v) (y * 640)) in
  let both16 name y want_set =
    check_bool (name ^ " (screen_pixel)") (px y <> 0) want_set;
    check_bool (name ^ " (screen16)") (frame y <> 0) want_set
  in
  set_start v line_bytes_16;
  (* 화면 행 y 는 메모리 행 y + 1 *)
  both16 "12h scrolled, row 0 shows memory row 1" 0 true;
  both16 "12h scrolled, row 1 shows memory row 2" 1 false;
  (* 12h 는 행당 1줄: line compare 99 -> 100 행부터 주소 0 *)
  set_line_compare v 99;
  both16 "12h split, row 99" 99 false;
  both16 "12h split, row 100 (panel row 0)" 100 true;
  both16 "12h split, row 101 (panel row 1)" 101 true;
  both16 "12h split, row 102 (panel row 2)" 102 false

let () =
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end;
  print_endline "VGA split screen: all passed"
