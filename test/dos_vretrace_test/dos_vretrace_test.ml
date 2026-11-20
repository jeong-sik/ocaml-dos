(* 포트 0x3DA bit3(수직 귀선)은 VGA 한 프레임에 한 번, 두 줄 동안 켜지고
   bit0(블랭킹)은 수평·수직 블랭킹에서 켜진다.
   줄 수와 귀선 자리는 SeaBIOS stdvgamodes.c 의 CRTC 표, 한 줄은 800 도트 /
   25.175MHz 다. 아래 사이클 값은 4.77MHz(속도 4) 기준이다. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let pc = Dos_ports.pc_speed

let fast =
  match Dos_ports.speed_of_mhz 12. with
  | Ok s -> s
  | Error e -> failwith e

let ports ~speed ~mode =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  Dos_video.set_mode video mode ~clear:true;
  Dos_ports.create ~speed ~mpu401:false ~mem ~video ~sound_blaster:false
    ~cmos:(Dos_cmos.create ~clock:(fun () -> invalid_arg "no clock in this test"))

let status_port = 0x3DA
let vsync_bit p = Dos_ports.port_in p status_port land 0x08 <> 0

(* 귀선 폭 303 사이클보다 촘촘하게 훑는다. *)
let stride = 50

(* [0, until) 사이클에서 bit3 이 꺼짐 → 켜짐으로 바뀐 횟수 *)
let rising_edges p ~until =
  let rec go now prev n =
    if now >= until then n
    else begin
      Dos_ports.set_now p now;
      let v = vsync_bit p in
      go (now + stride) v (if v && not prev then n + 1 else n)
    end
  in
  go 0 false 0

(* 처음 켜지는 사이클, 켜져 있는 사이클 수, 다음에 켜지는 사이클.
   100ms(어느 프레임보다 길다) 안에 못 찾으면 -1 이다. *)
let pulses p =
  let limit = Dos_ports.cpu_hz p / 10 in
  let rec find ~from want =
    if from < 0 || from >= limit then -1
    else begin
      Dos_ports.set_now p from;
      if vsync_bit p = want then from else find ~from:(from + 1) want
    end
  in
  let start = find ~from:0 true in
  let stop = find ~from:start false in
  let next = find ~from:stop true in
  (start, (if stop < 0 then -1 else stop - start), next)

let pulse_width = 303

(* 모드 무리마다: 첫 귀선 사이클, 다음 귀선 사이클(= 첫 귀선 + 프레임),
   1 초(4,772,728 사이클)에 켜지는 횟수. 프레임은 449 줄 68097 사이클,
   525 줄 79624 사이클이다. *)
let families =
  [ ("text 03h", 0x03, 62486, 130583, 70);
    ("CGA 04h", 0x04, 62486, 130583, 70);
    ("EGA 0Dh", 0x0D, 62486, 130583, 70);
    ("VGA 13h", 0x13, 62486, 130583, 70);
    ("350 lines 0Fh", 0x0F, 58694, 126791, 70);
    ("350 lines 10h", 0x10, 58694, 126791, 70);
    ("480 lines 11h", 0x11, 74316, 153940, 60);
    ("480 lines 12h", 0x12, 74316, 153940, 60) ]

let test_families () =
  List.iter (fun (name, mode, start, next, per_second) ->
      let p = ports ~speed:pc ~mode in
      let s, w, n = pulses p in
      check_int (name ^ ": first retrace cycle") s start;
      check_int (name ^ ": retrace width") w pulse_width;
      check_int (name ^ ": next retrace cycle") n next;
      let q = ports ~speed:pc ~mode in
      check_int (name ^ ": retraces per second")
        (rising_edges q ~until:(Dos_ports.cpu_hz q)) per_second)
    families

let test_speed () =
  (* 12MHz 기계도 1 초에 70 번이다. 첫 귀선은 같은 13.09ms 자리에 온다. *)
  let p = ports ~speed:fast ~mode:0x13 in
  let s, w, n = pulses p in
  check_int "fast: first retrace cycle" s 156215;
  check_int "fast: retrace width" w 758;
  check_int "fast: next retrace cycle" n 326459;
  let q = ports ~speed:fast ~mode:0x13 in
  check_int "fast: retraces per second" (rising_edges q ~until:(Dos_ports.cpu_hz q)) 70

(* ---------- bit0: 수평·수직 블랭킹 ----------
   bit0 은 화면을 그리는 동안 0, 한 줄 800 도트 가운데 수평 블랭킹과 표시
   줄이 끝난 뒤 프레임 끝까지 1 이다. 수평 블랭킹은 640 도트부터 640 도트
   폭 모드는 784, 320 도트 폭 모드는 768 미만이다(SeaBIOS CRTC 표). *)
let blank_bit p = Dos_ports.port_in p status_port land 1 <> 0

let blank_at p c =
  Dos_ports.set_now p c;
  blank_bit p

(* 프레임 첫머리에서 [dot] 도트째가 되는 가장 이른 사이클 *)
let cycle_of_dot p dot =
  let hz = Dos_ports.cpu_hz p in
  ((dot * hz) + 25_174_999) / 25_175_000

let line_dot line in_line = (line * 800) + in_line

let test_display_enable_edges () =
  (* 4.77MHz 에서 한 줄은 151.65 사이클, 640 도트는 121.3, 784 도트는
     148.6 사이클째다. *)
  let p = ports ~speed:pc ~mode:0x13 in
  check "13h c=121 draws" (not (blank_at p 121));
  check "13h c=122 hblank" (blank_at p 122);
  check "13h c=148 hblank" (blank_at p 148);
  check "13h c=149 draws" (not (blank_at p 149));
  check "13h c=152 next line draws" (not (blank_at p 152));
  (* 표시 줄 400 개는 60,666.6 사이클째에 끝난다. 399 번째 줄 표시 한가운데와
     400 번째 줄 표시 한가운데를 가른다. *)
  check "13h c=60600 line 399 draws" (not (blank_at p 60_600));
  check "13h c=60700 line 400 vblank" (blank_at p 60_700);
  check "13h c=68000 vblank" (blank_at p 68_000);
  check "13h c=68098 next frame draws" (not (blank_at p 68_098));
  (* 320 도트 폭 표는 끝이 768 도트(145.6 사이클째)다. 640 도트 폭은 아직 블랭킹이다. *)
  let q = ports ~speed:pc ~mode:0x0D in
  check "0Dh c=145 hblank" (blank_at q 145);
  check "0Dh c=146 draws" (not (blank_at q 146));
  check "13h c=146 still hblank" (blank_at p 146);
  (* 350·480 줄 모드는 표시 줄 수가 다르다. *)
  let r = ports ~speed:pc ~mode:0x10 in
  check "10h c=53000 line 349 draws" (not (blank_at r 53_000));
  check "10h c=53200 line 350 vblank" (blank_at r 53_200);
  let v = ports ~speed:pc ~mode:0x12 in
  check "12h c=72600 line 478 draws" (not (blank_at v 72_600));
  check "12h c=73000 line 481 vblank" (blank_at v 73_000)

let test_display_enable_speed () =
  (* 도트 시각으로 보면 CPU 속도와 상관없다. *)
  List.iter (fun (name, speed) ->
      let p = ports ~speed ~mode:0x13 in
      let at line in_line = blank_at p (cycle_of_dot p (line_dot line in_line)) in
      check (name ^ ": line 10 dot 100 draws") (not (at 10 100));
      check (name ^ ": line 10 dot 700 hblank") (at 10 700);
      check (name ^ ": line 10 dot 790 draws") (not (at 10 790));
      check (name ^ ": line 399 dot 700 hblank") (at 399 700);
      check (name ^ ": line 400 dot 100 vblank") (at 400 100);
      check (name ^ ": line 448 dot 100 vblank") (at 448 100))
    [ ("pc", pc); ("12MHz", fast) ]

(* 100MHz 에서는 한 도트에 사이클이 4 개라 모든 도트 자리를 정확히 짚는다.
   경계: 640 도트부터 784 미만이 블랭킹이다(320 도트 폭 모드는 768 미만). *)
let test_display_enable_exact_dots () =
  let hundred =
    match Dos_ports.speed_of_mhz 100. with Ok s -> s | Error e -> failwith e
  in
  let at p line in_line = blank_at p (cycle_of_dot p (line_dot line in_line)) in
  let p = ports ~speed:hundred ~mode:0x13 in
  check "exact: line 0 dot 0 draws" (not (at p 0 0));
  check "exact: dot 639 draws" (not (at p 10 639));
  check "exact: dot 640 hblank" (at p 10 640);
  check "exact: dot 783 hblank" (at p 10 783);
  check "exact: dot 784 draws" (not (at p 10 784));
  check "exact: line 399 dot 799 draws" (not (at p 399 799));
  check "exact: line 400 dot 0 vblank" (at p 400 0);
  check "exact: last line dot 799 vblank" (at p 448 799);
  let q = ports ~speed:hundred ~mode:0x0D in
  check "exact 0Dh: dot 767 hblank" (at q 10 767);
  check "exact 0Dh: dot 768 draws" (not (at q 10 768))

(* 표시 줄 안에서 bit0 이 이어서 1 인 구간은 수평 블랭킹 하나(27 사이클)뿐이다.
   수직 블랭킹은 몇 천 사이클을 이어서 1 이다. 게임이 "블랭킹이 N 번 읽는 동안
   이어지면 수직 귀선" 으로 가르는 것이 이 차이에 기댄다. *)
let longest_blank_run p ~from ~until ~stride =
  let rec go c run best =
    if c >= until then max run best
    else
      let b = blank_at p c in
      go (c + stride) (if b then run + 1 else 0) (max best (if b then run + 1 else 0))
  in
  go from 0 0

let test_blank_runs () =
  let p = ports ~speed:pc ~mode:0x13 in
  (* 표시 줄 400 개: 27 사이클 / 5 = 최대 6 번 읽는 동안 *)
  let h = longest_blank_run p ~from:0 ~until:60_600 ~stride:5 in
  check_int "hblank run at stride 5 is 6 reads" h 6;
  (* 수직 블랭킹 60,700 부터 프레임 끝 68,097 까지 *)
  let v = longest_blank_run p ~from:60_700 ~until:68_097 ~stride:5 in
  check "vblank run is over 1000 reads" (v > 1000)

(* 프레임 한 번에 1 인 사이클 수: 400 줄 × 27.3 + 49 줄 × 151.65 = 18,351 *)
let test_blank_share () =
  let p = ports ~speed:pc ~mode:0x13 in
  let rec count c n = if c >= 68_097 then n else count (c + 1) (if blank_at p c then n + 1 else n) in
  let n = count 0 0 in
  check "blank cycles per frame near 18351" (n >= 18_100 && n <= 18_600)

(* 게스트가 3DAh 를 돌며 귀선이 켜지는 횟수를 BIOS 틱 18 번 동안 센다.
   18 틱 = 18 × 65536 × 4 = 4,718,592 사이클. 귀선은 62486 + 68097k
   사이클에 켜지므로 k = 0..68, 69 번이다. 프로그램은 사이클 0 근처에서
   시작한다. *)
let counter_com =
  "\xFB"                               (* sti *)
  ^ "\x31\xC0\x8E\xC0"                 (* xor ax,ax; mov es,ax *)
  ^ "\x26\x8B\x1E\x6C\x04"             (* mov bx,[es:046Ch] *)
  ^ "\x31\xC9"                         (* xor cx,cx — 센 횟수 *)
  ^ "\xBA\xDA\x03"                     (* mov dx,03DAh *)
  ^ "\x30\xE4"                         (* xor ah,ah — 앞 값 *)
  (* L: *)
  ^ "\xEC\x24\x08"                     (* in al,dx; and al,8 *)
  ^ "\x74\x05"                         (* jz S *)
  ^ "\x84\xE4\x75\x01"                 (* test ah,ah; jnz S *)
  ^ "\x41"                             (* inc cx *)
  (* S: *)
  ^ "\x88\xC4"                         (* mov ah,al *)
  ^ "\x26\x8B\x36\x6C\x04"             (* mov si,[es:046Ch] *)
  ^ "\x29\xDE\x83\xFE\x12"             (* sub si,bx; cmp si,18 *)
  ^ "\x72\xE8"                         (* jb L *)
  ^ "\x89\x0E\x00\x03"                 (* mov [0300h],cx *)
  ^ "\xB8\x00\x4C\xCD\x21"             (* mov ax,4C00h; int 21h *)

let com_psp = 0x10000

(* 3DAh bit0 이 60 번 이어서 1 일 때까지 기다린다. 한 번 읽는 데 20 사이클
   넘게 드니 수평 블랭킹(27 사이클)으로는 끝나지 않고 수직 블랭킹에서만
   끝난다. 읽을 때마다 뒤집히는 bit0 이면 영원히 끝나지 않는다. *)
let vblank_wait_com =
  "\xBA\xDA\x03"                      (* mov dx,03DAh *)
  ^ "\xB9\x3C\x00"                    (* L: mov cx,60 *)
  ^ "\xEC\xA8\x01\x74\xFB"            (* W: in al,dx; test al,1; jz W *)
  ^ "\xEC\x24\x01\xE0\xFB"            (* R: in al,dx; and al,1; loopne R *)
  ^ "\x09\xC9\x75\xEF"                (* or cx,cx; jnz L *)
  ^ "\x89\x0E\x00\x03"                (* mov [0300h],cx *)
  ^ "\xB8\x00\x4C\xCD\x21"            (* mov ax,4C00h; int 21h *)

let test_vblank_wait () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m vblank_wait_com;
  Dos_machine.run m ~max_steps:2_000_000;
  check "vblank wait exited" (Dos_machine.exited m);
  let rd a = Dos_machine.mem_read m a in
  check_int "vblank wait cx" (rd (com_psp + 0x300) lor (rd (com_psp + 0x301) lsl 8)) 0

let test_guest () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m counter_com;
  Dos_machine.run m ~max_steps:5_000_000;
  check "counter exited" (Dos_machine.exited m);
  let rd a = Dos_machine.mem_read m a in
  let n = rd (com_psp + 0x300) lor (rd (com_psp + 0x301) lsl 8) in
  check_int "guest counts 69 retraces in 18 ticks" n 69

let () =
  test_families ();
  test_speed ();
  test_display_enable_edges ();
  test_display_enable_speed ();
  test_display_enable_exact_dots ();
  test_blank_runs ();
  test_blank_share ();
  test_guest ();
  test_vblank_wait ();
  if !failed = 0 then print_endline "dos vretrace: all passed"
  else begin
    Printf.eprintf "dos vretrace: %d failures\n%!" !failed;
    exit 1
  end
