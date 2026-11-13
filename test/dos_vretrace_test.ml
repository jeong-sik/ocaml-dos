(* 포트 0x3DA bit3(수직 귀선) — VGA 한 프레임에 한 번, 두 줄 동안 켜진다.
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
  Dos_ports.create ~speed ~mem ~video

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

let test_display_enable () =
  (* bit0 은 지금처럼 읽을 때마다 뒤집힌다 — 이 PR 은 bit3 만 바꾼다. *)
  let p = ports ~speed:pc ~mode:0x13 in
  let a = Dos_ports.port_in p status_port land 1 in
  let b = Dos_ports.port_in p status_port land 1 in
  check "bit0 flips on each read" (a <> b)

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
  test_display_enable ();
  test_guest ();
  if !failed = 0 then print_endline "dos vretrace: all passed"
  else begin
    Printf.eprintf "dos vretrace: %d failures\n%!" !failed;
    exit 1
  end
