(* OPL2(YM3812) 타이머와 상태 포트. 소리는 없다.

   기준:
   - Jeffrey S. Lee, "Programming the AdLib/Sound Blaster FM Music Chips"
     2.0: 상태 bit7·6·5, 레지스터 02h·03h(값에서 올라가 넘친다), 04h
     (bit7 이 서면 나머지 무시, 마스크, 시작), AdLib 공식 감지 순서
   - DOSBox Staging opl.cpp: 넘침까지 (256 - 값) 칸, 칸 경계에서 시작,
     넘친 뒤 다시 세기, 마스크된 타이머는 플래그를 세우지 않는다,
     OPL2 상태의 아래 비트는 06h *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %02X want %02X\n%!" name got want
  end

let t1_step = 382                      (* 80µs, 4.77MHz 사이클 *)
let t2_step = 1527                     (* 320µs *)
let status_port = 0x388
let data_port = 0x389
let idle = 0x06                        (* OPL2 의 아래 비트 *)
let t1_done = 0xC6                     (* IRQ + 타이머 1 + 06h *)
let t2_done = 0xA6                     (* IRQ + 타이머 2 + 06h *)
let both_done = 0xE6
let detect_mask = 0xE0

let fresh () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  Dos_ports.create ~mem ~video

let write t reg v = Dos_ports.port_out t status_port reg; Dos_ports.port_out t data_port v
let status t ~now = Dos_ports.set_now t now; Dos_ports.port_in t status_port
let at t now = Dos_ports.set_now t now

(* AdLib 공식 감지: 60h, 80h, 읽기, FFh·21h, 80µs 넘게 기다리기, 읽기 *)
let test_detect () =
  let t = fresh () in
  write t 0x04 0x60;
  write t 0x04 0x80;
  check "quiet status" (status t ~now:0) idle;
  check "quiet & E0h" (status t ~now:0 land detect_mask) 0x00;
  check "OPL2, not OPL3 (& 06h <> 0)" (if status t ~now:0 land 0x06 = 0 then 0 else 1) 1;
  write t 0x02 0xFF;
  write t 0x04 0x21;
  (* FFh 는 한 칸이다. 사이클 0 에서 시작했으니 382 사이클에 넘친다. *)
  check "FFh: not yet at 381" (status t ~now:(t1_step - 1)) idle;
  check "FFh: overflow at 382" (status t ~now:t1_step) t1_done;
  check "after 80us & E0h" (status t ~now:t1_step land detect_mask) 0xC0;
  write t 0x04 0x60;
  write t 0x04 0x80;
  check "reset" (status t ~now:t1_step) idle

let test_count () =
  (* 값 FCh 는 네 칸. 칸 경계 사이에서 시작하면 그 앞 경계부터 센다. *)
  let t = fresh () in
  write t 0x02 0xFC;
  at t (t1_step * 10 + 100);
  write t 0x04 0x01;
  let due = (t1_step * 10) + (4 * t1_step) in
  check "FCh: quiet before" (status t ~now:(due - 1)) idle;
  check "FCh: four steps" (status t ~now:due) t1_done;
  (* 값 00h 는 256 칸이다. *)
  let t = fresh () in
  write t 0x02 0x00;
  write t 0x04 0x01;
  check "00h: quiet at 255 steps" (status t ~now:(255 * t1_step)) idle;
  check "00h: 256 steps" (status t ~now:(256 * t1_step)) t1_done

let test_timer2 () =
  (* 타이머 2 는 320µs 칸이고 상태 bit5 를 세운다. 42h = 타이머 1 마스크,
     타이머 2 시작. *)
  let t = fresh () in
  write t 0x03 0xFC;
  write t 0x04 0x42;
  check "t2: quiet before" (status t ~now:((4 * t2_step) - 1)) idle;
  check "t2: bit5" (status t ~now:(4 * t2_step)) t2_done;
  (* 타이머 2 도 넘친 뒤 자기 주기로 다시 센다. *)
  at t (4 * t2_step);
  write t 0x04 0x80;
  check "t2: quiet before second" (status t ~now:((8 * t2_step) - 1)) idle;
  check "t2: second overflow" (status t ~now:(8 * t2_step)) t2_done;
  write t 0x02 0xFF;
  write t 0x04 0x03;                   (* 둘 다 시작, 마스크 없음 *)
  let t1_due = (8 * t2_step / t1_step * t1_step) + t1_step in
  check "both flags" (status t ~now:t1_due) both_done

let test_mask () =
  (* 41h: 타이머 1 마스크 + 시작. 넘쳐도 플래그가 서지 않는다. *)
  let t = fresh () in
  write t 0x02 0xFF;
  write t 0x04 0x41;
  check "masked: no flag" (status t ~now:(3 * t1_step)) idle;
  (* 01h 로 마스크를 풀면 돌던 타이머의 다음 넘침에서 선다. *)
  at t (3 * t1_step);
  write t 0x04 0x01;
  check "unmasked: still quiet before next" (status t ~now:((4 * t1_step) - 1)) idle;
  check "unmasked: next overflow" (status t ~now:(4 * t1_step)) t1_done;
  (* 마스크를 걸면 선 플래그도 지운다. *)
  at t (4 * t1_step);
  write t 0x04 0x41;
  check "masking clears the flag" (status t ~now:(4 * t1_step)) idle;
  (* 80h 는 플래그만 지운다. 마스크는 남아 다음 넘침도 가린다. *)
  write t 0x04 0x80;
  check "reset keeps the mask" (status t ~now:(10 * t1_step)) idle

let test_control () =
  let t = fresh () in
  write t 0x02 0xFE;                   (* 두 칸 *)
  write t 0x04 0x01;
  check "first overflow" (status t ~now:(2 * t1_step)) t1_done;
  (* 81h: bit7 이 서면 나머지 비트는 무시 — 플래그만 지우고 계속 돈다. *)
  at t (2 * t1_step);
  write t 0x04 0x81;
  check "reset clears" (status t ~now:(2 * t1_step)) idle;
  check "keeps counting: second overflow" (status t ~now:(4 * t1_step)) t1_done;
  (* 여러 번 넘친 뒤에도 다음 넘침은 주기 끝에 온다. *)
  at t (4 * t1_step);
  write t 0x04 0x80;
  check "reset again" (status t ~now:((9 * t1_step) + 5)) t1_done;
  at t ((9 * t1_step) + 5);
  write t 0x04 0x80;
  check "next period after catch-up" (status t ~now:((10 * t1_step) - 1)) idle;
  check "next period end" (status t ~now:(10 * t1_step)) t1_done;
  (* 시작 비트를 내리면 멈춘다. *)
  at t (10 * t1_step);
  write t 0x04 0x80;
  write t 0x04 0x00;
  check "stopped" (status t ~now:(100 * t1_step)) idle;
  (* 돌던 타이머는 시작 비트를 다시 써도 처음부터 세지 않는다. *)
  let t = fresh () in
  write t 0x02 0xFE;
  write t 0x04 0x01;
  at t t1_step;
  write t 0x04 0x01;
  check "restart keeps the running count" (status t ~now:(2 * t1_step)) t1_done;
  (* 멈춰도 플래그는 남고, 다시 시작하면 지운다. *)
  at t (2 * t1_step);
  write t 0x04 0x00;
  check "stop keeps the flag" (status t ~now:(2 * t1_step)) t1_done;
  write t 0x04 0x01;
  check "start clears the flag" (status t ~now:(2 * t1_step)) idle;
  (* 읽기 전에 멈춰도 그 전에 넘친 몫은 플래그로 남는다 — 넘침은 읽는
     때가 아니라 넘친 때 일어난다. *)
  let t = fresh () in
  write t 0x02 0xFF;
  write t 0x04 0x01;
  at t (t1_step + 100);
  write t 0x04 0x00;
  check "overflow before stop stays" (status t ~now:(t1_step + 100)) t1_done

let test_snapshot () =
  (* 마스크·다음 넘침·플래그가 스냅숏을 건넌다. *)
  let t = fresh () in
  write t 0x02 0xFE;
  write t 0x03 0xFF;
  write t 0x04 0x23;                   (* 둘 다 시작, 타이머 2 마스크 *)
  at t t1_step;
  let w = Dos_snap_codec.writer () in
  Dos_ports.write_state w t;
  let u = fresh () in
  Dos_ports.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) u;
  check "restored: timer 1 due, timer 2 masked" (status u ~now:(2 * t1_step)) t1_done;
  check "original agrees" (status t ~now:(2 * t1_step)) t1_done;
  (* 타이머 2 는 1527 에 넘치지만 마스크라 플래그가 없다. *)
  check "restored: timer 2 stays masked" (status u ~now:(2 * t2_step)) t1_done;
  (* 이미 선 플래그도 건넌다. *)
  let w = Dos_snap_codec.writer () in
  Dos_ports.write_state w t;
  let v = fresh () in
  Dos_ports.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) v;
  check "restored: raised flag" (status v ~now:(2 * t1_step)) t1_done

(* 게스트가 AdLib 공식 감지를 돈다. 기다림은 상태 포트를 40 번 읽는 것
   (1000 사이클 남짓, 약 210µs)이다. 옛 모델은 FFh 에 256 칸(약 20ms)을
   기다려 C0h 대신 00h 를 냈다. *)
let detect_com =
  let out reg v = "\xB0" ^ String.make 1 (Char.chr reg) ^ "\xEE\x42\xB0" ^ String.make 1 (Char.chr v) ^ "\xEE\x4A" in
  "\xBA\x88\x03"                       (* mov dx,388h *)
  ^ out 0x04 0x60 ^ out 0x04 0x80      (* mov al,reg; out dx,al; inc dx; mov al,v; out dx,al; dec dx *)
  ^ "\xEC\xA2\x00\x03"                 (* in al,dx; mov [0300h],al *)
  ^ out 0x02 0xFF ^ out 0x04 0x21
  ^ "\xB9\x28\x00\xEC\xE2\xFD"         (* mov cx,40; L: in al,dx; loop L *)
  ^ "\xEC\xA2\x01\x03"                 (* in al,dx; mov [0301h],al *)
  ^ out 0x04 0x60 ^ out 0x04 0x80
  ^ "\xB8\x00\x4C\xCD\x21"

let com_psp = 0x10000

let test_guest () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m detect_com;
  Dos_machine.run m ~max_steps:10_000;
  check "guest exited" (if Dos_machine.exited m then 1 else 0) 1;
  check "guest: step 3 & E0h" (Dos_machine.mem_read m (com_psp + 0x300) land detect_mask) 0x00;
  check "guest: step 7 & E0h" (Dos_machine.mem_read m (com_psp + 0x301) land detect_mask) 0xC0

let () =
  test_detect ();
  test_count ();
  test_timer2 ();
  test_mask ();
  test_control ();
  test_snapshot ();
  test_guest ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos OPL: all passed\n"
