(* 포트 0x61 bit4(리프레시) — PIT 채널 1 의 리프레시 요청마다, 곧 PIT 18 틱
   = 15.0857µs 마다 뒤집힌다(PCTIM003 7.37 절). 4.77MHz 에서 72 사이클이다. *)

let failed = ref 0

let check_int name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let refresh_port = 0x61
let refresh_bit = 0x10
let toggle_cycles = 72

let test_port () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  let t = Dos_ports.create ~mem ~video in
  let bit now = Dos_ports.set_now t now; Dos_ports.port_in t refresh_port land refresh_bit in
  List.iter (fun (now, want) -> check_int (Printf.sprintf "bit4 at %d cycles" now) (bit now) want)
    [ (0, 0); (toggle_cycles - 1, 0); (toggle_cycles, refresh_bit);
      ((2 * toggle_cycles) - 1, refresh_bit); (2 * toggle_cycles, 0) ]

(* 게스트가 틱이 바뀐 직후부터 다음 틱까지 bit4 가 바뀐 횟수를 센다.
   한 틱 = 65536 × 4 = 262144 사이클이라 262144 / 72 = 3640 번 남짓이다.
   한 바퀴는 66 사이클 이하라 72 사이클 안에 꼭 한 번 읽는다. 다음 틱의
   IRQ0 처리 동안은 읽지 못해 몇 번 덜 센다. 옛 158 사이클이면 1659 번이다. *)
let counter_com =
  "\xFB"                               (* sti *)
  ^ "\x31\xC0\x8E\xC0"                 (* xor ax,ax; mov es,ax *)
  ^ "\x26\x8B\x1E\x6C\x04"             (* mov bx,[es:046Ch] *)
  ^ "\x26\x3B\x1E\x6C\x04\x74\xF9"     (* W: cmp bx,[es:046Ch]; je W *)
  ^ "\x26\x8B\x1E\x6C\x04"             (* mov bx,[es:046Ch] — 새 틱 *)
  ^ "\x31\xC9"                         (* xor cx,cx *)
  ^ "\xE4\x61\x24\x10\x88\xC4"         (* in al,61h; and al,10h; mov ah,al *)
  (* L: *)
  ^ "\xE4\x61\x24\x10"                 (* in al,61h; and al,10h *)
  ^ "\x38\xE0\x74\x03"                 (* cmp al,ah; je S *)
  ^ "\x41\x88\xC4"                     (* inc cx; mov ah,al *)
  (* S: *)
  ^ "\x26\x3B\x1E\x6C\x04\x74\xEE"     (* cmp bx,[es:046Ch]; je L *)
  ^ "\x89\x0E\x00\x03"                 (* mov [0300h],cx *)
  ^ "\xB8\x00\x4C\xCD\x21"             (* mov ax,4C00h; int 21h *)

let com_psp = 0x10000
let toggles_per_tick = (65536 * 4) / toggle_cycles
(* IRQ0 처리 동안 못 읽어 덜 세는 몫의 상한 *)
let missed_during_irq0 = 8

let test_guest () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m counter_com;
  Dos_machine.run m ~max_steps:2_000_000;
  check_int "counter exited" (if Dos_machine.exited m then 1 else 0) 1;
  let rd a = Dos_machine.mem_read m a in
  let n = rd (com_psp + 0x300) lor (rd (com_psp + 0x301) lsl 8) in
  if n > toggles_per_tick + 1 || n < toggles_per_tick - missed_during_irq0 then begin
    incr failed;
    Printf.eprintf "FAIL toggles in one tick: got %d want %d..%d\n%!" n
      (toggles_per_tick - missed_during_irq0) (toggles_per_tick + 1)
  end

let () =
  test_port ();
  test_guest ();
  if !failed = 0 then print_endline "dos refresh: all passed"
  else begin
    Printf.eprintf "dos refresh: %d failures\n%!" !failed;
    exit 1
  end
