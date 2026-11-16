(* OPL2(YM3812) 상태 포트 — FMDRV.COM 의 감지 순서 그대로(게스트
   0x122b 루틴 실측). 소리는 없고, 감지 계약만 지킨다.

   계약: 정지 상태 읽기는 상위 3비트가 0. 타이머1 무장(reg2=0FFh,
   reg4=021h) 후 만료보다 긴 시간이 흐르면 읽기가 0C0h|플래그. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let video = Dos_video.create ~mem in
  let t = Dos_ports.create ~mem ~video in
  let write reg dat =
    Dos_ports.port_out t 0x388 reg;
    Dos_ports.port_out t 0x389 dat
  in
  let status () = Dos_ports.port_in t 0x388 in
  let cyc = ref 0 in
  let advance dt = cyc := !cyc + dt; Dos_ports.set_now t !cyc in
  (* FMDRV 감지: 마스크+리셋 → 정지 읽기 → 타이머1 무장+마스크 → 지연
     → 만료 읽기 → 다시 마스크+리셋 *)
  write 0x04 0x60;
  write 0x04 0x80;
  let quiet = status () in
  check "quiet high bits 0" (quiet land 0xE0) 0x00;
  write 0x02 0xFF;
  write 0x04 0x21;
  (* 만료(382×256 ≈ 97.8K 사이클)보다 긴 지연 — 감지 루틴의 루프에 해당 *)
  advance 900_000;
  let expired = status () in
  check "expired high bits 0xC0" (expired land 0xE0) 0xC0;
  check "timer1 flag set" (expired land 0x03) 0x01;
  check "timer2 flag clear" (expired land 0x02) 0x00;
  write 0x04 0x60;
  write 0x04 0x80;
  check "flag reset clears status" (status ()) 0x00;
  (* 미무장 채로 시간이 흘러도 상태는 그대로 *)
  advance 5_000_000;
  check "unarmed stays quiet" (status ()) 0x00;
  (* 타이머2: 320µs 스텝 — 만료 전엔 조용, 지나면 비트1 *)
  write 0x03 0x04;
  write 0x04 0x22;
  advance ((1527 * 5) / 2);
  check "t2 before expiry quiet" (status () land 0xE0) 0x00;
  advance ((1527 * 5) / 2 + 100);
  let t2 = status () in
  check "t2 expired" (t2 land 0xE0) 0xC0;
  check "timer2 flag set" (t2 land 0x03) 0x02;
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_string "dos OPL: all passed\n"
