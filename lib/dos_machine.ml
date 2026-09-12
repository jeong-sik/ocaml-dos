(* DOS 머신 M2a. Cpu86 + 1MB RAM + 텍스트 비디오(0xB8000) + INT 표면.
   INT 는 Cpu86 의 호스트 훅으로 받아 OCaml 에서 서빙한다 — 코어의
   스택 무변경 계약 그대로, 훅 안에서 레지스터와 VRAM 만 바꾼다. *)

type t = {
  mem : Bytes.t;                (** 1MB *)
  cpu : Cpu86.t;
  mutable cursor : int;         (** 텍스트 화면 오프셋(셀 단위) *)
  mutable exited : bool;
  mutable exit_code : int;
  keys : int Queue.t;           (** BIOS 스캔 코드 — 하네스가 넣는다 *)
}

let vram_base = 0xB8000
let cols = 80
let rows = 25

(* CGA 16색 — attribute 하위니블=전경, 상위니블=배경. *)
let cga_palette = [|
  (0x00, 0x00, 0x00); (0x00, 0x00, 0xAA); (0x00, 0xAA, 0x00); (0x00, 0xAA, 0xAA);
  (0xAA, 0x00, 0x00); (0xAA, 0x00, 0xAA); (0xAA, 0x55, 0x00); (0xAA, 0xAA, 0xAA);
  (0x55, 0x55, 0x55); (0x55, 0x55, 0xFF); (0x55, 0xFF, 0x55); (0x55, 0xFF, 0xFF);
  (0xFF, 0x55, 0x55); (0xFF, 0x55, 0xFF); (0xFF, 0xFF, 0x55); (0xFF, 0xFF, 0xFF) |]

let mem_read t a = Char.code (Bytes.get t.mem (a land 0xfffff))

let create () =
  let mem = Bytes.make (1024 * 1024) '\000' in
  let t_ref : t option ref = ref None in
  let read a = Char.code (Bytes.get mem (a land 0xfffff)) in
  let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff)) in
  let cpu =
    Cpu86.create ~read ~write
      ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> ())
  in
  let t =
    { mem; cpu; cursor = 0; exited = false; exit_code = 0; keys = Queue.create () }
  in
  t_ref := Some t;
  (* INT 표면: 벡터별 서빙. 훅은 코어가 호출 시점 레지스터를 이미 갖고
     있다 — AH 로 기능을 가린다. *)
  Cpu86.set_int_hook cpu (fun vec ->
      let ah = Cpu86.reg8 t.cpu 4 in
      match vec with
      | 0x10 ->
        (match ah with
         | 0x0E (* teletype 출력 — BL 하위니블이 전경색 *) ->
           let ch = Cpu86.reg8 t.cpu 0 in
           if ch = 0x0D then t.cursor <- (t.cursor / cols) * cols
           else if ch = 0x0A then t.cursor <- min (cols * rows) (t.cursor + cols)
           else begin
             let attr = Cpu86.reg8 t.cpu 3 land 0x0f in
             let cell = vram_base + (t.cursor * 2) in
             Bytes.set t.mem cell (Char.chr ch);
             Bytes.set t.mem (cell + 1) (Char.chr attr);
             t.cursor <- min (cols * rows - 1) (t.cursor + 1)
           end
         | 0x0F (* 모드 읽기: AL=모드 3, AH=80 *) ->
           Cpu86.set_reg8 t.cpu 0 3;
           Cpu86.set_reg8 t.cpu 4 cols
         | 0x00 (* 모드 세팅 — M2a 는 텍스트 3 번만 산다 *) -> ()
         | _ -> ())
      | 0x21 ->
        (match ah with
         | 0x4C (* 종료 *) ->
           t.exited <- true;
           t.exit_code <- Cpu86.reg8 t.cpu 0
         | 0x02 (* 문자 출력 — teletype 로 우회 *) ->
           Cpu86.set_reg8 t.cpu 4 0x0E;
           let ch = Cpu86.reg8 t.cpu 0 in
           Cpu86.set_reg8 t.cpu 0 ch;
           (* 재귀 대신 직접 서빙: 간단히 커서 진행만 *)
           if ch = 0x0D then t.cursor <- (t.cursor / cols) * cols
           else if ch = 0x0A then t.cursor <- min (cols * rows) (t.cursor + cols)
           else begin
             let cell = vram_base + (t.cursor * 2) in
             Bytes.set t.mem cell (Char.chr ch);
             Bytes.set t.mem (cell + 1) '\x07';
             t.cursor <- min (cols * rows - 1) (t.cursor + 1)
           end
         | _ -> ())
      | 0x16 ->
        (match ah with
         | 0x00 | 0x10 ->
           if Queue.is_empty t.keys then Cpu86.set_reg8 t.cpu 0 0
           else Cpu86.set_reg8 t.cpu 0 (Queue.pop t.keys)
         | 0x01 | 0x11 ->
           (* 키 있으면 ZF=0 + AL, 없으면 ZF=1 *)
           if Queue.is_empty t.keys then
             Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) lor Cpu86.f_zero)
           else begin
             Cpu86.set_flags t.cpu ((Cpu86.flags t.cpu) land lnot Cpu86.f_zero);
             Cpu86.set_reg8 t.cpu 0 (Queue.peek t.keys)
           end
         | _ -> ())
      | 0x20 (* PSP INT 20h — RET 로 돌아온 프로그램의 종료 *) ->
        t.exited <- true;
        t.exit_code <- 0
      | _ -> ());
  t

let load_com t image =
  Bytes.blit (Bytes.of_string image) 0 t.mem 0x100 (String.length image);
  (* PSP: INT 20h, 커맨드라인 길이 0. RET 복귀지 스택 top = 0x0000. *)
  Bytes.set t.mem 0x00 '\xcd';
  Bytes.set t.mem 0x01 '\x20';
  Bytes.set t.mem 0x80 '\x00';
  Cpu86.set_seg t.cpu 1 0;  (* CS *)
  Cpu86.set_seg t.cpu 3 0;  (* DS *)
  Cpu86.set_seg t.cpu 0 0;  (* ES *)
  Cpu86.set_seg t.cpu 2 0;  (* SS *)
  Cpu86.set_ip t.cpu 0x100;
  Cpu86.set_reg16 t.cpu 4 0xFFFE;  (* SP *)
  Bytes.set t.mem 0xFFFE '\x00';
  Bytes.set t.mem 0xFFFF '\x00';
  t.exited <- false;
  t.exit_code <- 0

let step t =
  if t.exited || Cpu86.halted t.cpu then 2 else Cpu86.step t.cpu

let run t ~max_steps =
  let n = ref 0 in
  while (not t.exited) && not (Cpu86.halted t.cpu) && !n < max_steps do
    ignore (step t);
    incr n
  done

let exited t = t.exited
let exit_code t = t.exit_code
let halted t = Cpu86.halted t.cpu

let screen_text t =
  let b = Buffer.create (cols * (rows + 1)) in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let ch = Char.code (Bytes.get t.mem (vram_base + ((r * cols + c) * 2))) in
      Buffer.add_char b (if ch >= 32 && ch < 127 then Char.chr ch else ' ')
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let frame_rgb t =
  (* 640x400: 각 셀 8x8 글리프 스케일업. 속성 바이트의 하위니블=전경,
     상위니블=배경. 폰트는 1비트/행 — MSB 가 왼쪽. *)
  let img = Bytes.make (640 * 400 * 3) '\000' in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let cell = vram_base + ((r * cols + c) * 2) in
      let ch = Char.code (Bytes.get t.mem cell) in
      let attr = Char.code (Bytes.get t.mem (cell + 1)) in
      let fr, fg, fb = cga_palette.(attr land 0x0f) in
      let br, bg, bb = cga_palette.((attr lsr 4) land 0x07) in
      let glyph = Font8x8.glyph ch in
      for gy = 0 to 7 do
        let bits = glyph.(gy) in
        let ybase = ((r * 8 + gy) * 640 + c * 8) * 3 in
        for gx = 0 to 7 do
          let on = bits land (0x80 lsr gx) <> 0 in
          let i = ybase + (gx * 3) in
          let rr, gg, bb2 = if on then (fr, fg, fb) else (br, bg, bb) in
          Bytes.set img i (Char.chr rr);
          Bytes.set img (i + 1) (Char.chr gg);
          Bytes.set img (i + 2) (Char.chr bb2)
        done
      done
    done
  done;
  Bytes.to_string img

let push_key t sc = Queue.push sc t.keys
