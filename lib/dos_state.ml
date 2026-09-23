(* 기계의 상태와 그 상태를 만지는 기본기. INT 표면(Dos_bios, Dos_dos)과
   배선(Dos_machine)이 이 타입을 함께 쓴다.

   결정론: 시계도 난수도 호스트에서 오지 않는다. 날짜·시각은 고정된
   기준시각에 CPU 누적 사이클로 환산한 경과를 더해 만든다. 같은 이미지에
   같은 키를 같은 순서로 넣으면 같은 화면이 나온다. *)

let vram_base = 0xB8000
let rows = 25

(* 4.77MHz — IBM PC 의 CPU 클럭. 사이클을 초로 바꾸는 유일한 환산비다. *)
let cpu_hz = 4_772_727

type handle = {
  hname : string;
  mutable data : Bytes.t;
  mutable pos : int;
}

type mouse = {
  mutable mouse_present : bool;
  mutable mouse_x : int;
  mutable mouse_y : int;
  mutable mouse_buttons : int;
  mutable mouse_visible : bool;
  mutable mouse_dx : int;
  mutable mouse_dy : int;
}

(* ---------- AH=4Bh EXEC 의 부모 프레임 ----------

   자식을 띄울 때 부모의 CPU·프로세스 상태를 통째로 찍어둔다. 레지스터
   세그먼트 IP 플래그를 값으로 저장한다 — Cpu86.t 를 통째로 복제하지
   않는 이유는 사이클 누적 같은 기계 전체의 상태가 부모의 것이어야
   하기 때문이다. 스냅숏의 IP 는 이미 INT 21h 명령 다음을 가리킨다
   (fetch 가 IP 를 전진시킨 뒤 훅이 돈다). 그 지점이 부모의 재개점이다. *)
type cpu_snapshot = {
  snap_regs : int array;                  (** 8 × 16비트 범용 *)
  snap_segs : int array;                  (** 4 × 세그먼트 *)
  snap_ip : int;
  snap_flags : int;                       (* Cpu86.flags 합성 값 *)
}

type exec_frame = {
  parent : cpu_snapshot;
  parent_psp : int;
  parent_dta : int;
  parent_free_base : int;
  parent_free_top : int;
  parent_blocks : (int * int) list;
  parent_handles : (int * handle) list;   (** 자식 종료 시 이 목록 밖의 핸들을 닫는다 *)
}

type t = {
  mem : Bytes.t;                              (** 1MB *)
  cpu : Cpu86.t;
  ports : Dos_ports.t;
  mutable exited : bool;
  mutable exit_code : int;
  video : Dos_video.t;
  host_files : (string, Bytes.t) Hashtbl.t;   (** 하네스가 마운트한 파일 *)
  handles : (int, handle) Hashtbl.t;
  mutable next_handle : int;
  fcbs : (int, Bytes.t * int) Hashtbl.t;
  mutable dta : int;                          (** 전송 주소(물리) *)
  mutable psp_seg : int;
  mutable kbd_wait : bool;                    (** 키를 기다리다 굶었다 *)
  mutable ext_scan_pending : int;             (** INT 21h 바이트 읽기용 확장키 스캔 대기 *)
  mutable kbd_requests : int;                 (** 빈 링을 만난 횟수 *)
  mutable last_tick : int;                    (** 마지막 IRQ0 의 사이클 *)
  mutable pending_irq0 : bool;                (** IF 가 꺼져 못 넣은 틱 *)
  mutable free_base : int;                    (** 할당 가능 첫 세그먼트 *)
  mutable free_top : int;                     (** 할당 가능 상한 세그먼트 *)
  mutable blocks : (int * int) list;          (** (세그먼트, paras) *)
  mutable find_queue : string list;           (** findnext 가 남긴 이름 *)
  mutable stubs : (int * int) list;           (** (벡터, ROM 스텁 오프셋) *)
  mutable exec_frames : exec_frame list;      (** EXEC 로 띄운 자식의 부모 프레임 *)
  mutable last_child_code : int;              (** AH=4Dh 가 돌려줄 마지막 자식 코드 *)
  (* 기준 시각 — 여기에 경과를 더해 날짜·시각을 만든다 *)
  mutable epoch_year : int;
  mutable epoch_month : int;
  mutable epoch_day : int;
  mutable epoch_hour : int;
  mutable epoch_min : int;
  mutable epoch_sec : int;
  (* EMS 4.0 스텁(Dos_ems)의 상태 *)
  mutable ems_next_handle : int;
  ems_pages : (int, Bytes.t array) Hashtbl.t;
        (** 핸들 → 논리 페이지 배열(각 16KB) *)
  mutable ems_mapped : (int * int) array;
        (** 물리 페이지 0-3 → 지금 겹쳐진 (핸들, 논리 페이지) *)
  mouse : mouse;
}

(* ---------- 메모리 ---------- *)

(* 화면의 열 수는 모드가 정한다 — 40 열 화면을 80 으로 계산하면 커서와
   스크롤이 한 줄씩 어긋난다. *)
let cols t = max 1 (Dos_video.text_cols t.video)

let rd8 t a = Char.code (Bytes.get t.mem (a land 0xfffff))
let wr8 t a v = Bytes.set t.mem (a land 0xfffff) (Char.chr (v land 0xff))
let rd16 t a = rd8 t a lor (rd8 t (a + 1) lsl 8)
let wr16 t a v = wr8 t a v; wr8 t (a + 1) (v lsr 8)

let seg_off t sreg reg = ((Cpu86.seg t.cpu sreg lsl 4) + Cpu86.reg16 t.cpu reg)

(* DS:DX 의 ASCIIZ — INT 21h 파일명 계열이 쓴다. *)
let asciiz_at t base =
  let b = Buffer.create 16 in
  let i = ref 0 in
  let stop = ref false in
  while (not !stop) && !i < 128 do
    let c = rd8 t (base + !i) in
    if c = 0 then stop := true
    else begin Buffer.add_char b (Char.chr c); incr i end
  done;
  Buffer.contents b

(* ---------- 플래그 도우미 ---------- *)

(* 플래그로 결과를 돌려주는 서비스(INT 21h 의 CF, INT 16h AH=01h 의
   ZF)는 현재 플래그를 고치는 것으로 충분하다 — IVT 가 아직 우리
   스텁이면 훅이 서비스를 곧장 부르고(프레임 push 없음) iret 도 없
   다. 스택의 프레임을 손대면 게스트의 push 값이 오염된다(삼국지3
   MAIN 의 push cs 값 11ad 가 11ac 로 깎힌 실측 — CF 비트 클리어로).
   게스트가 벡터를 훅해 체인하는 경로가 생기면 그때 프레임 워드
   수정이 필요해진다. *)
let set_cf t on =
  let f = Cpu86.flags t.cpu in
  Cpu86.set_flags t.cpu
    (if on then f lor Cpu86.f_carry else f land lnot Cpu86.f_carry)

let ok t = set_cf t false

let fail t code =
  Cpu86.set_reg16 t.cpu 0 code;
  set_cf t true

(* ---------- 시계 ---------- *)

let days_in_month y m =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | _ -> if (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 then 29 else 28

let elapsed_seconds t = Cpu86.cycles t.cpu / cpu_hz

(* 기준 시각 + 경과. 윤년까지 세되 달력 라이브러리는 쓰지 않는다 —
   게스트가 보는 건 DOS 의 날짜 필드뿐이다. *)
let now_fields t =
  let total = (t.epoch_hour * 3600) + (t.epoch_min * 60) + t.epoch_sec
              + elapsed_seconds t in
  let days = total / 86400 in
  let rest = total mod 86400 in
  let y = ref t.epoch_year and m = ref t.epoch_month and d = ref t.epoch_day in
  for _ = 1 to days do
    incr d;
    if !d > days_in_month !y !m then begin
      d := 1; incr m;
      if !m > 12 then begin m := 1; incr y end
    end
  done;
  (!y, !m, !d, rest / 3600, rest mod 3600 / 60, rest mod 60)

(* 1980-01-01 부터의 요일 계산용 — DOS AH=2Ah 가 AL 에 요일을 준다.
   1980-01-01 은 화요일이므로 2 에서 시작한다. *)
let day_of_week (y, m, d) =
  let days = ref 0 in
  for yy = 1980 to y - 1 do
    days := !days + if (yy mod 4 = 0 && yy mod 100 <> 0) || yy mod 400 = 0
                    then 366 else 365
  done;
  for mm = 1 to m - 1 do days := !days + days_in_month y mm done;
  days := !days + d - 1;
  (!days + 2) mod 7

(* ---------- 커서와 화면 ---------- *)

(* 커서의 진실 원천은 BDA 0x450/0x451 이다(실기 BIOS 와 같다). CRTC
   레지스터도 같은 값으로 맞춰 둔다 — 두 벌로 갈라지면 게스트가 직접
   CRTC 를 읽을 때 딴 자리를 본다. *)
let get_cursor t = (rd8 t 0x450, rd8 t 0x451)   (* (열, 행) *)

let set_cursor t col row =
  let w = cols t in
  let col = max 0 (min (w - 1) col) and row = max 0 (min (rows - 1) row) in
  wr8 t 0x450 col;
  wr8 t 0x451 row;
  let linear = (row * w) + col in
  Dos_ports.port_out t.ports 0x3D4 0x0E;
  Dos_ports.port_out t.ports 0x3D5 (linear lsr 8);
  Dos_ports.port_out t.ports 0x3D4 0x0F;
  Dos_ports.port_out t.ports 0x3D5 (linear land 0xff)

let cell_addr t r c = vram_base + (((r * cols t) + c) * 2)

let read_cell t r c = rd16 t (cell_addr t r c)
let write_cell t r c v = wr16 t (cell_addr t r c) v

(* 창 스크롤 — INT 10h AH=06/07 과 teletype 의 줄 넘침이 함께 쓴다.
   [lines]=0 은 창 전체를 지운다. *)
let scroll_window t ~up ~lines ~top ~left ~bottom ~right ~attr =
  let blank = (attr lsl 8) lor 0x20 in
  let lines = if lines = 0 then bottom - top + 1 else lines in
  if up then
    for r = top to bottom do
      for c = left to right do
        let src = r + lines in
        write_cell t r c (if src > bottom then blank else read_cell t src c)
      done
    done
  else
    for r = bottom downto top do
      for c = left to right do
        let src = r - lines in
        write_cell t r c (if src < top then blank else read_cell t src c)
      done
    done

(* 글자 하나 찍기 — INT 10h teletype 과 INT 21h 출력이 공유한다.
   마지막 줄을 넘으면 화면을 한 줄 올린다. 넘침을 클램프로 막으면
   글자가 마지막 칸에 계속 덮어써져 출력이 통째로 사라진다. *)
let put_char t ch attr =
  let col, row = get_cursor t in
  let col = ref col and row = ref row in
  (match ch with
   | 0x0D -> col := 0
   | 0x0A -> incr row
   | 0x08 -> if !col > 0 then decr col
   | 0x09 -> col := min (cols t - 1) ((!col + 8) / 8 * 8)
   | 0x07 -> ()                       (* 벨 — 소리는 내지 않는다 *)
   | _ ->
     write_cell t !row !col ((attr lsl 8) lor (ch land 0xff));
     incr col;
     if !col >= cols t then begin col := 0; incr row end);
  if !row >= rows then begin
    scroll_window t ~up:true ~lines:1 ~top:0 ~left:0 ~bottom:(rows - 1)
      ~right:(cols t - 1) ~attr;
    row := rows - 1
  end;
  set_cursor t !col !row

let put_string t s attr = String.iter (fun c -> put_char t (Char.code c) attr) s

(* ---------- BIOS 키 링 ---------- *)

(* 0x40:0x1E-0x3D 의 16워드 링이 키의 단일 진실 원천이다. 호스트 큐를
   따로 두면 게스트가 링을 직접 다룰 때 두 사본이 갈라진다(실측: AH=00
   이 호스트 큐만 비워 ZZT 가 AH=01 폴링을 영원히 돌았다).
   게스트는 포인터를 직접 쓸 수 있다(키 버퍼 비우기) — 범위 밖 값은
   '빈 버퍼' 로 읽는다. *)
let ring_lo = 0x1E
let ring_hi = 0x3C

let clamp_ptr v = if v < ring_lo || v > ring_hi then ring_lo else v

let bda_ring_head = 0x41A
let bda_ring_tail = 0x41C
let ring_head t = clamp_ptr (rd16 t bda_ring_head)
let ring_tail t = clamp_ptr (rd16 t bda_ring_tail)
let key_pending t = ring_head t <> ring_tail t

let peek_key t = rd16 t (0x400 + ring_head t)

let pop_key t =
  let h = ring_head t in
  let w = rd16 t (0x400 + h) in
  wr16 t 0x41A (if h >= ring_hi then ring_lo else h + 2);
  w

let push_key t word =
  let head = ring_head t and tail = ring_tail t in
  let nxt = if tail >= ring_hi then ring_lo else tail + 2 in
  if nxt <> head then begin
    wr16 t (0x400 + tail) word;
    wr16 t 0x41C nxt;
    Dos_ports.set_scancode t.ports ((word lsr 8) land 0xff)
  end

(* ---------- 인터럽트 전달 ---------- *)

(* 게스트 IVT 핸들러에 진짜 인터럽트 프레임(flags/cs/ip)을 밀어 넣는다.
   false = 그 벡터는 게스트가 안 걸었다(호스트가 서빙할 몫).
   실기 순서: 현재 플래그를 그대로 밀고 '그 다음에' IF·TF 를 끈다.
   끄지 않으면 핸들러 도중에 다음 틱이 들어와 프레임이 쌓인다. IRET 이
   밀어둔 값을 되돌리므로 IF 는 핸들러가 끝나면 살아난다. *)
let deliver_ivt t v =
  let off = rd16 t (v * 4) and sg = rd16 t ((v * 4) + 2) in
  if off <> 0 || sg <> 0 then begin
    let flags = Cpu86.flags t.cpu in
    let push w =
      Cpu86.set_reg16 t.cpu 4 (Cpu86.reg16 t.cpu 4 - 2);
      let base = Cpu86.seg t.cpu 2 lsl 4 and o = Cpu86.reg16 t.cpu 4 in
      wr8 t (base + o) (w land 0xff);
      wr8 t (base + ((o + 1) land 0xffff)) (w lsr 8)
    in
    push flags;
    push (Cpu86.seg t.cpu 1);
    push (Cpu86.dump_ip t.cpu);
    Cpu86.set_flags t.cpu
      (flags land lnot Cpu86.f_interrupt land lnot Cpu86.f_trap);
    Cpu86.set_seg t.cpu 1 sg;
    Cpu86.set_ip t.cpu off;
    true
  end
  else false

(* 게스트가 키를 물었는데 링이 비어 있었다. 래치는 읽기가 성공해야
   내려가므로 "지금 기다리는가" 만 알려주고 "몇 번 물었는가" 는 못
   알려준다 — 한 구간 안에서 물었다가 받아간 것을 보려면 계수가 있어야
   한다. *)
let starve t =
  t.kbd_wait <- true;
  t.kbd_requests <- t.kbd_requests + 1

let interrupts_enabled t = Cpu86.flags t.cpu land Cpu86.f_interrupt <> 0
