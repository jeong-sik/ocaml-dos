(* MC146818 실시간 시계와 CMOS RAM. 계약은 dos_cmos.mli.

   번지 배치는 IBM AT 를 따른다(Padgett Peterson, "CMOS memory map",
   bochs techspec/CMOS-reference.txt): 00h~09h 시각, 0Ah~0Dh 상태,
   15h·16h 기본 메모리, 17h·18h 와 30h·31h 확장 메모리, 2Eh·2Fh 는
   10h~2Dh 의 바이트 합(2Eh 가 윗자리), 32h 세기. *)

type wall_time = {
  year : int;
  month : int;
  day : int;
  weekday : int;
  hour : int;
  minute : int;
  second : int;
}

type t = {
  clock : unit -> wall_time;
  mutable index : int;
  mutable rate : int;  (** 레지스터 A 의 bit0~6 *)
  mutable control : int;  (** 레지스터 B *)
  mutable periodic_seen : bool;
      (** [periodic_since] 전에 이미 선 PF — 레지스터 A 를 바꾸기 전의 주기 *)
  mutable periodic_since : int;
      (** 이 사이클 뒤의 주기 끝만 PF 를 세운다. 레지스터 C 를 읽거나
          레지스터 A 를 쓴 때 *)
  mutable irq_line : bool;
      (** IRQ8 이 올라가 있다: IRQF 가 섰고 레지스터 C 를 아직 안 읽었다 *)
  mutable irq_at : int;
      (** IRQF 가 다음에 서는 사이클. 오를 일이 없으면 [no_irq] *)
}

let index_mask = 0x7F

(* 레지스터 A: 32.768kHz 시간축(010)과 1024Hz 주기(0110). *)
let default_rate = 0x26
let rate_mask = 0x7F
let update_in_progress = 0x80

(* 레지스터 A 의 DV2-DV0(bit6~4)이 010 일 때만 발진기가 돌고 나눔 사슬이
   센다. RS3-RS0(bit3~0)은 주기 인터럽트의 탭을 고른다. 0 은 탭이 없고,
   1·2 는 128·256 틱, 3 이상은 2^(RS-1) 틱이다(32.768kHz 한 틱).
   86Box src/nvr_at.c nvr_osc_running·timer_load_count. *)
let divider_mask = 0x70
let oscillator_on = 0x20
let rate_select_mask = 0x0F
let rtc_base_hz = 32768
let short_rate_shift = 6

(* 레지스터 B: 24시간, BCD. *)
let default_control = 0x02
let control_set = 0x80
let control_periodic = 0x40
let control_binary = 0x04
let control_24_hour = 0x02

(* 레지스터 C: bit7 IRQF(켜 둔 인터럽트의 깃발이 섰다), bit6 PF(주기). *)
let flag_irq = 0x80
let flag_periodic = 0x40

(* 레지스터 D bit7: 전지가 살아 있고 RAM 이 유효하다. *)
let valid_ram = 0x80

(* 12시간 꼴에서 시 레지스터의 bit7 은 오후다. *)
let pm = 0x80
let hours_per_half_day = 12

(* 86Box src/nvr_at.c: UIP 를 켜고 244µs 뒤에 시각을 바꾸고, 다시
   1984µs 뒤에 UIP 를 끈다. *)
let uip_before_us = 244
let uip_after_us = 1984
let us_per_second = 1_000_000

let base_memory_kb = 640
(* 1MB 위 확장 메모리. POST 가 센 크기를 17h·18h(사본 30h·31h)에 둔다. XMS
   드라이버가 나눠 주는 15MB(Dos_xms)와 같은 메모리다. *)
let extended_memory_kb = Dos_xms.capacity_kb

let reg_seconds = 0x00
let reg_minutes = 0x02
let reg_hours = 0x04
let reg_weekday = 0x06
let reg_day = 0x07
let reg_month = 0x08
let reg_year = 0x09
let reg_a = 0x0A
let reg_b = 0x0B
let reg_c = 0x0C
let reg_d = 0x0D
let reg_base_memory = 0x15
let reg_extended_memory = 0x17
let reg_extended_memory_copy = 0x30
let reg_checksum_hi = 0x2E
let reg_checksum_lo = 0x2F
let reg_century = 0x32
let checksum_first = 0x10
let checksum_last = 0x2D

let no_irq = max_int

let create ~clock =
  { clock; index = 0; rate = default_rate; control = default_control;
    periodic_seen = false; periodic_since = 0; irq_line = false; irq_at = no_irq }

let select c v = c.index <- v land index_mask
let index c = c.index

let bcd v = ((v / 10) lsl 4) lor (v mod 10)

(* 시각과 날짜의 꼴은 레지스터 B 가 정한다. 세기 바이트는 BIOS 가 BCD 로
   쓰는 RAM 이라 늘 BCD 다. *)
let encode c v = if c.control land control_binary <> 0 then v else bcd v

let hour c h =
  if c.control land control_24_hour <> 0 then encode c h
  else
    let h12 = match h mod hours_per_half_day with 0 -> hours_per_half_day | x -> x in
    encode c h12 lor if h >= hours_per_half_day then pm else 0

let low v = v land 0xFF
let high v = (v lsr 8) land 0xFF

(* RAM 표. 크기는 낮은 바이트가 앞이다. 10h~2Dh 에서 0 이 아닌 칸은 기본·
   확장 메모리뿐이다. *)
let table reg =
  if reg = reg_base_memory then low base_memory_kb
  else if reg = reg_base_memory + 1 then high base_memory_kb
  else if reg = reg_extended_memory || reg = reg_extended_memory_copy then low extended_memory_kb
  else if reg = reg_extended_memory + 1 || reg = reg_extended_memory_copy + 1 then
    high extended_memory_kb
  else 0

let checksum =
  let sum = ref 0 in
  for reg = checksum_first to checksum_last do
    sum := !sum + table reg
  done;
  !sum

let updating c ~now ~cpu_hz =
  c.control land control_set = 0
  &&
  let into_second = now mod cpu_hz in
  let cycles us = us * cpu_hz / us_per_second in
  into_second < cycles uip_after_us || into_second >= cpu_hz - cycles uip_before_us

(* 고른 탭의 한 주기, 32.768kHz 틱 수. 발진기가 멈췄거나 RS 가 0 이면
   없다. *)
let periodic_ticks rate =
  if rate land divider_mask <> oscillator_on then None
  else
    match rate land rate_select_mask with
    | 0 -> None
    | (1 | 2) as rs -> Some (1 lsl (rs + short_rate_shift))
    | rs -> Some (1 lsl (rs - 1))

(* 나눔 사슬은 기계가 켜질 때(사이클 0) 센 것으로 친다. 그래서 주기의 끝은
   사이클 0 에서 한 주기씩 떨어져 있고, 탭을 바꿔도 사슬의 위상은 그대로다.
   [period_index] 가 늘면 그 사이에 주기 끝이 있었다. *)
let period_index ~ticks ~cpu_hz cycle = cycle * rtc_base_hz / (ticks * cpu_hz)

(* [periodic_since] 뒤 첫 주기 끝의 사이클: [period_index] 가 하나 느는
   가장 이른 사이클. *)
let next_end ~ticks ~cpu_hz since =
  let k = period_index ~ticks ~cpu_hz since + 1 in
  ((k * ticks * cpu_hz) + rtc_base_hz - 1) / rtc_base_hz

(* IRQ8 은 IRQF 가 0 에서 1 로 바뀔 때 오른다(86Box timer_update_irq:
   B 와 C 의 PIE·PF 가 겹치면 IRQF 를 세우고 줄을 올린다). 줄이 올라가
   있는 동안은 주기가 더 끝나도 새로 오르지 않는다. 그 사이의 끝은 PF 로만
   남는다. *)
let schedule c ~cpu_hz =
  c.irq_at <-
    (if c.irq_line || c.control land control_periodic = 0 then no_irq
     else if c.periodic_seen then c.periodic_since
     else
       match periodic_ticks c.rate with
       | None -> no_irq
       | Some ticks -> next_end ~ticks ~cpu_hz c.periodic_since)

let periodic_flag c ~now ~cpu_hz =
  c.periodic_seen
  ||
  match periodic_ticks c.rate with
  | None -> false
  | Some ticks ->
    period_index ~ticks ~cpu_hz now > period_index ~ticks ~cpu_hz c.periodic_since

(* 레지스터 C 를 읽으면 깃발이 모두 지워진다(86Box nvr_reg_read). AF·UF 는
   만들지 않아 늘 0 이다. *)
let read_flags c ~now ~cpu_hz =
  let pf = periodic_flag c ~now ~cpu_hz in
  let irqf = pf && c.control land control_periodic <> 0 in
  c.periodic_seen <- false;
  c.periodic_since <- now;
  c.irq_line <- false;
  schedule c ~cpu_hz;
  (if irqf then flag_irq else 0) lor if pf then flag_periodic else 0

let read c ~now ~cpu_hz =
  let reg = c.index in
  let wall () = c.clock () in
  if reg = reg_seconds then encode c (wall ()).second
  else if reg = reg_minutes then encode c (wall ()).minute
  else if reg = reg_hours then hour c (wall ()).hour
  else if reg = reg_weekday then encode c ((wall ()).weekday + 1)
  else if reg = reg_day then encode c (wall ()).day
  else if reg = reg_month then encode c (wall ()).month
  else if reg = reg_year then encode c ((wall ()).year mod 100)
  else if reg = reg_century then bcd ((wall ()).year / 100)
  else if reg = reg_a then
    c.rate lor if updating c ~now ~cpu_hz then update_in_progress else 0
  else if reg = reg_b then c.control
  else if reg = reg_c then read_flags c ~now ~cpu_hz
  else if reg = reg_d then valid_ram
  else if reg = reg_checksum_hi then high checksum
  else if reg = reg_checksum_lo then low checksum
  else table reg

let write c v ~now ~cpu_hz =
  if c.index = reg_a then begin
    (* 옛 탭이 이미 세운 PF 는 남는다. 새 탭은 지금부터 센다. *)
    c.periodic_seen <- periodic_flag c ~now ~cpu_hz;
    c.periodic_since <- now;
    c.rate <- v land rate_mask;
    schedule c ~cpu_hz
  end
  else if c.index = reg_b then begin
    c.control <- low v;
    (* PIE 를 끄면 IRQF 가 내려가 줄도 내려간다. *)
    if c.control land control_periodic = 0 then c.irq_line <- false;
    schedule c ~cpu_hz
  end

(* 기계가 매 스텝 부른다. 거의 늘 [now < irq_at] 라 비교 하나로 끝난다. *)
let[@inline] irq_rises c ~now =
  if now >= c.irq_at then begin
    c.irq_line <- true;
    c.irq_at <- no_irq;
    true
  end
  else false

let irq_line c = c.irq_line

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

let index_r = C.range 0 index_mask
let rate_r = C.range 0 rate_mask

let write_state w
    { clock = _; index; rate; control; periodic_seen; periodic_since; irq_line; irq_at } =
  C.put w ~what:"cmos index" index_r index;
  C.put w ~what:"cmos register A" rate_r rate;
  C.put w ~what:"cmos register B" C.byte control;
  C.put_bool w periodic_seen;
  C.put w ~what:"cmos periodic since" C.count periodic_since;
  C.put_bool w irq_line;
  C.put w ~what:"cmos next IRQ8" C.count irq_at

let read_state r c =
  c.index <- C.get r ~what:"cmos index" index_r;
  c.rate <- C.get r ~what:"cmos register A" rate_r;
  c.control <- C.get r ~what:"cmos register B" C.byte;
  c.periodic_seen <- C.get_bool r;
  c.periodic_since <- C.get r ~what:"cmos periodic since" C.count;
  c.irq_line <- C.get_bool r;
  c.irq_at <- C.get r ~what:"cmos next IRQ8" C.count
