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
}

let index_mask = 0x7F

(* 레지스터 A: 32.768kHz 시간축(010)과 1024Hz 주기(0110). *)
let default_rate = 0x26
let rate_mask = 0x7F
let update_in_progress = 0x80

(* 레지스터 B: 24시간, BCD. *)
let default_control = 0x02
let control_set = 0x80
let control_binary = 0x04
let control_24_hour = 0x02

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
let extended_memory_kb = 0

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

let create ~clock = { clock; index = 0; rate = default_rate; control = default_control }

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

(* RAM 표. 크기는 낮은 바이트가 앞이다. 10h~2Dh 에서 0 이 아닌 칸은 기본
   메모리뿐이다. *)
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
  else if reg = reg_c then 0
  else if reg = reg_d then valid_ram
  else if reg = reg_checksum_hi then high checksum
  else if reg = reg_checksum_lo then low checksum
  else table reg

let write c v =
  if c.index = reg_a then c.rate <- v land rate_mask
  else if c.index = reg_b then c.control <- low v

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

let index_r = C.range 0 index_mask
let rate_r = C.range 0 rate_mask

let write_state w { clock = _; index; rate; control } =
  C.put w ~what:"cmos index" index_r index;
  C.put w ~what:"cmos register A" rate_r rate;
  C.put w ~what:"cmos register B" C.byte control

let read_state r c =
  c.index <- C.get r ~what:"cmos index" index_r;
  c.rate <- C.get r ~what:"cmos register A" rate_r;
  c.control <- C.get r ~what:"cmos register B" C.byte
