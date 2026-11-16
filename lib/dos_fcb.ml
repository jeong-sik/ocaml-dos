(* FCB 필드 배치와 레코드 번호 — MS-DOS 4.0 CPMFCB.INC 의 배치,
   FCBIO2.ASM 의 GetExtended·GetExtent·SetExtent·GetRR·GetRecSize. *)

open Dos_state

(* 확장 FCB: FFh, 예약 5 바이트, 속성 1 바이트. 그 뒤가 보통 FCB 다. *)
let extended_mark = 0xFF
let extended_header = 7
let attribute = 6

let drive = 0x00
let name = 0x01
let extent = 0x0C
let record_size = 0x0E
let file_size = 0x10
let date = 0x14
let time = 0x16
let next_record = 0x20
let random_record = 0x21

(* 17h: 새 이름의 이름 8 + 확장자 3 칸 *)
let rename_target = 0x11

let default_record_size = 128
let records_per_extent = 128
let next_record_mask = 0x7F

(* GetRR: 레코드가 이 크기 이상이면 랜덤 레코드의 넷째 바이트를 버린다 *)
let wide_record_limit = 64

let locate t addr =
  if rd8 t addr = extended_mark then addr + extended_header else addr

(* TransFCB 의 SAttrib — 보통 FCB 는 0 *)
let search_attribute t addr =
  if rd8 t addr = extended_mark then rd8 t (addr + attribute) else 0

(* GetExtent: 블록 × 128 + 다음 레코드의 낮은 7 비트 *)
let current t fcb =
  (rd16 t (fcb + extent) * records_per_extent)
  + (rd8 t (fcb + next_record) land next_record_mask)

(* SetExtent: 레코드 번호를 다시 블록과 다음 레코드로 나눈다. 블록은 16 비트라
   wr16 이 위를 버린다. *)
let set_current t fcb r =
  wr8 t (fcb + next_record) (r land next_record_mask);
  wr16 t (fcb + extent) (r / records_per_extent)

(* GetRecSize: 0 이면 128 로 고쳐 쓰고 128 을 쓴다 *)
let record_size_of t fcb =
  match rd16 t (fcb + record_size) with
  | 0 ->
    wr16 t (fcb + record_size) default_record_size;
    default_record_size
  | n -> n

let random t fcb ~record_size =
  let low = rd16 t (fcb + random_record) lor (rd8 t (fcb + random_record + 2) lsl 16) in
  if record_size >= wide_record_limit then low
  else low lor (rd8 t (fcb + random_record + 3) lsl 24)

(* 랜덤 레코드를 쓴다. 넷째 바이트를 쓸지는 부르는 쪽마다 규칙이 다르다
   — 24h 는 레코드 크기로, 23h 는 그 바이트가 0 이 아닐 때만 쓴다. *)
let store_random t fcb r ~fourth =
  wr16 t (fcb + random_record) (r land 0xFFFF);
  wr8 t (fcb + random_record + 2) (r lsr 16);
  if fourth then wr8 t (fcb + random_record + 3) (r lsr 24)

let records_for ~record_size bytes = (bytes + record_size - 1) / record_size
