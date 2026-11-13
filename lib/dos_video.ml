(* 비디오 어댑터. EGA/VGA 16색 모드의 평면 넷과 그래픽 컨트롤러가 여기
   산다 — 같은 주소에 평면이 겹쳐 있어 1MB 배열 안에 둘 수 없다. *)

type kind = Text | Cga4 | Cga2 | Planar | Linear256

let plane_size = 0x10000
let plane_count = 4
let planar_base = 0xA0000
let planar_top = planar_base + plane_size
let cga_base = 0xB8000

(* 그래픽 컨트롤러 레지스터 번호 *)
let gc_set_reset = 0
let gc_enable_sr = 1
let gc_color_compare = 2
let gc_data_rotate = 3
let gc_read_map = 4
let gc_mode = 5
(* 6 번(Misc)은 저장만 하고 읽지 않는다. 거기 있는 메모리 창 선택
   비트로 0xA0000 창을 옮길 수 있지만, 우리는 창의 주인을 모드에서
   정한다 — BIOS 로 모드를 세운 프로그램은 이 둘이 늘 일치한다. *)
let gc_color_dont_care = 7
let gc_bit_mask = 8
let gc_count = 9

(* 시퀀서: 2 번이 평면 쓰기 마스크, 4 번이 메모리 모드 *)
let seq_map_mask = 2
let seq_memory_mode = 4
let seq_count = 5

(* 메모리 모드의 chain-4 비트. 켜져 있으면 주소의 아래 두 비트가 평면을
   고른다(13h). 끄면 256색 모드도 맵 마스크로 평면을 고른다(Mode X). *)
let chain4 = 0x08

(* 모드를 세운 뒤의 메모리 모드 값. SeaBIOS vgasrc/stdvgamodes.c 의 sequ_*
   네 번째 바이트다. *)
let memory_mode_of = function
  | Text | Cga4 -> 0x02
  | Cga2 | Planar -> 0x06
  | Linear256 -> 0x0E

(* CRTC 레지스터 번호 *)
let crtc_count = 25
let crtc_h_display_end = 0x01
let crtc_overflow = 0x07
let crtc_max_scan_line = 0x09
let crtc_start_high = 0x0C
let crtc_start_low = 0x0D
let crtc_v_retrace_end = 0x11
let crtc_v_display_end = 0x12
let crtc_offset = 0x13
let crtc_underline = 0x14
let crtc_mode_control = 0x17

(* 0x11 bit7 이 서 있으면 0-7 번은 쓰기가 막힌다. 7 번(overflow)의 bit4
   (line compare 의 9번째 비트)만 예외다. *)
let crtc_protect = 0x80
let crtc_last_protected = 0x07
let overflow_line_compare = 0x10

(* 크기와 주소를 정하는 비트 *)
let overflow_vde8_shift = 1          (* 07h bit1 = 표시 끝 bit8 *)
let overflow_vde9_shift = 6          (* 07h bit6 = 표시 끝 bit9 *)
let max_scan_rows = 0x1f             (* 09h: 행 하나의 줄 수 - 1 *)
let max_scan_double = 0x80           (* 09h: 두 배 주사 *)
let underline_dword = 0x40           (* 14h: dword 주소 *)
let mode_byte = 0x40                 (* 17h: 1 = byte, 0 = word *)
let mode_wrap_ma15 = 0x20            (* 17h: word 모드에서 MA15 를 0번 비트로 *)

(* 그래픽 모드의 CRTC 부팅값. SeaBIOS vgasrc/stdvgamodes.c 의 crtc_0d,
   crtc_0e, crtc_0f(0Fh·10h), crtc_11(11h·12h), crtc_13 이다. 이 값에서
   나오는 크기가 모드표의 크기와 같다. *)
let crtc_mode0d =
  [| 0x2D; 0x27; 0x28; 0x90; 0x2B; 0x80; 0xBF; 0x1F;
     0x00; 0xC0; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
     0x9C; 0x8E; 0x8F; 0x14; 0x00; 0x96; 0xB9; 0xE3;
     0xFF |]

let crtc_mode0e =
  [| 0x5F; 0x4F; 0x50; 0x82; 0x54; 0x80; 0xBF; 0x1F;
     0x00; 0xC0; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
     0x9C; 0x8E; 0x8F; 0x28; 0x00; 0x96; 0xB9; 0xE3;
     0xFF |]

let crtc_mode0f =
  [| 0x5F; 0x4F; 0x50; 0x82; 0x54; 0x80; 0xBF; 0x1F;
     0x00; 0x40; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
     0x83; 0x85; 0x5D; 0x28; 0x0F; 0x63; 0xBA; 0xE3;
     0xFF |]

let crtc_mode11 =
  [| 0x5F; 0x4F; 0x50; 0x82; 0x54; 0x80; 0x0B; 0x3E;
     0x00; 0x40; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
     0xEA; 0x8C; 0xDF; 0x28; 0x00; 0xE7; 0x04; 0xE3;
     0xFF |]

let crtc_mode13 =
  [| 0x5F; 0x4F; 0x50; 0x82; 0x54; 0x80; 0xBF; 0x1F;
     0x00; 0x41; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
     0x9C; 0x8E; 0x8F; 0x28; 0x40; 0x96; 0xB9; 0xA3;
     0xFF |]

(* 글자 클록 하나가 내는 점 수. 16색은 평면 넷의 같은 바이트에서 여덟
   점을, 256색은 평면마다 한 바이트씩 네 점을 낸다. *)
let dots_16 = 8
let dots_256 = 4

(* BIOS 가 13h 의 점을 찍는 줄 너비(INT 10h AH=0Ch/0Dh). CRTC 가 무엇을
   보여 주든 BIOS 는 0xA0000 + y*320 + x 에 쓴다. *)
let bios_line_256 = 320

let attr_count = 21

type t = {
  mem : Bytes.t;
  planes : Bytes.t array;
  latches : int array;
  mutable mode : int;
  mutable kind : kind;
  (** [mode] 의 종류. 모드가 바뀔 때마다 다시 구하는 파생값이다 — 메모리
      접근마다 모드표를 찾지 않으려고 둔다. 스냅숏에 넣지 않는다. *)
  mutable gc_index : int;
  gc : int array;
  mutable seq_index : int;
  seq : int array;
  mutable attr_index : int;
  mutable attr_is_data : bool;
  attr : int array;
  mutable cga_color_select : int;
  mutable crtc_index : int;
  crtc : int array;
}

(* 모드표. 텍스트의 크기는 글자 격자를 8x16 으로 편 것이다. *)
let spec = function
  | 0 | 1 -> (Text, 320, 400, 40)
  | 2 | 3 -> (Text, 640, 400, 80)
  | 7 -> (Text, 640, 400, 80)
  | 4 | 5 -> (Cga4, 320, 200, 40)
  | 6 -> (Cga2, 640, 200, 80)
  | 0x0D -> (Planar, 320, 200, 40)
  | 0x0E -> (Planar, 640, 200, 80)
  | 0x0F | 0x10 -> (Planar, 640, 350, 80)
  | 0x11 | 0x12 -> (Planar, 640, 480, 80)
  | 0x13 -> (Linear256, 320, 200, 40)
  (* 모르는 번호는 텍스트로 본다 — 화면을 안 그리는 것보다 낫고,
     지금 모드가 무엇인지는 mode 로 확인할 수 있다. *)
  | _ -> (Text, 640, 400, 80)

let kind_of_mode m = let k, _, _, _ = spec m in k

(* 모드를 세울 때 CRTC 에 싣는 표. 텍스트와 CGA 모드의 화면은 CRTC 를
   거치지 않고 모드표에서 그리므로 표를 싣지 않는다. *)
let crtc_table = function
  | 0x0D -> Some crtc_mode0d
  | 0x0E -> Some crtc_mode0e
  | 0x0F | 0x10 -> Some crtc_mode0f
  | 0x11 | 0x12 -> Some crtc_mode11
  | 0x13 -> Some crtc_mode13
  | _ -> None
let boot_mode = 3

let create ~mem =
  {
    mem;
    planes = Array.init plane_count (fun _ -> Bytes.make plane_size '\000');
    latches = Array.make plane_count 0;
    mode = boot_mode;
    kind = kind_of_mode boot_mode;
    gc_index = 0;
    gc = Array.make gc_count 0;
    seq_index = 0;
    seq = Array.make seq_count 0;
    attr_index = 0;
    attr_is_data = false;
    (* 속성 팔레트 기본값은 항등 — 색 번호가 곧 DAC 자리다. BIOS 가
       심는 EGA 호환 값(00,01,...,3F)을 쓰려면 DAC 의 그 자리에도 같은
       색이 있어야 하는데, 우리 DAC 의 기본값은 0-15 가 CGA 색이다.
       둘 중 하나를 고르는 문제라 항등을 골랐다 — 팔레트를 건드리지
       않는 게임이 바른 색을 본다. *)
    attr = Array.init attr_count (fun i -> if i < 16 then i else 0);
    cga_color_select = 0x30;
    crtc_index = 0;
    crtc = Array.make crtc_count 0;
  }

let mode t = t.mode
let kind t = t.kind

(* 그래픽 화면의 크기는 CRTC 가 정한다. 가로는 표시 끝 글자 수에 글자당
   점 수([dots])를 곱한 값이다. 세로는 표시 줄 수(0x12, overflow 의 bit1 이 8번째, bit6 이 9번째
   비트)를 한 행이 차지하는 줄 수로 나눈 값이다. 행 하나는 0x09 의 아래 다섯
   비트 + 1 줄이고, bit7(scan doubling)이 서 있으면 그 두 배다. 13h 부팅값은
   320x200 이고, Mode X 는 세로 레지스터를 바꿔 320x240 을 만든다. *)
let crtc_dims t ~dots =
  let r i = t.crtc.(i) in
  let o = r crtc_overflow in
  let lines =
    (r crtc_v_display_end
     lor (((o lsr overflow_vde8_shift) land 1) lsl 8)
     lor (((o lsr overflow_vde9_shift) land 1) lsl 9))
    + 1
  in
  let msl = r crtc_max_scan_line in
  let per_row =
    ((msl land max_scan_rows) + 1) * (if msl land max_scan_double <> 0 then 2 else 1)
  in
  ((r crtc_h_display_end + 1) * dots, max 1 (lines / per_row))

let dims t =
  match spec t.mode with
  | Planar, _, _, _ -> crtc_dims t ~dots:dots_16
  | Linear256, _, _, _ -> crtc_dims t ~dots:dots_256
  | (Text | Cga4 | Cga2), w, h, _ -> (w, h)
let text_cols t = let _, _, _, c = spec t.mode in c
let planes t = t.planes
let attr_palette t = t.attr
let set_attr_palette t i v = if i >= 0 && i < attr_count then t.attr.(i) <- v
let cga_color_select t = t.cga_color_select
let set_cga_color_select t v = t.cga_color_select <- v land 0xff

let set_mode t m ~clear =
  t.mode <- m;
  t.kind <- kind_of_mode m;
  t.gc_index <- 0;
  t.seq_index <- 0;
  t.attr_index <- 0;
  t.attr_is_data <- false;
  Array.fill t.gc 0 gc_count 0;
  Array.fill t.seq 0 seq_count 0;
  Array.fill t.latches 0 plane_count 0;
  (* 모드 세팅 직후의 기본값: 네 평면 모두 쓰기 가능, 비트 전부 통과. *)
  t.seq.(seq_map_mask) <- 0x0F;
  t.gc.(gc_bit_mask) <- 0xFF;
  t.seq.(seq_memory_mode) <- memory_mode_of t.kind;
  Option.iter (fun table -> Array.blit table 0 t.crtc 0 crtc_count) (crtc_table m);
  Array.iteri (fun i _ -> t.attr.(i) <- (if i < 16 then i else 0)) t.attr;
  t.cga_color_select <- 0x30;
  if clear then
    match spec m with
    | Text, _, _, cols ->
      (* 글자는 공백, 속성은 흰 글자 검은 바탕 *)
      let cells = cols * 25 in
      for i = 0 to cells - 1 do
        Bytes.set t.mem (cga_base + (i * 2)) ' ';
        Bytes.set t.mem (cga_base + (i * 2) + 1) '\007'
      done
    | (Cga4 | Cga2), _, _, _ -> Bytes.fill t.mem cga_base 0x4000 '\000'
    | (Planar | Linear256), _, _, _ ->
      Array.iter (fun p -> Bytes.fill p 0 plane_size '\000') t.planes

(* 13h 도 평면 넷에 산다. 실기와 같다 — chain-4 를 끄면(Mode X) 같은
   메모리가 평면별로 보인다. 그래서 256색 모드도 0xA0000 창을 가진다. *)
let owns_address t a =
  a >= planar_base && a < planar_top
  && (match kind t with Planar | Linear256 -> true | Text | Cga4 | Cga2 -> false)

(* chain-4 가 켜진 256색: 주소 a 는 평면 (a & 3) 의 (a & ~3) 자리다.
   그래서 13h 의 64000 바이트는 각 평면의 네 바이트마다 하나씩 앉는다. *)
let chained t =
  kind t = Linear256 && t.seq.(seq_memory_mode) land chain4 <> 0

let chained_offset off = off land lnot 3

(* ---------- 평면 읽기·쓰기 ---------- *)

let rotate v n = if n = 0 then v land 0xff
  else ((v lsr n) lor (v lsl (8 - n))) land 0xff

let alu fn data latch =
  match fn land 3 with
  | 0 -> data
  | 1 -> data land latch
  | 2 -> data lor latch
  | _ -> data lxor latch

(* 읽기는 래치를 채운다 — 부수효과가 본체다. 읽기-쓰기 관용구가 이
   래치로 건드리지 않을 비트를 보존한다. *)
let load_latches t off =
  for p = 0 to plane_count - 1 do
    t.latches.(p) <- Char.code (Bytes.get t.planes.(p) off)
  done

let planar_read t off =
  load_latches t off;
  if t.gc.(gc_mode) land 0x08 = 0 then t.latches.(t.gc.(gc_read_map) land 3)
  else begin
    (* 읽기 모드 1: 색 비교. 네 평면의 비트가 비교색과 같은 자리에 1. *)
    let cc = t.gc.(gc_color_compare) land 0x0f in
    let care = t.gc.(gc_color_dont_care) land 0x0f in
    let r = ref 0 in
    for bit = 0 to 7 do
      let m = 1 lsl bit in
      let same = ref true in
      for p = 0 to plane_count - 1 do
        if care land (1 lsl p) <> 0 then begin
          let pb = t.latches.(p) land m <> 0 in
          let cb = cc land (1 lsl p) <> 0 in
          if pb <> cb then same := false
        end
      done;
      if !same then r := !r lor m
    done;
    !r
  end

let planar_write t off v =
  let map = t.seq.(seq_map_mask) land 0x0f in
  let wmode = t.gc.(gc_mode) land 3 in
  let fn = (t.gc.(gc_data_rotate) lsr 3) land 3 in
  let rot = t.gc.(gc_data_rotate) land 7 in
  let bit_mask = t.gc.(gc_bit_mask) land 0xff in
  for p = 0 to plane_count - 1 do
    if map land (1 lsl p) <> 0 then begin
      let latch = t.latches.(p) in
      let data, mask =
        match wmode with
        | 0 ->
          let d =
            if t.gc.(gc_enable_sr) land (1 lsl p) <> 0 then
              if t.gc.(gc_set_reset) land (1 lsl p) <> 0 then 0xff else 0x00
            else rotate v rot
          in
          (alu fn d latch, bit_mask)
        | 1 ->
          (* 래치를 그대로 옮긴다. 비트 마스크도 논리연산도 끼지 않는다 —
             화면 한 조각을 옆으로 복사할 때 쓰는 길이다. *)
          (latch, 0xff)
        | 2 ->
          let d = if v land (1 lsl p) <> 0 then 0xff else 0x00 in
          (alu fn d latch, bit_mask)
        | _ ->
          (* 쓰기 모드 3: set/reset 값이 데이터가 되고, 회전한 피연산자와
             비트 마스크의 AND 가 실제 마스크가 된다. *)
          let d =
            if t.gc.(gc_set_reset) land (1 lsl p) <> 0 then 0xff else 0x00
          in
          (d, bit_mask land rotate v rot)
      in
      (* 마스크가 0 인 자리는 메모리의 현재 값이 아니라 래치에서 온다 —
         실기 계약이고, 읽기-쓰기 관용구가 이것에 기댄다. *)
      let nv = (data land mask) lor (latch land lnot mask) in
      Bytes.set t.planes.(p) off (Char.chr (nv land 0xff))
    end
  done

let mem_read t a =
  let off = (a - planar_base) land 0xffff in
  if chained t then begin
    load_latches t (chained_offset off);
    t.latches.(off land 3)
  end
  else planar_read t off

(* chain-4 쓰기는 바이트를 그대로 둔다. 맵 마스크도 그래픽 컨트롤러도
   거치지 않는다 — 13h 가 RAM 에 살던 때와 같은 동작이다. 실기가 chain-4
   에서 맵 마스크를 어떻게 쓰는지는 확인하지 못했다. 모드를 세우면 맵
   마스크가 0Fh 라 BIOS 로 모드를 세운 프로그램은 차이를 보지 못한다. *)
let mem_write t a v =
  let off = (a - planar_base) land 0xffff in
  let v = v land 0xff in
  if chained t then Bytes.set t.planes.(off land 3) (chained_offset off) (Char.chr v)
  else planar_write t off v

(* ---------- 포트 ---------- *)

let port_in t p =
  match p land 0xffff with
  | 0x3C4 -> Some t.seq_index
  | 0x3C5 ->
    Some (if t.seq_index < seq_count then t.seq.(t.seq_index) else 0xff)
  | 0x3CE -> Some t.gc_index
  | 0x3CF -> Some (if t.gc_index < gc_count then t.gc.(t.gc_index) else 0xff)
  | 0x3C0 -> Some t.attr_index
  | 0x3C1 ->
    Some (if t.attr_index < attr_count then t.attr.(t.attr_index) else 0xff)
  | 0x3B4 | 0x3D4 -> Some t.crtc_index
  | 0x3B5 | 0x3D5 ->
    Some (if t.crtc_index < crtc_count then t.crtc.(t.crtc_index) else 0xff)
  | _ -> None

let crtc_write t v =
  let i = t.crtc_index in
  if i < crtc_count then
    if i <= crtc_last_protected && t.crtc.(crtc_v_retrace_end) land crtc_protect <> 0 then begin
      if i = crtc_overflow then
        t.crtc.(i) <-
          (t.crtc.(i) land lnot overflow_line_compare) lor (v land overflow_line_compare)
    end
    else t.crtc.(i) <- v

let port_out t p v =
  let v = v land 0xff in
  match p land 0xffff with
  | 0x3C4 -> t.seq_index <- v land 0x1f; true
  | 0x3C5 -> if t.seq_index < seq_count then t.seq.(t.seq_index) <- v; true
  | 0x3CE -> t.gc_index <- v land 0x1f; true
  | 0x3CF -> if t.gc_index < gc_count then t.gc.(t.gc_index) <- v; true
  | 0x3C0 ->
    (* 한 포트가 번갈아 index 와 data 를 받는다. 어느 차례인지는 이
       어댑터가 기억하고, 0x3DA 를 읽으면 index 차례로 되돌아간다. *)
    if t.attr_is_data then begin
      if t.attr_index < attr_count then t.attr.(t.attr_index) <- v;
      t.attr_is_data <- false
    end
    else begin
      t.attr_index <- v land 0x1f;
      t.attr_is_data <- true
    end;
    true
  | 0x3B4 | 0x3D4 -> t.crtc_index <- v; true
  | 0x3B5 | 0x3D5 -> crtc_write t v; true
  | _ -> false

let crtc_index t = t.crtc_index
let crtc t i = t.crtc.(i)

let reset_attr_flip t = t.attr_is_data <- false

(* ---------- 픽셀 ---------- *)

(* CGA 그래픽은 짝수 줄과 홀수 줄이 8KB 떨어져 있다. 한 덩어리로 보면
   화면이 위아래로 갈라진다. *)
let cga_row_addr y = cga_base + (if y land 1 = 0 then 0 else 0x2000)
                     + ((y / 2) * 80)

(* BIOS 가 16색 점을 찍는 한 줄의 바이트 수 — 모드표의 가로를 8 로 나눈
   값이다. CRTC 의 크기나 행 간격을 바꿔도 BIOS 는 같은 자리에 쓴다.
   SeaBIOS 는 줄 너비를 CRTC 13h 에서 구한다(vgafb.c gfx_planar,
   stdvga.c stdvga_get_linelength). 그래서 행 간격을 바꾼 뒤 AH=0Ch 를
   부르는 프로그램에서만 둘이 다르다. *)
let bios_line_16 t = let _, w, _, _ = spec t.mode in w / dots_16

let put_pixel t ~x ~y ~color =
  match kind t with
  | Text -> ()
  | Cga4 ->
    let a = cga_row_addr y + (x / 4) in
    let shift = (3 - (x mod 4)) * 2 in
    let old = Char.code (Bytes.get t.mem a) in
    let nv = (old land lnot (3 lsl shift)) lor ((color land 3) lsl shift) in
    Bytes.set t.mem a (Char.chr nv)
  | Cga2 ->
    let a = cga_row_addr y + (x / 8) in
    let bit = 7 - (x mod 8) in
    let old = Char.code (Bytes.get t.mem a) in
    let nv =
      if color land 1 <> 0 then old lor (1 lsl bit)
      else old land lnot (1 lsl bit)
    in
    Bytes.set t.mem a (Char.chr nv)
  | Planar ->
    let off = ((y * bios_line_16 t) + (x / 8)) land 0xffff in
    let bit = 1 lsl (7 - (x mod 8)) in
    for p = 0 to plane_count - 1 do
      let old = Char.code (Bytes.get t.planes.(p) off) in
      let nv =
        if color land (1 lsl p) <> 0 then old lor bit else old land lnot bit
      in
      Bytes.set t.planes.(p) off (Char.chr nv)
    done
  | Linear256 ->
    (* BIOS 가 하는 그대로 CPU 쓰기 한 번이다 — chain-4 를 끈 뒤라면 맵
       마스크가 고른 평면에 간다. *)
    mem_write t (planar_base + (((y * bios_line_256) + x) land 0xffff)) color

(* ---------- 그래픽 화면 ---------- *)

(* CRTC 의 주소 카운터(MA)가 평면 주소가 되는 방식(FreeVGA CRTC 14h·17h).
   14h bit6(dword)이 서 있으면 MA 를 두 비트 민다. 아니면 17h bit6 이
   고른다. 0 이면 word 모드라 한 비트 밀고, 17h bit5 가 고른 MA13 이나
   MA15 를 0번 비트에 둔다. 1 이면 byte 모드라 그대로 쓴다. *)
let scan_address t =
  if t.crtc.(crtc_underline) land underline_dword <> 0 then fun ma -> (ma lsl 2) land 0xffff
  else if t.crtc.(crtc_mode_control) land mode_byte <> 0 then fun ma -> ma land 0xffff
  else
    let wrap = if t.crtc.(crtc_mode_control) land mode_wrap_ma15 <> 0 then 15 else 13 in
    fun ma -> let ma = ma land 0xffff in ((ma lsl 1) lor ((ma lsr wrap) land 1)) land 0xffff

let start_address t = (t.crtc.(crtc_start_high) lsl 8) lor t.crtc.(crtc_start_low)

(* 한 행이 지날 때 MA 가 늘어나는 양. 주소 크기(byte·word·dword)는
   [scan_address] 가 곱한다. *)
let row_stride t = t.crtc.(crtc_offset) * 2

(* CRTC 가 화면을 훑는 차례대로 글자 하나가 읽는 평면 주소를 [f] 에
   넘긴다. 행 y 는 MA = 시작 주소 + y × [row_stride] 에서 시작하고, 글자
   하나가 MA 를 하나 늘린다. 한 행의 글자 수는 가로를 [dots] 로 나눈
   값이다. split screen(line compare)과 픽셀 패닝은 모델에 없다. *)
let scan_chars t ~dots f =
  let w, h = crtc_dims t ~dots in
  let address = scan_address t in
  let start = start_address t and stride = row_stride t in
  for y = 0 to h - 1 do
    let row = start + (y * stride) in
    for c = 0 to (w / dots) - 1 do
      f (address (row + c))
    done
  done

let plane_byte t p a = Char.code (Bytes.get t.planes.(p) a)

(* 16색 점의 색 번호: 네 평면에서 같은 비트를 모은다(bit p = 평면 p). *)
let color16 t a bit =
  let c = ref 0 in
  for p = 0 to plane_count - 1 do
    if (plane_byte t p a lsr bit) land 1 = 1 then c := !c lor (1 lsl p)
  done;
  !c

(* 화면에 보이는 점들을 위에서 아래로, 왼쪽에서 오른쪽으로 [f] 에 넘긴다.
   256색은 글자 하나가 평면 0-3 에서 한 바이트씩, 네 점을 낸다. *)
let iter256 t f =
  scan_chars t ~dots:dots_256 (fun a ->
      for p = 0 to plane_count - 1 do f (plane_byte t p a) done)

(* 16색은 글자 하나가 여덟 점을 내고, 왼쪽 점이 바이트의 최상위 비트다. *)
let iter16 t f =
  scan_chars t ~dots:dots_16 (fun a ->
      for bit = dots_16 - 1 downto 0 do f (color16 t a bit) done)

let collect t ~dots iter =
  let w, h = crtc_dims t ~dots in
  let out = Bytes.create (w * h) in
  let i = ref 0 in
  iter (fun c -> Bytes.set out !i (Char.chr c); incr i);
  out

let screen256 t = collect t ~dots:dots_256 (iter256 t)

(* 속성 컨트롤러가 색 번호 0-15 를 DAC 자리로 옮긴다. *)
let screen16 t = collect t ~dots:dots_16 (fun f -> iter16 t (fun c -> f t.attr.(c)))

(* 지금 모드가 실제로 그리는 메모리의 지문. 화면이 멈췄는지 보는 데
   쓴다 — 픽셀을 다 만들어 비교하는 것보다 훨씬 싸고, 모드마다 어느
   메모리가 화면인지는 여기만 안다. 충돌해도 손해는 "안 변했다고 잘못
   읽는" 것뿐이라 64비트가 필요 없다. *)
let digest_step h b = ((h * 31) + b) land 0x3FFFFFFFFFFFFFF

(* 16색 글자 하나의 여덟 점은 평면 넷의 같은 자리 네 바이트에서 나온다.
   그 네 바이트를 한 값으로 묶어 지문에 넣으면 점을 만들지 않아도 된다. *)
let char_planes t a =
  plane_byte t 0 a lor (plane_byte t 1 a lsl 8) lor (plane_byte t 2 a lsl 16)
  lor (plane_byte t 3 a lsl 24)

let screen_digest t =
  let h = ref 0 in
  let over bytes base len =
    for i = 0 to len - 1 do
      h := digest_step !h (Char.code (Bytes.get bytes (base + i)))
    done
  in
  (match spec t.mode with
   | Text, _, _, cols -> over t.mem cga_base (cols * 25 * 2)
   | (Cga4 | Cga2), _, _, _ -> over t.mem cga_base 0x4000
   | Planar, _, _, _ -> scan_chars t ~dots:dots_16 (fun a -> h := digest_step !h (char_planes t a))
   | Linear256, _, _, _ -> iter256 t (fun c -> h := digest_step !h c));
  !h

let get_pixel t ~x ~y =
  match kind t with
  | Text -> 0
  | Cga4 ->
    let a = cga_row_addr y + (x / 4) in
    let shift = (3 - (x mod 4)) * 2 in
    (Char.code (Bytes.get t.mem a) lsr shift) land 3
  | Cga2 ->
    let a = cga_row_addr y + (x / 8) in
    (Char.code (Bytes.get t.mem a) lsr (7 - (x mod 8))) land 1
  | Planar ->
    let off = ((y * bios_line_16 t) + (x / 8)) land 0xffff in
    let bit = 7 - (x mod 8) in
    let c = ref 0 in
    for p = 0 to plane_count - 1 do
      if (Char.code (Bytes.get t.planes.(p) off) lsr bit) land 1 = 1 then
        c := !c lor (1 lsl p)
    done;
    !c
  | Linear256 ->
    (* BIOS 가 읽는 자리(y*320+x)의 값. 래치는 건드리지 않는다 — 관측이
       기계 상태를 바꾸면 안 된다. *)
    let off = ((y * bios_line_256) + x) land 0xffff in
    if chained t then Char.code (Bytes.get t.planes.(off land 3) (chained_offset off))
    else Char.code (Bytes.get t.planes.(t.gc.(gc_read_map) land 3) off)

let screen_pixel t ~x ~y =
  let ma ~dots = start_address t + (y * row_stride t) + (x / dots) in
  match kind t with
  | Linear256 -> plane_byte t (x land 3) (scan_address t (ma ~dots:dots_256))
  | Planar ->
    color16 t (scan_address t (ma ~dots:dots_16)) (dots_16 - 1 - (x mod dots_16))
  | Text | Cga4 | Cga2 -> get_pixel t ~x ~y

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. Keep [write_state] and [read_state] in one order;
   both name the same range for each field. *)
let write_state w t =
  let { mem = _ (* the machine's RAM: the machine writes it *);
        planes; latches; mode; kind = _ (* derived from mode *); gc_index; gc; seq_index; seq; attr_index;
        attr_is_data; attr; cga_color_select; crtc_index; crtc } = t
  in
  let put what v = C.put w ~what C.byte v in
  Array.iter (C.put_bytes w) planes;
  C.put_int_array w ~what:"latches" C.byte latches;
  put "video mode" mode;
  put "gc_index" gc_index;
  C.put_int_array w ~what:"gc" C.byte gc;
  put "seq_index" seq_index;
  C.put_int_array w ~what:"seq" C.byte seq;
  put "attr_index" attr_index;
  C.put_bool w attr_is_data;
  C.put_int_array w ~what:"attr" C.byte attr;
  put "cga_color_select" cga_color_select;
  put "crtc_index" crtc_index;
  C.put_int_array w ~what:"crtc" C.byte crtc

let read_state r t =
  let get what = C.get r ~what C.byte in
  Array.iter (C.fill_bytes r) t.planes;
  C.fill_int_array r ~what:"latches" C.byte t.latches;
  t.mode <- get "video mode";
  t.kind <- kind_of_mode t.mode;
  t.gc_index <- get "gc_index";
  C.fill_int_array r ~what:"gc" C.byte t.gc;
  t.seq_index <- get "seq_index";
  C.fill_int_array r ~what:"seq" C.byte t.seq;
  t.attr_index <- get "attr_index";
  t.attr_is_data <- C.get_bool r;
  C.fill_int_array r ~what:"attr" C.byte t.attr;
  t.cga_color_select <- get "cga_color_select";
  t.crtc_index <- get "crtc_index";
  C.fill_int_array r ~what:"crtc" C.byte t.crtc
