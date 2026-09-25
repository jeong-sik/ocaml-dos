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

(* 시퀀서: 2 번이 평면 쓰기 마스크 *)
let seq_map_mask = 2
let seq_count = 5

let attr_count = 21

type t = {
  mem : Bytes.t;
  planes : Bytes.t array;
  latches : int array;
  mutable mode : int;
  mutable gc_index : int;
  gc : int array;
  mutable seq_index : int;
  seq : int array;
  mutable attr_index : int;
  mutable attr_is_data : bool;
  attr : int array;
  mutable cga_color_select : int;
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

let create ~mem =
  {
    mem;
    planes = Array.init plane_count (fun _ -> Bytes.make plane_size '\000');
    latches = Array.make plane_count 0;
    mode = 3;
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
  }

let mode t = t.mode
let kind t = let k, _, _, _ = spec t.mode in k
let dims t = let _, w, h, _ = spec t.mode in (w, h)
let text_cols t = let _, _, _, c = spec t.mode in c
let planes t = t.planes
let attr_palette t = t.attr
let set_attr_palette t i v = if i >= 0 && i < attr_count then t.attr.(i) <- v
let cga_color_select t = t.cga_color_select
let set_cga_color_select t v = t.cga_color_select <- v land 0xff

let set_mode t m ~clear =
  t.mode <- m;
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
    | Planar, _, _, _ ->
      Array.iter (fun p -> Bytes.fill p 0 plane_size '\000') t.planes
    | Linear256, _, _, _ -> Bytes.fill t.mem planar_base (320 * 200) '\000'

let owns_address t a =
  kind t = Planar && a >= planar_base && a < planar_top

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
let mem_read t a =
  let off = (a - planar_base) land 0xffff in
  for p = 0 to plane_count - 1 do
    t.latches.(p) <- Char.code (Bytes.get t.planes.(p) off)
  done;
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

let mem_write t a v =
  let off = (a - planar_base) land 0xffff in
  let v = v land 0xff in
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
  | _ -> None

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
  | _ -> false

let reset_attr_flip t = t.attr_is_data <- false

(* ---------- 픽셀 ---------- *)

(* CGA 그래픽은 짝수 줄과 홀수 줄이 8KB 떨어져 있다. 한 덩어리로 보면
   화면이 위아래로 갈라진다. *)
let cga_row_addr y = cga_base + (if y land 1 = 0 then 0 else 0x2000)
                     + ((y / 2) * 80)

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
    let w, _ = dims t in
    let off = ((y * (w / 8)) + (x / 8)) land 0xffff in
    let bit = 1 lsl (7 - (x mod 8)) in
    for p = 0 to plane_count - 1 do
      let old = Char.code (Bytes.get t.planes.(p) off) in
      let nv =
        if color land (1 lsl p) <> 0 then old lor bit else old land lnot bit
      in
      Bytes.set t.planes.(p) off (Char.chr nv)
    done
  | Linear256 ->
    let w, _ = dims t in
    Bytes.set t.mem (planar_base + (y * w) + x) (Char.chr (color land 0xff))

(* 지금 모드가 실제로 그리는 메모리의 지문. 화면이 멈췄는지 보는 데
   쓴다 — 픽셀을 다 만들어 비교하는 것보다 훨씬 싸고, 모드마다 어느
   메모리가 화면인지는 여기만 안다. 충돌해도 손해는 "안 변했다고 잘못
   읽는" 것뿐이라 64비트가 필요 없다. *)
let digest_step h b = ((h * 31) + b) land 0x3FFFFFFFFFFFFFF

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
   | Planar, w, ht, _ ->
     let len = min plane_size (w / 8 * ht) in
     Array.iter (fun p -> over p 0 len) t.planes
   | Linear256, w, ht, _ -> over t.mem planar_base (w * ht));
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
    let w, _ = dims t in
    let off = ((y * (w / 8)) + (x / 8)) land 0xffff in
    let bit = 7 - (x mod 8) in
    let c = ref 0 in
    for p = 0 to plane_count - 1 do
      if (Char.code (Bytes.get t.planes.(p) off) lsr bit) land 1 = 1 then
        c := !c lor (1 lsl p)
    done;
    !c
  | Linear256 ->
    let w, _ = dims t in
    Char.code (Bytes.get t.mem (planar_base + (y * w) + x))

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. Keep [write_state] and [read_state] in one order. *)
let write_state w t =
  let { mem = _ (* the machine's RAM: the machine writes it *);
        planes; latches; mode; gc_index; gc; seq_index; seq; attr_index;
        attr_is_data; attr; cga_color_select } = t
  in
  Array.iter (C.put_bytes w) planes;
  C.put_int_array w latches;
  C.put_int w mode;
  C.put_int w gc_index;
  C.put_int_array w gc;
  C.put_int w seq_index;
  C.put_int_array w seq;
  C.put_int w attr_index;
  C.put_bool w attr_is_data;
  C.put_int_array w attr;
  C.put_int w cga_color_select

let read_state r t =
  let byte () = C.get_int r ~min:0 ~max:0xff in
  Array.iter (C.fill_bytes r) t.planes;
  C.fill_int_array r ~min:0 ~max:0xff t.latches;
  t.mode <- byte ();
  t.gc_index <- byte ();
  C.fill_int_array r ~min:0 ~max:0xff t.gc;
  t.seq_index <- byte ();
  C.fill_int_array r ~min:0 ~max:0xff t.seq;
  t.attr_index <- byte ();
  t.attr_is_data <- C.get_bool r;
  C.fill_int_array r ~min:0 ~max:0xff t.attr;
  t.cga_color_select <- byte ()
