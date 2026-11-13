(* LIM EMS 4.0 Function 24 — 메모리 영역 옮기기(AX=5700h)와 바꾸기(5701h).

   DS:SI 의 구조체가 길이(더블워드)와 원본·대상을 준다. 한쪽은
   conventional(종류 0: 세그먼트와 오프셋)이거나 expanded(종류 1: 핸들,
   첫 논리 페이지, 페이지 안 오프셋)다. 매핑은 바꾸지 않는다.

   - expanded 쪽은 [Dos_ems_pages.peek_byte]·[poke_byte] 로 읽고 쓴다.
     프레임에 떠 있는 페이지는 프레임이 최신 사본이다.
   - conventional 쪽은 CPU 버스를 지난다. 실기 드라이버의 REP MOVS 처럼,
     평면 모드의 A0000h 창에 닿는 바이트는 그래픽 컨트롤러를 지난다
     (#117 과 같은 까닭).

   검사 순서는 명세가 정하지 않는다. 여기서는 길이 96h, 종류 98h, 그다음
   원본과 대상을 차례로 본다. expanded 면 핸들 83h, 오프셋 95h, 첫 페이지
   8Ah, 영역 끝 93h 이고, conventional 이면 1MB 를 넘는 끝 A2h 다. 그다음
   겹침을 본다.

   - conventional 쪽이 expanded 영역이 떠 있는 프레임 자리와 겹치면 94h.
   - 같은 바이트 공간(같은 핸들, 또는 둘 다 conventional)에서 겹치면
     옮기기는 대상이 원본의 온전한 사본을 받도록 방향을 고른다. 같은
     핸들이면 92h 로 알린다. 바꾸기는 97h 로 거절한다 — 명세는 겹치는
     바꾸기를 모두 무효라 하고, 코드는 같은 핸들 경우(97h)만 둔다.
     conventional 끼리도 97h 로 답한다.

   길이 0 은 검사를 지나면 아무것도 하지 않는다. *)

open Dos_state
module P = Dos_ems_pages

type side =
  | Conventional of int                          (* 첫 물리 주소 *)
  | Expanded of { handle : int; start : int }    (* 핸들 안 바이트 위치 *)

let one_mb = 0x100000
let conventional_type = 0
let expanded_type = 1
let last_offset = P.page_bytes - 1               (* 3FFFh *)

(* 구조체: 길이 더블워드, 그다음 원본과 대상이 같은 모양으로 하나씩 —
   종류 바이트, 핸들, 오프셋, 세그먼트 또는 논리 페이지(워드 셋). *)
let source_at = 4
let dest_at = 11
let handle_at = 1
let offset_at = 3
let seg_page_at = 5

let lin seg off = (seg lsl 4) + (off land 0xffff)
let peek8 t seg off = rd8 t (lin seg off)
let peek16 t seg off = peek8 t seg off lor (peek8 t seg (off + 1) lsl 8)
let peek32 t seg off = peek16 t seg off lor (peek16 t seg (off + 2) lsl 16)

let side t seg off ~length =
  let kind = peek8 t seg off in
  let handle = peek16 t seg (off + handle_at) in
  let offset = peek16 t seg (off + offset_at) in
  let seg_page = peek16 t seg (off + seg_page_at) in
  if kind = conventional_type then
    let a = (seg_page lsl 4) + offset in
    if a + length > one_mb then Error P.Wraps_1mb else Ok (Conventional a)
  else if kind = expanded_type then
    match Hashtbl.find_opt t.ems_pages handle with
    | None -> Error P.Invalid_handle
    | Some pages ->
      let n = Array.length pages in
      let start = (seg_page * P.page_bytes) + offset in
      if offset > last_offset then Error P.Offset_past_page
      else if seg_page >= n then Error P.Logical_out_of_range
      else if start + length > n * P.page_bytes then Error P.Past_handle_pages
      else Ok (Expanded { handle; start })
  else Error P.Undefined_memory_type

let overlaps (a, la) (b, lb) = a < b + lb && b < a + la

(* expanded 영역 가운데 프레임 창에 떠 있는 부분의 (물리 주소, 길이)들. *)
let shown_ranges t ~handle ~start ~length =
  List.filter_map
    (fun q ->
      match t.ems_mapped.(q) with
      | Some (h, p) when h = handle ->
        let base = p * P.page_bytes in
        let lo = max start base and hi = min (start + length) (base + P.page_bytes) in
        if lo < hi then Some (P.frame_addr q + (lo - base), hi - lo) else None
      | Some _ | None -> None)
    (List.init P.frame_pages Fun.id)

let conventional_meets_expanded t ~length a b =
  match a, b with
  | Conventional c, Expanded { handle; start } | Expanded { handle; start }, Conventional c ->
    List.exists (overlaps (c, length)) (shown_ranges t ~handle ~start ~length)
  | Conventional _, Conventional _ | Expanded _, Expanded _ -> false

(* 같은 바이트 공간이면 그 공간에서의 두 시작 위치. *)
let same_space a b =
  match a, b with
  | Conventional x, Conventional y -> Some (x, y)
  | Expanded x, Expanded y when x.handle = y.handle -> Some (x.start, y.start)
  | Expanded _, Expanded _ | Conventional _, Expanded _ | Expanded _, Conventional _ -> None

let read t = function
  | Conventional a -> fun i -> Cpu86.bus_read t.cpu (a + i)
  | Expanded { handle; start } -> fun i -> P.peek_byte t handle (start + i)

let write t = function
  | Conventional a -> fun i v -> Cpu86.bus_write t.cpu (a + i) v
  | Expanded { handle; start } -> fun i v -> P.poke_byte t handle (start + i) v

let transfer t ~exchange ~length source dest =
  (* 겹치면, 대상이 원본보다 뒤에서 시작하는가 *)
  let overlap =
    match same_space source dest with
    | Some (s, d) when overlaps (s, length) (d, length) -> Some (d > s)
    | Some _ | None -> None
  in
  if conventional_meets_expanded t ~length source dest then
    Error P.Conventional_overlaps_expanded
  else if exchange then
    match overlap with
    | Some _ -> Error P.Exchange_overlap
    | None ->
      let rd_s = read t source and wr_s = write t source in
      let rd_d = read t dest and wr_d = write t dest in
      for i = 0 to length - 1 do
        let a = rd_s i and b = rd_d i in
        wr_s i b;
        wr_d i a
      done;
      Ok ()
  else begin
    let rd_s = read t source and wr_d = write t dest in
    let copy i = wr_d i (rd_s i) in
    (match overlap with
     | Some true -> for i = length - 1 downto 0 do copy i done
     | Some false | None -> for i = 0 to length - 1 do copy i done);
    match overlap, source with
    | Some _, Expanded _ -> Error P.Moved_over_source
    | Some _, Conventional _ | None, (Expanded _ | Conventional _) -> Ok ()
  end

let move_region t ~exchange ~seg ~off =
  let length = peek32 t seg off in
  let known at =
    let k = peek8 t seg (off + at) in
    k = conventional_type || k = expanded_type
  in
  if length > one_mb then Error P.Longer_than_1mb
  else if not (known source_at && known dest_at) then Error P.Undefined_memory_type
  else
    match side t seg (off + source_at) ~length with
    | Error e -> Error e
    | Ok source ->
      (match side t seg (off + dest_at) ~length with
       | Error e -> Error e
       | Ok dest -> transfer t ~exchange ~length source dest)
