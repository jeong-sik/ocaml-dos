(* LIM EMS 4.0 의 expanded memory 자체: 핸들, 논리 페이지, 프레임에 겹친
   페이지, 핸들별로 저장된 매핑. 레지스터와 게스트 배열을 읽고 쓰는 일은
   Dos_ems(INT 67h)가 한다. 출처는 LIM EMS 4.0 명세(1987, 300275-005)의
   Function 번호로 적는다.

   프레임은 0xD000-0xEFFF(물리 페이지 넷). 페이지 내용은 호스트 배열에
   있고, 매핑은 프레임의 지금 내용을 옛 논리 페이지에 되저장한 뒤 새
   논리 페이지를 프레임에 복사한다 — 게스트가 프레임에 직접 쓴 값은
   다음 매핑까지 그 페이지에 묻어 간다. 소리 없는 실패는 없다: 못
   지키는 요청은 LIM 상태 코드로 답한다. *)

open Dos_state

let page_frame = 0xD000
let page_size = 0x400                            (* 16KB = 0x400 단락 *)
let frame_pages = 4
let page_bytes = page_size * 16
let total_pages = 0x0800                         (* 32MB. Function 13 이 적은
                                                    LIM 4.0 의 최대다 *)

(* 핸들은 0-254 다. 0 은 운영체제 핸들로, 드라이버가 설치될 때부터 열려
   있고 이 드라이버에서는 페이지가 없다. 프로그램이 받는 번호는 1-254 이고,
   다 쓰였으면 85h 다. 해제한 번호는 다시 쓴다 — 가장 낮은 빈 번호.
   출처: Function 4 의 Note 와 오류 코드 표, Function 12. *)
let os_handle = 0
let first_handle = 1
let max_handle = 254

(* Function 5·17: 논리 페이지 FFFFh 는 그 물리 페이지를 비운다. *)
let unmap_page = 0xFFFF

let boot_handles () =
  let pages = Hashtbl.create 2 in
  Hashtbl.replace pages os_handle [||];
  pages

type status =
  | Invalid_handle                               (* 83h *)
  | Undefined_function                           (* 84h *)
  | No_more_handles                              (* 85h *)
  | Map_still_saved                              (* 86h *)
  | More_than_total                              (* 87h *)
  | More_than_free                               (* 88h *)
  | Zero_pages                                   (* 89h *)
  | Logical_out_of_range                         (* 8Ah *)
  | Physical_out_of_range                        (* 8Bh *)
  | Map_already_saved                            (* 8Dh *)
  | No_saved_map                                 (* 8Eh *)
  | Invalid_subfunction                          (* 8Fh *)
  | Moved_over_source                            (* 92h, 옮기기는 됐다 *)
  | Past_handle_pages                            (* 93h *)
  | Conventional_overlaps_expanded               (* 94h *)
  | Offset_past_page                             (* 95h *)
  | Longer_than_1mb                              (* 96h *)
  | Exchange_overlap                             (* 97h *)
  | Undefined_memory_type                        (* 98h *)
  | Wraps_1mb                                    (* A2h *)
  | Corrupt_map                                  (* A3h *)

let status_code = function
  | Invalid_handle -> 0x83
  | Undefined_function -> 0x84
  | No_more_handles -> 0x85
  | Map_still_saved -> 0x86
  | More_than_total -> 0x87
  | More_than_free -> 0x88
  | Zero_pages -> 0x89
  | Logical_out_of_range -> 0x8A
  | Physical_out_of_range -> 0x8B
  | Map_already_saved -> 0x8D
  | No_saved_map -> 0x8E
  | Invalid_subfunction -> 0x8F
  | Moved_over_source -> 0x92
  | Past_handle_pages -> 0x93
  | Conventional_overlaps_expanded -> 0x94
  | Offset_past_page -> 0x95
  | Longer_than_1mb -> 0x96
  | Exchange_overlap -> 0x97
  | Undefined_memory_type -> 0x98
  | Wraps_1mb -> 0xA2
  | Corrupt_map -> 0xA3

let free_handle t =
  let rec go h =
    if h > max_handle then None
    else if Hashtbl.mem t.ems_pages h then go (h + 1)
    else Some h
  in
  go first_handle

let frame_addr p = (page_frame * 16) + (p * page_bytes)

let free_pages t =
  let used =
    Hashtbl.fold (fun _ pages acc -> acc + Array.length pages) t.ems_pages 0
  in
  max 0 (total_pages - used)

(* 페이지 실체는 처음 건드릴 때 만든다 — "남은 전부"를 받아가도 실제로
   겹치는 몇 장만 산다. *)
let ensure_page pages i =
  if Bytes.length pages.(i) = 0 then pages.(i) <- Bytes.make page_bytes '\000';
  pages.(i)

(* 프레임과 저장된 매핑은 있는 페이지만 가리킨다. 해제·재할당이 먼저
   떼어 내고(forget), 스냅숏 복원이 검사한다. *)
let page_of t (h, log) =
  match Hashtbl.find_opt t.ems_pages h with
  | Some pages when log < Array.length pages -> ensure_page pages log
  | Some _ | None -> invalid_arg "Dos_ems: a mapping names a page that does not exist"

let store_frame t phys =
  match t.ems_mapped.(phys) with
  | None -> ()
  | Some page -> Bytes.blit t.mem (frame_addr phys) (page_of t page) 0 page_bytes

(* 물리 페이지 [phys] 에 [target] 을 겹친다. [target] 은 검사를 마친 값이다.

   이미 그 자리에 있는 페이지면 아무 일도 없다. 실기 EMS 에서 프레임은
   페이지 그 자체다 — 여기서 되저장본을 다시 겹치면 게스트가 그동안
   프레임에 쓴 값이 사라진다. 삼국지3 는 이 호출(AX=4403h BX=0)을 수시로
   부른다.

   아니면 프레임의 지금 내용을 옛 논리 페이지에 되저장한다. 같은 논리
   페이지가 다른 물리 페이지에도 떠 있으면 그 창이 최신이라 먼저 그
   창으로 맞춘 뒤 가져온다. 두 창이 서로의 쓰기를 곧바로 보지는 못한다 —
   프레임을 복사로 흉내 내는 한계다.

   비운 물리 페이지([None])의 바이트는 그대로 남는다. 명세는 읽고 쓸 수
   없게 된다고 하지만, 여기서는 아무 페이지에도 묻어 가지 않는 RAM 이다. *)
let show t phys target =
  if t.ems_mapped.(phys) <> target then begin
    store_frame t phys;
    (match target with
     | None -> ()
     | Some page ->
       Array.iteri (fun q shown -> if q <> phys && shown = target then store_frame t q)
         t.ems_mapped;
       Bytes.blit (page_of t page) 0 t.mem (frame_addr phys) page_bytes);
    t.ems_mapped.(phys) <- target
  end

(* ---------- 핸들 안의 바이트 ----------

   57h 는 한 핸들의 논리 페이지를 이어 붙인 바이트 열을 본다. [x] 번째
   바이트는 논리 페이지 [x / page_bytes] 의 [x mod page_bytes] 자리다.
   그 페이지가 프레임에 떠 있으면 프레임이 최신 사본이라 거기서 읽고,
   떠 있는 창 모두에 쓴다. 매핑은 그대로다(Function 24: "The current
   mapping context is maintained"). *)
let windows t page =
  List.filter (fun q -> t.ems_mapped.(q) = Some page) (List.init frame_pages Fun.id)

let peek_byte t h x =
  let page = (h, x / page_bytes) and o = x mod page_bytes in
  match windows t page with
  | q :: _ -> rd8 t (frame_addr q + o)
  | [] -> Char.code (Bytes.get (page_of t page) o)

let poke_byte t h x v =
  let page = (h, x / page_bytes) and o = x mod page_bytes in
  match windows t page with
  | [] -> Bytes.set (page_of t page) o (Char.chr (v land 0xff))
  | qs -> List.iter (fun q -> wr8 t (frame_addr q + o) v) qs

(* 핸들 [h] 의 [from] 번부터 뒤 페이지가 사라진다. 프레임과 저장된 매핑에서
   그 페이지를 뗀다. 프레임 바이트는 되저장하지 않는다 — 갈 곳이 없다.
   저장된 매핑에서도 떼는 까닭: 번호가 다시 쓰인 핸들의 엉뚱한 페이지를
   48h 가 겹치지 않게 한다. *)
let forget t h ~from =
  let gone = function Some (mh, ml) -> mh = h && ml >= from | None -> false in
  let clear m = Array.iteri (fun i shown -> if gone shown then m.(i) <- None) m in
  clear t.ems_mapped;
  Hashtbl.iter (fun _ m -> clear m) t.ems_saved

(* 논리 페이지 [log] 를 겹칠 수 있나. FFFFh 는 비우기라 핸들을 보지 않는다
   (DOSBox 도 같다). *)
let target t ~handle ~log =
  if log = unmap_page then Ok None
  else
    match Hashtbl.find_opt t.ems_pages handle with
    | None -> Error Invalid_handle
    | Some pages ->
      if log < Array.length pages then Ok (Some (handle, log)) else Error Logical_out_of_range

(* 58h 가 알려 주는 매핑 가능 세그먼트면 그 물리 페이지. Function 17 은
   세그먼트가 그 경계와 정확히 맞아야 한다고 한다. *)
let physical_of_segment seg =
  let d = seg - page_frame in
  if d >= 0 && d mod page_size = 0 && d / page_size < frame_pages then Some (d / page_size)
  else None

let allocate t ~count ~zero_ok =
  if count = 0 && not zero_ok then Error Zero_pages
  else if count > total_pages then Error More_than_total
  else if count > free_pages t then Error More_than_free
  else
    match free_handle t with
    | None -> Error No_more_handles
    | Some h ->
      Hashtbl.replace t.ems_pages h (Array.make count Bytes.empty);
      Ok h

(* Function 6. 47h 로 저장한 매핑이 남아 있으면 86h 다. 운영체제 핸들은
   닫히지 않고 0 페이지로 재할당된다(Function 4 의 Note). *)
let release t h =
  match Hashtbl.find_opt t.ems_pages h with
  | None -> Error Invalid_handle
  | Some _ when Hashtbl.mem t.ems_saved h -> Error Map_still_saved
  | Some _ ->
    forget t h ~from:0;
    if h = os_handle then Hashtbl.replace t.ems_pages h [||]
    else Hashtbl.remove t.ems_pages h;
    Ok ()

(* Function 18. 늘리면 뒤에 새 페이지가 붙고, 줄이면 뒤에서 떨어진다. *)
let reallocate t h ~count =
  match Hashtbl.find_opt t.ems_pages h with
  | None -> Error Invalid_handle
  | Some pages ->
    let have = Array.length pages in
    if count > total_pages then Error More_than_total
    else if count - have > free_pages t then Error More_than_free
    else begin
      forget t h ~from:count;
      Hashtbl.replace t.ems_pages h
        (Array.init count (fun i -> if i < have then pages.(i) else Bytes.empty));
      Ok ()
    end

(* Function 8·9: 핸들마다 프레임 매핑 하나를 저장해 둔다. 복원하면 비운다. *)
let save_map t h =
  if not (Hashtbl.mem t.ems_pages h) then Error Invalid_handle
  else if Hashtbl.mem t.ems_saved h then Error Map_already_saved
  else begin
    Hashtbl.replace t.ems_saved h (Array.copy t.ems_mapped);
    Ok ()
  end

let restore_map t h =
  if not (Hashtbl.mem t.ems_pages h) then Error Invalid_handle
  else
    match Hashtbl.find_opt t.ems_saved h with
    | None -> Error No_saved_map
    | Some m ->
      Hashtbl.remove t.ems_saved h;
      Array.iteri (show t) m;
      Ok ()
