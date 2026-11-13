(* LIM EMS 4.0 — INT 67h.

   실기 386 에는 EMM386/QEMM 이 expanded memory 를 줬다. conventional 이
   모자라던 게임 런타임은 INT 21h AH=35h AL=67h 로 벡터를 읽어 그
   세그먼트 +0x0A 의 "EMMXXXX0"(DOS 디바이스 헤더의 이름 칸) 로 드라이버
   를 알아보고(삼국지3 런타임 실측) 페이지를 할당받아 프레임에 겹쳐
   쓴다. 이 드라이버가 지키는 기능: 상태·프레임·할당·매핑과 비우기·
   해제·버전(40h-46h), 핸들별 매핑 저장·복원(47h·48h), 핸들 조회
   (4Bh-4Dh), 재할당(51h), 매핑 가능 주소(58h), 0 페이지 할당(5Ah).
   출처는 LIM EMS 4.0 명세(1987, 300275-005)의 Function 번호로 적는다.

   프레임은 0xD000-0xEFFF(물리 페이지 넷). 페이지 내용은 호스트 배열에
   있고, 매핑은 프레임의 지금 내용을 옛 논리 페이지에 되저장한 뒤 새
   논리 페이지를 프레임에 복사한다 — 게스트가 프레임에 직접 쓴 값은
   다음 매핑까지 그 페이지에 묻어 간다. 소리 없는 실패는 없다: 못
   지키는 요청은 LIM 오류 코드로 답한다. *)

open Dos_state

let page_frame = 0xD000
let page_size = 0x400                            (* 16KB = 0x400 단락 *)
let frame_pages = 4
let page_bytes = page_size * 16
let emm_name = "EMMXXXX0"
let emm_version = 0x40                           (* 4.0 *)
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

(* 게스트 배열의 워드. 오프셋은 세그먼트 안에서 돈다. *)
let lin seg off = (seg lsl 4) + (off land 0xffff)
let poke16 t seg off v = wr8 t (lin seg off) v; wr8 t (lin seg (off + 1)) (v lsr 8)

(* 4Dh 와 58h 배열의 칸: 워드 둘. *)
let entry_bytes = 4

let service t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 in
  let al = Cpu86.reg8 cpu 0 in
  let bx = Cpu86.reg16 cpu 3 in
  let dx = Cpu86.reg16 cpu 2 in
  let es = Cpu86.seg cpu 0 and di = Cpu86.reg16 cpu 7 in
  let set_bx = Cpu86.set_reg16 cpu 3 and set_dx = Cpu86.set_reg16 cpu 2 in
  let set_al = Cpu86.set_reg8 cpu 0 in
  let result =
    match ah with
    | 0x40 -> set_al 0; Ok ()
    | 0x41 -> set_bx page_frame; Ok ()
    | 0x42 ->
      (* Function 3: BX 가 남은 페이지, DX 가 전체 페이지다. *)
      set_bx (free_pages t);
      set_dx total_pages;
      Ok ()
    | 0x43 -> Result.map set_dx (allocate t ~count:bx ~zero_ok:false)
    | 0x44 ->
      if al >= frame_pages then Error Physical_out_of_range
      else Result.map (show t al) (target t ~handle:dx ~log:bx)
    | 0x45 -> release t dx
    | 0x46 -> set_al emm_version; Ok ()
    | 0x47 -> save_map t dx
    | 0x48 -> restore_map t dx
    | 0x4B -> set_bx (Hashtbl.length t.ems_pages); Ok ()
    | 0x4C ->
      (match Hashtbl.find_opt t.ems_pages dx with
       | None -> Error Invalid_handle
       | Some pages -> set_bx (Array.length pages); Ok ())
    | 0x4D ->
      let handles =
        Hashtbl.fold (fun h pages acc -> (h, Array.length pages) :: acc) t.ems_pages []
        |> List.sort compare
      in
      List.iteri
        (fun i (h, n) ->
          poke16 t es (di + (i * entry_bytes)) h;
          poke16 t es (di + (i * entry_bytes) + 2) n)
        handles;
      set_bx (List.length handles);
      Ok ()
    | 0x51 ->
      (* 실패해도 BX 는 그 전 페이지 수다(Function 18 RESULTS). *)
      (match Hashtbl.find_opt t.ems_pages dx with
       | None -> Error Invalid_handle
       | Some pages ->
         let r = reallocate t dx ~count:bx in
         set_bx (match r with Ok () -> bx | Error _ -> Array.length pages);
         r)
    | 0x58 ->
      (match al with
       | 0x00 ->
         (* 매핑 가능 주소 배열: (세그먼트, 물리 페이지) 쌍 넷. *)
         for p = 0 to frame_pages - 1 do
           poke16 t es (di + (p * entry_bytes)) (page_frame + (p * page_size));
           poke16 t es (di + (p * entry_bytes) + 2) p
         done;
         Cpu86.set_reg16 cpu 1 frame_pages;
         Ok ()
       | 0x01 -> Cpu86.set_reg16 cpu 1 frame_pages; Ok ()
       | _ -> Error Invalid_subfunction)
    | 0x5A ->
      (* Function 27: 00h 표준, 01h 날 페이지. 이 드라이버에서는 둘이 같고,
         43h 와 달리 0 페이지도 받는다. *)
      (match al with
       | 0x00 | 0x01 -> Result.map set_dx (allocate t ~count:bx ~zero_ok:true)
       | _ -> Error Invalid_subfunction)
    | _ -> Error Undefined_function
  in
  Cpu86.set_reg8 cpu 4 (match result with Ok () -> 0 | Error s -> status_code s)
