(* LIM EMS 4.0 드라이버 — INT 67h.

   실기 386 에는 EMM386/QEMM 이 expanded memory 를 줬다. conventional 이
   모자라던 게임 런타임은 INT 21h AH=35h AL=67h 로 벡터를 읽어 그
   세그먼트 +0x0A 의 "EMMXXXX0"(DOS 디바이스 헤더의 이름 칸) 로 드라이버
   를 알아보고(삼국지3 런타임 실측) 페이지를 할당받아 프레임에 겹쳐
   쓴다. 이 모듈은 레지스터와 게스트 메모리의 배열을 읽고, expanded
   memory 모델(Dos_ems_pages)을 부른 뒤 결과를 레지스터와 배열에 둔다.
   출처는 LIM EMS 4.0 명세(1987, 300275-005)의 Function 번호로 적는다. *)

open Dos_state
module P = Dos_ems_pages

let emm_name = "EMMXXXX0"
let emm_version = 0x40                           (* 4.0 *)
let page_frame = P.page_frame
let frame_pages = P.frame_pages
let page_bytes = P.page_bytes
let os_handle = P.os_handle
let max_handle = P.max_handle
let boot_handles = P.boot_handles

(* ---------- 게스트 배열 ----------

   워드는 리틀 엔디언이고, 오프셋은 세그먼트 안에서 돈다.

   4Eh·4Fh 가 주고받는 배열의 모양은 드라이버가 정한다(Function 15:
   크기는 드라이버를 올린 뒤에 물어야 한다). 여기서는 물리 페이지마다
   핸들과 논리 페이지 두 워드이고, 빈 자리는 FFFFh 둘이다. 부분 배열은
   칸마다 그 앞에 물리 페이지 번호 워드를 붙이고, 맨 앞에 칸 수 워드를
   둔다. 59h 의 "context save area size" 는 전체 배열의 크기다. *)

let lin seg off = (seg lsl 4) + (off land 0xffff)
let peek16 t seg off = rd8 t (lin seg off) lor (rd8 t (lin seg (off + 1)) lsl 8)
let poke16 t seg off v = wr8 t (lin seg off) v; wr8 t (lin seg (off + 1)) (v lsr 8)

let word_bytes = 2
let entry_bytes = 2 * word_bytes
let empty_word = 0xFFFF
let map_bytes = frame_pages * entry_bytes
let partial_entry_bytes = word_bytes + entry_bytes
let partial_bytes n = word_bytes + (n * partial_entry_bytes)

let poke_entry t seg off = function
  | None -> poke16 t seg off empty_word; poke16 t seg (off + word_bytes) empty_word
  | Some (h, l) -> poke16 t seg off h; poke16 t seg (off + word_bytes) l

(* 지금 없는 핸들이나 페이지를 가리키는 칸은 A3h(배열이 깨졌다)다. *)
let peek_entry t seg off =
  let h = peek16 t seg off and l = peek16 t seg (off + word_bytes) in
  if h = empty_word && l = empty_word then Ok None
  else
    match P.target t ~handle:h ~log:l with
    | Ok (Some _ as shown) -> Ok shown
    | Ok None | Error _ -> Error P.Corrupt_map

(* [f 0] .. [f (n-1)] 을 차례로, 처음 실패에서 멈춘다. *)
let collect n f =
  let rec go i acc =
    if i = n then Ok (List.rev acc)
    else match f i with Error e -> Error e | Ok x -> go (i + 1) (x :: acc)
  in
  go 0 []

(* Function 15. 읽은 칸은 모두 검사한 뒤에 겹친다 — 깨진 배열은 아무것도
   바꾸지 않는다. *)
let get_map t seg off =
  Array.iteri (fun p shown -> poke_entry t seg (off + (p * entry_bytes)) shown) t.ems_mapped

let set_map t seg off =
  collect frame_pages (fun p -> peek_entry t seg (off + (p * entry_bytes)))
  |> Result.map (List.iteri (P.show t))

(* Function 16. 00h 의 입력은 칸 수 워드와 매핑 가능 세그먼트 워드들이다. *)
let get_partial t ~list_seg ~list_off ~seg ~off =
  let n = peek16 t list_seg list_off in
  if n > frame_pages then Error P.Corrupt_map
  else
    let physical i =
      match P.physical_of_segment (peek16 t list_seg (list_off + word_bytes + (i * word_bytes))) with
      | Some p -> Ok p
      | None -> Error P.Physical_out_of_range
    in
    collect n physical
    |> Result.map (fun pages ->
           poke16 t seg off n;
           List.iteri
             (fun i p ->
               let at = off + word_bytes + (i * partial_entry_bytes) in
               poke16 t seg at p;
               poke_entry t seg (at + word_bytes) t.ems_mapped.(p))
             pages)

let set_partial t seg off =
  let n = peek16 t seg off in
  if n > frame_pages then Error P.Corrupt_map
  else
    let entry i =
      let at = off + word_bytes + (i * partial_entry_bytes) in
      let p = peek16 t seg at in
      if p >= frame_pages then Error P.Corrupt_map
      else Result.map (fun shown -> (p, shown)) (peek_entry t seg (at + word_bytes))
    in
    collect n entry |> Result.map (List.iter (fun (p, shown) -> P.show t p shown))

(* Function 17. 명세대로 앞에서부터 겹치고 틀린 칸에서 멈춘다 — 그 앞
   칸들은 겹친 채로 남는다(8Ah·8Bh 설명). *)
let map_multiple t ~handle ~count ~seg ~off ~physical =
  if count > frame_pages then Error P.Physical_out_of_range
  else
    let rec go i =
      if i = count then Ok ()
      else
        let at = off + (i * entry_bytes) in
        match physical (peek16 t seg (at + word_bytes)) with
        | None -> Error P.Physical_out_of_range
        | Some p ->
          (match P.target t ~handle ~log:(peek16 t seg at) with
           | Error e -> Error e
           | Ok shown -> P.show t p shown; go (i + 1))
    in
    go 0

let physical_of_number p = if p < frame_pages then Some p else None

(* Function 26 00h: 날 페이지 크기(단락), 대체 레지스터 세트 수, 매핑 저장
   배열 크기, DMA 레지스터 세트 수, DMA 방식. 대체·DMA 세트는 없다. *)
let hardware_info = [ P.page_size; 0; map_bytes; 0; 0 ]

let service t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 in
  let al = Cpu86.reg8 cpu 0 in
  let bx = Cpu86.reg16 cpu 3 in
  let cx = Cpu86.reg16 cpu 1 in
  let dx = Cpu86.reg16 cpu 2 in
  let es = Cpu86.seg cpu 0 and di = Cpu86.reg16 cpu 7 in
  let ds = Cpu86.seg cpu 3 and si = Cpu86.reg16 cpu 6 in
  let set_bx = Cpu86.set_reg16 cpu 3 and set_dx = Cpu86.set_reg16 cpu 2 in
  let set_al = Cpu86.set_reg8 cpu 0 in
  let result =
    match ah with
    | 0x40 -> set_al 0; Ok ()
    | 0x41 -> set_bx page_frame; Ok ()
    | 0x42 ->
      (* Function 3: BX 가 남은 페이지, DX 가 전체 페이지다. *)
      set_bx (P.free_pages t);
      set_dx P.total_pages;
      Ok ()
    | 0x43 -> Result.map set_dx (P.allocate t ~count:bx ~zero_ok:false)
    | 0x44 ->
      if al >= frame_pages then Error P.Physical_out_of_range
      else Result.map (P.show t al) (P.target t ~handle:dx ~log:bx)
    | 0x45 -> P.release t dx
    | 0x46 -> set_al emm_version; Ok ()
    | 0x47 -> P.save_map t dx
    | 0x48 -> P.restore_map t dx
    | 0x4B -> set_bx (Hashtbl.length t.ems_pages); Ok ()
    | 0x4C ->
      (match Hashtbl.find_opt t.ems_pages dx with
       | None -> Error P.Invalid_handle
       | Some pages -> set_bx (Array.length pages); Ok ())
    | 0x4D ->
      let handles =
        Hashtbl.fold (fun h pages acc -> (h, Array.length pages) :: acc) t.ems_pages []
        |> List.sort compare
      in
      List.iteri
        (fun i (h, n) ->
          poke16 t es (di + (i * entry_bytes)) h;
          poke16 t es (di + (i * entry_bytes) + word_bytes) n)
        handles;
      set_bx (List.length handles);
      Ok ()
    | 0x4E ->
      (match al with
       | 0x00 -> get_map t es di; Ok ()
       | 0x01 -> set_map t ds si
       | 0x02 -> get_map t es di; set_map t ds si
       | 0x03 -> set_al map_bytes; Ok ()
       | _ -> Error P.Invalid_subfunction)
    | 0x4F ->
      (match al with
       | 0x00 -> get_partial t ~list_seg:ds ~list_off:si ~seg:es ~off:di
       | 0x01 -> set_partial t ds si
       | 0x02 ->
         if bx > frame_pages then Error P.Physical_out_of_range
         else (set_al (partial_bytes bx); Ok ())
       | _ -> Error P.Invalid_subfunction)
    | 0x50 ->
      (match al with
       | 0x00 ->
         map_multiple t ~handle:dx ~count:cx ~seg:ds ~off:si ~physical:physical_of_number
       | 0x01 ->
         map_multiple t ~handle:dx ~count:cx ~seg:ds ~off:si ~physical:P.physical_of_segment
       | _ -> Error P.Invalid_subfunction)
    | 0x51 ->
      (* 실패해도 BX 는 그 전 페이지 수다(Function 18 RESULTS). *)
      (match Hashtbl.find_opt t.ems_pages dx with
       | None -> Error P.Invalid_handle
       | Some pages ->
         let r = P.reallocate t dx ~count:bx in
         set_bx (match r with Ok () -> bx | Error _ -> Array.length pages);
         r)
    | 0x58 ->
      (match al with
       | 0x00 ->
         (* 매핑 가능 주소 배열: (세그먼트, 물리 페이지) 쌍 넷. *)
         for p = 0 to frame_pages - 1 do
           poke16 t es (di + (p * entry_bytes)) (page_frame + (p * P.page_size));
           poke16 t es (di + (p * entry_bytes) + word_bytes) p
         done;
         Cpu86.set_reg16 cpu 1 frame_pages;
         Ok ()
       | 0x01 -> Cpu86.set_reg16 cpu 1 frame_pages; Ok ()
       | _ -> Error P.Invalid_subfunction)
    | 0x59 ->
      (match al with
       | 0x00 ->
         List.iteri (fun i v -> poke16 t es (di + (i * word_bytes)) v) hardware_info;
         Ok ()
       | 0x01 ->
         (* 날 페이지가 16KB 라 표준 페이지 수와 같다. *)
         set_bx (P.free_pages t);
         set_dx P.total_pages;
         Ok ()
       | _ -> Error P.Invalid_subfunction)
    | 0x5A ->
      (* Function 27: 00h 표준, 01h 날 페이지. 이 드라이버에서는 둘이 같고,
         43h 와 달리 0 페이지도 받는다. *)
      (match al with
       | 0x00 | 0x01 -> Result.map set_dx (P.allocate t ~count:bx ~zero_ok:true)
       | _ -> Error P.Invalid_subfunction)
    | _ -> Error P.Undefined_function
  in
  Cpu86.set_reg8 cpu 4 (match result with Ok () -> 0 | Error s -> P.status_code s)
