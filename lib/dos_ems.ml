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

(* 4Dh 와 58h 배열의 칸: 워드 둘. 오프셋은 세그먼트 안에서 돈다. *)
let lin seg off = (seg lsl 4) + (off land 0xffff)
let poke16 t seg off v = wr8 t (lin seg off) v; wr8 t (lin seg (off + 1)) (v lsr 8)
let word_bytes = 2
let entry_bytes = 2 * word_bytes

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
    | 0x5A ->
      (* Function 27: 00h 표준, 01h 날 페이지. 이 드라이버에서는 둘이 같고,
         43h 와 달리 0 페이지도 받는다. *)
      (match al with
       | 0x00 | 0x01 -> Result.map set_dx (P.allocate t ~count:bx ~zero_ok:true)
       | _ -> Error P.Invalid_subfunction)
    | _ -> Error P.Undefined_function
  in
  Cpu86.set_reg8 cpu 4 (match result with Ok () -> 0 | Error s -> P.status_code s)
