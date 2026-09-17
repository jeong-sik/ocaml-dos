(* LIM EMS 4.0 스텁 — INT 67h.

   실기 386 에는 EMM386/QEMM 이 expanded memory 를 줬다. conventional 이
   모자라던 게임 런타임은 INT 21h AH=35h AL=67h 로 벡터를 읽어 그
   세그먼트 +0x0A 의 "EMMXXXX0"(DOS 디바이스 헤더의 이름 칸) 로 드라이버
   를 알아보고(삼국지3 런타임 실측) 페이지를 할당받아 프레임에 겹쳐
   쓴다. 이 스텁은 그 계약의 쓰는 부분만 지킨다: 상태·프레임·할당·
   매핑·해제·버전·매핑 가능 주소.

   프레임은 0xD000-0xEFFF(물리 페이지 넷). 페이지 내용은 호스트 배열에
   있고, 매핑은 프레임의 지금 내용을 옛 논리 페이지에 되저장한 뒤 새
   논리 페이지를 프레임에 복사한다 — 게스트가 프레임에 직접 쓴 값은
   다음 매핑까지 그 페이지에 묻어 간다. 소리 없는 실패는 없다: 못
   지키는 요청은 LIM 오류 코드로 답한다. *)

open Dos_state

let page_frame = 0xD000
let page_size = 0x400                            (* 16KB = 0x400 단락 *)
let frame_pages = 4
let emm_name = "EMMXXXX0"
let emm_version = 0x40                           (* 4.0 *)
let total_pages = 0x0800                         (* 32MB — 통보용. 카운트를
                                                    BX 로 읽는 관례(아래 0x43)
                                                    에서는 여유 계산이 필요
                                                    없지만 넉넉히 둔다 *)

let frame_addr p = (page_frame * 16) + (p * page_size * 16)

let free_pages t =
  let used =
    Hashtbl.fold (fun _ pages acc -> acc + Array.length pages) t.ems_pages 0
  in
  max 0 (total_pages - used)

(* 페이지 실체는 처음 건드릴 때 만든다 — "남은 전부"를 받아가도 실제로
   겹치는 몇 장만 산다. *)
let ensure_page pages i =
  if Bytes.length pages.(i) = 0 then
    pages.(i) <- Bytes.make (page_size * 16) '\000';
  pages.(i)

let service t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 in
  let al = Cpu86.reg8 cpu 0 in
  let ok () = Cpu86.set_reg8 cpu 4 0 in
  let err code = Cpu86.set_reg8 cpu 4 code in
  match ah with
  | 0x40 ->
    (* 상태 *)
    ok ();
    Cpu86.set_reg8 cpu 0 0
  | 0x41 ->
    ok ();
    Cpu86.set_reg16 cpu 3 page_frame
  | 0x42 ->
    ok ();
    Cpu86.set_reg16 cpu 3 total_pages;
    Cpu86.set_reg16 cpu 2 (free_pages t)
  | 0x43 ->
    (* KOEI 런타임은 요청 페이지 수를 BX 에 싣는다(실측: 두 호출 모두
       bx=1). DX 는 AH=42h 의 잔여값이 그대로 남아 LIM 표준(DX=카운트)
       대로 읽으면 첫 호출이 잔여 전부를 삼켜 둘째 할당 검사(잔여<요청)
       가 죽는다 — 이 게임이 돌아간 기계의 EMB 은 BX 를 읽었다는 뜻이다.
       우리가 재현하는 기계도 그 관례를 따른다. 페이지 실체는 처음 겹칠
       때 만든다. *)
    let want = Cpu86.reg16 cpu 3 in
    if want = 0 then err 0x89                    (* 0 페이지는 못 받는다 *)
    else if want > free_pages t then err 0x87    (* 페이지 부족 *)
    else begin
      let h = t.ems_next_handle in
      t.ems_next_handle <- h + 1;
      Hashtbl.replace t.ems_pages h (Array.make want Bytes.empty);
      Cpu86.set_reg16 cpu 2 h;
      ok ()
    end
  | 0x44 ->
    let phys = al and log = Cpu86.reg16 cpu 3 and h = Cpu86.reg16 cpu 2 in
    if phys >= frame_pages then err 0x8B         (* 물리 페이지 범위 밖 *)
    else begin
      match Hashtbl.find_opt t.ems_pages h with
      | None -> err 0x83                         (* 알 수 없는 핸들 *)
      | Some pages ->
        if log >= Array.length pages then err 0x8A
        else begin
          (* 프레임의 지금 내용을 옛 논리 페이지에 되저장하고 새 페이지를
             겹친다. *)
          (match t.ems_mapped.(phys) with
           | oh, ol when oh > 0 && (oh, ol) <> (h, log) ->
             (match Hashtbl.find_opt t.ems_pages oh with
              | Some opages when ol < Array.length opages ->
                Bytes.blit t.mem (frame_addr phys) (ensure_page opages ol) 0
                  (page_size * 16)
              | _ -> ())
           | _ -> ());
          Bytes.blit (ensure_page pages log) 0 t.mem (frame_addr phys)
            (page_size * 16);
          t.ems_mapped.(phys) <- (h, log);
          ok ()
        end
    end
  | 0x45 ->
    let h = Cpu86.reg16 cpu 2 in
    if Hashtbl.mem t.ems_pages h then begin
      Hashtbl.remove t.ems_pages h;
      for p = 0 to frame_pages - 1 do
        match t.ems_mapped.(p) with
        | mh, _ when mh = h -> t.ems_mapped.(p) <- (0, 0)
        | _ -> ()
      done;
      ok ()
    end
    else err 0x83
  | 0x46 ->
    ok ();
    Cpu86.set_reg8 cpu 0 emm_version
  | 0x58 ->
    (match al with
     | 0x00 ->
       (* 매핑 가능 주소 배열: (세그먼트, 물리 페이지) 쌍 넷. *)
       let es = Cpu86.seg cpu 0 and di = Cpu86.reg16 cpu 7 in
       for p = 0 to frame_pages - 1 do
         wr16 t (((es lsl 4) + di + (p * 4)) land 0xfffff)
           (page_frame + (p * page_size));
         wr16 t (((es lsl 4) + di + (p * 4) + 2) land 0xfffff) p
       done;
       Cpu86.set_reg16 cpu 1 frame_pages;
       ok ()
     | 0x01 ->
       Cpu86.set_reg16 cpu 1 frame_pages;
       ok ()
     | _ -> err 0x8F)
  | _ -> err 0x84                                 (* 모르는 기능 *)
