(* DOS 표면 — INT 21h 와 INT 20h.

   파일 계약: 하네스가 마운트한 것만 보인다. 호스트 경로로 나가는 길은
   없다. 쓰기는 메모리 사본에 쌓였다가 닫을 때 마운트 표에 되돌아간다 —
   같은 세션 안에서 저장하고 다시 불러오는 게 그래서 된다(게임 세이브).
   호스트 디스크의 원본은 바뀌지 않는다.

   메모리 계약: 프로그램 뒤부터 0x9FFF 까지가 할당 가능한 자리다.
   첫 맞는 자리(first fit)로 준다 — MCB 사슬을 게스트에게 보여주지는
   않는다. 사슬을 직접 훑는 프로그램은 이 모델 밖이고, 그때는 조용히
   틀리는 대신 할당 실패로 답한다. *)

open Dos_state

let dos_version_major = 5
let dos_version_minor = 0
let default_drive = 2                  (* C: *)
let conv_mem_top = 0x9FFF              (* 비디오 메모리 바로 앞 *)

(* ---------- 이름과 와일드카드 ---------- *)

(* "NAME.EXT" 를 8+3 칸으로. 비교는 여기서만 한다 — 대소문자 무시. *)
let to_8_3 name =
  let name = String.uppercase_ascii name in
  let base, ext =
    match String.rindex_opt name '.' with
    | Some i ->
      (String.sub name 0 i, String.sub name (i + 1) (String.length name - i - 1))
    | None -> (name, "")
  in
  let pad s n =
    if String.length s >= n then String.sub s 0 n
    else s ^ String.make (n - String.length s) ' '
  in
  (pad base 8, pad ext 3)

(* DOS 의 '*' 는 그 칸의 남은 자리를 전부 '?' 로 바꾸는 것과 같다. *)
let expand_stars field =
  let b = Bytes.make (String.length field) ' ' in
  let star = ref false in
  String.iteri
    (fun i c ->
      if !star then Bytes.set b i '?'
      else if c = '*' then begin star := true; Bytes.set b i '?' end
      else Bytes.set b i c)
    field;
  Bytes.to_string b

let matches_pattern ~pattern ~name =
  let pb, pe = to_8_3 pattern and nb, ne = to_8_3 name in
  let pb = expand_stars pb and pe = expand_stars pe in
  let cmp p n =
    let okay = ref true in
    String.iteri (fun i c -> if c <> '?' && c <> n.[i] then okay := false) p;
    !okay
  in
  cmp pb nb && cmp pe ne

(* ---------- 열린 파일 ---------- *)

let is_console h = h >= 0 && h <= 2

let open_handle t name data =
  let h = t.next_handle in
  t.next_handle <- t.next_handle + 1;
  Hashtbl.replace t.handles h { hname = name; data; pos = 0 };
  h

(* 닫을 때 마운트 표로 되돌린다 — 저장한 파일을 같은 세션에서 다시
   열 수 있어야 게임의 세이브/로드가 성립한다. *)
let close_handle t h =
  match Hashtbl.find_opt t.handles h with
  | None -> false
  | Some hd ->
    if hd.hname <> "" then Hashtbl.replace t.host_files hd.hname hd.data;
    Hashtbl.remove t.handles h;
    true

(* ---------- 메모리 할당 ---------- *)

let block_end (seg, paras) = seg + paras

let largest_free t =
  let sorted = List.sort compare t.blocks in
  let best = ref 0 and cursor = ref t.free_base in
  List.iter
    (fun b ->
      let seg, _ = b in
      if seg - !cursor > !best then best := seg - !cursor;
      if block_end b > !cursor then cursor := block_end b)
    sorted;
  if t.free_top - !cursor > !best then best := t.free_top - !cursor;
  max 0 !best

let allocate t paras =
  let sorted = List.sort compare t.blocks in
  let cursor = ref t.free_base and placed = ref None in
  List.iter
    (fun b ->
      let seg, _ = b in
      if !placed = None && seg - !cursor >= paras then placed := Some !cursor;
      if block_end b > !cursor then cursor := block_end b)
    sorted;
  if !placed = None && t.free_top - !cursor >= paras then placed := Some !cursor;
  match !placed with
  | Some seg -> t.blocks <- (seg, paras) :: t.blocks; Some seg
  | None -> None

let free_block t seg =
  if List.mem_assoc seg t.blocks then begin
    t.blocks <- List.remove_assoc seg t.blocks;
    true
  end
  else false

let resize_block t seg paras =
  match List.assoc_opt seg t.blocks with
  | None -> Error 9                        (* 잘못된 블록 주소 *)
  | Some cur ->
    if paras <= cur then begin
      t.blocks <- (seg, paras) :: List.remove_assoc seg t.blocks;
      Ok ()
    end
    else begin
      (* 뒤로 늘릴 자리가 있는가 *)
      let limit =
        List.fold_left
          (fun acc b ->
            let s, _ = b in
            if s > seg && s < acc then s else acc)
          t.free_top
          (List.remove_assoc seg t.blocks |> List.map (fun (s, p) -> (s, p)))
      in
      if seg + paras <= limit then begin
        t.blocks <- (seg, paras) :: List.remove_assoc seg t.blocks;
        Ok ()
      end
      else Error (limit - seg)             (* 최대 가능 크기를 알린다 *)
    end

(* ---------- findfirst / findnext ---------- *)

let fill_find_block t name =
  let data =
    match Hashtbl.find_opt t.host_files name with
    | Some d -> d
    | None -> Bytes.empty
  in
  let size = Bytes.length data in
  wr8 t (t.dta + 0x15) 0x20;                 (* 속성: 보관 *)
  wr16 t (t.dta + 0x16) 0;                   (* 시각 *)
  wr16 t (t.dta + 0x18) 0x2821;              (* 날짜: 1000-01-01 자리표시 *)
  wr16 t (t.dta + 0x1A) (size land 0xffff);
  wr16 t (t.dta + 0x1C) ((size lsr 16) land 0xffff);
  String.iteri (fun i c -> wr8 t (t.dta + 0x1E + i) (Char.code c)) name;
  wr8 t (t.dta + 0x1E + String.length name) 0

(* ---------- FCB ---------- *)

let fcb_name t fcb =
  let take n off =
    let b = Buffer.create 12 in
    for i = 0 to n - 1 do
      let c = rd8 t (fcb + off + i) in
      if c <> 0x20 then Buffer.add_char b (Char.chr c)
    done;
    Buffer.contents b
  in
  let base = take 8 1 and ext = take 3 9 in
  if ext = "" then base else base ^ "." ^ ext

let fcb_service t ah =
  let cpu = t.cpu in
  let fcb = seg_off t 3 2 in
  match ah with
  | 0x0F ->
    let name = String.uppercase_ascii (fcb_name t fcb) in
    (match Hashtbl.find_opt t.host_files name with
     | Some data ->
       Hashtbl.replace t.fcbs fcb (data, 0);
       wr16 t (fcb + 0x0E) 128;
       wr16 t (fcb + 0x10) (Bytes.length data land 0xffff);
       wr16 t (fcb + 0x12) ((Bytes.length data lsr 16) land 0xffff);
       for i = 0 to 3 do wr8 t (fcb + 0x20 + i) 0 done;
       Cpu86.set_reg8 cpu 0 0
     | None -> Cpu86.set_reg8 cpu 0 0xFF)
  | 0x10 -> Hashtbl.remove t.fcbs fcb; Cpu86.set_reg8 cpu 0 0
  | 0x14 ->
    let recsize = max 1 (rd16 t (fcb + 0x0E)) in
    (match Hashtbl.find_opt t.fcbs fcb with
     | Some (data, pos) ->
       let avail = Bytes.length data - pos in
       if avail <= 0 then Cpu86.set_reg8 cpu 0 1
       else begin
         let take = min recsize avail in
         Bytes.blit data pos t.mem t.dta take;
         Hashtbl.replace t.fcbs fcb (data, pos + take);
         let recno = rd8 t (fcb + 0x20) + 1 in
         if recno >= 128 then begin
           wr16 t (fcb + 0x0C) (rd16 t (fcb + 0x0C) + 1);
           wr8 t (fcb + 0x20) 0
         end
         else wr8 t (fcb + 0x20) recno;
         Cpu86.set_reg8 cpu 0 (if take < recsize then 1 else 0)
       end
     | None -> Cpu86.set_reg8 cpu 0 0xFF)
  | 0x21 ->
    let recsize = max 1 (rd16 t (fcb + 0x0E)) in
    let recno =
      rd8 t (fcb + 0x21) lor (rd8 t (fcb + 0x22) lsl 8)
      lor (rd8 t (fcb + 0x23) lsl 16)
    in
    (match Hashtbl.find_opt t.fcbs fcb with
     | Some (data, _) ->
       let off = recno * recsize in
       if off >= Bytes.length data then Cpu86.set_reg8 cpu 0 1
       else begin
         let take = min recsize (Bytes.length data - off) in
         Bytes.blit data off t.mem t.dta take;
         Hashtbl.replace t.fcbs fcb (data, off + take);
         Cpu86.set_reg8 cpu 0 (if take < recsize then 1 else 0)
       end
     | None -> Cpu86.set_reg8 cpu 0 0xFF)
  | _ -> Cpu86.set_reg8 cpu 0 0

(* ---------- 콘솔 입력 ---------- *)

(* 실기는 키가 올 때까지 블록한다. 빈 링이면 굶주림을 올리고 0 을
   돌려준다 — 하네스가 그걸 보고 키를 넣는다. *)
let console_read t =
  if key_pending t then begin
    t.kbd_wait <- false;
    Some (pop_key t)
  end
  else begin
    t.kbd_wait <- true;
    None
  end

let ascii_of_key w =
  let a = w land 0xff in
  if a = 0 then 0 else a

(* ---------- INT 21h ---------- *)

let service t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 in
  match ah with
  | 0x00 -> t.exited <- true; t.exit_code <- 0
  | 0x4C -> t.exited <- true; t.exit_code <- Cpu86.reg8 cpu 0
  | 0x4D -> Cpu86.set_reg16 cpu 0 t.exit_code
  | 0x01 ->
    (match console_read t with
     | Some w ->
       let c = ascii_of_key w in
       Cpu86.set_reg8 cpu 0 c;
       if c <> 0 then put_char t c 0x07
     | None -> Cpu86.set_reg8 cpu 0 0)
  | 0x07 | 0x08 ->
    (match console_read t with
     | Some w -> Cpu86.set_reg8 cpu 0 (ascii_of_key w)
     | None -> Cpu86.set_reg8 cpu 0 0)
  | 0x02 -> put_char t (Cpu86.reg8 cpu 2) 0x07
  | 0x06 ->
    (* 직접 콘솔 입출력. DL=FF 면 읽기(ZF 로 있고없음), 아니면 쓰기.
       Turbo Pascal 런타임의 에러 메시지가 이 길로 나온다 — 없으면
       화면에 아무 흔적 없이 죽는다(실측: "Runtime error 006"). *)
    let dl = Cpu86.reg8 cpu 2 in
    if dl = 0xFF then
      if key_pending t then begin
        Cpu86.set_reg8 cpu 0 (ascii_of_key (pop_key t));
        Cpu86.set_flags cpu (Cpu86.flags cpu land lnot Cpu86.f_zero)
      end
      else begin
        Cpu86.set_reg8 cpu 0 0;
        Cpu86.set_flags cpu (Cpu86.flags cpu lor Cpu86.f_zero)
      end
    else put_char t dl 0x07
  | 0x09 ->
    (* '$' 로 끝나는 문자열. 길이를 안 주는 대신 끝 표시를 찾는다. *)
    let base = seg_off t 3 2 in
    let i = ref 0 and stop = ref false in
    while (not !stop) && !i < 65536 do
      let c = rd8 t (base + !i) in
      if c = Char.code '$' then stop := true
      else begin put_char t c 0x07; incr i end
    done
  | 0x0A ->
    (* 줄 입력: DS:DX 의 0 번째 바이트가 최대 길이, 1 번째가 실제 길이,
       2 번째부터 글자. CR 을 만나면 끝난다. 키가 없으면 굶주림만
       올리고 돌아간다 — 하네스가 키를 채우면 게스트가 다시 부른다. *)
    let base = seg_off t 3 2 in
    let limit = rd8 t base in
    let n = ref (rd8 t (base + 1)) in
    let stop = ref false in
    while (not !stop) && !n < limit - 1 do
      match console_read t with
      | None -> stop := true
      | Some w ->
        let c = ascii_of_key w in
        if c = 0x0D then begin
          wr8 t (base + 2 + !n) 0x0D;
          wr8 t (base + 1) !n;
          put_char t 0x0D 0x07;
          put_char t 0x0A 0x07;
          stop := true;
          n := limit                       (* 끝났다는 표시 *)
        end
        else if c = 0x08 then begin
          if !n > 0 then begin decr n; put_char t 0x08 0x07 end
        end
        else if c <> 0 then begin
          wr8 t (base + 2 + !n) c;
          incr n;
          put_char t c 0x07
        end
    done;
    if !n < limit then wr8 t (base + 1) !n
  | 0x0B ->
    Cpu86.set_reg8 cpu 0 (if key_pending t then 0xFF else 0x00);
    if not (key_pending t) then t.kbd_wait <- true
  | 0x0C ->
    (* 버퍼를 비우고 AL 이 가리키는 입력 함수를 다시 부른다 *)
    while key_pending t do ignore (pop_key t) done;
    Cpu86.set_reg8 cpu 4 (Cpu86.reg8 cpu 0);
    (match Cpu86.reg8 cpu 0 with
     | 0x01 | 0x06 | 0x07 | 0x08 | 0x0A -> ()
     | _ -> Cpu86.set_reg8 cpu 0 0)
  | 0x0D -> ()                              (* 디스크 리셋 *)
  | 0x0E -> Cpu86.set_reg8 cpu 0 (default_drive + 1)
  | 0x19 -> Cpu86.set_reg8 cpu 0 default_drive
  | 0x1A -> t.dta <- seg_off t 3 2
  | 0x2F ->
    Cpu86.set_seg cpu 0 ((t.dta lsr 4) land 0xffff);
    Cpu86.set_reg16 cpu 3 (t.dta land 0x0f)
  | 0x25 ->
    let v = Cpu86.reg8 cpu 0 in
    wr16 t (v * 4) (Cpu86.reg16 cpu 2);
    wr16 t ((v * 4) + 2) (Cpu86.seg cpu 3)
  | 0x35 ->
    let v = Cpu86.reg8 cpu 0 in
    Cpu86.set_reg16 cpu 3 (rd16 t (v * 4));
    Cpu86.set_seg cpu 0 (rd16 t ((v * 4) + 2))
  | 0x2A ->
    let y, m, d, _, _, _ = now_fields t in
    Cpu86.set_reg16 cpu 1 y;
    Cpu86.set_reg8 cpu 6 m;
    Cpu86.set_reg8 cpu 2 d;
    Cpu86.set_reg8 cpu 0 (day_of_week (y, m, d))
  | 0x2B ->
    t.epoch_year <- Cpu86.reg16 cpu 1;
    t.epoch_month <- Cpu86.reg8 cpu 6;
    t.epoch_day <- Cpu86.reg8 cpu 2;
    Cpu86.set_reg8 cpu 0 0
  | 0x2C ->
    let _, _, _, h, mi, s = now_fields t in
    Cpu86.set_reg8 cpu 5 h;
    Cpu86.set_reg8 cpu 1 mi;
    Cpu86.set_reg8 cpu 6 s;
    Cpu86.set_reg8 cpu 2 0
  | 0x2D ->
    t.epoch_hour <- Cpu86.reg8 cpu 5;
    t.epoch_min <- Cpu86.reg8 cpu 1;
    t.epoch_sec <- Cpu86.reg8 cpu 6;
    Cpu86.set_reg8 cpu 0 0
  | 0x30 ->
    Cpu86.set_reg8 cpu 0 dos_version_major;
    Cpu86.set_reg8 cpu 4 dos_version_minor;
    Cpu86.set_reg8 cpu 3 0xFF;              (* OEM: 일반 *)
    Cpu86.set_reg16 cpu 1 0
  | 0x33 -> Cpu86.set_reg8 cpu 2 0          (* Ctrl-Break 검사 꺼짐 *)
  | 0x36 ->
    (* 남은 디스크 공간: 섹터/클러스터, 가용 클러스터, 섹터 크기,
       전체 클러스터. 마운트 표는 메모리 위라 넉넉한 값을 답한다. *)
    Cpu86.set_reg16 cpu 0 4;
    Cpu86.set_reg16 cpu 3 0x1000;
    Cpu86.set_reg16 cpu 1 512;
    Cpu86.set_reg16 cpu 2 0x2000
  | 0x39 | 0x3A | 0x3B ->
    (* 디렉터리가 없는 모델이다. 조용한 성공 대신 '경로 없음' 으로
       답한다 — 성공으로 속이면 게스트가 그 뒤를 헛돈다. *)
    fail t 3
  | 0x3C | 0x5B ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    if ah = 0x5B && Hashtbl.mem t.host_files name then fail t 0x50
    else begin
      Hashtbl.replace t.host_files name Bytes.empty;
      let h = open_handle t name Bytes.empty in
      Cpu86.set_reg16 cpu 0 h;
      ok t
    end
  | 0x3D ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    (match Hashtbl.find_opt t.host_files name with
     | Some data ->
       Cpu86.set_reg16 cpu 0 (open_handle t name data);
       ok t
     | None -> fail t 2)
  | 0x3E -> if close_handle t (Cpu86.reg16 cpu 3) then ok t else fail t 6
  | 0x45 ->
    (match Hashtbl.find_opt t.handles (Cpu86.reg16 cpu 3) with
     | Some hd ->
       let h = t.next_handle in
       t.next_handle <- t.next_handle + 1;
       Hashtbl.replace t.handles h { hd with pos = hd.pos };
       Cpu86.set_reg16 cpu 0 h;
       ok t
     | None -> fail t 6)
  | 0x46 ->
    (match Hashtbl.find_opt t.handles (Cpu86.reg16 cpu 3) with
     | Some hd ->
       Hashtbl.replace t.handles (Cpu86.reg16 cpu 1) { hd with pos = hd.pos };
       ok t
     | None -> fail t 6)
  | 0x3F ->
    let h = Cpu86.reg16 cpu 3 in
    let want = Cpu86.reg16 cpu 1 in
    let dst = seg_off t 3 2 in
    if h = 0 then begin
      (* stdin: 키 링에서 CR 까지. 키가 없으면 굶주림을 올린다. *)
      let n = ref 0 and stop = ref false in
      while (not !stop) && !n < want do
        match console_read t with
        | None -> stop := true
        | Some w ->
          let c = ascii_of_key w in
          if c = 0 then ()
          else begin
            wr8 t (dst + !n) c;
            incr n;
            if c = 0x0D && !n < want then begin
              wr8 t (dst + !n) 0x0A; incr n; stop := true
            end
          end
      done;
      Cpu86.set_reg16 cpu 0 !n;
      ok t
    end
    else if is_console h then begin Cpu86.set_reg16 cpu 0 0; ok t end
    else
      (match Hashtbl.find_opt t.handles h with
       | None -> fail t 6
       | Some hd ->
         let take = min want (max 0 (Bytes.length hd.data - hd.pos)) in
         Bytes.blit hd.data hd.pos t.mem dst take;
         hd.pos <- hd.pos + take;
         Cpu86.set_reg16 cpu 0 take;
         ok t)
  | 0x40 ->
    let h = Cpu86.reg16 cpu 3 in
    let n = Cpu86.reg16 cpu 1 in
    let src = seg_off t 3 2 in
    if is_console h then begin
      (* 실기 DOS 는 부팅 때 stdin/stdout/stderr 를 열어 준다 — 없으면
         런타임이 잘못된 핸들(6)로 즉사한다(실측). *)
      for i = 0 to n - 1 do put_char t (rd8 t (src + i)) 0x07 done;
      Cpu86.set_reg16 cpu 0 n;
      ok t
    end
    else
      (match Hashtbl.find_opt t.handles h with
       | None -> fail t 6
       | Some hd ->
         let chunk = Bytes.create n in
         for i = 0 to n - 1 do
           Bytes.set chunk i (Char.chr (rd8 t (src + i)))
         done;
         let len = Bytes.length hd.data in
         let grown =
           if hd.pos + n <= len then begin
             let copy = Bytes.copy hd.data in
             Bytes.blit chunk 0 copy hd.pos n;
             copy
           end
           else begin
             let head = Bytes.sub hd.data 0 (min hd.pos len) in
             let gap =
               if hd.pos > len then Bytes.make (hd.pos - len) '\000'
               else Bytes.empty
             in
             Bytes.concat Bytes.empty [ head; gap; chunk ]
           end
         in
         hd.data <- grown;
         hd.pos <- hd.pos + n;
         Cpu86.set_reg16 cpu 0 n;
         ok t)
  | 0x41 ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    if Hashtbl.mem t.host_files name then begin
      Hashtbl.remove t.host_files name;
      ok t
    end
    else fail t 2
  | 0x42 ->
    let h = Cpu86.reg16 cpu 3 in
    let off32 = (Cpu86.reg16 cpu 1 lsl 16) lor Cpu86.reg16 cpu 2 in
    (match Hashtbl.find_opt t.handles h with
     | None -> fail t 6
     | Some hd ->
       let signed =
         if off32 >= 0x80000000 then off32 - 0x100000000 else off32 in
       let size = Bytes.length hd.data in
       let newpos =
         match Cpu86.reg8 cpu 0 with
         | 0 -> signed
         | 1 -> hd.pos + signed
         | _ -> size + signed
       in
       if newpos < 0 then fail t 6
       else begin
         hd.pos <- newpos;
         Cpu86.set_reg16 cpu 0 (newpos land 0xffff);
         Cpu86.set_reg16 cpu 2 ((newpos lsr 16) land 0xffff);
         ok t
       end)
  | 0x43 ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    if Hashtbl.mem t.host_files name then begin
      if Cpu86.reg8 cpu 0 = 0 then Cpu86.set_reg16 cpu 1 0x20;
      ok t
    end
    else fail t 2
  | 0x44 ->
    (* IOCTL. 전부 실패시키면 런타임이 핸들을 무효로 보고 죽는다(실측).
       콘솔은 문자 장치 비트, 파일은 읽기·쓰기 가능. *)
    (match Cpu86.reg8 cpu 0 with
     | 0x00 ->
       let h = Cpu86.reg16 cpu 3 in
       if is_console h then begin Cpu86.set_reg16 cpu 2 0x80D3; ok t end
       else if Hashtbl.mem t.handles h then begin
         Cpu86.set_reg16 cpu 2 0x0002; ok t
       end
       else fail t 6
     | 0x06 | 0x07 -> Cpu86.set_reg8 cpu 0 0xFF; ok t
     | _ -> fail t 1)
  | 0x47 ->
    (* 현재 디렉터리는 늘 루트다 — 빈 문자열이 루트를 뜻한다. *)
    wr8 t (seg_off t 3 6) 0;
    Cpu86.set_reg16 cpu 0 0x0100;
    ok t
  | 0x48 ->
    (match allocate t (Cpu86.reg16 cpu 3) with
     | Some seg -> Cpu86.set_reg16 cpu 0 seg; ok t
     | None ->
       Cpu86.set_reg16 cpu 3 (largest_free t);
       fail t 8)
  | 0x49 -> if free_block t (Cpu86.seg cpu 0) then ok t else fail t 9
  | 0x4A ->
    (match resize_block t (Cpu86.seg cpu 0) (Cpu86.reg16 cpu 3) with
     | Ok () -> ok t
     | Error 9 -> fail t 9
     | Error avail -> Cpu86.set_reg16 cpu 3 (max 0 avail); fail t 8)
  | 0x4E ->
    let pattern = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    let all = Hashtbl.fold (fun k _ acc -> k :: acc) t.host_files [] in
    let hits =
      List.sort compare
        (List.filter (fun n -> matches_pattern ~pattern ~name:n) all)
    in
    (match hits with
     | [] -> t.find_queue <- []; fail t 2
     | first :: rest -> t.find_queue <- rest; fill_find_block t first; ok t)
  | 0x4F ->
    (match t.find_queue with
     | [] -> fail t 18                      (* 더 이상 없음 *)
     | next :: rest -> t.find_queue <- rest; fill_find_block t next; ok t)
  | 0x56 ->
    let src = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    let dst = String.uppercase_ascii (asciiz_at t ((Cpu86.seg cpu 0 lsl 4)
                                                   + Cpu86.reg16 cpu 7)) in
    (match Hashtbl.find_opt t.host_files src with
     | Some d ->
       Hashtbl.remove t.host_files src;
       Hashtbl.replace t.host_files dst d;
       ok t
     | None -> fail t 2)
  | 0x57 ->
    if Hashtbl.mem t.handles (Cpu86.reg16 cpu 3) then begin
      if Cpu86.reg8 cpu 0 = 0 then begin
        Cpu86.set_reg16 cpu 1 0;
        Cpu86.set_reg16 cpu 2 0x2821
      end;
      ok t
    end
    else fail t 6
  | 0x62 -> Cpu86.set_reg16 cpu 3 t.psp_seg
  | 0x68 -> ok t                            (* 파일 반영 — 메모리라 즉시 *)
  | 0x0F | 0x10 | 0x14 | 0x21 | 0x24 -> fcb_service t ah
  | _ -> ()

let terminate t =
  t.exited <- true;
  t.exit_code <- 0

(* 실기 DOS 는 .EXE 에 남은 메모리를 통째로 준다. 힙이 필요한 프로그램은
   AH=4Ah 로 제 블록을 줄여 뒤를 내놓고, 그 다음에야 AH=48h 이 성공한다
   — Turbo Pascal 런타임이 정확히 그렇게 한다. 처음부터 남는 자리를
   비워 두면 4Ah 없이도 할당이 되어 실기와 다르게 움직인다. *)
let init_memory t ~psp_seg =
  t.free_base <- psp_seg;
  t.free_top <- conv_mem_top;
  t.blocks <- [ (psp_seg, conv_mem_top - psp_seg) ]
