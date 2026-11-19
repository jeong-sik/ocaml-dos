(* 나라별 서비스 — INT 21h AH=38h·63h·65h·66h. MS-DOS 4.0
   src/DOS/GETSET.ASM 의 $INTERNATIONAL·$ECS_call·$GetExtCntry·$GetSetCdPg 를
   따른다. 나라는 미국(1), 코드 페이지는 437 하나다(Dos_country). 다른
   나라나 코드 페이지는 NLSFUNC 가 있어야 하는데, 없으면 1(잘못된 기능)
   이다.

   이 기능들은 끝날 때 SYS_RET_OK(MS_CODE.ASM)가 그때의 AX 를 사용자 AX 로
   돌려준다. 기능은 사용자 AX 를 그대로 받는다(DISP.ASM 의 POP AX 뒤
   DISPCALL). 그래서 AX 를 바꾸지 않는 길은 AX 가 그대로 남는다. *)

open Dos_state

let error_invalid_function = 1
let active_marker = 0xFFFF
let ax = 0
let cx = 1
let dx = 2
let bx = 3
let si = 6
let di = 7
let dl = 2
let es = 0
let ds = 3
let segment_mask = 0xffff

let write_wrapped t seg off s =
  String.iteri (fun i c -> wr8 t (physical seg ((off + i) land segment_mask)) (Char.code c)) s

(* ---------- AH=38h ----------

   $INTERNATIONAL. AL=FFh 면 나라는 BX, 아니면 AL 이다. RBIL 은 에러를
   2 로 적지만 소스를 따른다. DX=FFFFh 면 「나라 바꾸기」로, 지금 나라와
   같을 때만 된다 — AL=0 도 거절이다. 성공하면 AX=나라 번호다. 얻기는 BX
   도 나라 번호로 바꾸고(international_ok3), DS:DX 에 24 바이트를 쓴다
   (DI 는 16 비트에서 돈다). 바꾸기는 BX 를 그대로 둔다
   (international_set 은 international_ok 로 바로 간다). *)
let all_in_bx = 0xFF

let international t =
  let cpu = t.cpu in
  let al = Cpu86.reg8 cpu ax in
  let requested = if al = all_in_bx then Cpu86.reg16 cpu bx else al in
  let current = Dos_country.country_code in
  let target = Cpu86.reg16 cpu dx in
  let succeed () = Cpu86.set_reg16 cpu ax current; ok t in
  if target = active_marker then
    (if requested = current then succeed () else fail t error_invalid_function)
  else if requested = 0 || requested = current then begin
    write_wrapped t (Cpu86.seg cpu ds) target (Dos_country.info Dos_bios.country);
    Cpu86.set_reg16 cpu bx current;
    succeed ()
  end
  else fail t error_invalid_function

(* ---------- AH=63h ----------

   $ECS_call. 미국판은 DBCS 빌드가 아니다(src/INC/DBCS.SW). 그 갈래는
   AL=0 이면 DS:SI 에 DBCS 표(범위 없음)를 주고, AL 이 무엇이든 CF=0 으로
   끝난다. *)
let lead_byte_table t =
  let cpu = t.cpu in
  if Cpu86.reg8 cpu ax = 0 then begin
    Cpu86.set_seg cpu ds Dos_bios.country.segment;
    Cpu86.set_reg16 cpu si (Dos_country.dbcs_ranges Dos_bios.country)
  end;
  ok t

(* ---------- AH=65h, AL<20h: 표 얻기 ----------

   CX 가 5 보다 작으면 거절한다. BX(코드 페이지)·DX(나라)는 FFFFh 면 지금
   것이고, 지금 것과 다르면 거절한다. 표 번호를 못 찾아도 거절이다. 거절은
   버퍼를 건드리지 않는다.

   AL=01h: 번호, 길이 워드, ccDosCountry 부터의 바이트를 쓴다. 길이는
   min(38, CX−3) 이다(setsize). 그 밖: 번호와 far 포인터 5 바이트. 끝나면
   CX=쓴 바이트 수, AX=시스템 코드 페이지다(CHKTYPE 의 BX). *)
let min_info_size = 5
let header_size = 3                      (* 번호 1 + 길이 워드 2 *)

let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

let record_for t id =
  let cpu = t.cpu in
  let size = Cpu86.reg16 cpu cx in
  let pick r current = let v = Cpu86.reg16 cpu r in if v = active_marker then current else v in
  let l = Dos_bios.country in
  let tag = String.make 1 (Char.chr id) in
  if size < min_info_size
     || pick dx Dos_country.country_code <> Dos_country.country_code
     || pick bx Dos_country.code_page <> Dos_country.code_page
  then None
  else if id = Dos_country.country_info_id then
    let data = Dos_country.ext_info l in
    let data = String.sub data 0 (min (String.length data) (size - header_size)) in
    Some (tag ^ le16 (String.length data) ^ data)
  else
    Option.map
      (fun tb -> tag ^ le16 (Dos_country.table_offset l tb) ^ le16 l.segment)
      (Dos_country.table_of_id id)

let get_table t id =
  let cpu = t.cpu in
  match record_for t id with
  | None -> fail t error_invalid_function
  | Some r ->
    write_wrapped t (Cpu86.seg cpu es) (Cpu86.reg16 cpu di) r;
    Cpu86.set_reg16 cpu cx (String.length r);
    Cpu86.set_reg16 cpu ax Dos_country.code_page;
    ok t

(* ---------- AH=65h, AL≥20h: 대문자로 ----------

   20h: DL 을 대문자로. 21h: DS:DX 의 CX 바이트. 22h: DS:DX 의 ASCIZ.
   23h: DL 이 Y·y 면 AX=1, N·n 이면 0, 그 밖은 2. AL 의 80h 비트는 파일
   이름 표를 고르지만, 그 뒤 AL 을 그대로 20h..23h 와 견주어 A0h..A3h 는
   잘못된 기능이 된다(RBIL: MS-DOS 4.00~7.10 의 버그). 두 표의 값이 같으니
   표는 하나로 쓴다.

   20h..22h 는 마지막으로 바꾼 글자를 AL 에 남긴다(GETLET3 의 AL, AH 는
   65h 그대로). 21h 의 CX=0 은 AX 를 건드리지 않는다. 22h 는 NUL 에서 끝나
   AL=0 이다. *)
let cap_one_char = 0x20
let cap_string = 0x21
let cap_asciiz = 0x22
let check_yes_no = 0x23
let high_byte = 0xff00

(* 세그먼트 전체에 NUL 이 없으면 MS-DOS 는 영원히 돈다. 한 바퀴 돌면 바꿀
   글자가 남지 않으니 여기서 멈춘다. *)
let segment_size = 0x10000

let answer_code = function
  | Dos_country.No -> 0
  | Dos_country.Yes -> 1
  | Dos_country.Neither -> 2

let capitalize t fn =
  let cpu = t.cpu in
  let finish last =
    Cpu86.set_reg16 cpu ax ((Cpu86.reg16 cpu ax land high_byte) lor last);
    ok t
  in
  let at i = physical (Cpu86.seg cpu ds) ((Cpu86.reg16 cpu dx + i) land segment_mask) in
  let up a = let c = Dos_country.upcase (rd8 t a) in wr8 t a c; c in
  if fn = cap_one_char then begin
    let c = Dos_country.upcase (Cpu86.reg8 cpu dl) in
    Cpu86.set_reg8 cpu dl c;
    finish c
  end
  else if fn = cap_string then begin
    let n = Cpu86.reg16 cpu cx in
    if n = 0 then ok t
    else begin
      let last = ref 0 in
      for i = 0 to n - 1 do last := up (at i) done;
      finish !last
    end
  end
  else if fn = cap_asciiz then begin
    let rec go i =
      if i < segment_size && rd8 t (at i) <> 0 then (ignore (up (at i)); go (i + 1))
    in
    go 0;
    finish 0
  end
  else if fn = check_yes_no then begin
    Cpu86.set_reg16 cpu ax (answer_code (Dos_country.answer (Cpu86.reg8 cpu dl)));
    ok t
  end
  else fail t error_invalid_function

let extended_country t =
  let al = Cpu86.reg8 t.cpu ax in
  if al >= cap_one_char then capitalize t al else get_table t al

(* ---------- AH=66h ----------

   $GetSetCdPg. AL=1: BX=지금 코드 페이지, DX=시스템 코드 페이지. AL=2
   (바꾸기)는 NLSFUNC 가 있어야 한다. 그 밖은 잘못된 기능이다. *)
let get_code_page = 1

let code_page t =
  let cpu = t.cpu in
  if Cpu86.reg8 cpu ax = get_code_page then begin
    Cpu86.set_reg16 cpu bx Dos_country.code_page;
    Cpu86.set_reg16 cpu dx Dos_country.code_page;
    ok t
  end
  else fail t error_invalid_function
