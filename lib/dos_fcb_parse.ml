(* INT 21h AH=29h — MS-DOS 4.0 src/DOS/FCB.ASM MakeFcb 를 옮겼다.
   레지스터 이름은 주석에 남긴다: SI 는 읽는 자리, DI 는 FCB 안의 자리,
   DL 은 돌려줄 값, CX 는 필드에 남은 칸이다. *)

type options = {
  skip_separator : bool;
  keep_drive : bool;
  keep_name : bool;
  keep_extension : bool;
}

let options_of_al al =
  { skip_separator = al land 1 <> 0; keep_drive = al land 2 <> 0;
    keep_name = al land 4 <> 0; keep_extension = al land 8 <> 0 }

type outcome = { al : int; next : int }

(* ---------- 글자 분류: FCB.ASM CharType ----------

   표의 기본값은 FFh 이고, 항목은 비트를 지운다. 지워진 비트가 그 성질이
   있다는 뜻이다(GetLet·DELIM·SPCHK 가 TEST 로 ZF 를 본다).
   비트 값은 src/INC/DOSSYM.INC. *)
let f_chk = 1      (* 지워지면 이름의 끝 *)
let f_delim = 2    (* 지워지면 구분자 *)
let f_sp_chk = 4   (* 지워지면 공백·탭 *)
let f_fcb = 8      (* 지워지면 FCB 이름에 못 오는 글자 *)
let all_set = 0xFF
let first_printable = 0x20
let tab = 0x09

let char_type c =
  let clear bits = all_set land lnot bits in
  if c = tab then clear (f_fcb lor f_chk lor f_delim lor f_sp_chk)
  else if c < first_printable then clear (f_fcb lor f_chk)
  else
    match Char.chr c with
    | '.' -> clear f_chk
    | '"' | '/' | '\\' | '[' | ']' -> clear (f_fcb lor f_chk)
    | ':' | '<' | '|' | '>' | '+' | '=' | ';' | ',' -> clear (f_fcb lor f_chk lor f_delim)
    | ' ' -> clear (f_chk lor f_delim lor f_sp_chk)
    | _ -> all_set

let ends_name c = char_type c land f_chk = 0
let is_delimiter c = char_type c land f_delim = 0
let is_space c = char_type c land f_sp_chk = 0

(* ---------- 대문자 ----------

   80h..A7h 는 COUNTRY.SYS 미국 437 표(src/DEV/COUNTRY/MKCNTRY.ASM
   us_ucase — SETUCASE 와 SETUCASEFILE 이 같은 표)이고, A8h 부터는 그대로다.
   표의 순서는 80h 부터다. *)
let us437_upcase_80_a7 =
  [| 128; 154; 069; 065; 142; 065; 143; 128;
     069; 069; 069; 073; 073; 073; 142; 143;
     144; 146; 146; 079; 153; 079; 085; 085;
     089; 153; 154; 155; 156; 157; 158; 159;
     065; 073; 079; 085; 165; 165; 166; 167 |]
let high_half = 0x80
let lower_a = Char.code 'a'
let lower_z = Char.code 'z'
let case_bit = 0x20

let upcase c =
  if c >= lower_a && c <= lower_z then c - case_bit
  else if c >= high_half && c - high_half < Array.length us437_upcase_80_a7 then
    us437_upcase_80_a7.(c - high_half)
  else c

(* ---------- MakeFcb ---------- *)

let blank = Char.code ' '
let star = Char.code '*'
let question = Char.code '?'
let colon = Char.code ':'
let dot = Char.code '.'
let drive_base = Char.code '@'     (* SUB AL,"@" — A=1 *)
let byte_mask = 0xFF
let offset_mask = 0xFFFF
let drive_field = 0
let name_field = 1
let name_width = 8
let extension_field = 9
let extension_width = 3
let reserved_field = 12             (* 현재 블록·레코드 크기 — 늘 0 *)
let fcb_width = 16
let wildcards = 1
let bad_drive = 0xFF

let parse opts ~drive_exists ~read ~write si0 =
  let si = ref si0 and di = ref drive_field in
  let dl_bad = ref false and dl_wild = ref false in
  let advance n = si := (!si + n) land offset_mask in
  (* GETLET: LODSB 뒤 대문자 *)
  let get_letter () = let c = upcase (read !si) in advance 1; c in
  (* SCANB: 공백·탭을 건너뛴다(대문자 전 글자로 본다) *)
  let rec skip_blanks () = if is_space (read !si) then begin advance 1; skip_blanks () end in
  let store c = write !di c; incr di in
  let fill n c = for _ = 1 to n do store c done in
  if not opts.keep_drive then write drive_field 0;
  if opts.keep_name then di := extension_field
  else begin di := name_field; fill name_width blank end;
  if opts.keep_extension then di := reserved_field else fill extension_width blank;
  fill (fcb_width - reserved_field) 0;
  (* 앞 공백, 그리고 bit 0 이면 구분자 하나 *)
  skip_blanks ();
  if opts.skip_separator && is_delimiter (read !si) then begin advance 1; skip_blanks () end;
  (* 드라이브: 글자 뒤에 ':' 가 오면 *)
  let c = get_letter () in
  if (not (ends_name c)) && read !si = colon then begin
    advance 1;
    let n = c - drive_base in
    if n <= 0 || not (drive_exists n) then dl_bad := true;
    write drive_field (n land byte_mask)
  end
  else advance (-1);
  di := name_field;
  (* MUSTGETWORD: 끝 문자까지 읽는다. 칸이 차면 넘는 글자는 버린다. *)
  let must_get_word width =
    let rec go cx =
      let c = get_letter () in
      if ends_name c then begin fill cx blank; advance (-1) end   (* FILLNAM *)
      else if cx = 0 then go 0
      else begin
        let cx = cx - 1 in
        let c, cx = if c = star then begin fill cx question; (question, 0) end else (c, cx) in
        store c;
        if c = question then dl_wild := true;
        go cx
      end
    in
    go width
  in
  (* GETWORD: 이름의 첫 글자가 끝 문자면 이름 칸을 그대로 둔다(NONAM) *)
  let c = get_letter () in
  if ends_name c then begin di := extension_field; advance (-1) end
  else begin advance (-1); must_get_word name_width end;
  (* 확장자: '.' 뒤는 MUSTGETWORD — 글자가 없어도 공백으로 덮는다 *)
  if read !si = dot then begin advance 1; di := extension_field; must_get_word extension_width end;
  let al = if !dl_bad then bad_drive else if !dl_wild then wildcards else 0 in
  { al; next = !si }
