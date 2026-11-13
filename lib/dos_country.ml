(* 미국(국가 1), 코드 페이지 437 — MS-DOS 4.0 커널의 기본표
   src/DOS/DOSMES.ASM 의 COUNTRY_CDPG·UCASE_TAB·FILE_UCASE_TAB·FILE_CHAR_TAB·
   COLLATE_TAB·DBCS_TAB·MAP_CASE. COUNTRY.SYS 를 읽지 않은 DOS 가 쓰는
   값이다. COUNTRY.SYS 원본 src/DEV/COUNTRY/MKCNTRY.ASM 의 us437_info·
   us_ucase 와 값이 같다. *)

let country_code = 1                     (* CID_US *)
let code_page = 437                      (* ccDosCodePage 와 ccSysCodePage *)

let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let high_half = 0x80
let byte_count = 0x100
let lower_a = Char.code 'a'
let lower_z = Char.code 'z'
let case_bit = 0x20
let is_lower c = c >= lower_a && c <= lower_z

(* ---------- 대문자 ----------

   80h..A7h 는 UCASE_TAB 표이고(FILE_UCASE_TAB 도 값이 같다), A8h 부터는
   그대로다. 표의 순서는 80h 부터다. *)
let upcase_80_a7 =
  [| 128; 154; 069; 065; 142; 065; 143; 128;
     069; 069; 069; 073; 073; 073; 142; 143;
     144; 146; 146; 079; 153; 079; 085; 085;
     089; 153; 154; 155; 156; 157; 158; 159;
     065; 073; 079; 085; 165; 165; 166; 167 |]

let upcase c =
  if is_lower c then c - case_bit
  else if c >= high_half && c - high_half < Array.length upcase_80_a7 then
    upcase_80_a7.(c - high_half)
  else c

(* ---------- 정렬 순서(COLLATE_TAB) ----------

   a-z 는 A-Z 와 같은 자리다. 80h..AFh 는 아래 글자로 정렬하고, E1h(ß)는
   S 다. 그 밖은 제자리다. *)
let collate_80_af =
  "CUEAAAAC" ^ "EEEIIIAA" ^ "EAAOOOUU" ^ "YOU$$$$$"
  ^ "AIOUNN\xa6\xa7" ^ "?\xa9\xaa\xab\xac!\"\""
let sharp_s = 0xE1

let collate c =
  if is_lower c then c - case_bit
  else if c >= high_half && c - high_half < String.length collate_80_af then
    Char.code collate_80_af.[c - high_half]
  else if c = sharp_s then Char.code 'S'
  else c

(* ---------- YES/NO ----------

   65h AL=23h. src/DOS/MSDOS.SKL 이 NLS_YES·NLS_NO 를 COMMAND 메시지
   205·206 에서, NLS_yes2·NLS_no2 를 MSDOS 메시지 2·3 에서 가져온다
   (src/MESSAGES/USA-MS.MSG: "Y", "N", "y", "n"). *)
type answer = No | Yes | Neither

let answer c =
  if c = Char.code 'Y' || c = Char.code 'y' then Yes
  else if c = Char.code 'N' || c = Char.code 'n' then No
  else Neither

(* ---------- ROM 안의 자리 ---------- *)

type layout = {
  segment : int;
  case_map : int;     (* MAP_CASE *)
  ucase : int;        (* UCASE_TAB — 길이 워드부터 *)
  file_ucase : int;   (* FILE_UCASE_TAB *)
  file_chars : int;   (* FILE_CHAR_TAB *)
  collate : int;      (* COLLATE_TAB *)
  dbcs : int;         (* DBCS_TAB *)
}

(* 65h 의 표 번호(DOSCNTRY.INC SetUcase..SetDBCS). COUNTRY_CDPG 의 항목
   순서다. *)
type table = Ucase | File_ucase | File_chars | Collate | Dbcs

let tables = [ Ucase; File_ucase; File_chars; Collate; Dbcs ]

let table_id = function
  | Ucase -> 2
  | File_ucase -> 4
  | File_chars -> 5
  | Collate -> 6
  | Dbcs -> 7

let table_of_id id = List.find_opt (fun tb -> table_id tb = id) tables

let table_offset l = function
  | Ucase -> l.ucase
  | File_ucase -> l.file_ucase
  | File_chars -> l.file_chars
  | Collate -> l.collate
  | Dbcs -> l.dbcs

let country_info_id = 1                  (* SetCountryInfo *)
let length_word = 2

let dbcs_ranges l = l.dbcs + length_word

(* ---------- INT 21h AH=38h 가 주는 표 ----------

   src/INC/DOSCNTRY.INC 의 ccDFormat 부터 OLD_COUNTRY_SIZE(24) 바이트. 뒤의
   예약 10 바이트는 MS-DOS 4.0 이 복사하지 않는다. 순서: 날짜 형식,
   통화 기호(5), 천 단위(2), 소수점(2), 날짜(2), 시각(2), 통화 형식,
   통화 소수 자리, 시각 형식, 대문자 루틴(far), 목록 구분(2). *)
let date_mdy = 0
let currency_symbol_before = 0
let currency_digits = 2
let hour12 = 0
let info_size = 24
let reserved_size = 10

let info l =
  let b = Buffer.create info_size in
  let byte v = Buffer.add_char b (Char.chr v) in
  let word v = Buffer.add_string b (le16 v) in
  let asciz width s =
    Buffer.add_string b s;
    Buffer.add_string b (String.make (width - String.length s) '\000')
  in
  word date_mdy;
  asciz 5 "$";
  asciz 2 ",";
  asciz 2 ".";
  asciz 2 "-";
  asciz 2 ":";
  byte currency_symbol_before;
  byte currency_digits;
  byte hour12;
  word l.case_map;
  word l.segment;
  asciz 2 ",";
  Buffer.contents b

(* 65h AL=01h 가 주는 표: ccDosCountry 부터 NEW_COUNTRY_SIZE(38) 바이트 —
   나라, 코드 페이지, 위의 24 바이트, 예약 10 바이트. *)
let ext_info l =
  le16 country_code ^ le16 code_page ^ info l ^ String.make reserved_size '\000'

(* ---------- 표 ---------- *)

let sized n f = le16 n ^ String.init n (fun i -> Char.chr (f i))

(* 길이 128, 80h..FFh 의 대문자 *)
let ucase_table = sized (byte_count - high_half) (fun i -> upcase (high_half + i))

(* FILE_CHAR_TAB: 길이 22, 그 뒤 예약 24 바이트. RBIL 표 01754 의 순서다 —
   01h, 이름에 쓸 수 있는 가장 작은·큰 글자, 00h, 뺄 범위의 처음·끝,
   02h, 끝 글자의 수, 끝 글자들. *)
let terminators = ".\"/\\[]:|<>+=;,"
let file_chars_reserved = 24

let file_chars_table =
  let body =
    "\x01\x00\xff" ^ "\x00\x00\x20" ^ "\x02"
    ^ String.make 1 (Char.chr (String.length terminators)) ^ terminators
  in
  le16 (String.length body) ^ body ^ String.make file_chars_reserved '\000'

let collate_table = sized byte_count collate

(* DBCS_TAB: 범위 0 개, 그 뒤 16 바이트 0. 63h 는 길이 워드 다음을 준다 —
   첫 두 바이트 0 이 표의 끝이다. *)
let dbcs_room = 16
let dbcs_table = le16 0 ^ String.make dbcs_room '\000'

(* ---------- 대문자 루틴 ----------

   38h·65h 표의 far 포인터가 가리키는 루틴. DOSMES.ASM MAP_CASE 처럼
   FAR CALL 로 부르고, AL 이 80h 이상이면 UCASE_TAB 으로 바꾼다. 80h 아래는
   그대로 둔다. 플래그는 MAP_CASE 의 CMP·SUB AL,80h 와 같은 값이 남는다. *)
let case_map_code ~ucase =
  "\x3c\x80"                                            (* cmp al,80h *)
  ^ "\x72\x07"                                          (* jb done *)
  ^ "\x53"                                              (* push bx *)
  ^ "\xbb" ^ le16 (ucase + length_word - high_half)     (* mov bx,UCASE_TAB+2-80h *)
  ^ "\x2e\xd7"                                          (* cs: xlat *)
  ^ "\x5b"                                              (* pop bx *)
  ^ "\xcb"                                              (* done: retf *)

(* 루틴, 대문자 표, 파일 이름 대문자 표, 이름 글자 표, 정렬 표, DBCS 표를
   이 순서로 붙인다. 루틴의 길이는 표의 자리에 따라 바뀌지 않는다. *)
let image ~segment ~at =
  let routine_size = String.length (case_map_code ~ucase:0) in
  let ucase = at + routine_size in
  let file_ucase = ucase + String.length ucase_table in
  let file_chars = file_ucase + String.length ucase_table in
  let collate = file_chars + String.length file_chars_table in
  let dbcs = collate + String.length collate_table in
  let l = { segment; case_map = at; ucase; file_ucase; file_chars; collate; dbcs } in
  ( l,
    String.concat ""
      [ case_map_code ~ucase; ucase_table; ucase_table; file_chars_table;
        collate_table; dbcs_table ] )
