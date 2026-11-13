(* 미국(국가 1), 코드 페이지 437 — MS-DOS 4.0 COUNTRY.SYS 원본
   src/DEV/COUNTRY/MKCNTRY.ASM 의 us437_info·us_ucase. 커널에 든 기본표는
   찾지 못했다. MKCNTRY.ASM 은 미국 437 항목을 "3.3 & under compatibility"
   를 지키는 표로 적는다. *)

let country_code = 1                     (* CID_US *)

(* ---------- 대문자 ----------

   80h..A7h 는 us_ucase 표(SETUCASE 와 SETUCASEFILE 이 같은 표)이고, A8h
   부터는 그대로다. 표의 순서는 80h 부터다. *)
let upcase_80_a7 =
  [| 128; 154; 069; 065; 142; 065; 143; 128;
     069; 069; 069; 073; 073; 073; 142; 143;
     144; 146; 146; 079; 153; 079; 085; 085;
     089; 153; 154; 155; 156; 157; 158; 159;
     065; 073; 079; 085; 165; 165; 166; 167 |]
let high_half = 0x80
let byte_count = 0x100
let lower_a = Char.code 'a'
let lower_z = Char.code 'z'
let case_bit = 0x20

let upcase c =
  if c >= lower_a && c <= lower_z then c - case_bit
  else if c >= high_half && c - high_half < Array.length upcase_80_a7 then
    upcase_80_a7.(c - high_half)
  else c

(* ---------- INT 21h AH=38h 가 주는 표 ----------

   src/INC/DOSCNTRY.INC 의 ccDFormat 부터 OLD_COUNTRY_SIZE(24) 바이트. 뒤의
   예약 10 바이트는 MS-DOS 4.0 이 복사하지 않는다. MKCNTRY.INC CINFO 순서:
   날짜 형식, 통화 기호(5), 천 단위(2), 소수점(2), 날짜(2), 시각(2),
   통화 형식, 통화 소수 자리, 시각 형식, 대문자 루틴(far), 목록 구분(2). *)
let date_mdy = 0
let currency_symbol_before = 0
let currency_digits = 2
let hour12 = 0
let info_size = 24

let info ~case_map:(segment, offset) =
  let b = Buffer.create info_size in
  let byte v = Buffer.add_char b (Char.chr v) in
  let word v = byte (v land 0xff); byte (v lsr 8) in
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
  word offset;
  word segment;
  asciz 2 ",";
  Buffer.contents b

(* ---------- 대문자 루틴 ----------

   38h 표의 far 포인터가 가리키는 루틴. RBIL INT 21/38: FAR CALL, AL 이
   80h 이상이면 대문자로 바꾼다. 80h 아래는 그대로 둔다. 루틴 뒤에 80h..FFh
   의 대문자 128 바이트가 온다. *)
let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

let case_map_code ~at =
  let code table =
    "\x3c\x80"                            (* cmp al,80h *)
    ^ "\x72\x07"                          (* jb done *)
    ^ "\x53"                              (* push bx *)
    ^ "\xbb" ^ le16 (table - high_half)   (* mov bx,table-80h *)
    ^ "\x2e\xd7"                          (* cs: xlat *)
    ^ "\x5b"                              (* pop bx *)
    ^ "\xcb"                              (* done: retf *)
  in
  (* 표는 루틴 바로 뒤다. 루틴의 길이는 표의 자리에 따라 바뀌지 않는다. *)
  let table = at + String.length (code at) in
  code table ^ String.init (byte_count - high_half) (fun i -> Char.chr (upcase (high_half + i)))
