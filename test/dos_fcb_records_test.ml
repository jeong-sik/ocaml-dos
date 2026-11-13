(* FCB 레코드 번호 — MS-DOS 4.0 FCBIO2.ASM(FCBIO, GetExtent·SetExtent·GetRR·
   GetRecSize, $FCB_Open), FCBIO.ASM($Get_FCB_Position 24h,
   $Get_FCB_File_Length 23h). 확장 FCB(FFh 머리 7 바이트)도 같은 길로 간다. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let b v = String.make 1 (Char.chr (v land 0xff))
let psp = 0x10000
let quit = "\xb8\x00\x4c\xcd\x21"

let slot k = 0x4000 + (0x40 * k)
let dta k = 0x5000 + (0x80 * k)
let result_at = 0x7000
let save_at = 0x7100
let filler = 0xEE
let unset = 0xAA

(* FCB 필드 (보통 FCB 시작에서) *)
let f_extent = 0x0C and f_recsize = 0x0E and f_size = 0x10
let f_nr = 0x20 and f_rr = 0x21

(* AL 결과와 중간 바이트를 적어 둘 칸을 차례로 나눠 준다 *)
let results = ref [] and saves = ref []
let next_result = ref 0 and next_save = ref 0

let call ah ~dx name want =
  let k = !next_result in
  incr next_result;
  results := (name, k, want) :: !results;
  "\xba" ^ w dx ^ "\xb4" ^ b ah ^ "\xcd\x21" ^ "\xa2" ^ w (result_at + k)

let save addr name want =
  let k = !next_save in
  incr next_save;
  saves := (name, k, want) :: !saves;
  "\xa0" ^ w addr ^ "\xa2" ^ w (save_at + k)

let set_dta k = "\xba" ^ w (dta k) ^ "\xb4\x1a\xcd\x21"
let set_word addr v = "\xc7\x06" ^ w addr ^ w v

(* 슬롯마다 보통 FCB 의 시작. 슬롯 7 만 확장 FCB 다. *)
let ext_slot = 7
let fcb k = if k = ext_slot then slot k + 7 else slot k

let rec_size = 300
let rec_byte i = ((i * 7) + 3) land 0xff
let big_size = 0x1000001

let program =
  let s8 = fcb 8 and s9 = fcb 9 and s10 = fcb 10 and s11 = fcb 11 in
  String.concat ""
    [ call 0x24 ~dx:(fcb 0) "24h wide" 0;
      call 0x24 ~dx:(fcb 1) "24h narrow" 0;
      call 0x24 ~dx:(fcb 2) "24h size 0" 0;
      call 0x23 ~dx:(fcb 3) "23h size 0" 0;
      call 0x23 ~dx:(fcb 4) "23h size 1" 0;
      call 0x23 ~dx:(fcb 5) "23h big" 0;
      call 0x23 ~dx:(fcb 6) "23h missing" 0xFF;
      (* 확장 FCB: DX 는 FFh 머리를 가리킨다 *)
      call 0x0F ~dx:(slot ext_slot) "ext open" 0;
      set_dta 0; call 0x14 ~dx:(slot ext_slot) "seq 0" 0;
      set_dta 1; call 0x14 ~dx:(slot ext_slot) "seq 1" 0;
      set_dta 2; call 0x14 ~dx:(slot ext_slot) "seq 2 partial" 3;
      set_dta 3; call 0x14 ~dx:(slot ext_slot) "seq 3 eof" 1;
      (* 21h 는 NR 을 RR 로 맞추고 나아가지 않는다 *)
      call 0x0F ~dx:s8 "open 8" 0;
      set_word (s8 + f_recsize) 100;
      set_word (s8 + f_rr) 2; set_word (s8 + f_rr + 2) 0;
      set_dta 4; call 0x21 ~dx:s8 "random 2" 0;
      save (s8 + f_nr) "NR after random 2" 2;
      set_dta 5; call 0x14 ~dx:s8 "seq after random" 0;
      save (s8 + f_nr) "NR after seq" 3;
      set_word (s8 + f_rr) 3;
      set_dta 6; call 0x21 ~dx:s8 "random 3 eof" 1;
      save (s8 + f_nr) "NR after eof" 3;
      set_word (s8 + f_recsize) 128; set_word (s8 + f_rr) 2;
      set_dta 7; call 0x21 ~dx:s8 "random partial" 3;
      (* 레코드가 64 바이트보다 작으면 RR 넷째 바이트까지 본다 *)
      call 0x0F ~dx:s9 "open 9" 0;
      set_word (s9 + f_recsize) 10;
      set_word (s9 + f_rr) 2; set_word (s9 + f_rr + 2) 0x0100;
      call 0x21 ~dx:s9 "narrow 4-byte RR" 1;
      save (s9 + f_nr) "narrow NR" 2;
      save (s9 + f_extent) "narrow extent lo" 0;
      set_word (s9 + f_recsize) 100;
      set_dta 8; call 0x21 ~dx:s9 "wide 3-byte RR" 0;
      (* 다음 레코드는 낮은 7 비트만 — 레코드 85h 는 블록 1, 레코드 5 *)
      call 0x0F ~dx:s11 "open 11" 0;
      set_word (s11 + f_recsize) 1;
      set_word (s11 + f_rr) 0x85; set_word (s11 + f_rr + 2) 0;
      set_dta 10; call 0x21 ~dx:s11 "random 85h" 0;
      save (s11 + f_nr) "85h NR" 0x05;
      save (s11 + f_extent) "85h extent" 0x01;
      (* 레코드 크기 0 은 128 로 고쳐 쓴다 *)
      call 0x0F ~dx:s10 "open 10" 0;
      set_word (s10 + f_recsize) 0; set_word (s10 + f_rr) 0; set_word (s10 + f_rr + 2) 0;
      set_dta 9; call 0x21 ~dx:s10 "random size 0" 0;
      save (s10 + f_recsize) "size 0 written back" 128;
      call 0x10 ~dx:s10 "close 10" 0;
      quit ]

let () =
  let m = Dos_machine.create () in
  let mount = Dos_machine.mount_file m in
  mount "REC.DAT" (String.init rec_size (fun i -> Char.chr (rec_byte i)));
  mount "BIG.DAT" (String.make big_size 'x');
  Dos_machine.load_com m program;
  let put a v = Dos_state.wr8 m (psp + a) v in
  let put16 a v = put a v; put (a + 1) (v lsr 8) in
  let put_name a n = String.iteri (fun i c -> put (a + 1 + i) (Char.code c)) n in
  let rr4 a = for i = 0 to 3 do put (a + f_rr + i) unset done in
  for k = 0 to 11 do
    for i = 0 to 0x3F do put (slot k + i) 0 done;
    rr4 (fcb k)
  done;
  for k = 0 to 10 do for i = 0 to 0x7F do put (dta k + i) filler done done;
  put16 (fcb 0 + f_extent) 2; put (fcb 0 + f_nr) 0x85; put16 (fcb 0 + f_recsize) 128;
  for i = 0 to 3 do put (fcb 0 + f_rr + i) (0xDD - (0x11 * i)) done;
  put16 (fcb 1 + f_extent) 0xFFFF; put (fcb 1 + f_nr) 0x7F; put16 (fcb 1 + f_recsize) 10;
  put16 (fcb 2 + f_extent) 1;
  List.iter (fun k -> put_name (fcb k) "REC     DAT") [ 3; 4; ext_slot; 8; 9; 10; 11 ];
  put16 (fcb 4 + f_recsize) 1;
  put_name (fcb 5) "BIG     DAT"; put16 (fcb 5 + f_recsize) 1;
  put_name (fcb 6) "GONE    DAT";
  put (slot ext_slot) 0xFF; put (slot ext_slot + 6) 0x02;
  put16 (fcb ext_slot + f_extent) 0x1234;
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let rd a = Dos_state.rd8 m (psp + a) in
  let rd16 a = rd a lor (rd (a + 1) lsl 8) in
  List.iter (fun (n, k, want) -> check ("AL " ^ n) (rd (result_at + k)) want) !results;
  List.iter (fun (n, k, want) -> check n (rd (save_at + k)) want) !saves;
  let rr name k want =
    List.iteri (fun i v -> check (Printf.sprintf "%s RR[%d]" name i) (rd (fcb k + f_rr + i)) v) want
  in
  (* 24h: 블록 2, NR 85h → 레코드 2×128+5. 넷째 바이트는 레코드 128 이라 그대로 *)
  rr "24h wide" 0 [ 0x05; 0x01; 0x00; 0xAA ];
  check "24h leaves NR" (rd (fcb 0 + f_nr)) 0x85;
  rr "24h narrow" 1 [ 0xFF; 0xFF; 0x7F; 0x00 ];
  rr "24h size 0" 2 [ 0x80; 0x00; 0x00; 0x00 ];
  check "24h keeps size 0" (rd16 (fcb 2 + f_recsize)) 0;
  (* 23h: 300 바이트 / 128 → 3 (올림), 고쳐 쓰지 않는다 *)
  rr "23h size 0" 3 [ 0x03; 0x00; 0x00; 0xAA ];
  check "23h keeps size 0" (rd16 (fcb 3 + f_recsize)) 0;
  rr "23h size 1" 4 [ 0x2C; 0x01; 0x00; 0xAA ];
  rr "23h big" 5 [ 0x01; 0x00; 0x00; 0x01 ];
  rr "23h missing" 6 [ 0xAA; 0xAA; 0xAA; 0xAA ];
  let e = fcb ext_slot in
  check "ext open drive" (rd e) 3;
  check "ext open record size" (rd16 (e + f_recsize)) 128;
  check "ext open size" (rd16 (e + f_size)) rec_size;
  check "ext extent" (rd16 (e + f_extent)) 0;
  check "ext NR" (rd (e + f_nr)) 3;
  rr "ext open keeps RR" ext_slot [ 0xAA; 0xAA; 0xAA; 0xAA ];
  check "ext header kept" (rd (slot ext_slot)) 0xFF;
  let dta_is name k ~from ~len =
    for i = 0 to 0x7F do
      let want = if i < len then rec_byte (from + i) else 0 in
      check (Printf.sprintf "%s DTA[%d]" name i) (rd (dta k + i)) want
    done
  in
  dta_is "seq 0" 0 ~from:0 ~len:128;
  dta_is "seq 1" 1 ~from:128 ~len:128;
  dta_is "seq 2 partial" 2 ~from:256 ~len:44;
  let untouched name k ~from =
    for i = from to 0x7F do check (Printf.sprintf "%s DTA[%d]" name i) (rd (dta k + i)) filler done
  in
  untouched "seq 3 eof" 3 ~from:0;
  let record_100 name k =
    for i = 0 to 99 do check (Printf.sprintf "%s DTA[%d]" name i) (rd (dta k + i)) (rec_byte (200 + i)) done;
    untouched name k ~from:100
  in
  record_100 "random 2" 4;
  record_100 "seq after random" 5;
  untouched "random 3 eof" 6 ~from:0;
  dta_is "random partial" 7 ~from:256 ~len:44;
  record_100 "wide 3-byte RR" 8;
  dta_is "random size 0" 9 ~from:0 ~len:128;
  check "random 85h DTA[0]" (rd (dta 10)) (rec_byte 0x85);
  untouched "random 85h" 10 ~from:1

let () =
  if !failed = 0 then print_endline "dos FCB records: all passed"
  else begin
    Printf.eprintf "dos FCB records: %d failures\n%!" !failed;
    exit 1
  end
