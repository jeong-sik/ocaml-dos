(* FCB 쓰기 — MS-DOS 4.0 FCBIO2.ASM(FCBIO: 15h·22h·27h·28h, $FCB_Create),
   FCBIO.ASM($FCB_Close), MKNODE.ASM(메타 문자가 든 이름). 쓴 바이트는
   닫을 때 FCB 의 파일 크기까지 파일이 된다. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %S want %S\n%!" name got want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let b v = String.make 1 (Char.chr (v land 0xff))
let psp = 0x10000
let quit = "\xb8\x00\x4c\xcd\x21"
let result_at = 0x7000
let save_at = 0x7100
let filler = 0xEE
let unset = 0xAA

let slot k = 0x4000 + (0x40 * k)
let dta k = 0x5000 + (0x40 * k)
let ext_slot = 12
let fcb k = if k = ext_slot then slot k + 7 else slot k

let f_extent = 0x0C and f_recsize = 0x0E and f_size = 0x10 and f_date = 0x14
let f_nr = 0x20 and f_rr = 0x21

let results = ref [] and saves = ref []
let next_result = ref 0 and next_save = ref 0

let call ah k name want =
  let i = !next_result in
  incr next_result;
  results := (name, i, want) :: !results;
  "\xba" ^ w (if k = ext_slot then slot k else fcb k) ^ "\xb4" ^ b ah ^ "\xcd\x21"
  ^ "\xa2" ^ w (result_at + i)

let fresh_save name want =
  let i = !next_save in
  incr next_save;
  saves := (name, i, want) :: !saves;
  save_at + (2 * i)

let save_byte addr name want = "\xa0" ^ w addr ^ "\xa2" ^ w (fresh_save name want)
let save_cx name want = "\x89\x0e" ^ w (fresh_save name want)
let set_word addr v = "\xc7\x06" ^ w addr ^ w v
let set_dta k = "\xba" ^ w (dta k) ^ "\xb4\x1a\xcd\x21"
let set_cx v = "\xb9" ^ w v
let set_rr k v = set_word (fcb k + f_rr) (v land 0xffff) ^ set_word (fcb k + f_rr + 2) (v lsr 16)

let old_byte i = ((i * 13) + 5) land 0xff
let old n = String.init n (fun i -> Char.chr (old_byte i))

let program =
  String.concat ""
    [ (* 만들고 순차로 쓴다. 닫기 전에는 디렉터리 항목이 0 바이트다. *)
      call 0x16 0 "create NEW" 0;
      set_word (fcb 0 + f_recsize) 4;
      set_dta 0; call 0x15 0 "seq write 1" 0;
      set_dta 1; call 0x15 0 "seq write 2" 0;
      save_byte (fcb 0 + f_nr) "NR after two writes" 2;
      call 0x23 2 "size before close" 0;
      save_byte (fcb 2 + f_rr) "records before close" 0;
      call 0x10 0 "close NEW" 0;
      call 0x23 2 "size after close" 0;
      save_byte (fcb 2 + f_rr) "records after close" 1;
      call 0x16 1 "create EMPTY" 0;
      call 0x10 1 "close EMPTY" 0;
      (* 22h 는 NR 을 RR 로 맞추고 나아가지 않는다 — 다음 14h 가 같은 레코드를 읽는다 *)
      call 0x0F 3 "open OLD" 0;
      set_word (fcb 3 + f_recsize) 10; set_rr 3 5;
      set_dta 2; call 0x22 3 "random write" 0;
      save_byte (fcb 3 + f_nr) "NR after random write" 5;
      set_dta 5; call 0x14 3 "seq read after random write" 0;
      save_byte (fcb 3 + f_nr) "NR after seq read" 6;
      call 0x10 3 "close OLD" 0;
      (* 끝 너머에 쓰면 사이가 0 으로 찬다 *)
      call 0x0F 4 "open OLD2" 0;
      set_word (fcb 4 + f_recsize) 10; set_rr 4 12;
      set_dta 3; call 0x22 4 "write past the end" 0;
      call 0x10 4 "close OLD2" 0;
      (* 28h: CX 레코드를 쓰고 RR 도 나아간다. CX=0 은 길이를 RR 자리로 맞춘다 *)
      call 0x16 5 "create BLK" 0;
      set_word (fcb 5 + f_recsize) 3; set_rr 5 0;
      set_dta 4; set_cx 4; call 0x28 5 "block write" 0;
      save_cx "block write CX" 4;
      save_byte (fcb 5 + f_rr) "RR after block write" 4;
      save_byte (fcb 5 + f_nr) "NR after block write" 4;
      set_rr 5 2; set_cx 0; call 0x28 5 "block write 0 records" 0;
      save_cx "truncate CX" 0;
      save_byte (fcb 5 + f_rr) "RR after truncate" 2;
      save_byte (fcb 5 + f_size) "size after truncate" 6;
      call 0x10 5 "close BLK" 0;
      (* 27h: 일부 레코드는 0 으로 채워 한 레코드로 센다 *)
      call 0x0F 6 "open OLD3" 0;
      set_word (fcb 6 + f_recsize) 10; set_rr 6 0;
      set_dta 6; set_cx 5; call 0x27 6 "block read to the end" 3;
      save_cx "block read CX" 3;
      save_byte (fcb 6 + f_rr) "RR after block read" 3;
      set_dta 7; set_cx 2; call 0x27 6 "block read at the end" 1;
      save_cx "block read at the end CX" 0;
      save_byte (fcb 6 + f_rr) "RR stays at the end" 3;
      (* 열린 채 지워진 파일을 닫으면 FF *)
      call 0x0F 7 "open OLD4" 0;
      call 0x13 8 "delete OLD4" 0;
      call 0x10 7 "close a deleted file" 0xFF;
      (* 쓰지 않았으면 FCB 크기를 바꿔도 파일은 그대로다 *)
      call 0x0F 9 "open OLD5" 0;
      set_word (fcb 9 + f_size) 3;
      call 0x10 9 "close unwritten" 0;
      (* 썼으면 FCB 크기가 파일 길이가 된다 *)
      call 0x0F 10 "open OLD6" 0;
      set_word (fcb 10 + f_recsize) 4; set_rr 10 0;
      set_dta 8; call 0x22 10 "write OLD6" 0;
      set_word (fcb 10 + f_size) 8;
      call 0x10 10 "close written" 0;
      call 0x16 11 "create with ?" 0xFF;
      call 0x16 ext_slot "create a volume label" 0xFF;
      call 0x16 13 "create over a file" 0;
      call 0x23 15 "size right after create" 0;
      save_byte (fcb 15 + f_rr) "records right after create" 0;
      call 0x10 13 "close emptied" 0;
      (* 레코드가 64 바이트보다 작으면 블록 I/O 가 RR 넷째 바이트도 쓴다 — 2^24 를 넘을 때 보인다 *)
      call 0x16 16 "create HUGE" 0;
      set_word (fcb 16 + f_recsize) 1; set_rr 16 0xFFFFFF;
      set_dta 9; set_cx 2; call 0x28 16 "block write across 2^24" 0;
      call 0x10 16 "close HUGE" 0;
      set_dta 0; call 0x15 14 "write unopened" 0xFF;
      set_cx 2; call 0x28 14 "block write unopened" 0xFF;
      save_cx "unopened CX" 0;
      quit ]

let () =
  let m = Dos_machine.create () in
  List.iter (fun (n, len) -> Dos_machine.mount_file m n (old len))
    [ ("OLD.DAT", 300); ("OLD2.DAT", 100); ("OLD3.DAT", 25); ("OLD4.DAT", 9);
      ("OLD5.DAT", 20); ("OLD6.DAT", 20); ("OLD7.DAT", 12) ];
  Dos_machine.load_com m program;
  let put a v = Dos_state.wr8 m (psp + a) v in
  let put_s a s = String.iteri (fun i c -> put (a + i) (Char.code c)) s in
  for k = 0 to 16 do
    for i = 0 to 0x3F do put (slot k + i) 0 done;
    for i = 0 to 3 do put (fcb k + f_rr + i) unset done
  done;
  for k = 0 to 9 do for i = 0 to 0x3F do put (dta k + i) filler done done;
  List.iter (fun (k, s) -> put_s (dta k) s)
    [ (0, "ABCD"); (1, "EFGH"); (2, "0123456789"); (3, "XXXXXXXXXX");
      (4, "abcdefghijkl"); (8, "WXYZ"); (9, "HI") ];
  List.iter (fun (k, n) -> put_s (fcb k + 1) n)
    [ (0, "NEW     DAT"); (1, "EMPTY   DAT"); (2, "NEW     DAT"); (3, "OLD     DAT");
      (4, "OLD2    DAT"); (5, "BLK     DAT"); (6, "OLD3    DAT"); (7, "OLD4    DAT");
      (8, "OLD4    DAT"); (9, "OLD5    DAT"); (10, "OLD6    DAT"); (11, "BAD?    DAT");
      (ext_slot, "LABEL   DAT"); (13, "OLD7    DAT"); (14, "NOPE    DAT"); (15, "OLD7    DAT");
      (16, "HUGE    DAT") ];
  put (slot ext_slot) 0xFF; put (slot ext_slot + 6) 0x08;
  ignore (Dos_machine.run_until m ~max_steps:200_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let rd a = Dos_state.rd8 m (psp + a) in
  let rd16 a = rd a lor (rd (a + 1) lsl 8) in
  List.iter (fun (n, i, want) -> check ("AL " ^ n) (rd (result_at + i)) want) !results;
  List.iter (fun (n, i, want) -> check n (rd16 (save_at + (2 * i))) want) !saves;
  let content n = Option.value (Dos_machine.read_mounted m n) ~default:"<none>" in
  let patch s at p = String.sub s 0 at ^ p ^ String.sub s (at + String.length p) (String.length s - at - String.length p) in
  List.iter
    (fun (n, want) -> check_s ("file " ^ n) (content n) want)
    [ ("NEW.DAT", "ABCDEFGH"); ("EMPTY.DAT", "");
      ("OLD.DAT", patch (old 300) 50 "0123456789");
      ("OLD2.DAT", old 100 ^ String.make 20 '\000' ^ "XXXXXXXXXX");
      ("BLK.DAT", "abcdef"); ("OLD3.DAT", old 25); ("OLD4.DAT", "<none>");
      ("OLD5.DAT", old 20); ("OLD6.DAT", "WXYZ" ^ String.sub (old 20) 4 4);
      ("OLD7.DAT", ""); ("BAD?.DAT", "<none>"); ("LABEL.DAT", "<none>");
      ("NOPE.DAT", "<none>") ];
  (* 만든 FCB: C:, 쓴 날짜 *)
  let y, mo, d, _, _, _ = Dos_state.now_fields m in
  check "created drive" (rd (fcb 0)) 3;
  check "written date" (rd16 (fcb 0 + f_date)) (((y - 1980) lsl 9) lor (mo lsl 5) lor d);
  check "created extent" (rd16 (fcb 0 + f_extent)) 0;
  check "date of a write through an opened FCB" (rd16 (fcb 3 + f_date)) (((y - 1980) lsl 9) lor (mo lsl 5) lor d);
  List.iteri (fun i v -> check (Printf.sprintf "RR[%d] across 2^24" i) (rd (fcb 16 + f_rr + i)) v) [ 0x01; 0x00; 0x00; 0x01 ];
  (match Dos_machine.read_mounted m "HUGE.DAT" with
   | Some s ->
     check "HUGE length" (String.length s) 0x1000001;
     check_s "HUGE tail" (String.sub s 0xFFFFFF 2) "HI"
   | None -> check "HUGE exists" 0 1);
  check "block write sets RR byte 3 for small records" (rd (fcb 5 + f_rr + 3)) 0;
  (* 14h 는 22h 가 쓴 레코드를 다시 읽는다 *)
  String.iteri (fun i c -> check (Printf.sprintf "reread[%d]" i) (rd (dta 5 + i)) (Char.code c)) "0123456789";
  for i = 0 to 24 do check (Printf.sprintf "block read[%d]" i) (rd (dta 6 + i)) (old_byte i) done;
  for i = 25 to 29 do check (Printf.sprintf "block read blank[%d]" i) (rd (dta 6 + i)) 0 done;
  check "block read stops at 30" (rd (dta 6 + 30)) filler;
  check "eof block read leaves DTA" (rd (dta 7)) filler

let () =
  if !failed = 0 then print_endline "dos FCB write: all passed"
  else begin
    Printf.eprintf "dos FCB write: %d failures\n%!" !failed;
    exit 1
  end
