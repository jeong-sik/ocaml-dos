(* FCB 찾기 11h·12h (MS-DOS 4.0 SEARCH.ASM $DIR_SEARCH_FIRST·$DIR_SEARCH_NEXT).
   찾은 파일은 DTA 에 [드라이브 1 바이트 + 디렉터리 항목 32 바이트]로 쓰고,
   확장 FCB 면 그 앞에 FFh·0 다섯 개·검색 속성이 붙는다. 못 찾으면 AL=FFh 이고
   DTA 는 그대로다. 파일은 이름순으로 나온다. *)

let mount m name data =
  match Dos_machine.mount_file m name data with
  | Ok () -> ()
  | Error e -> failwith ("mount " ^ e)

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
let result_at = 0x7000
let filler = 0xEE
let slot_len = 0x40

let fcb_at k = 0x4000 + (slot_len * k)
let dta_at k = 0x5000 + (slot_len * k)

let ext_header attr = "\xff\x00\x00\x00\x00\x00" ^ b attr

(* 슬롯 k 의 FCB: (확장 머리, 드라이브 바이트, 이름 8 + 확장자 3) *)
let fcbs =
  [ (0, "", 0, "CASTLE  RAN");
    (1, "", 0, "CASTLE  ???");
    (2, "", 0, "ZZZ     ???");
    (3, ext_header 0x02, 3, "????????TXT");
    (4, "", 0, "A/B     TXT") ]

(* (이름, AH, FCB 슬롯, DTA 슬롯, 기대 AL) *)
let steps =
  [ ("first CASTLE.RAN", 0x11, 0, 0, 0x00);
    ("first CASTLE.???", 0x11, 1, 1, 0x00);
    ("next CASTLE.???", 0x12, 1, 2, 0x00);
    ("next past the end", 0x12, 1, 3, 0xFF);
    ("first ZZZ.???", 0x11, 2, 4, 0xFF);
    ("first extended ????????.TXT", 0x11, 3, 5, 0x00);
    ("first with a bad character", 0x11, 4, 6, 0xFF) ]

let code =
  String.concat ""
    (List.mapi
       (fun i (_, ah, k, d, _) ->
         "\xba" ^ w (dta_at d) ^ "\xb4\x1a\xcd\x21"
         ^ "\xba" ^ w (fcb_at k) ^ "\xb4" ^ b ah ^ "\xcd\x21"
         ^ "\xa2" ^ w (result_at + i))
       steps)
  ^ "\xb8\x00\x4c\xcd\x21"

let () =
  let m = Dos_machine.create () in
  mount m "CASTLE.EXE" "abc";
  mount m "CASTLE.RAN" "12345";
  mount m "NOTES.TXT" "1234567";
  Dos_machine.load_com m code;
  let base = Dos_machine.psp_seg_of m * 16 in
  let put addr s = String.iteri (fun i c -> Dos_machine.mem_write m (base + addr + i) (Char.code c)) s in
  List.iter (fun (k, hdr, drive, pat) -> put (fcb_at k) (hdr ^ b drive ^ pat)) fcbs;
  List.iteri (fun d _ -> put (dta_at d) (String.make slot_len (Char.chr filler))) steps;
  Dos_machine.run m ~max_steps:200_000;
  check "exited" (if Dos_machine.exited m then 1 else 0) 1;
  let byte a = Dos_machine.mem_read m (base + a) in
  let word a = byte a lor (byte (a + 1) lsl 8) in
  let str a n = String.init n (fun i -> Char.chr (byte (a + i))) in
  List.iteri (fun i (name, _, _, _, want) -> check ("AL " ^ name) (byte (result_at + i)) want) steps;
  (* 디렉터리 항목: 이름 11, 속성, 예약 10, 시각, 날짜, 첫 클러스터, 크기 *)
  let entry name ~at ~drive ~fname ~size =
    check (name ^ ": drive byte") (byte at) drive;
    check_s (name ^ ": name") (str (at + 1) 11) fname;
    check (name ^ ": attribute") (byte (at + 12)) 0x20;
    check (name ^ ": time") (word (at + 23)) 0;
    check (name ^ ": date") (word (at + 25)) 0x2821;
    check (name ^ ": cluster") (word (at + 27)) 0;
    check (name ^ ": size low") (word (at + 29)) size;
    check (name ^ ": size high") (word (at + 31)) 0
  in
  (* 드라이브 0 은 현재 드라이브(C:)가 된다 *)
  entry "CASTLE.RAN" ~at:(dta_at 0) ~drive:3 ~fname:"CASTLE  RAN" ~size:5;
  entry "first of CASTLE.???" ~at:(dta_at 1) ~drive:3 ~fname:"CASTLE  EXE" ~size:3;
  entry "next of CASTLE.???" ~at:(dta_at 2) ~drive:3 ~fname:"CASTLE  RAN" ~size:5;
  (* 확장 FCB: 머리 7 바이트의 속성은 검색 속성이다 *)
  check_s "extended: header" (str (dta_at 5) 7) (ext_header 0x02);
  entry "extended" ~at:(dta_at 5 + 7) ~drive:3 ~fname:"NOTES   TXT" ~size:7;
  (* 못 찾으면 DTA 를 건드리지 않는다 *)
  List.iter
    (fun (name, d) -> check (name ^ ": DTA untouched") (byte (dta_at d)) filler)
    [ ("next past the end", 3); ("ZZZ.???", 4); ("bad character", 6) ];
  if !failed = 0 then print_endline "dos fcb find: all passed"
  else begin
    Printf.eprintf "dos fcb find: %d failures\n%!" !failed;
    exit 1
  end
