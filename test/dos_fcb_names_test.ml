(* FCB 지우기(13h)·이름 바꾸기(17h) — MS-DOS 4.0 FCBIO.ASM $FCB_Delete·
   $FCB_Rename, DELETE.ASM, RENAME.ASM(NEW_RENAME, BAD_ACC), MACRO2.ASM
   TransFCB, DIR.ASM 의 볼륨 레이블 검색. *)

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
let slot k = 0x4000 + (0x40 * k)

(* 이름 8 + 확장자 3 칸 *)
let fields name ext =
  let pad s n = s ^ String.make (n - String.length s) ' ' in
  pad name 8 ^ pad ext 3

type fcb = { at : int; header : int option; src : string; dst : string }

let fcbs = ref []
let results = ref []
let next = ref 0

(* 슬롯 하나에 FCB 를 놓고 AH 를 부른다. [attr] 가 있으면 확장 FCB 다. *)
let call ?attr ah ~src ?(dst = fields "" "") name want =
  let k = !next in
  incr next;
  fcbs := { at = slot k; header = attr; src; dst } :: !fcbs;
  results := (name, k, want) :: !results;
  "\xba" ^ w (slot k) ^ "\xb4" ^ b ah ^ "\xcd\x21" ^ "\xa2" ^ w (result_at + k)

let del ?attr src name want = call ?attr 0x13 ~src name want
let ren ?attr src dst name want = call ?attr 0x17 ~src ~dst name want

let program =
  String.concat ""
    [ del (fields "?" "TXT") "delete ?.TXT" 0;
      del (fields "NONE" "DAT") "delete nothing" 0xFF;
      del ~attr:0x08 (fields "C" "DAT") "volume-only search" 0xFF;
      del ~attr:0x29 (fields "C" "DAT") "volume plus ignored bits" 0xFF;
      del ~attr:0x0A (fields "D" "DAT") "volume plus hidden" 0;
      del (fields "c" "dat") "lower case" 0;
      del (fields "Z*" "TMP") "star fills the field" 0;
      del (fields "" "TMP") "blank name" 0xFF;
      del (fields "Y" "*") "star in the extension" 0;
      ren (fields "R?" "DOC") (fields "S?" "OLD") "rename R?.DOC" 0;
      ren (fields "P?" "DAT") (fields "Q?" "DAT") "rename into an existing name" 0xFF;
      ren (fields "X" "OLD") (fields "X" "OLD") "rename onto itself" 0xFF;
      ren (fields "KEEP" "BAK") (fields "KE:P" "BAK") "bad target char" 0xFF;
      ren (fields "KEEP" "BAK") (fields "" "SAV") "blank target" 0xFF;
      ren (fields "NONE" "DAT") (fields "X" "DAT") "rename nothing" 0xFF;
      ren ~attr:0x00 (fields "KEEP" "BAK") (fields "*" "SAV") "extended, star target" 0;
      quit ]

let files =
  [ ("A.TXT", "a"); ("B.TXT", "b"); ("C.DAT", "c"); ("D.DAT", "d");
    ("Z1.TMP", "z1"); ("Z2.TMP", "z2");
    ("R1.DOC", "r1"); ("R2.DOC", "r2"); ("R3.TXT", "r3");
    ("P1.DAT", "p1"); ("P2.DAT", "p2"); ("Q2.DAT", "q2");
    ("X.OLD", "x"); ("KEEP.BAK", "keep"); ("Y.A1", "y1"); ("Y.B2", "y2") ]

let () =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> Dos_machine.mount_file m n d) files;
  Dos_machine.load_com m program;
  let put a v = Dos_state.wr8 m (psp + a) v in
  let put_s a s = String.iteri (fun i c -> put (a + i) (Char.code c)) s in
  List.iter
    (fun { at; header; src; dst } ->
      for i = 0 to 0x3F do put (at + i) 0 done;
      let fcb =
        match header with
        | None -> at
        | Some attr -> put at 0xFF; put (at + 6) attr; at + 7
      in
      put_s (fcb + 1) src;
      put_s (fcb + 0x11) dst)
    !fcbs;
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  List.iter
    (fun (n, k, want) -> check ("AL " ^ n) (Dos_state.rd8 m (psp + result_at + k)) want)
    !results;
  let content n = Option.value (Dos_machine.read_mounted m n) ~default:"<none>" in
  let want =
    [ ("A.TXT", "<none>"); ("B.TXT", "<none>"); ("R3.TXT", "r3");
      ("C.DAT", "<none>"); ("D.DAT", "<none>");
      ("Z1.TMP", "<none>"); ("Z2.TMP", "<none>");
      ("R1.DOC", "<none>"); ("R2.DOC", "<none>"); ("S1.OLD", "r1"); ("S2.OLD", "r2");
      (* P1 은 Q1 이 되고, P2 → Q2 에서 멈춘다 *)
      ("Q1.DAT", "p1"); ("P1.DAT", "<none>"); ("P2.DAT", "p2"); ("Q2.DAT", "q2");
      ("X.OLD", "x"); ("KE:P.BAK", "<none>");
      ("KEEP.BAK", "<none>"); ("KEEP.SAV", "keep");
      ("Y.A1", "<none>"); ("Y.B2", "<none>") ]
  in
  List.iter (fun (n, c) -> check_s ("file " ^ n) (content n) c) want

let () =
  if !failed = 0 then print_endline "dos FCB names: all passed"
  else begin
    Printf.eprintf "dos FCB names: %d failures\n%!" !failed;
    exit 1
  end
