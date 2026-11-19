(* INT 21h AH=6Ch — MS-DOS 4.0 FILE.ASM $Extended_Open, MKNODE.ASM
   make_type(없을 때 실패면 만들지 않는다), FILEMODE.INC 상수. *)

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
let psp = 0x10000
let out = 0x7000
let names = 0x6000
let cf = 1
let cx_in = 0x0020                          (* 보관 속성 — 실패하면 그대로 *)
let quit = "\xb8\x00\x4c\xcd\x21"

let files = [ "HAVE.DAT"; "NEW.DAT"; "KEEP.DAT"; "GONE.DAT"; "MISS2.DAT"; "SUB" ]
let name_at n =
  let rec go off = function
    | [] -> failwith n
    | x :: rest -> if x = n then names + off else go (off + String.length x + 1) rest
  in
  go 0 files

(* 6Ch 를 부르고 AX·CX·FLAGS 를 out+6k 에 둔다. CF 는 기대와 반대로 넣는다. *)
let ext k ~dx ~file ~carry =
  let at = out + (6 * k) in
  "\xb8\x00\x6c\xbb\x02\x00\xb9" ^ w cx_in ^ "\xba" ^ w dx ^ "\xbe" ^ w (name_at file)
  ^ (if carry then "\xf9" else "\xf8") ^ "\xcd\x21\x9c"
  ^ "\xa3" ^ w at ^ "\x89\x0e" ^ w (at + 2) ^ "\x58\xa3" ^ w (at + 4)

let mkdir file = "\xb4\x39\xba" ^ w (name_at file) ^ "\xcd\x21"

(* k 번째 호출이 받은 핸들을 닫는다 — 닫을 때 그 핸들의 바이트가 파일이 된다 *)
let close k = "\x8b\x1e" ^ w (out + (6 * k)) ^ "\xb4\x3e\xcd\x21"

let ok_ k ~dx ~file = ext k ~dx ~file ~carry:true
let err k ~dx ~file = ext k ~dx ~file ~carry:false

let program =
  String.concat ""
    [ ok_ 0 ~dx:0x0001 ~file:"HAVE.DAT";
      err 1 ~dx:0x0001 ~file:"NEW.DAT";
      err 2 ~dx:0x0010 ~file:"HAVE.DAT";
      ok_ 3 ~dx:0x0010 ~file:"NEW.DAT";
      ok_ 4 ~dx:0x0012 ~file:"HAVE.DAT";
      (* 0 이 먼저 연 핸들도 비워진 버퍼를 같이 본다 *)
      close 0; close 4;
      err 5 ~dx:0x0002 ~file:"GONE.DAT";
      err 6 ~dx:0x0000 ~file:"KEEP.DAT";
      err 7 ~dx:0x0003 ~file:"KEEP.DAT";
      err 8 ~dx:0x0020 ~file:"KEEP.DAT";
      err 9 ~dx:0x0201 ~file:"KEEP.DAT";
      ok_ 10 ~dx:0x0101 ~file:"KEEP.DAT";
      ok_ 11 ~dx:0x0011 ~file:"KEEP.DAT";
      ok_ 12 ~dx:0x0012 ~file:"MISS2.DAT";
      mkdir "SUB";
      err 13 ~dx:0x0011 ~file:"SUB";
      err 14 ~dx:0x0001 ~file:"SUB";
      quit ]

let opened = 1 and created = 2 and replaced = 3

let () =
  let m = Dos_machine.create () in
  let mount n data =
    match Dos_machine.mount_file m n data with Ok () -> () | Error e -> failwith e
  in
  mount "HAVE.DAT" "abc";
  mount "KEEP.DAT" "xyz";
  Dos_machine.load_com m program;
  ignore
    (List.fold_left
       (fun off n ->
         String.iteri (fun i c -> Dos_state.wr8 m (psp + off + i) (Char.code c)) (n ^ "\000");
         off + String.length n + 1)
       names files);
  ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let r k i = Dos_state.rd16 m (psp + out + (6 * k) + (2 * i)) in
  let success name k action =
    check (name ^ ": handle") (Bool.to_int (r k 0 >= 5 && r k 0 < 20)) 1;
    check (name ^ ": CX action") (r k 1) action;
    check (name ^ ": CF") (r k 2 land cf) 0
  and error name k code =
    check (name ^ ": AX") (r k 0) code;
    check (name ^ ": CX kept") (r k 1) cx_in;
    check (name ^ ": CF") (r k 2 land cf) cf
  in
  success "01h on a file" 0 opened;
  error "01h on nothing" 1 2;
  error "10h on a file" 2 0x50;
  success "10h on nothing" 3 created;
  success "12h on a file" 4 replaced;
  error "02h on nothing" 5 2;
  error "00h" 6 1;
  error "03h" 7 1;
  error "20h" 8 1;
  error "reserved bit 9" 9 1;
  success "bit 8 is not reserved" 10 opened;
  success "11h on a file" 11 opened;
  success "12h on nothing" 12 created;
  error "11h on a directory" 13 5;
  error "01h on a directory" 14 5;
  let content n = Option.value (Dos_machine.read_mounted m n) ~default:"<none>" in
  check_s "replaced file is empty" (content "HAVE.DAT") "";
  check_s "opened file keeps its bytes" (content "KEEP.DAT") "xyz";
  check_s "created file" (content "NEW.DAT") "";
  check_s "created with 12h" (content "MISS2.DAT") "";
  check_s "02h does not create" (content "GONE.DAT") "<none>"

let () =
  if !failed = 0 then print_endline "dos extended open: all passed"
  else begin
    Printf.eprintf "dos extended open: %d failures\n%!" !failed;
    exit 1
  end
