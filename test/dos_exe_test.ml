(* M2b: MZ EXE 로더(재배치), INT 21h 파일 표면, VGA Mode 13h. *)

let failed = ref 0
let checkb name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %b want %b\n%!" name got want
  end
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

(* 최소 MZ 헤더 + 재배치 테이블 + 이미지. reloc = (이미지 내 off, seg). *)
let make_exe ~image ~ip ~cs ~ss ~sp relocs =
  let b = Buffer.create (32 + String.length image) in
  let w v = Buffer.add_char b (Char.chr (v land 0xff));
            Buffer.add_char b (Char.chr ((v lsr 8) land 0xff)) in
  Buffer.add_string b "MZ";
  let total = 32 + String.length image in
  w ((total - 32) mod 512); if ((total - 32) mod 512) = 0 then (); (* last page *)
  w (((total - 32) + 511) / 512);  (* pages: 근사 — 로더는 안 읽음 *)
  w (List.length relocs);
  w 2;            (* header paras: 32 바이트 *)
  w 0; w 0;       (* minalloc/maxalloc *)
  w ss; w sp;
  w 0;            (* checksum *)
  w ip; w cs;
  w 0x1C;         (* reloc table offset *)
  w 0;            (* overlay *)
  List.iter (fun (o, sg) -> w o; w sg) relocs;
  (* reloc 이 없어도 헤더는 선언한 2 paras(32 바이트) 로 맞춘다 —
     로더가 건너뛰는 만큼 파일에 실제로 있어야 이미지 시작이 맞는다. *)
  if relocs = [] then Buffer.add_string b "\x00\x00\x00\x00";
  Buffer.add_string b image;
  Buffer.contents b

let () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m
    ("\xb4\x0e\xb3\x07\xbe\x15\x01\xac\x08\xc0\x74\x04\xcd\x10\xeb\xf7\xb8\x00\x4c\xcd\x21" ^ "Hi\x00");
  ignore m

let () =
  (* 1) EXE 로더 + 재배치: 이미지 0x1E 의 워드에 reloc 걸고 로드 후
     0x1000(로드 세그) 이 심어지는지, 그리고 코드가 실행되는지. *)
  (* DS 는 PSP 로 시작하므로 push cs/pop ds 가 선행한다 (ZZT 엔트리와
     같은 관례). msg 는 0x17. *)
  let img =
    "\x0e\x1f\xb4\x0e\xb3\x07\xbe\x17\x00\xac\x08\xc0\x74\x04\xcd\x10\xeb\xf7\xb8\x00\x4c\xcd\x21Hi\x00\x00\x00"
  in
  (* 0x20 위치의 워드(0)에 재배치 *)
  let exe = make_exe ~image:img ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE [ (0x20, 0x0000) ] in
  let m = Dos_machine.create () in
  Dos_machine.load_exe m exe;
  (* 재배치 적용: 이미지 0x20 워드에 image_seg 가 심어진다 — 주소는
     load_exe 의 PSP 배치(memtop 공식)에서 계산 *)
  let base = Dos_machine.psp_seg_of m * 16 in
  let image_seg = Dos_machine.psp_seg_of m + 0x10 in
  let reloc_word = Dos_machine.mem_read m (base + 0x100 + 0x20)
                   lor (Dos_machine.mem_read m (base + 0x100 + 0x21) lsl 8) in
  check "exe reloc applied" reloc_word image_seg;
  Dos_machine.run m ~max_steps:2000;
  checkb "exe hello exited" (Dos_machine.exited m) true;
  let txt = Dos_machine.screen_text m in
  let contains hay needle =
    let n = String.length needle and m2 = String.length hay in
    let rec at i j = j = n || (hay.[i + j] = needle.[j] && at i (j + 1)) in
    let rec go i = i + n <= m2 && (at i 0 || go (i + 1)) in
    go 0
  in
  checkb "exe hello text" (contains txt "Hi") true;

  (* 2) INT 21h: TEST.DAT 4바이트 읽기 → exit code = 읽은 수. *)
  let code =
    "\x0e\x1f"          (* push cs; pop ds *)
    ^ "\xb8\x00\x3d"    (* mov ax,3D00h *)
    ^ "\xba\x1c\x00"    (* mov dx,0x1C (name) *)
    ^ "\xcd\x21"        (* int 21h → AX=handle *)
    ^ "\x8b\xd8"        (* mov bx,ax *)
    ^ "\xb4\x3f"        (* mov ah,3Fh *)
    ^ "\xb9\x04\x00"    (* mov cx,4 *)
    ^ "\xba\x26\x00"    (* mov dx,0x26 (buf) *)
    ^ "\xcd\x21"        (* int 21h → AX=4 *)
    ^ "\xb4\x4c"        (* mov ah,4Ch — AL=읽은 수 *)
    ^ "\xcd\x21"        (* int 21h *)
    ^ "\xeb\xfe"        (* jmp $ (도달하지 않음) *)
    ^ "TEST.DAT\x00"    (* 0x1C *)
    ^ "\x00\x00\x00\x00\x00\x00"  (* 0x26 buf *)
  in
  let exe = make_exe ~image:code ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE [] in
  let m = Dos_machine.create () in
  Dos_machine.mount_file m "TEST.DAT" "ABCD123456";
  Dos_machine.load_exe m exe;
  Dos_machine.run m ~max_steps:2000;
  checkb "int21 open+read exited" (Dos_machine.exited m) true;
  check "int21 read 4 bytes" (Dos_machine.exit_code m) 4;
  (* 읽은 내용이 버퍼(이미지 0x26)에 — 이미지 시작 = PSP+0x100 *)
  let base = Dos_machine.psp_seg_of m * 16 in
  check "int21 buffer A" (Dos_machine.mem_read m (base + 0x100 + 0x26)) (Char.code 'A');
  check "int21 buffer D" (Dos_machine.mem_read m (base + 0x100 + 0x29)) (Char.code 'D');
  (* 없는 파일: open 실패 CF → 간단 프로그램은 그냥 검증 생략 *)

  (* 3) VGA 13h: 모드 전환 + A000 픽셀 + 팔레트/렌더. *)
  let code =
    "\xb8\x13\x00"   (* mov ax,0013h *)
    ^ "\xcd\x10"     (* int 10h *)
    ^ "\xb8\x00\xa0" (* mov ax,0A000h *)
    ^ "\x8e\xd8"     (* mov ds,ax *)
    ^ "\xbb\x00\x00" (* mov bx,0 *)
    ^ "\xb0\x04"     (* mov al,4 *)
    ^ "\x88\x07"     (* mov [bx],al *)
    ^ "\xb8\x00\x4c" (* mov ax,4C00h *)
    ^ "\xcd\x21"
  in
  let exe = make_exe ~image:code ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE [] in
  let m = Dos_machine.create () in
  Dos_machine.load_exe m exe;
  Dos_machine.run m ~max_steps:500;
  checkb "vga program exited" (Dos_machine.exited m) true;
  let w, h = Dos_machine.frame_dims m in
  check "vga width" w 320;
  check "vga height" h 200;
  check "vga pixel 0 = color 4" (Dos_machine.mem_read m 0xA0000) 4;
  let rgb = Dos_machine.frame_rgb m in
  (* 색 4 = 빨강 계열 (기본 팔레트 R 우세) *)
  let r = Char.code rgb.[0] and g = Char.code rgb.[1] and b = Char.code rgb.[2] in
  if not (r > g && r > b) then begin
    incr failed;
    Printf.eprintf "FAIL vga pixel red: r=%d g=%d b=%d\n%!" r g b
  end;

  (* #30: an "MZ"-signed image shorter than the fixed header (0x1C bytes)
     used to index past the string and raise a bare
     [Invalid_argument "index out of bounds"]. It now raises a named error
     before touching any header field past the magic bytes. *)
  checkb "a truncated MZ image raises a named error, not an index fault"
    (try
       Dos_machine.load_exe (Dos_machine.create ()) ("MZ" ^ String.make 8 '\x00');
       false
     with
     | Invalid_argument message -> message = "MZ header too short"
     | _ -> false)
    true;

  if !failed = 0 then print_endline "dos M2b: all passed"
  else begin
    Printf.eprintf "dos M2b: %d failures\n%!" !failed;
    exit 1
  end
