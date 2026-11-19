(* M2b: MZ EXE 로더(재배치), INT 21h 파일 표면, VGA Mode 13h. *)

let mount m name data =
  match Dos_machine.mount_file m name data with
  | Ok () -> ()
  | Error e -> failwith ("mount " ^ e)

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

(* 루트 EXE 의 PSP 는 COM 과 같은 0x1000 이다 *)
let root_psp = 0x1000

(* 최소 MZ 헤더 + 재배치 테이블 + 이미지. reloc = (이미지 내 off, seg). *)
let make_exe ?(minalloc = 0) ?(maxalloc = 0) ~image ~ip ~cs ~ss ~sp relocs =
  let b = Buffer.create (32 + String.length image) in
  let w v = Buffer.add_char b (Char.chr (v land 0xff));
            Buffer.add_char b (Char.chr ((v lsr 8) land 0xff)) in
  Buffer.add_string b "MZ";
  (* 페이지 수는 헤더까지 센다 — 로더는 [페이지 × 512] 를 모듈 끝으로 본다 *)
  let total = 32 + String.length image in
  w (total mod 512);              (* 마지막 페이지 바이트 *)
  w ((total + 511) / 512);        (* 페이지 수 *)
  w (List.length relocs);
  w 2;            (* header paras: 32 바이트 *)
  w minalloc; w maxalloc;
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
  mount m "TEST.DAT" "ABCD123456";
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
  (* 13h 화면은 RAM 이 아니라 평면에 있다 — 화면에 보이는 점으로 읽는다. *)
  check "vga pixel 0 = color 4" (Dos_machine.pixel m ~x:0 ~y:0) 4;
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

  (* #73: 적재 모듈은 헤더의 페이지 수가 정한다(MS-DOS 4.0 EXEC.ASM
     exec_save_start). 파일 뒤에 붙은 데이터는 싣지 않고, 메모리 배치도
     모듈 크기로 센다. 마지막 페이지 바이트(02h)는 보지 않으므로 같은
     페이지 안의 뒤 데이터는 실린다. *)
  let exit7 = "\xb8\x07\x4c\xcd\x21" in                (* mov ax,4C07h; int 21h *)
  let small = make_exe ~image:exit7 ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE [] in
  let module_bytes = 512 - 32 in                        (* 1 페이지 − 헤더 *)
  let trailer = String.make (700 * 1024) '\xaa' in
  let m = Dos_machine.create () in
  Dos_machine.load_exe m (small ^ trailer);
  check "trailer: the root psp does not depend on the file length"
    (Dos_machine.psp_seg_of m) root_psp;
  let base = (Dos_machine.psp_seg_of m + 0x10) * 16 in
  check "trailer: the rest of the last page loads"
    (Dos_machine.mem_read m (base + String.length exit7)) 0xaa;
  check "trailer: the last module byte loads"
    (Dos_machine.mem_read m (base + module_bytes - 1)) 0xaa;
  check "trailer: nothing past the pages loads"
    (Dos_machine.mem_read m (base + module_bytes)) 0;
  Dos_machine.run m ~max_steps:1000;
  check "trailer: program runs" (Dos_machine.exit_code m) 7;

  (* "ZM" 도 EXE 서명이다(EXEC.ASM exe_valid_old_signature) *)
  let zm = Bytes.of_string small in
  Bytes.set zm 0 'Z'; Bytes.set zm 1 'M';
  let m = Dos_machine.create () in
  Dos_machine.load_exe m (Bytes.to_string zm);
  Dos_machine.run m ~max_steps:1000;
  check "ZM signature runs" (Dos_machine.exit_code m) 7;

  (* 파일이 페이지 끝보다 511 바이트 짧으면 있는 만큼 싣고, 512 바이트
     짧으면 형식 오류다(EXEC.ASM exec_read_ok). 배치는 모자란 파일이
     아니라 페이지 수로 센다. *)
  let with_pages exe pages =
    let b = Bytes.of_string exe in
    Bytes.set b 4 (Char.chr (pages land 0xff));
    Bytes.set b 5 (Char.chr (pages lsr 8));
    Bytes.to_string b
  in
  let pad n = exit7 ^ String.make (n - String.length exit7) '\x00' in
  let short511 =
    with_pages (make_exe ~image:(pad (513 - 32)) ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE []) 2
  in
  let m = Dos_machine.create () in
  Dos_machine.load_exe m short511;
  check "511 short: loaded at the root psp"
    (Dos_machine.psp_seg_of m) root_psp;
  Dos_machine.run m ~max_steps:1000;
  check "511 short: program runs" (Dos_machine.exit_code m) 7;
  (* 루트 EXE 는 남은 메모리를 통째로 받고, AH=4Ah 로 줄이면 그 위가 비어
     AH=48h 가 성공한다. 위쪽에 실려 있으면 줄여도 위가 비지 않아 실패한다
     (MS C 런타임의 R6009, Snarf). 성공이면 종료 코드 7, 실패면 1. *)
  let shrink_then_alloc =
    "\xb4\x4a\xbb\x00\x01\xcd\x21"          (* mov ah,4Ah; mov bx,100h; int 21h *)
    ^ "\xb4\x48\xbb\x00\x20\xcd\x21"        (* mov ah,48h; mov bx,2000h; int 21h *)
    ^ "\x72\x05"                                (* jc fail *)
    ^ "\xb8\x07\x4c\xcd\x21"                  (* mov ax,4C07h; int 21h *)
    ^ "\xb8\x01\x4c\xcd\x21"                  (* fail: mov ax,4C01h; int 21h *)
  in
  let m = Dos_machine.create () in
  Dos_machine.load_exe m
    (make_exe ~maxalloc:0xFFFF ~image:shrink_then_alloc ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE []);
  Dos_machine.run m ~max_steps:1000;
  check "a root EXE that shrinks itself can then allocate" (Dos_machine.exit_code m) 7;

  (* 블록은 PSP(10h)·적재 모듈·그 뒤 maxalloc 문단까지다(minalloc 보다 적지
     않게, 메모리가 모자라면 있는 만큼). 런처처럼 maxalloc 을 작게 적은 EXE 는
     처음부터 위가 비어 있어 AH=4Ah 없이 AH=48h 가 된다. PSP:2 가 블록 끝이다. *)
  let alloc_only =
    "\xb4\x48\xbb\x00\x20\xcd\x21"          (* mov ah,48h; mov bx,2000h; int 21h *)
    ^ "\x72\x05"                                (* jc fail *)
    ^ "\xb8\x07\x4c\xcd\x21"                  (* mov ax,4C07h; int 21h *)
    ^ "\xb8\x01\x4c\xcd\x21"                  (* fail: mov ax,4C01h; int 21h *)
  in
  let module_paras = (512 - 32) / 16 in               (* 한 페이지 − 헤더 *)
  let load_with ~minalloc ~maxalloc =
    let m = Dos_machine.create () in
    Dos_machine.load_exe m
      (make_exe ~minalloc ~maxalloc ~image:alloc_only ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE []);
    m
  in
  let psp_top m =
    let a = Dos_machine.psp_seg_of m * 16 in
    Dos_machine.mem_read m (a + 2) lor (Dos_machine.mem_read m (a + 3) lsl 8)
  in
  let launcher = load_with ~minalloc:0x10 ~maxalloc:0x20 in
  check "maxalloc: PSP:2 is the end of the block"
    (psp_top launcher) (root_psp + 0x10 + module_paras + 0x20);
  Dos_machine.run launcher ~max_steps:1000;
  check "maxalloc: the memory above a small block is free" (Dos_machine.exit_code launcher) 7;
  let greedy = load_with ~minalloc:0 ~maxalloc:0xFFFF in
  check "maxalloc 0FFFFh: the program gets the rest of memory" (psp_top greedy) 0x9FFF;
  Dos_machine.run greedy ~max_steps:1000;
  check "maxalloc 0FFFFh: nothing is left to allocate" (Dos_machine.exit_code greedy) 1;
  let inverted = load_with ~minalloc:0x40 ~maxalloc:0x10 in
  check "minalloc wins over a smaller maxalloc"
    (psp_top inverted) (root_psp + 0x10 + module_paras + 0x40);
  let raises name image want =
    checkb name
      (try Dos_machine.load_exe (Dos_machine.create ()) image; false
       with Invalid_argument message -> message = want)
      true
  in
  raises "512 short: named error"
    (with_pages (make_exe ~image:(pad (512 - 32)) ~ip:0 ~cs:0 ~ss:0 ~sp:0xFFFE []) 2)
    "MZ file is a page or more shorter than its header says";
  raises "pages ending in the header: named error" (with_pages small 0)
    "MZ pages end inside the header";
  let many_relocs = Bytes.of_string small in
  Bytes.set many_relocs 6 '\x40';                        (* 64 항목 = 256 바이트 *)
  raises "relocations past the file: named error" (Bytes.to_string many_relocs)
    "MZ relocation table runs past the file";
  raises "no MZ signature: named error" "\xb8\x07\x4c\xcd\x21" "not an MZ image";

  if !failed = 0 then print_endline "dos M2b: all passed"
  else begin
    Printf.eprintf "dos M2b: %d failures\n%!" !failed;
    exit 1
  end
