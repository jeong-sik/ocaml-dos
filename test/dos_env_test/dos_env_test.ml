(* 환경 블록 — PSP+2Ch 가 가리키는 "이름=값" 목록과 그 뒤 꼬리(워드 1,
   프로그램 이름). MS-DOS 4.0 src/DOS/EXEC.ASM 의 exec_check_environ ·
   exec_get_environ_len · exec_save_environ 을 따른다.

   자식은 자기 환경 세그먼트와 CS 를 0:4F0h(BIOS 의 프로그램 간 통신
   자리)에 남긴다. 부모는 EXEC 결과 AX 와 AH=48h 로 잰 가장 큰 빈 블록을
   그 옆에 남긴다. 테스트는 기계가 멈춘 뒤 이 값과 환경 바이트를 읽는다. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end
let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %S want %S\n%!" name got want
  end

let w16 b v =
  Buffer.add_char b (Char.chr (v land 0xff));
  Buffer.add_char b (Char.chr ((v lsr 8) land 0xff))

(* 0:4F0h 의 칸 *)
let child_env_slot = 0x4F0
let child_cs_slot = 0x4F2
let exec_ax_slot = 0x4F4
let largest_slot = 0x4F8
let child_code = 0x55
let parent_seg = 0x1000            (* 루트 COM 의 PSP *)

(* 자식: mov ax,[2Ch] / 0:4F0h ← ax, 0:4F2h ← cs / 코드 55h 로 끝 *)
let child =
  "\xa1\x2c\x00"                   (* mov ax,[002Ch] *)
  ^ "\x31\xdb\x8e\xc3"             (* xor bx,bx; mov es,bx *)
  ^ "\x26\xa3\xf0\x04"             (* mov es:[04F0h],ax *)
  ^ "\x26\x8c\x0e\xf2\x04"         (* mov es:[04F2h],cs *)
  ^ "\xb8\x55\x4c\xcd\x21"         (* mov ax,4C55h; int 21h *)

type exec = { name : string; env : int }

type op =
  | Shrink of int                  (* AH=4Ah, ES=PSP *)
  | Exec of exec                   (* AX=4B00h, 결과 AX 를 남김 *)
  | Probe_largest                  (* AH=48h BX=FFFFh, 돌아온 BX 를 남김 *)

let op_len = function Shrink _ -> 7 | Exec _ -> 11 + 10 | Probe_largest -> 7 + 11
let epb_len = 14

(* 부모 COM. 코드 뒤에 이름들, 그 뒤에 EPB 들을 둔다. [data_at] 을 주면
   이미지의 그 오프셋(PSP 기준)에 [data] 를 놓는다. *)
let parent ?(data_at = 0) ?(data = "") ops =
  let code_len = List.fold_left (fun n o -> n + op_len o) 0 ops + 5 in
  let execs = List.filter_map (function Exec e -> Some e | _ -> None) ops in
  let names_at = 0x100 + code_len in
  let name_offs, epbs_at =
    List.fold_left
      (fun (acc, at) e -> (acc @ [ at ], at + String.length e.name + 1))
      ([], names_at) execs
  in
  let b = Buffer.create 256 in
  let store_ax slot =
    Buffer.add_string b "\x06\x31\xdb\x8e\xc3\x26\xa3"; w16 b slot;
    Buffer.add_string b "\x07"
  in
  let k = ref 0 in
  List.iter
    (function
      | Shrink n -> Buffer.add_string b "\xb4\x4a\xbb"; w16 b n; Buffer.add_string b "\xcd\x21"
      | Exec _ ->
        Buffer.add_string b "\xb8\x00\x4b\xba"; w16 b (List.nth name_offs !k);
        Buffer.add_string b "\xbb"; w16 b (epbs_at + (!k * epb_len));
        Buffer.add_string b "\xcd\x21";
        store_ax (exec_ax_slot + (2 * !k));
        incr k
      | Probe_largest ->
        Buffer.add_string b "\xb4\x48\xbb\xff\xff\xcd\x21";
        Buffer.add_string b "\x06\x31\xc0\x8e\xc0\x26\x89\x1e"; w16 b largest_slot;
        Buffer.add_string b "\x07")
    ops;
  Buffer.add_string b "\xb8\x00\x4c\xcd\x21";
  assert (Buffer.length b = code_len);
  List.iter (fun e -> Buffer.add_string b e.name; Buffer.add_char b '\x00') execs;
  List.iter
    (fun e -> w16 b e.env; Buffer.add_string b (String.make (epb_len - 2) '\x00'))
    execs;
  let image = Buffer.contents b in
  if data = "" then image
  else begin
    let at = data_at - 0x100 in
    assert (at >= String.length image);
    image ^ String.make (at - String.length image) '\x00' ^ data
  end

let word m a = Dos_machine.mem_read m a lor (Dos_machine.mem_read m (a + 1) lsl 8)
let bytes_at m a n = String.init n (fun i -> Char.chr (Dos_machine.mem_read m (a + i)))

let run ?name ?(files = [ ("CHILD.COM", child) ]) ?(setup = fun _ -> ()) image =
  let m = Dos_machine.create () in
  List.iter (fun (n, d) -> Dos_machine.mount_file m n d) files;
  Dos_machine.load_com ?name m image;
  setup m;
  Dos_machine.run m ~max_steps:400_000;
  check "machine exits" (if Dos_machine.exited m then 1 else 0) 1;
  m

let tail name = "\x01\x00" ^ name ^ "\x00"
let empty_list = "\x00\x00"

let () =
  (* 1) 루트: 이름을 주면 0x500 에 빈 목록과 꼬리 C:\NAME, 없으면 목록만 *)
  let m = Dos_machine.create () in
  Dos_machine.load_com ~name:"game.com" m (parent []);
  let root_env = 0x500 * 16 in
  check "root PSP points at 0x500" (word m ((parent_seg * 16) + 0x2C)) 0x500;
  let want = empty_list ^ tail "C:\\GAME.COM" in
  check_s "root tail" (bytes_at m root_env (String.length want)) want;
  let m = Dos_machine.create () in
  Dos_machine.mem_write m (root_env + 2) 0x77;
  Dos_machine.load_com m (parent []);
  check_s "no name, no tail" (bytes_at m root_env 3) (empty_list ^ "\x77");
  (* 0x500..0x5FF 를 넘는 이름은 거절. 바이트 수 = 목록 2 + 워드 2 + "C:\\"
     3 + 이름 + NUL 1 이라 248 자까지 들어간다. *)
  let long = String.make 249 'A' in
  check "a name past the root environment is refused"
    (try Dos_machine.load_com ~name:long (Dos_machine.create ()) (parent []); 0
     with Invalid_argument _ -> 1)
    1;
  check "a name filling the root environment fits"
    (try Dos_machine.load_com ~name:(String.make 248 'A') (Dos_machine.create ()) (parent []); 1
     with Invalid_argument _ -> 0)
    1

let () =
  (* 2) EPB 환경 0: 부모 환경의 목록(꼬리는 빼고)을 새 블록에 옮기고 자식
     꼬리를 붙인다. 이름은 호출자가 준 그대로(소문자)다. 블록은 부모 블록
     바로 뒤(0x1020), 자식 PSP 는 그 다음 문단. *)
  let m =
    run ~name:"PARENT.COM" (parent [ Shrink 0x20; Exec { name = "child.com"; env = 0 } ])
  in
  check "child code" (word m exec_ax_slot) child_code;
  let env = word m child_env_slot in
  check "child environment block" env 0x1020;
  check "child PSP after its environment" (word m child_cs_slot) 0x1021;
  check_s "child environment bytes" (bytes_at m (env * 16) 14)
    (empty_list ^ tail "child.com")

let () =
  (* 3) EPB 가 준 환경을 옮긴다 — 같은 세그먼트를 쓰지 않는다. 목록 9 바이트
     + 꼬리 12 바이트 = 21 바이트라 2 문단을 받는다. *)
  let list = "A=1\x00B=2\x00\x00" in
  let image =
    parent ~data_at:0x180 ~data:list
      [ Shrink 0x20; Exec { name = "CHILD.COM"; env = parent_seg + 0x18 } ]
  in
  let m = run image in
  check "child code (EPB env)" (word m exec_ax_slot) child_code;
  let env = word m child_env_slot in
  check "EPB environment is copied" env 0x1020;
  check "child PSP after two paragraphs" (word m child_cs_slot) 0x1022;
  check_s "copied list and tail" (bytes_at m (env * 16) 21) (list ^ tail "CHILD.COM")

let () =
  (* 4) 목록 길이 상한: 끝 NUL 두 개까지 0x7FFF 바이트는 받고 0x8000 은 오류
     10(잘못된 환경)이다. 목록은 빈 메모리 2000:0 에 둔다. *)
  let at = 0x2000 in
  let fill n m =
    for i = 0 to n - 1 do Dos_machine.mem_write m ((at * 16) + i) (Char.code 'A') done;
    Dos_machine.mem_write m ((at * 16) + n) 0;
    Dos_machine.mem_write m ((at * 16) + n + 1) 0
  in
  let image = parent [ Shrink 0x20; Exec { name = "CHILD.COM"; env = at } ] in
  let m = run ~setup:(fill 0x7FFD) image in
  check "0x7FFF-byte list accepted" (word m exec_ax_slot) child_code;
  let m = run ~setup:(fill 0x7FFE) image in
  check "0x8000-byte list is error 10" (word m exec_ax_slot) 10

let () =
  (* 5) 환경을 받은 뒤 실패하면 블록을 놓는다(EXEC.ASM exec_bomb →
     exec_dealloc).
     - 헤더 오류 11: 다음 자식의 환경이 같은 자리(0x1020)에 온다.
     - 자리 오류 8: 부모가 빈 곳을 10h 문단만 남기면 환경(1 문단)은 들어가고
       COM(1000h 문단)은 안 들어간다. 그 뒤 AH=48h 가 10h 를 본다. *)
  let bad_exe = "MZ\x00\x00\x00\x00\x00\x00\x02\x00" ^ String.make 22 '\x00' in
  (* 헤더 2 문단, 페이지 0 — 페이지가 헤더 안에서 끝난다 *)
  let m =
    run ~files:[ ("CHILD.COM", child); ("BAD.EXE", bad_exe) ]
      (parent
         [ Shrink 0x20; Exec { name = "BAD.EXE"; env = 0 };
           Exec { name = "CHILD.COM"; env = 0 } ])
  in
  check "bad header is error 11" (word m exec_ax_slot) 11;
  check "next child after a format error" (word m (exec_ax_slot + 2)) child_code;
  check "environment released after error 11" (word m child_env_slot) 0x1020;
  let leave = 0x10 in
  let m =
    run
      (parent
         [ Shrink (0x9FFF - parent_seg - leave); Exec { name = "CHILD.COM"; env = 0 };
           Probe_largest ])
  in
  check "no room for the program is error 8" (word m exec_ax_slot) 8;
  check "environment released after error 8" (word m largest_slot) leave;
  (* 환경도 안 들어가면 같은 8 이다 *)
  let m = run (parent [ Exec { name = "CHILD.COM"; env = 0 }; Probe_largest ]) in
  check "no room for the environment is error 8" (word m exec_ax_slot) 8;
  check "nothing allocated" (word m largest_slot) 0

let () =
  (* 6) 부모 PSP 의 환경이 0 이면 자식도 환경이 없다(exec_check_environ) *)
  let image = parent [ Shrink 0x20; Exec { name = "CHILD.COM"; env = 0 } ] in
  let m =
    run ~setup:(fun m -> Dos_machine.mem_write m ((parent_seg * 16) + 0x2C) 0;
                 Dos_machine.mem_write m ((parent_seg * 16) + 0x2D) 0) image
  in
  check "no environment, child code" (word m exec_ax_slot) child_code;
  check "no environment, PSP+2Ch is 0" (word m child_env_slot) 0;
  check "no environment, PSP at the first free paragraph" (word m child_cs_slot) 0x1020

let () =
  (* 7) 상주 종료는 환경 블록을 남긴다(RBIL INT 21/31). TSR 의 환경 0x1020,
     PSP 0x1021, 남긴 PSP 블록 (0x1021, 0x13). 다음 자식의 환경은 0x1034. *)
  let tsr = "\xba\x20\x01\xcd\x27" in                     (* mov dx,0x120; int 27h *)
  let m =
    run ~files:[ ("CHILD.COM", child); ("TSR.COM", tsr) ]
      (parent
         [ Shrink 0x20; Exec { name = "TSR.COM"; env = 0 };
           Exec { name = "CHILD.COM"; env = 0 } ])
  in
  check "child after a TSR" (word m (exec_ax_slot + 2)) child_code;
  check "TSR environment stays" (word m child_env_slot) 0x1034

let () =
  if !failed = 0 then print_endline "dos environment: all passed"
  else begin
    Printf.eprintf "dos environment: %d failures\n%!" !failed;
    exit 1
  end
