(* INT 10h AX=101Ah — 색 페이지 상태 읽기(VGA 전용). BL=페이징 방식(모드
   제어 10h 비트 7), BH=현재 쪽(4쪽 방식은 색 선택 14h 비트 3-2, 16쪽 방식은
   비트 3-0). 게임은 BX=FFFFh 로 불러 BX 가 바뀌었는지로 VGA 를 가린다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04X want %04X\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let out = 0x300
let psp = 0x10000
let attr_mode_control = 0x10
let attr_color_select = 0x14

(* mov ax,101Ah; mov bx,0FFFFh; int 10h; mov [0300h],bx; mov ax,4C00h; int 21h *)
let program =
  "\xb8" ^ w16 0x101A ^ "\xbb\xff\xff\xcd\x10" ^ "\x89\x1e" ^ w16 out ^ "\xb8\x00\x4c\xcd\x21"

(* [mode_control] 와 [color_select] 를 속성 레지스터에 두고 돌린 뒤 BX 를 읽는다 *)
let bx_after ~mode_control ~color_select =
  let m = Dos_machine.create () in
  Dos_machine.load_com m program;
  Dos_video.set_attr_palette m.Dos_state.video attr_mode_control mode_control;
  Dos_video.set_attr_palette m.Dos_state.video attr_color_select color_select;
  Dos_machine.run m ~max_steps:10_000;
  check "program exited" (if Dos_machine.exited m then 1 else 0) 1;
  Dos_machine.mem_read m (psp + out) lor (Dos_machine.mem_read m (psp + out + 1) lsl 8)

let () =
  (* 4쪽 방식(P54S=0): BL=0, BH 는 색 선택의 비트 3-2 *)
  check "4 pages, page 0" (bx_after ~mode_control:0x0C ~color_select:0x00) 0x0000;
  check "4 pages, page 3" (bx_after ~mode_control:0x0C ~color_select:0x0C) 0x0300;
  check "4 pages, page 1" (bx_after ~mode_control:0x0C ~color_select:0x04) 0x0100;
  (* 비트 3-2 밖의 비트는 보지 않는다 *)
  check "4 pages, bits outside 3-2 ignored" (bx_after ~mode_control:0x0C ~color_select:0xF3) 0x0000;
  (* 16쪽 방식(P54S=1): BL=1, BH 는 색 선택의 비트 3-0 *)
  check "16 pages, page 10" (bx_after ~mode_control:0x8C ~color_select:0x0A) 0x0A01;
  check "16 pages, page 15" (bx_after ~mode_control:0x8C ~color_select:0xFF) 0x0F01;
  (* 모드 제어의 다른 비트는 방식에 영향을 주지 않는다 *)
  check "16 pages with other mode bits" (bx_after ~mode_control:0xFF ~color_select:0x05) 0x0501;
  check "4 pages with other mode bits" (bx_after ~mode_control:0x7F ~color_select:0x08) 0x0200

let () =
  if !failed = 0 then print_endline "dos vga color page: all passed"
  else begin
    Printf.eprintf "dos vga color page: %d failures\n%!" !failed;
    exit 1
  end
