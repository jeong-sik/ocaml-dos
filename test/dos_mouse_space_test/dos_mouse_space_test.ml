(* 마우스 가상 화면 표 — CuteMouse ctmouse.asm setupvideo 의 모드별 화면·칸.
   BDA 를 쓴 메모리에서 읽는 길(current)과 모드 번호에서 바로 고르는 길을
   함께 본다. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let check_space name (s : Dos_mouse_space.t) ~width ~height ~cell_w ~cell_h =
  check (name ^ " 너비") s.width width;
  check (name ^ " 높이") s.height height;
  check (name ^ " 칸 너비") s.cell_w cell_w;
  check (name ^ " 칸 높이") s.cell_h cell_h

(* BIOS 가 모드를 세운 뒤 BDA 에 남기는 값: 0x449 모드, 0x44A 열 수, 0x484 마지막 줄 *)
let bda ~mode ~columns ~last_row =
  let mem = Bytes.make 0x500 '\000' in
  Bytes.set_uint8 mem 0x449 mode;
  Bytes.set_uint16_le mem 0x44A columns;
  Bytes.set_uint8 mem 0x484 last_row;
  Dos_mouse_space.current mem

let test_text () =
  check_space "모드 3 80x25" (bda ~mode:3 ~columns:80 ~last_row:24)
    ~width:640 ~height:200 ~cell_w:8 ~cell_h:8;
  check_space "모드 2 80x25" (bda ~mode:2 ~columns:80 ~last_row:24)
    ~width:640 ~height:200 ~cell_w:8 ~cell_h:8;
  check_space "모드 7 80x25" (bda ~mode:7 ~columns:80 ~last_row:24)
    ~width:640 ~height:200 ~cell_w:8 ~cell_h:8;
  (* 40열은 글자 하나가 16 이라 너비는 그대로 640 *)
  check_space "모드 1 40x25" (bda ~mode:1 ~columns:40 ~last_row:24)
    ~width:640 ~height:200 ~cell_w:16 ~cell_h:8;
  check_space "모드 0 40x25" (bda ~mode:0 ~columns:40 ~last_row:24)
    ~width:640 ~height:200 ~cell_w:16 ~cell_h:8;
  (* 43·50줄은 BDA 의 마지막 줄을 따른다 *)
  check_space "모드 3 80x50" (bda ~mode:3 ~columns:80 ~last_row:49)
    ~width:640 ~height:400 ~cell_w:8 ~cell_h:8;
  check_space "모드 3 132열" (bda ~mode:3 ~columns:132 ~last_row:24)
    ~width:1056 ~height:200 ~cell_w:8 ~cell_h:8;
  (* 마지막 줄 바이트가 0 이면(옛 기계) 200 줄 *)
  check_space "마지막 줄 0" (bda ~mode:3 ~columns:80 ~last_row:0)
    ~width:640 ~height:200 ~cell_w:8 ~cell_h:8

let test_graphics () =
  let g mode = bda ~mode ~columns:40 ~last_row:24 in
  List.iter
    (fun (mode, height, cell_w) ->
      check_space (Printf.sprintf "모드 %02Xh" mode) (g mode)
        ~width:640 ~height ~cell_w ~cell_h:1)
    [ (0x04, 200, 2); (0x05, 200, 2); (0x06, 200, 1); (0x0D, 200, 2);
      (0x0E, 200, 1); (0x0F, 350, 1); (0x10, 350, 1); (0x11, 480, 1);
      (0x12, 480, 1); (0x13, 200, 2);
      (* 표에 없는 모드 *)
      (0x08, 200, 1); (0x6A, 200, 1) ];
  (* 그래픽 모드는 BDA 의 열·줄을 보지 않는다 *)
  check_space "모드 13h 열 80" (bda ~mode:0x13 ~columns:80 ~last_row:49)
    ~width:640 ~height:200 ~cell_w:2 ~cell_h:1

let test_of_mode_matches_current () =
  List.iter
    (fun mode ->
      let a = Dos_mouse_space.of_mode ~mode ~columns:80 ~last_row:24 in
      let b = bda ~mode ~columns:80 ~last_row:24 in
      check (Printf.sprintf "모드 %02Xh 두 길이 같다" mode) (Bool.to_int (a = b)) 1)
    [ 0; 1; 2; 3; 4; 5; 6; 7; 0x0D; 0x0E; 0x0F; 0x10; 0x11; 0x12; 0x13 ]

let test_range_center_cells () =
  let s = bda ~mode:3 ~columns:80 ~last_row:24 in
  check "가로 범위 시작" (fst (Dos_mouse_space.range_x s)) 0;
  check "가로 범위 끝" (snd (Dos_mouse_space.range_x s)) 639;
  check "세로 범위 끝" (snd (Dos_mouse_space.range_y s)) 199;
  check "가운데 x" (fst (Dos_mouse_space.center s)) 320;
  check "가운데 y" (snd (Dos_mouse_space.center s)) 100;
  check "텍스트 칸 x" (Dos_mouse_space.cell_x s 327) 320;
  check "텍스트 칸 x 경계" (Dos_mouse_space.cell_x s 328) 328;
  check "텍스트 칸 y" (Dos_mouse_space.cell_y s 15) 8;
  let w = bda ~mode:1 ~columns:40 ~last_row:24 in
  check "40열 칸 x" (Dos_mouse_space.cell_x w 31) 16;
  let v = bda ~mode:0x13 ~columns:40 ~last_row:24 in
  check "13h 칸 x" (Dos_mouse_space.cell_x v 321) 320;
  check "13h 칸 y 는 그대로" (Dos_mouse_space.cell_y v 101) 101;
  let e = bda ~mode:0x12 ~columns:80 ~last_row:29 in
  check "12h 가운데 y" (snd (Dos_mouse_space.center e)) 240;
  check "12h 칸 x 는 그대로" (Dos_mouse_space.cell_x e 321) 321

let () =
  test_text ();
  test_graphics ();
  test_of_mode_matches_current ();
  test_range_center_cells ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n" !failed;
    exit 1
  end;
  print_endline "dos_mouse_space_test: ok"
