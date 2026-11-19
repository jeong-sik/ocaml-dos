(* The mouse driver draws its cursor into video memory (issue #77). COM
   programs call INT 10h and INT 33h and stop at "jmp $" pauses; the test
   looks at the screen there, then steps over the pause. The expected
   points come from the DOSBox-X default arrow, written out again here so a
   changed table fails. *)

open Dos_state

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got 0x%x want 0x%x\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let regs ?(bx = 0) ?(cx = 0) ?(dx = 0) ax =
  "\xb8" ^ w16 ax ^ "\xbb" ^ w16 bx ^ "\xb9" ^ w16 cx ^ "\xba" ^ w16 dx
let mouse ?bx ?cx ?dx ax = regs ?bx ?cx ?dx ax ^ "\xcd\x33"
let video ax = regs ax ^ "\xcd\x10"
let es_is_cs = "\x0e\x07"           (* push cs; pop es *)
let pause = "\xeb\xfe"              (* jmp $ *)
let quit = "\xb8\x00\x4c\xcd\x21"

let show = 0x01
let hide = 0x02
let set_position = 0x04
let graphics_shape = 0x09
let text_shape = 0x0A
let reset = 0x00
let mode_320x200_256 = 0x0013
let mode_640x480_16 = 0x0012
let mode_80x25_text = 0x0003
let keep_screen = 0x80

let cs_reg = 1
let pause_byte_0 = 0xEB
let pause_byte_1 = 0xFE
let pause_len = 2
let max_steps = 200_000

let at_pause m =
  let a = (Cpu86.seg m.cpu cs_reg * 16) + Cpu86.dump_ip m.cpu in
  Dos_machine.mem_read m a = pause_byte_0 && Dos_machine.mem_read m (a + 1) = pause_byte_1

let to_pause name m =
  ignore (Dos_machine.run_until m ~max_steps ~stop:at_pause);
  check (name ^ ": reached the pause") (at_pause m)

let step_over m = Cpu86.set_ip m.cpu (Cpu86.dump_ip m.cpu + pause_len)

let machine code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  m

(* DOSBox-X mouse.cpp defaultScreenMask, defaultCursorMask *)
let arrow_screen =
  [| 0x3FFF; 0x1FFF; 0x0FFF; 0x07FF; 0x03FF; 0x01FF; 0x00FF; 0x007F;
     0x003F; 0x001F; 0x01FF; 0x00FF; 0x30FF; 0xF87F; 0xF87F; 0xFCFF |]
let arrow_cursor =
  [| 0x0000; 0x4000; 0x6000; 0x7000; 0x7800; 0x7C00; 0x7E00; 0x7F00;
     0x7F80; 0x7C00; 0x6C00; 0x4600; 0x0600; 0x0300; 0x0300; 0x0000 |]

let side = 16
let lit = 0x0F

let arrow_point ~under ~row ~col =
  let bit = 1 lsl (side - 1 - col) in
  let kept = if arrow_screen.(row) land bit <> 0 then under else 0 in
  if arrow_cursor.(row) land bit <> 0 then kept lxor lit else kept

let pixel m x y = Dos_video.get_pixel m.video ~x ~y
let fill m ~x ~y ~w ~h color =
  for yy = y to y + h - 1 do
    for xx = x to x + w - 1 do Dos_video.put_pixel m.video ~x:xx ~y:yy ~color done
  done

let block_is name m ~x ~y f =
  let bad = ref 0 in
  for row = 0 to side - 1 do
    for col = 0 to side - 1 do
      if pixel m (x + col) (y + row) <> f ~row ~col then incr bad
    done
  done;
  checki (name ^ ": points that differ") !bad 0

(* ---------- 320x200x256 ---------- *)

let back = 0xA5

let test_256 () =
  let code =
    String.concat ""
      [ video mode_320x200_256; mouse reset; mouse set_position ~cx:320 ~dx:100; pause;
        mouse show; pause;
        mouse set_position ~cx:40 ~dx:10; pause;
        mouse hide; pause;
        mouse show; pause;
        mouse hide; pause;
        mouse hide; mouse show; pause;
        mouse show; video (mode_320x200_256 lor keep_screen); pause;
        quit ]
  in
  let m = machine code in
  to_pause "256 setup" m;
  (* virtual x 320 is point 160 in a 320-wide mode *)
  fill m ~x:150 ~y:95 ~w:40 ~h:30 back;
  check "256: nothing drawn while hidden" (m.mouse.mouse_drawn = Not_drawn);
  step_over m;
  to_pause "256 show" m;
  checki "256: row 0 left point is cleared" (pixel m 160 100) 0;
  checki "256: row 0 third point keeps the screen" (pixel m 162 100) back;
  checki "256: row 1 second point is lit" (pixel m 161 101) lit;
  checki "256: row 15 last point keeps the screen" (pixel m 175 115) back;
  block_is "256 arrow" m ~x:160 ~y:100 (arrow_point ~under:back);
  checki "256: the point right of the block is untouched" (pixel m 176 100) back;
  step_over m;
  to_pause "256 position while shown" m;
  block_is "256 04h puts the old place back" m ~x:160 ~y:100 (fun ~row:_ ~col:_ -> back);
  block_is "256 04h draws at the new place" m ~x:20 ~y:10 (arrow_point ~under:0);
  step_over m;
  to_pause "256 hide" m;
  block_is "256 hidden" m ~x:160 ~y:100 (fun ~row:_ ~col:_ -> back);
  block_is "256 hidden, 04h place" m ~x:20 ~y:10 (fun ~row:_ ~col:_ -> 0);
  check "256: hidden means not drawn" (m.mouse.mouse_drawn = Not_drawn);
  step_over m;
  to_pause "256 show again" m;
  (* the harness moves the mouse: the old place comes back, the new one is drawn *)
  Dos_machine.set_mouse m ~x:400 ~y:150 ~buttons:0;
  block_is "256 old place after a move" m ~x:160 ~y:100 (fun ~row:_ ~col:_ -> back);
  block_is "256 new place after a move" m ~x:200 ~y:150 (arrow_point ~under:0);
  let r =
    match Dos_snapshot.save m with
    | Error e -> failwith (Dos_snapshot.save_error_to_string e)
    | Ok s ->
      (match Dos_snapshot.restore s with
       | Ok r -> r
       | Error e -> failwith (Dos_snapshot.error_to_string e))
  in
  check "256: the drawn cursor survives a snapshot" (r.mouse.mouse_drawn = m.mouse.mouse_drawn);
  step_over r;
  to_pause "256 hide after restore" r;
  block_is "256 restored machine puts the background back" r ~x:200 ~y:150
    (fun ~row:_ ~col:_ -> 0);
  step_over r;
  to_pause "256 hide twice show once" r;
  check "256: two hides need two shows" (r.mouse.mouse_drawn = Not_drawn);
  block_is "256 still hidden" r ~x:200 ~y:150 (fun ~row:_ ~col:_ -> 0);
  step_over r;
  to_pause "256 mode set" r;
  checki "256: a mode set hides the cursor" r.mouse.mouse_cursor_flag cursor_flag_after_reset;
  check "256: a mode set leaves nothing drawn" (r.mouse.mouse_drawn = Not_drawn);
  block_is "256 kept screen has no cursor" r ~x:200 ~y:150 (fun ~row:_ ~col:_ -> 0)

(* A cursor of the program's own with a negative hot spot: all lit, drawn
   one point right and two below the mouse. The arrow is shown first, so
   09h has to put its place back. *)
let test_custom () =
  let masks = 0x400 in
  let code =
    String.concat ""
      [ video mode_320x200_256; mouse reset; mouse set_position ~cx:100 ~dx:50;
        mouse show; es_is_cs; mouse graphics_shape ~bx:0xFFFF ~cx:0xFFFE ~dx:masks; pause; quit ]
  in
  assert (0x100 + String.length code <= masks);
  let image =
    code ^ String.make (masks - 0x100 - String.length code) '\x90'
    ^ String.concat "" (List.init side (fun _ -> w16 0x0000))
    ^ String.concat "" (List.init side (fun _ -> w16 0xFFFF))
  in
  let m = machine image in
  to_pause "custom" m;
  block_is "custom block" m ~x:51 ~y:52 (fun ~row:_ ~col:_ -> lit);
  checki "custom: left of the block is untouched" (pixel m 50 52) 0;
  checki "custom: above the block is untouched" (pixel m 51 51) 0

(* ---------- 640x480x16: the corner cuts the block to one point ---------- *)

let test_planar_clip () =
  let code =
    String.concat ""
      [ video mode_640x480_16; mouse reset; mouse set_position ~cx:639 ~dx:479; pause;
        mouse show; pause; mouse hide; pause; quit ]
  in
  let m = machine code in
  to_pause "planar setup" m;
  let color = 0x05 in
  fill m ~x:620 ~y:460 ~w:20 ~h:20 color;
  step_over m;
  to_pause "planar show" m;
  checki "planar: the corner point is the arrow's top left (cleared)" (pixel m 639 479) 0;
  checki "planar: the point left of it is untouched" (pixel m 638 479) color;
  (match m.mouse.mouse_drawn with
   | Drawn_graphics { width = 1; height = 1; _ } -> ()
   | Drawn_graphics _ | Drawn_text _ | Not_drawn -> check "planar: one point kept" false);
  step_over m;
  to_pause "planar hide" m;
  checki "planar: the corner comes back" (pixel m 639 479) color

(* ---------- text ---------- *)

let cell m ~row ~col = read_cell_on m ~page:(active_page m) row col
let set_cell m ~row ~col v = write_cell_on m ~page:(active_page m) row col v

let test_text () =
  let code =
    String.concat ""
      [ video mode_80x25_text; mouse reset; mouse set_position ~cx:80 ~dx:40; pause;
        mouse show; pause;
        mouse text_shape ~bx:0 ~cx:0x00FF ~dx:0x4E00; pause;
        mouse reset; pause;
        mouse text_shape ~bx:1 ~cx:6 ~dx:7; mouse show; pause;
        quit ]
  in
  let m = machine code in
  to_pause "text setup" m;
  let under = 0x1E41 in
  (* virtual (80, 40) is column 10, row 5 *)
  set_cell m ~row:5 ~col:10 under;
  step_over m;
  to_pause "text show" m;
  checki "text: the default cursor flips the attribute" (cell m ~row:5 ~col:10)
    ((under land 0x77FF) lxor 0x7700);
  checki "text: the cell right of it is untouched" (cell m ~row:5 ~col:11) 0x0720;
  step_over m;
  to_pause "text own masks" m;
  checki "text: the program's masks" (cell m ~row:5 ~col:10) ((under land 0x00FF) lxor 0x4E00);
  step_over m;
  to_pause "text reset" m;
  checki "text: a reset puts the cell back" (cell m ~row:5 ~col:10) under;
  check "text: a reset hides the cursor" (m.mouse.mouse_drawn = Not_drawn);
  step_over m;
  to_pause "text hardware cursor" m;
  checki "text: a hardware cursor leaves the cell alone" (cell m ~row:5 ~col:10) under;
  check "text: a hardware cursor draws nothing" (m.mouse.mouse_drawn = Not_drawn)

let () =
  test_256 ();
  test_custom ();
  test_planar_clip ();
  test_text ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos mouse draw: all passed"
