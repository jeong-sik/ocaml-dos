(* CP437 글꼴 — ASCII 밖 글자(0x01-0x1F, 0x7F, 0x80-0xFE)도 글리프가 있다.
   상자 선은 칸 가장자리에서 이어지고, 반전 짝은 서로 비트를 뒤집은 것이다.
   글리프는 IBM 순서(최상위 비트가 왼쪽 점)로 본다. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let glyph = Font8x8.glyph
let rows = 8
let full_row = 0xff
let on g ~x ~y = g.(y) land (0x80 lsr x) <> 0
let popcount b = let rec go b n = if b = 0 then n else go (b lsr 1) (n + (b land 1)) in go b 0
let weight g = Array.fold_left (fun n b -> n + popcount b) 0 g
let blank g = Array.for_all (fun b -> b = 0) g

let nul = 0x00
let space = 0x20
let nbsp = 0xFF

let test_coverage () =
  let missing = ref [] in
  for ch = 0 to 0xff do
    let empty = blank (glyph ch) in
    let should_be_empty = ch = nul || ch = space || ch = nbsp in
    if empty <> should_be_empty then missing := ch :: !missing
  done;
  check
    (Printf.sprintf "빈칸은 00h·20h·FFh 뿐이다(어긋난 글자: %s)"
       (String.concat " " (List.rev_map (Printf.sprintf "%02X") !missing)))
    (!missing = [])

(* 다른 자리에 같은 그림이 두 번 들어가면 표를 옮기다 밀린 것이다 *)
let test_distinct () =
  let seen = Hashtbl.create 256 in
  for ch = 1 to 0xfe do
    let g = glyph ch in
    if not (blank g) then begin
      (match Hashtbl.find_opt seen g with
       | Some other -> check (Printf.sprintf "%02X 와 %02X 의 그림이 같다" other ch) false
       | None -> ());
      Hashtbl.replace seen g ch
    end
  done

(* 상자 선: 칸의 네 가장자리 가운데 어디로 이어지는가 *)
type sides = { up : bool; down : bool; left : bool; right : bool }

let single_box =
  [ (0xB3, "│", { up = true; down = true; left = false; right = false });
    (0xB4, "┤", { up = true; down = true; left = true; right = false });
    (0xBF, "┐", { up = false; down = true; left = true; right = false });
    (0xC0, "└", { up = true; down = false; left = false; right = true });
    (0xC1, "┴", { up = true; down = false; left = true; right = true });
    (0xC2, "┬", { up = false; down = true; left = true; right = true });
    (0xC3, "├", { up = true; down = true; left = false; right = true });
    (0xC4, "─", { up = false; down = false; left = true; right = true });
    (0xC5, "┼", { up = true; down = true; left = true; right = true });
    (0xD9, "┘", { up = true; down = false; left = true; right = false });
    (0xDA, "┌", { up = false; down = true; left = false; right = true }) ]

let double_box =
  [ (0xBA, "║", { up = true; down = true; left = false; right = false });
    (0xBB, "╗", { up = false; down = true; left = true; right = false });
    (0xBC, "╝", { up = true; down = false; left = true; right = false });
    (0xC8, "╚", { up = true; down = false; left = false; right = true });
    (0xC9, "╔", { up = false; down = true; left = false; right = true });
    (0xCA, "╩", { up = true; down = false; left = true; right = true });
    (0xCB, "╦", { up = false; down = true; left = true; right = true });
    (0xCC, "╠", { up = true; down = true; left = false; right = true });
    (0xCD, "═", { up = false; down = false; left = true; right = true });
    (0xCE, "╬", { up = true; down = true; left = true; right = true }) ]

(* ─ 이 켜진 행과 │ 이 켜진 열이 선의 자리다 *)
let lit_rows g = List.filter (fun y -> g.(y) = full_row) (List.init rows Fun.id)
let lit_cols g = List.filter (fun x -> Array.for_all (fun b -> b land (0x80 lsr x) <> 0) g) (List.init rows Fun.id)

let test_box_edges () =
  let edges name ~h_rows ~v_cols table =
    List.iter
      (fun (ch, sym, s) ->
        let g = glyph ch in
        let all_rows p = List.for_all p h_rows and all_cols p = List.for_all p v_cols in
        check (Printf.sprintf "%s %s 왼쪽" name sym) (all_rows (fun y -> on g ~x:0 ~y) = s.left);
        check (Printf.sprintf "%s %s 오른쪽" name sym) (all_rows (fun y -> on g ~x:7 ~y) = s.right);
        check (Printf.sprintf "%s %s 위" name sym) (all_cols (fun x -> on g ~x ~y:0) = s.up);
        check (Printf.sprintf "%s %s 아래" name sym) (all_cols (fun x -> on g ~x ~y:7) = s.down))
      table
  in
  let single_rows = lit_rows (glyph 0xC4) and single_cols = lit_cols (glyph 0xB3) in
  check "─ 은 한 행" (List.length single_rows = 1);
  check "│ 은 한 열" (List.length single_cols = 1);
  edges "홑줄" ~h_rows:single_rows ~v_cols:single_cols single_box;
  let double_rows = lit_rows (glyph 0xCD) and double_cols = lit_cols (glyph 0xBA) in
  check "═ 은 두 행" (List.length double_rows = 2);
  check "║ 은 두 열" (List.length double_cols = 2);
  edges "겹줄" ~h_rows:double_rows ~v_cols:double_cols double_box

let test_blocks () =
  let half_left = 0xF0 and half_right = 0x0F in
  check "█ 는 꽉 찼다" (Array.for_all (fun b -> b = full_row) (glyph 0xDB));
  check "▀ 는 위 절반" (Array.for_all Fun.id (Array.mapi (fun y b -> b = (if y < rows / 2 then full_row else 0)) (glyph 0xDF)));
  check "▄ 는 아래 절반" (Array.for_all Fun.id (Array.mapi (fun y b -> b = (if y >= rows / 2 then full_row else 0)) (glyph 0xDC)));
  check "▌ 는 왼쪽 절반" (Array.for_all (fun b -> b = half_left) (glyph 0xDD));
  check "▐ 는 오른쪽 절반" (Array.for_all (fun b -> b = half_right) (glyph 0xDE));
  check "░ < ▒ < ▓ < █"
    (weight (glyph 0xB0) < weight (glyph 0xB1)
     && weight (glyph 0xB1) < weight (glyph 0xB2)
     && weight (glyph 0xB2) < weight (glyph 0xDB))

(* ◘ 은 • 를, ◙ 은 ○ 을 뒤집은 그림이다 *)
let test_inverse_pairs () =
  let inverse a b = Array.for_all2 (fun x y -> x lxor full_row = y) (glyph a) (glyph b) in
  check "◘ = • 반전" (inverse 0x07 0x08);
  check "◙ = ○ 반전" (inverse 0x09 0x0A);
  check "☻ 는 ☺ 보다 많이 켜져 있다" (weight (glyph 0x02) > weight (glyph 0x01))

(* 화살표와 삼각형은 가리키는 쪽이 좁다 *)
let test_pointing () =
  let row g y = popcount g.(y) in
  let col g x = Array.fold_left (fun n b -> if b land (0x80 lsr x) <> 0 then n + 1 else n) 0 g in
  let up = glyph 0x1E and dn = glyph 0x1F and r = glyph 0x10 and l = glyph 0x11 in
  check "▲ 위가 좁다" (row up 1 < row up 4);
  check "▼ 아래가 좁다" (row dn 5 < row dn 1);
  check "► 오른쪽이 좁다" (col r 6 < col r 0);
  check "◄ 왼쪽이 좁다" (col l 0 < col l 6);
  let ua = glyph 0x18 and da = glyph 0x19 in
  check "↑ 머리가 위" (row ua 2 > row ua 5);
  check "↓ 머리가 아래" (row da 4 > row da 1);
  let ra = glyph 0x1A and la = glyph 0x1B in
  check "→ 머리가 오른쪽" (col ra 5 > col ra 1);
  check "← 머리가 왼쪽" (col la 1 > col la 5)

(* 텍스트 화면에서 두 칸에 걸친 ─ 가 끊기지 않는다 *)
let test_rendered_line () =
  let m = Dos_machine.create () in
  let vram = 0xB8000 and white = 0x07 in
  List.iteri
    (fun k ch ->
      Dos_machine.mem_write m (vram + (k * 2)) ch;
      Dos_machine.mem_write m (vram + (k * 2) + 1) white)
    [ 0xC4; 0xC4; 0x02 ];
  let f = Dos_machine.frame_rgb m in
  let width = 80 * 8 and scale = 2 in
  let lit_at x y = f.[((y * width) + x) * 3] <> '\000' in
  let y = List.hd (lit_rows (glyph 0xC4)) * scale in
  check "── 이 칸 경계에서 이어진다"
    (List.for_all (fun x -> lit_at x y) (List.init 16 Fun.id));
  check "☻ 가 화면에 그려진다"
    (List.exists (fun (x, y) -> lit_at (16 + x) y)
       (List.concat_map (fun y -> List.init 8 (fun x -> (x, y))) (List.init 16 Fun.id)))

let () =
  test_coverage ();
  test_distinct ();
  test_box_edges ();
  test_blocks ();
  test_inverse_pairs ();
  test_pointing ();
  test_rendered_line ();
  if !failed = 0 then print_endline "dos cp437 font: all passed"
  else begin
    Printf.eprintf "dos cp437 font: %d failures\n%!" !failed;
    exit 1
  end
