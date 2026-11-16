(* 터미널 바이트 → 사건, 프레임 → Kitty 그래픽. 키 이스케이프는 Kitty
   키보드 프로토콜 문서(keyboard-protocol.rst)의 꼴이고, base64 는 RFC 4648
   10절의 예다. *)

open Dosterm

let failed = ref 0

let fail fmt = Printf.ksprintf (fun s -> incr failed; prerr_endline ("FAIL " ^ s)) fmt

let show = function
  | Key w -> Printf.sprintf "Key %04X" w
  | Unmapped s -> Printf.sprintf "Unmapped %S" s
  | Mouse { x; y; code; released } ->
    Printf.sprintf "Mouse(%d,%d,%d,%b)" x y code released
  | Text_area { width; height } -> Printf.sprintf "Text_area %dx%d" width height
  | Cells { cols; rows } -> Printf.sprintf "Cells %dx%d" cols rows
  | Graphics { id; reply = Loaded } -> Printf.sprintf "Graphics %d OK" id
  | Device_attributes -> "Device_attributes"
  | Save_state -> "Save_state"
  | Load_state -> "Load_state"
  | Graphics { id; reply = Failed e } -> Printf.sprintf "Graphics %d %S" id e
  | Quit -> "Quit"

let shows evs = "[" ^ String.concat "; " (List.map show evs) ^ "]"

let events name input want =
  let got, tail = parse input in
  if got <> want || tail <> "" then
    fail "%s: got %s tail %S, want %s" name (shows got) tail (shows want)

let leaves name input want_events want_tail =
  let got, tail = parse input in
  if got <> want_events || tail <> want_tail then
    fail "%s: got %s tail %S, want %s tail %S" name (shows got) tail (shows want_events)
      want_tail

let check name got want = if got <> want then fail "%s: got %d want %d" name got want

let check_pair name (a, b) (c, d) =
  if (a, b) <> (c, d) then fail "%s: got (%d,%d) want (%d,%d)" name a b c d

let check_s name got want = if got <> want then fail "%s: got %S want %S" name got want

let esc = Key 0x011B
let enter = Key 0x1C0D

let test_keys () =
  events "letter" "a" [ Key 0x1E61 ];
  events "capital" "A" [ Key 0x1E41 ];
  events "digit" "7" [ Key 0x0837 ];
  events "return" "\r" [ enter ];
  events "tab" "\t" [ Key 0x0F09 ];
  events "delete byte" "\127" [ Key 0x0E08 ];
  events "legacy ctrl+c" "\003" [ Key 0x2E03 ];
  events "legacy alt+x" "\027x" [ Key 0x2D00 ];
  events "kitty esc" "\027[27u" [ esc ];
  events "kitty enter" "\027[13u" [ enter ];
  events "kitty ctrl+c" "\027[99;5u" [ Key 0x2E03 ];
  events "kitty alt+x" "\027[120;3u" [ Key 0x2D00 ];
  events "kitty shift+a" "\027[97;2u" [ Key 0x1E41 ];
  events "kitty caps lock is not a modifier" "\027[99;69u" [ Key 0x2E03 ];
  events "kitty shift+tab" "\027[9;2u" [ Key 0x0F00 ];
  events "keypad digit" "\027[57399u" [ Key 0x0B30 ];
  events "keypad enter" "\027[57414u" [ enter ];
  events "keypad up" "\027[57419u" [ Key 0x4800 ];
  events "modifier alone" "\027[57441u" [];
  events "release" "\027[97;1:3u" [];
  events "ctrl+alt" "\027[99;7u" [ Unmapped "99+mods" ];
  events "F13" "\027[57376u" [ Unmapped "kitty 57376" ]

let test_function_keys () =
  events "up" "\027[A" [ Key 0x4800 ];
  events "left" "\027[D" [ Key 0x4B00 ];
  events "home" "\027[H" [ Key 0x4700 ];
  events "end by tilde" "\027[8~" [ Key 0x4F00 ];
  events "page down" "\027[6~" [ Key 0x5100 ];
  events "ctrl+up" "\027[1;5A" [ Unmapped "up+mods" ];
  events "F1 letter" "\027[P" [ Key 0x3B00 ];
  events "F1 tilde" "\027[11~" [ Key 0x3B00 ];
  events "F3" "\027[13~" [ Key 0x3D00 ];
  events "F5" "\027[15~" [ Key 0x3F00 ];
  events "F6 skips 16" "\027[17~" [ Key 0x4000 ];
  events "F10" "\027[21~" [ Key 0x4400 ];
  events "shift+F1" "\027[1;2P" [ Key 0x5400 ];
  events "ctrl+F10" "\027[21;5~" [ Key 0x6700 ];
  events "alt+F5" "\027[15;3~" [ Key 0x6C00 ];
  events "F11 saves" "\027[23~" [ Save_state ];
  events "shift+F11 loads" "\027[23;2~" [ Load_state ];
  events "ctrl+F11" "\027[23;5~" [ Unmapped "F11+mods" ];
  events "F12 quits" "\027[24~" [ Quit ];
  events "SS3 F1" "\027OP" [ Key 0x3B00 ];
  events "SS3 down" "\027OB" [ Key 0x5000 ]

let test_mouse_and_reports () =
  events "press" "\027[<0;11;21M" [ Mouse { x = 10; y = 20; code = 0; released = false } ];
  events "release" "\027[<2;1;1m" [ Mouse { x = 0; y = 0; code = 2; released = true } ];
  events "text area" "\027[4;600;800t" [ Text_area { width = 800; height = 600 } ];
  events "cells" "\027[8;24;80t" [ Cells { cols = 80; rows = 24 } ];
  events "device attributes" "\027[?62;22c" [ Device_attributes ];
  events "c without ?" "\027[0c" [ Unmapped "CSI 0c" ];
  events "mixed" "a\027[Ab" [ Key 0x1E61; Key 0x4800; Key 0x3062 ];
  events "utf-8" "\xc3\xa9" [ Unmapped "\xc3\xa9" ]

(* 그래픽 답은 Kitty graphics-protocol 문서 「Querying support」 의 꼴이다:
   [ESC _Gi=31;OK ESC \\] *)
let test_graphics_replies () =
  events "loaded" "\027_Gi=3;OK\027\\" [ Graphics { id = 3; reply = Loaded } ];
  events "failed" "\027_Gi=1;ENOENT:No such file\027\\"
    [ Graphics { id = 1; reply = Failed "ENOENT:No such file" } ];
  events "more keys" "\027_Gi=3,p=7;OK\027\\" [ Graphics { id = 3; reply = Loaded } ];
  events "no id" "\027_Ga=q;OK\027\\" [];
  events "no message" "\027_Gi=3\027\\" [];
  events "between keys" "a\027_Gi=3;OK\027\\b"
    [ Key 0x1E61; Graphics { id = 3; reply = Loaded }; Key 0x3062 ];
  leaves "cut reply" "\027_Gi=3;O" [] "\027_Gi=3;O";
  leaves "cut before G" "\027_" [] "\027_";
  let alt_underscore, _ = parse "\027_" |> fun (_, t) -> parse (t ^ "x") in
  if List.length alt_underscore <> 2 || List.nth alt_underscore 1 <> Key 0x2D78 then
    fail "legacy alt+_ then x: got %s" (shows alt_underscore)

let test_cut_sequences () =
  leaves "lone ESC" "\027" [] "\027";
  leaves "cut CSI" "\027[1;5" [] "\027[1;5";
  leaves "cut after keys" "ab\027[" [ Key 0x1E61; Key 0x3062 ] "\027[";
  leaves "cut SS3" "\027O" [] "\027O";
  let first, tail = parse "\027[2" in
  let second, _ = parse (tail ^ "7u") in
  if first <> [] || second <> [ esc ] then fail "joined CSI";
  if lone_escape "\027" <> [ esc ] then fail "lone ESC is Esc";
  if lone_escape "\027[" <> [] then fail "a cut CSI is not Esc"

let test_buttons () =
  let ev code released = { x = 0; y = 0; code; released } in
  let b = buttons 0 (ev 0 false) in
  check "left down" b 1;
  let b = buttons b (ev 2 false) in
  check "right down" b 3;
  let b = buttons b (ev 0 true) in
  check "left up" b 2;
  check "motion keeps" (buttons b (ev 32 false)) 2;
  check "wheel keeps" (buttons b (ev 64 false)) 2;
  check "middle" (buttons 0 (ev 1 false)) 4

let test_geometry () =
  check_pair "80-column text" (mouse_space ~mode:3 ~frame_width:640) (640, 200);
  check_pair "40-column text" (mouse_space ~mode:1 ~frame_width:320) (320, 200);
  check_pair "13h" (mouse_space ~mode:0x13 ~frame_width:320) (640, 200);
  check_pair "0Dh" (mouse_space ~mode:0x0D ~frame_width:320) (640, 200);
  check_pair "10h" (mouse_space ~mode:0x10 ~frame_width:640) (640, 350);
  check_pair "12h" (mouse_space ~mode:0x12 ~frame_width:640) (640, 480);
  check_pair "middle" (to_guest ~box:(800, 600) ~space:(640, 200) (400, 300)) (320, 100);
  check_pair "corner" (to_guest ~box:(800, 600) ~space:(640, 200) (799, 599)) (639, 199);
  check_pair "past the box" (to_guest ~box:(800, 600) ~space:(640, 200) (900, 700)) (639, 199);
  check_pair "origin" (to_guest ~box:(800, 600) ~space:(640, 200) (0, 0)) (0, 0);
  check_pair "wide terminal" (fit ~cols:80 ~rows:24 ~cell_w:8 ~cell_h:16) (64, 24);
  check_pair "tall terminal" (fit ~cols:100 ~rows:50 ~cell_w:10 ~cell_h:20) (100, 37);
  check_pair "tiny" (fit ~cols:0 ~rows:0 ~cell_w:0 ~cell_h:0) (1, 1)

let test_base64 () =
  List.iter
    (fun (i, o) -> check_s ("base64 " ^ i) (base64 i) o)
    [ ("", ""); ("f", "Zg=="); ("fo", "Zm8="); ("foo", "Zm9v"); ("foob", "Zm9vYg==");
      ("fooba", "Zm9vYmE="); ("foobar", "Zm9vYmFy") ];
  check_s "high bytes" (base64 "\xff\xfe\x00") "//4A"

let count_sub s sub =
  let n = String.length sub in
  let rec go i k =
    if i + n > String.length s then k
    else if String.sub s i n = sub then go (i + n) (k + 1)
    else go (i + 1) k
  in
  go 0 0

let test_image () =
  check_s "one chunk"
    (image ~id:7 ~width:2 ~height:1 ~cols:3 ~rows:4 "abcdef")
    "\027_Ga=T,f=24,s=2,v=1,i=7,c=3,r=4,C=1,q=2,m=0;YWJjZGVm\027\\";
  let rgb = String.make 4000 'x' in
  let s = image ~id:1 ~width:40 ~height:33 ~cols:10 ~rows:5 rgb in
  check "two escapes" (count_sub s "\027_G") 2;
  check "first says more" (count_sub s "m=1;") 1;
  check "last says done" (count_sub s "\027_Gm=0;") 1;
  let first_payload =
    let start = String.index s ';' + 1 in
    String.index_from s start '\027' - start
  in
  check "first chunk is 4096" first_payload 4096;
  let frame = String.length "\027_G" + String.length "\027\\" in
  check "payload" (String.length s - (2 * frame) - String.length "a=T,f=24,s=40,v=33,i=1,c=10,r=5,C=1,q=2,m=1;" - String.length "m=0;")
    (String.length (base64 rgb));
  check_s "delete" (delete_image 3) "\027_Ga=d,d=I,i=3,q=2\027\\";
  check_s "image file"
    (image_file ~id:2 ~width:320 ~height:200 ~cols:40 ~rows:15 "/tmp/a")
    "\027_Ga=T,f=24,t=t,s=320,v=200,i=2,c=40,r=15,C=1,q=1;L3RtcC9h\027\\";
  check_s "probe" (probe_file ~id:3 "/tmp/a")
    "\027_Ga=q,f=24,t=t,s=1,v=1,i=3;L3RtcC9h\027\\";
  check_s "marker" temp_file_marker "tty-graphics-protocol"

let () =
  test_keys ();
  test_function_keys ();
  test_mouse_and_reports ();
  test_graphics_replies ();
  test_cut_sequences ();
  test_buttons ();
  test_geometry ();
  test_base64 ();
  test_image ();
  if !failed = 0 then print_endline "dosterm: all passed"
  else begin
    Printf.eprintf "dosterm: %d failures\n%!" !failed;
    exit 1
  end
