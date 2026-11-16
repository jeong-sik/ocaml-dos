type mouse = { x : int; y : int; code : int; released : bool }

type graphics_reply = Loaded | Failed of string

type event =
  | Key of int
  | Unmapped of string
  | Mouse of mouse
  | Text_area of { width : int; height : int }
  | Cells of { cols : int; rows : int }
  | Graphics of { id : int; reply : graphics_reply }
  | Device_attributes
  | Quit

(* ---------- 키 ---------- *)

let named name =
  match List.assoc_opt name Dos_machine.named_keys with
  | Some w -> Key w
  | None -> Unmapped name

let text_key c =
  match Dos_machine.key_of_string (String.make 1 c) with
  | Ok w -> Key w
  | Error _ -> Unmapped (String.make 1 c)

(* 글자 키의 스캔 코드. US 자판에 자리가 없으면 [None]. *)
let scan_of c =
  match Dos_machine.key_of_string (String.make 1 c) with
  | Ok w -> Some (w lsr 8)
  | Error _ -> None

let ctrl_mask = 0x1F

(* Ctrl+글자는 스캔 코드에 C0 제어 문자를, Alt+글자는 ASCII 0 을 붙인다
   (IBM BIOS 키 표). 글자가 아닌 키의 조합은 표마다 달라 넣지 않는다. *)
let ctrl_key c =
  match c, scan_of c with
  | ('a' .. 'z' | 'A' .. 'Z'), Some sc ->
    Key ((sc lsl 8) lor (Char.code (Char.uppercase_ascii c) land ctrl_mask))
  | _ -> Unmapped ("ctrl+" ^ String.make 1 c)

let alt_key c =
  match c, scan_of c with
  | ('a' .. 'z' | 'A' .. 'Z'), Some sc -> Key (sc lsl 8)
  | _ -> Unmapped ("alt+" ^ String.make 1 c)

(* Kitty 수식 값은 1 + 비트. 잠금 키(64·128)는 키를 바꾸지 않는다. *)
let mod_shift = 1
let mod_alt = 2
let mod_ctrl = 4
let mod_locks = 64 lor 128

type mods = Plain | Shift | Alt | Ctrl | Other

let mods_of field =
  let m = (max 1 field - 1) land lnot mod_locks in
  if m = 0 then Plain
  else if m = mod_shift then Shift
  else if m = mod_alt then Alt
  else if m = mod_ctrl then Ctrl
  else Other

(* F1-F10 의 스캔 코드는 3Bh 부터. Shift·Ctrl·Alt 는 54h·5Eh·68h 부터다
   (IBM BIOS 키 표). *)
let f_plain = 0x3B
let f_shift = 0x54
let f_ctrl = 0x5E
let f_alt = 0x68

let f_key n mods =
  let base =
    match mods with
    | Plain -> Some f_plain
    | Shift -> Some f_shift
    | Ctrl -> Some f_ctrl
    | Alt -> Some f_alt
    | Other -> None
  in
  match base with
  | Some b -> Key ((b + n - 1) lsl 8)
  | None -> Unmapped (Printf.sprintf "f%d+mods" n)

(* 글자가 아닌 키는 수식이 붙으면 다른 워드가 된다. 그 표는 아직 넣지 않았다. *)
let plain_only name mods =
  match mods with
  | Plain -> named name
  | Shift | Alt | Ctrl | Other -> Unmapped (name ^ "+mods")

let kp_first_digit = 57399
let kp_last_digit = 57408

let keypad =
  [ (57409, '.'); (57410, '/'); (57411, '*'); (57412, '-'); (57413, '+');
    (57415, '=') ]

let keypad_named =
  [ (57414, "enter"); (57417, "left"); (57418, "right"); (57419, "up");
    (57420, "down"); (57421, "pgup"); (57422, "pgdn"); (57423, "home");
    (57424, "end"); (57425, "insert"); (57426, "delete") ]

let first_modifier_key = 57441
let last_modifier_key = 57454

(* [CSI code;mods u] *)
let kitty_key code mods =
  if code = 27 then [ plain_only "esc" mods ]
  else if code = 13 then [ plain_only "enter" mods ]
  else if code = 9 then [ (if mods = Shift then named "backtab" else plain_only "tab" mods) ]
  else if code = 127 then [ plain_only "backspace" mods ]
  else if code >= kp_first_digit && code <= kp_last_digit then
    [ text_key (Char.chr (Char.code '0' + code - kp_first_digit)) ]
  else if code >= first_modifier_key && code <= last_modifier_key then []
  else
    match List.assoc_opt code keypad, List.assoc_opt code keypad_named with
    | Some c, _ -> [ text_key c ]
    | None, Some name -> [ plain_only name mods ]
    | None, None ->
      if code >= 0x20 && code < 0x7F then
        let c = Char.chr code in
        match mods with
        | Ctrl -> [ ctrl_key c ]
        | Alt -> [ alt_key c ]
        | Plain -> [ text_key c ]
        | Shift ->
          (match c with
           | 'a' .. 'z' -> [ text_key (Char.uppercase_ascii c) ]
           | _ -> [ Unmapped (Printf.sprintf "shift+%c" c) ])
        | Other -> [ Unmapped (Printf.sprintf "%d+mods" code) ]
      else [ Unmapped (Printf.sprintf "kitty %d" code) ]

let quit_tilde = 24

(* [CSI n;mods ~] *)
let tilde_key n mods =
  match n with
  | 2 -> plain_only "insert" mods
  | 3 -> plain_only "delete" mods
  | 5 -> plain_only "pgup" mods
  | 6 -> plain_only "pgdn" mods
  | 7 -> plain_only "home" mods
  | 8 -> plain_only "end" mods
  | 11 | 12 | 13 | 14 | 15 -> f_key (n - 10) mods
  | 17 | 18 | 19 | 20 | 21 -> f_key (n - 11) mods
  | n when n = quit_tilde -> Quit
  | n -> Unmapped (Printf.sprintf "CSI %d ~" n)

(* [CSI 1;mods X] 와 SS3 X 의 끝 글자 *)
let letter_key final mods =
  match final with
  | 'A' -> plain_only "up" mods
  | 'B' -> plain_only "down" mods
  | 'C' -> plain_only "right" mods
  | 'D' -> plain_only "left" mods
  | 'H' -> plain_only "home" mods
  | 'F' -> plain_only "end" mods
  | 'P' -> f_key 1 mods
  | 'Q' -> f_key 2 mods
  | 'R' -> f_key 3 mods
  | 'S' -> f_key 4 mods
  | c -> Unmapped (Printf.sprintf "final %C" c)

(* ---------- 이스케이프 나누기 ---------- *)

let int_or_zero s = Option.value (int_of_string_opt s) ~default:0

(* "a:b:c" 의 첫 칸 *)
let head_field s =
  match String.split_on_char ':' s with
  | h :: _ -> int_or_zero h
  | [] -> 0

let event_release = 3

(* [CSI params final]. params 는 CSI 와 끝 글자 사이 그대로다. *)
let csi params final =
  let fields = String.split_on_char ';' params in
  let mods_field () =
    match fields with
    | _ :: m :: _ -> head_field m
    | [ _ ] | [] -> 1
  in
  let released () =
    match fields with
    | _ :: m :: _ ->
      (match String.split_on_char ':' m with
       | _ :: e :: _ -> int_or_zero e = event_release
       | [ _ ] | [] -> false)
    | [ _ ] | [] -> false
  in
  match final with
  | 'u' ->
    if released () then []
    else (match fields with
        | code :: _ -> kitty_key (head_field code) (mods_of (mods_field ()))
        | [] -> [])
  | '~' ->
    if released () then []
    else (match fields with
        | n :: _ -> [ tilde_key (int_or_zero n) (mods_of (mods_field ())) ]
        | [] -> [])
  | ('M' | 'm') when String.length params > 0 && params.[0] = '<' ->
    (match String.split_on_char ';' (String.sub params 1 (String.length params - 1)) with
     | [ b; x; y ] ->
       [ Mouse { code = int_or_zero b; x = max 0 (int_or_zero x - 1);
                 y = max 0 (int_or_zero y - 1); released = final = 'm' } ]
     | _ -> [ Unmapped ("CSI " ^ params ^ String.make 1 final) ])
  (* DA1 답 [CSI ? … c] *)
  | 'c' when String.length params > 0 && params.[0] = '?' -> [ Device_attributes ]
  | 't' ->
    (match List.map int_or_zero fields with
     | [ 4; height; width ] -> [ Text_area { width; height } ]
     | [ 8; rows; cols ] -> [ Cells { cols; rows } ]
     | _ -> [])
  | 'A' | 'B' | 'C' | 'D' | 'H' | 'F' | 'P' | 'Q' | 'S' ->
    if released () then [] else [ letter_key final (mods_of (mods_field ())) ]
  | c -> [ Unmapped ("CSI " ^ params ^ String.make 1 c) ]

(* CSI 의 끝 글자는 0x40-0x7E 다. 그 앞은 매개변수(0x30-0x3F)와 중간
   바이트(0x20-0x2F)다. *)
let csi_final s from =
  let n = String.length s in
  let rec go i =
    if i >= n then None
    else
      let c = Char.code s.[i] in
      if c >= 0x40 && c <= 0x7E then Some i
      else if c >= 0x20 && c <= 0x3F then go (i + 1)
      else Some i
  in
  go from

let is_continuation c = Char.code c land 0xC0 = 0x80

let loaded_message = "OK"

(* [ESC _G 키=값,…;메시지 ESC \] 의 [G] 와 [ESC \] 사이. 키 [i] 가 없으면
   우리가 id 를 붙여 보낸 명령의 답이 아니니 버린다. *)
let graphics_reply body =
  match String.index_opt body ';' with
  | None -> []
  | Some k ->
    let message = String.sub body (k + 1) (String.length body - k - 1) in
    let id =
      List.find_map
        (fun kv ->
          match String.split_on_char '=' kv with
          | [ "i"; v ] -> int_of_string_opt v
          | _ -> None)
        (String.split_on_char ',' (String.sub body 0 k))
    in
    (match id with
     | Some id ->
       let reply = if message = loaded_message then Loaded else Failed message in
       [ Graphics { id; reply } ]
     | None -> [])

(* [from] 부터 처음 나오는 ST([ESC \])의 자리 *)
let find_st s from =
  let n = String.length s in
  let rec go i =
    if i + 1 >= n then None
    else if s.[i] = '\027' && s.[i + 1] = '\\' then Some i
    else go (i + 1)
  in
  go from

let plain c =
  match c with
  | '\r' | '\n' -> named "enter"
  | '\t' -> named "tab"
  | '\127' | '\b' -> named "backspace"
  | '\001' .. '\026' -> ctrl_key (Char.chr (Char.code c + Char.code 'a' - 1))
  | ' ' .. '~' -> text_key c
  | c -> Unmapped (String.make 1 c)

let parse s =
  let n = String.length s in
  let rest i = String.sub s i (n - i) in
  let rec go i acc =
    if i >= n then (List.rev acc, "")
    else
      match s.[i] with
      | '\027' ->
        if i + 1 >= n then (List.rev acc, rest i)
        else (
          match s.[i + 1] with
          | '[' -> (
            match csi_final s (i + 2) with
            | None -> (List.rev acc, rest i)
            | Some j ->
              let evs = csi (String.sub s (i + 2) (j - i - 2)) s.[j] in
              go (j + 1) (List.rev_append evs acc))
          | 'O' ->
            if i + 2 >= n then (List.rev acc, rest i)
            else go (i + 3) (letter_key s.[i + 2] Plain :: acc)
          (* APC. 터미널이 보내는 것은 그래픽 답([ESC _G])뿐이다. [ESC _] 에
             다른 글자가 오면 옛 꼴 Alt+_ 다. *)
          | '_' when i + 2 >= n -> (List.rev acc, rest i)
          | '_' when s.[i + 2] = 'G' -> (
            match find_st s (i + 3) with
            | None -> (List.rev acc, rest i)
            | Some j ->
              let evs = graphics_reply (String.sub s (i + 3) (j - i - 3)) in
              go (j + 2) (List.rev_append evs acc))
          | c -> go (i + 2) (alt_key c :: acc))
      | c when Char.code c >= 0x80 ->
        let rec stop j = if j < n && is_continuation s.[j] then stop (j + 1) else j in
        let j = stop (i + 1) in
        go j (Unmapped (String.sub s i (j - i)) :: acc)
      | c -> go (i + 1) (plain c :: acc)
  in
  go 0 []

let lone_escape tail = if tail = "\027" then [ named "esc" ] else []

(* ---------- 마우스 ---------- *)

let sgr_motion = 32
let sgr_wheel = 64
let left = 1
let right = 2
let middle = 4

let buttons prev m =
  if m.code land (sgr_motion lor sgr_wheel) <> 0 then prev
  else
    let bit =
      match m.code land 3 with
      | 0 -> left
      | 1 -> middle
      | 2 -> right
      | _ -> 0
    in
    if m.released then prev land lnot bit else prev lor bit

let mouse_width = 640
let height_200 = 200
let height_350 = 350
let height_480 = 480

let mouse_space ~mode ~frame_width =
  match mode with
  | 0x00 | 0x01 | 0x02 | 0x03 | 0x07 -> (frame_width, height_200)
  | 0x04 | 0x05 | 0x06 | 0x0D | 0x0E | 0x13 -> (mouse_width, height_200)
  | 0x0F | 0x10 -> (mouse_width, height_350)
  | _ -> (mouse_width, height_480)

let to_guest ~box:(bw, bh) ~space:(sw, sh) (x, y) =
  let scale v box space = max 0 (min (space - 1) (v * space / max 1 box)) in
  (scale x bw sw, scale y bh sh)

(* DOS 화면은 해상도와 상관없이 4:3 모니터에 찼다. *)
let aspect_w = 4
let aspect_h = 3

let fit ~cols ~rows ~cell_w ~cell_h =
  let cw = max 1 cell_w and ch = max 1 cell_h in
  let w = max 1 cols * cw and h = max 1 rows * ch in
  let bw, bh =
    if w * aspect_h > h * aspect_w then (h * aspect_w / aspect_h, h)
    else (w, w * aspect_h / aspect_w)
  in
  (max 1 (bw / cw), max 1 (bh / ch))

(* ---------- Kitty 그래픽 ---------- *)

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64 s =
  let n = String.length s in
  let out = Buffer.create (((n + 2) / 3) * 4) in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then begin
      let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let sextet k = alphabet.[(v lsr (18 - (6 * k))) land 0x3F] in
      Buffer.add_char out (sextet 0);
      Buffer.add_char out (sextet 1);
      Buffer.add_char out (if i + 1 < n then sextet 2 else '=');
      Buffer.add_char out (if i + 2 < n then sextet 3 else '=');
      go (i + 3)
    end
  in
  go 0;
  Buffer.contents out

let chunk = 4096
let apc_open = "\027_G"
let apc_close = "\027\\"

let image ~id ~width ~height ~cols ~rows rgb =
  let data = base64 rgb in
  let n = String.length data in
  let out = Buffer.create (n + (n / chunk * 16) + 64) in
  let rec go i first =
    let len = min chunk (n - i) in
    let more = if i + len < n then 1 else 0 in
    Buffer.add_string out apc_open;
    if first then
      Buffer.add_string out
        (Printf.sprintf "a=T,f=24,s=%d,v=%d,i=%d,c=%d,r=%d,C=1,q=2," width height id cols
           rows);
    Buffer.add_string out (Printf.sprintf "m=%d;" more);
    Buffer.add_string out (String.sub data i len);
    Buffer.add_string out apc_close;
    if more = 1 then go (i + len) false
  in
  go 0 true;
  Buffer.contents out

let delete_image id = Printf.sprintf "%sa=d,d=I,i=%d,q=2%s" apc_open id apc_close

let temp_file_marker = "tty-graphics-protocol"

(* q=1: 성공은 답하지 않고 실패만 답한다. 실행기는 실패 답을 받으면 직접
   보내기로 돌아간다. *)
let image_file ~id ~width ~height ~cols ~rows path =
  Printf.sprintf "%sa=T,f=24,t=t,s=%d,v=%d,i=%d,c=%d,r=%d,C=1,q=1;%s%s" apc_open width height
    id cols rows (base64 path) apc_close

let probe_file ~id path =
  Printf.sprintf "%sa=q,f=24,t=t,s=1,v=1,i=%d;%s%s" apc_open id (base64 path) apc_close
