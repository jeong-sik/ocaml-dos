(* DOS names, parsed once where the guest hands them over. See dos_path.mli. *)

type t = string list
type error = Bad_drive | Bad_name | Above_root

let root = []
let base_len = 8
let ext_len = 3
let separator = '\\'

let is_separator c = c = '\\' || c = '/'

(* MS-DOS 5 refuses these in a name; everything else from 0x21 up, the
   bytes from 0x80 included, is allowed. *)
let forbidden = "\"*+,/:;<=>?[\\]|"

let name_char c = Char.code c > 0x20 && not (String.contains forbidden c)
let wild_char c = name_char c || c = '?' || c = '*'

let truncate n s = if String.length s > n then String.sub s 0 n else s

(* One component, upper-cased and cut to 8.3. [ok] says which characters
   it may hold. *)
let component ~ok raw =
  let s = String.uppercase_ascii raw in
  let canonical b e = if e = "" then truncate base_len b else truncate base_len b ^ "." ^ truncate ext_len e in
  if not (String.for_all (fun c -> c = '.' || ok c) s) then Error Bad_name
  else
    match String.split_on_char '.' s with
    | [ b ] when b <> "" -> Ok (canonical b "")
    | [ b; e ] when b <> "" -> Ok (canonical b e)
    | _ -> Error Bad_name

(* The drive prefix, and whether the rest starts at the root. *)
let split_drive s =
  let n = String.length s in
  if n >= 2 && s.[1] = ':' then
    if Char.uppercase_ascii s.[0] = 'C' then Ok (String.sub s 2 (n - 2)) else Error Bad_drive
  else Ok s

(* Walk the components from [start]. The path is built reversed, so [..]
   drops its head. *)
let walk start parts =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | "." :: rest -> go acc rest
    | ".." :: rest -> (match acc with [] -> Error Above_root | _ :: up -> go up rest)
    | raw :: rest ->
      (match component ~ok:name_char raw with
       | Ok c -> go (c :: acc) rest
       | Error e -> Error e)
  in
  go (List.rev start) parts

let ( let* ) = Result.bind

(* The components of [s] after its drive, and where they start. [""] is not
   a name; a lone separator is the root. *)
let start_and_parts ~cwd s =
  if s = "" then Error Bad_name
  else
    let* rest = split_drive s in
    let absolute = rest <> "" && is_separator rest.[0] in
    let body = if absolute then String.sub rest 1 (String.length rest - 1) else rest in
    let parts =
      if body = "" then []
      else String.split_on_char separator (String.map (fun c -> if c = '/' then separator else c) body)
    in
    if List.mem "" parts then Error Bad_name
    else Ok ((if absolute then root else cwd), parts)

let parse ~cwd s =
  let* start, parts = start_and_parts ~cwd s in
  walk start parts

let parse_pattern ~cwd s =
  let* start, parts = start_and_parts ~cwd s in
  match List.rev parts with
  | [] -> Error Bad_name
  | last :: dirs_rev ->
    let* dir = walk start (List.rev dirs_rev) in
    let pattern = String.uppercase_ascii last in
    if String.for_all (fun c -> c = '.' || wild_char c) pattern then Ok (dir, pattern)
    else Error Bad_name

let key t = String.concat (String.make 1 separator) t

let of_key k =
  if k = "" then Some root
  else
    let parts = String.split_on_char separator k in
    match walk root parts with
    | Ok p when key p = k -> Some p
    | Ok _ | Error _ -> None

let parent t = match List.rev t with [] -> None | _ :: up -> Some (List.rev up)
let base t = match List.rev t with [] -> None | last :: _ -> Some last

let is_within ~dir t =
  let rec prefix a b =
    match a, b with
    | [], _ -> true
    | x :: a', y :: b' -> x = y && prefix a' b'
    | _ :: _, [] -> false
  in
  prefix dir t
