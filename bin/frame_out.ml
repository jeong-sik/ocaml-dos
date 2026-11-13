(* 프레임을 터미널로 넘긴다.

   터미널이 프레임을 늦게 받아 가면 쓰기가 막힌다. 막힌 동안에도 게스트는
   돌아야 하므로 stdout 을 비차단으로 두고 루프마다 쓸 수 있는 만큼만 쓴다.
   Kitty 그래픽은 한 그림의 청크 사이에 다른 그래픽 이스케이프를 넣지 못하게
   하므로 쓰고 있던 그림은 끝까지 쓴다. 터미널이 앞 그림을 다 받기 전에는
   새 프레임을 만들지 않는다 — 프레임을 만드는 일이 비싸서, 만들어 놓고
   버리면 그만큼 게스트와 쓰기가 함께 굶는다.

   쓰기를 domain 에 맡기면 이 에뮬레이터에서는 오히려 느려진다: 놀고 있는
   domain 하나만 있어도 minor GC 마다 domain 끼리 멈춰 맞추느라 게스트 5초가
   0.95초에서 3.2초로 늘었다(release, 텍스트 화면 틱 루프).

   같은 기계의 터미널이면 그림을 임시 파일로 넘긴다. base64 로 pty 를 거치면
   640x480 한 장이 1.2MB 이고, 이 Mac 의 pty 는 9.8MB/s 라 한 장에 0.12초가
   든다. 임시 파일로는 이스케이프 한 줄(200 바이트 안팎)이다. 터미널이 그
   길을 받는지는 시험 파일 하나로 물어보고 정한다:

   {v
   Probing ──OK 답──▶ Probed ──시험 파일이 지워짐──▶ Files
      │                                              │
      └──에러 답──▶ Direct ◀──────에러 답────────────┘
   v}

   답이 없으면 Probing 에 머물고 그동안은 직접 보낸다. Files 에서는
   터미널이 앞 프레임 파일을 지우기 전에는 새 프레임을 만들지 않는다 — 그
   파일이 남아 있다는 것이 터미널이 아직 못 읽었다는 뜻이다. *)

type frame = { rgb : string; width : int; height : int; cols : int; rows : int }

(* [dir] 은 우리 임시 폴더, [probe] 는 그 안의 시험 파일이다. *)
type transfer =
  | Direct  (** base64 를 이스케이프에 담는다 *)
  | Probing of { dir : string; probe : string }  (** 답을 기다린다 *)
  | Probed of { dir : string; probe : string }  (** 터미널이 읽었다. 지우기를 기다린다 *)
  | Files of string  (** 이 폴더의 임시 파일로 넘긴다 *)

type t = {
  dir : string option;  (** 끝낼 때 지울 우리 임시 폴더 *)
  mutable transfer : transfer;
  mutable pending : string;  (** 쓰고 있는 이스케이프 *)
  mutable pos : int;
  mutable image_id : int;  (** 화면에 있는 그림의 id *)
  mutable last_file : string option;  (** 터미널이 아직 안 지웠을 수 있는 프레임 파일 *)
  mutable files : int;  (** 만든 프레임 파일 수 — 다음 파일 이름 *)
  mutable sent : int;
  mutable via_files : int;
  mutable gave_up : string option;  (** 임시 파일 길을 버린 까닭 *)
}

(* Kitty 와 Ghostty 둘 다 TMPDIR 과 상관없이 /tmp 를 임시 폴더로 받는다
   (macOS 의 /private/tmp 포함). *)
let temp_root = "/tmp"

(* 그림 id 는 1 과 2 를 번갈아 쓴다. 시험은 그와 겹치지 않는 id 로 묻는다. *)
let first_image_id = 1
let second_image_id = 2
let probe_id = 3

(* 1x1 RGB 검정 *)
let probe_pixel = "\000\000\000"

let write_file path data = Out_channel.with_open_bin path (fun oc -> output_string oc data)

let remove path = try Sys.remove path with Sys_error _ -> ()

(* 임시 폴더를 만들고 시험 파일을 쓴 뒤 묻는다. 그러지 못하면 직접 보낸다. *)
let create () =
  let base =
    { dir = None; transfer = Direct; pending = ""; pos = 0; image_id = second_image_id;
      last_file = None; files = 0; sent = 0; via_files = 0; gave_up = None }
  in
  match Filename.temp_dir ~temp_dir:temp_root ("dosplay-" ^ Dosterm.temp_file_marker ^ "-") "" with
  | exception Sys_error e -> { base with gave_up = Some e }
  | dir -> (
    let probe = Filename.concat dir "probe.rgb" in
    match write_file probe probe_pixel with
    | exception Sys_error e -> { base with dir = Some dir; gave_up = Some e }
    | () ->
      { base with dir = Some dir; transfer = Probing { dir; probe };
                  pending = Dosterm.probe_file ~id:probe_id probe })

let writing o = o.pos < String.length o.pending

(* 터미널이 앞 그림을 아직 다 받지 않았다. *)
let busy o =
  writing o
  ||
  match o.last_file with
  | Some path -> Sys.file_exists path
  | None -> false

let give_up o reason =
  o.transfer <- Direct;
  o.gave_up <- Some reason;
  Option.iter remove o.last_file;
  o.last_file <- None

(* 답 하나로 길을 고른다. 상태와 답의 쌍마다 갈래를 적는다. *)
let on_reply o ~id reply =
  match o.transfer, reply with
  | Probing { dir; probe }, Dosterm.Loaded when id = probe_id ->
    o.transfer <- Probed { dir; probe }
  | Probing { probe; _ }, Dosterm.Failed e when id = probe_id ->
    remove probe;
    give_up o e
  (* 그림은 q=2(직접)라 답이 오지 않는다. 다른 프로그램의 답이면 우리 것이 아니다. *)
  | Probing _, (Dosterm.Loaded | Dosterm.Failed _) -> ()
  (* 시험 답은 한 번뿐이다. *)
  | Probed _, (Dosterm.Loaded | Dosterm.Failed _) -> ()
  (* q=1 은 성공을 답하지 않는다. *)
  | Files _, Dosterm.Loaded -> ()
  (* 프레임 파일을 못 읽었다. 남은 프레임은 직접 보낸다. *)
  | Files _, Dosterm.Failed e -> give_up o e
  | Direct, (Dosterm.Loaded | Dosterm.Failed _) -> ()

(* 그리기 차례마다 부른다. 터미널이 시험 파일을 지웠으면 그 길로 간다. *)
let tick o =
  match o.transfer with
  | Probed { dir; probe } -> if not (Sys.file_exists probe) then o.transfer <- Files dir
  | Direct | Probing _ | Files _ -> ()

let next_file o dir =
  let path = Filename.concat dir (Printf.sprintf "%d.rgb" o.files) in
  o.files <- o.files + 1;
  path

(* 새 그림을 올린 뒤 옛 그림을 지우면 깜빡이지 않는다. *)
let encode o { rgb; width; height; cols; rows } =
  let id = if o.image_id = first_image_id then second_image_id else first_image_id in
  let direct () = Dosterm.image ~id ~width ~height ~cols ~rows rgb in
  let image =
    match o.transfer with
    | Files dir -> (
      let path = next_file o dir in
      match write_file path rgb with
      | () ->
        o.last_file <- Some path;
        o.via_files <- o.via_files + 1;
        Dosterm.image_file ~id ~width ~height ~cols ~rows path
      | exception Sys_error e ->
        remove path;
        give_up o e;
        direct ())
    | Direct | Probing _ | Probed _ -> direct ()
  in
  o.pending <- "\027[H" ^ image ^ Dosterm.delete_image o.image_id;
  o.pos <- 0;
  o.image_id <- id;
  o.sent <- o.sent + 1

(* stdout 이 받는 만큼 쓴다. *)
let rec pump o =
  if writing o then
    match Unix.single_write_substring Unix.stdout o.pending o.pos (String.length o.pending - o.pos) with
    | n ->
      o.pos <- o.pos + n;
      pump o
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()

let post o f =
  encode o f;
  pump o

(* 끝낼 때 쓰던 이스케이프를 마저 쓴다. stdout 이 다시 차단 모드여야 한다. *)
let finish o =
  if writing o then
    ignore
      (Unix.write_substring Unix.stdout o.pending o.pos (String.length o.pending - o.pos))

(* 터미널이 지우지 않은 파일과 폴더를 지운다. *)
let cleanup o =
  Option.iter
    (fun dir ->
      (try Array.iter (fun name -> remove (Filename.concat dir name)) (Sys.readdir dir)
       with Sys_error _ -> ());
      try Sys.rmdir dir with Sys_error _ -> ())
    o.dir
