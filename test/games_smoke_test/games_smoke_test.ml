(* 실게임 연기 시험 — 프리웨어 게임 여럿을 부팅해 키를 눌러 가며 화면이
   살아 있는지 본다. 로더·DOS·BIOS·VGA 를 바꾼 뒤 게임이 시작조차 못 하게
   됐는지 한 번에 잡는 용도다. 장면의 정확한 모양(해시)은 보지 않는다.

   게임 파일은 저장소에 넣지 않는다. DOS_GAMES_DIR 아래에 게임마다 폴더를
   두고(scripts/fetch-freeware-games.sh 가 받아 푼다), 폴더가 없는 게임은
   건너뛴다. DOS_GAMES_DIR 이 없으면 전체를 건너뛴다.

   게임마다 단계를 지난다. 단계는 키를 하나씩 누르고 3M 스텝씩 돌린 뒤,
   조건이 맞을 때까지 1M 스텝씩 더 돌린다. 예산 안에 못 맞추면 실패다.
   게임이 끝나거나 [Cpu86.Unsupported] 가 나도 실패다. *)

let read_file path = In_channel.with_open_bin path In_channel.input_all
let key name =
  match Dos_machine.key_of_string name with Ok w -> w | Error e -> failwith e

let after_key_steps = 3_000_000
let poll_steps = 1_000_000

(* 장면이 나왔다는 조건. 화면 글자는 [text] 가, 그래픽 화면은 비디오 모드와
   서로 다른 색 수가 말해 준다 — 빈 화면이나 한 가지 색 화면은 못 넘는다. *)
type until =
  | Text of string
  | Graphics of { mode : int; min_colors : int }

type stage = { press : string list; until : until; budget : int }
type game = { name : string; exe : string; stages : stage list }

let games = [
  { name = "snarf"; exe = "SNARF.EXE";
    stages = [
      (* 소개 화면은 4색, 미로는 15색이다 *)
      { press = []; until = Graphics { mode = 0x10; min_colors = 3 }; budget = 40_000_000 };
      { press = [ "space"; "space"; "enter"; "enter" ];
        until = Graphics { mode = 0x10; min_colors = 10 }; budget = 60_000_000 } ] };
  { name = "thor"; exe = "CAVES.EXE";
    stages = [
      { press = []; until = Text "Begin your Journey"; budget = 40_000_000 };
      { press = [ "1"; "enter"; "space"; "space" ];
        until = Text "Health"; budget = 60_000_000 } ] };
  { name = "xargon"; exe = "XRFILE01.EXE";
    stages = [
      { press = []; until = Text "Press ENTER if this is correct"; budget = 40_000_000 };
      { press = [ "enter"; "enter"; "enter" ];
        until = Graphics { mode = 0x13; min_colors = 32 }; budget = 100_000_000 } ] };
  { name = "kilo"; exe = "KFILE1.EXE";
    stages = [
      { press = []; until = Text "Press ENTER if this is correct"; budget = 40_000_000 };
      { press = [ "enter"; "enter"; "enter" ];
        until = Graphics { mode = 0x13; min_colors = 32 }; budget = 100_000_000 } ] };
  { name = "jetpack"; exe = "JETPACK.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x13; min_colors = 32 }; budget = 40_000_000 } ] };
]

let contains text needle =
  let n = String.length needle and h = String.length text in
  let rec go i = i + n <= h && (String.sub text i n = needle || go (i + 1)) in
  go 0

(* 화면의 서로 다른 RGB 색 수 *)
let colors m =
  let rgb = Dos_machine.frame_rgb m in
  let seen = Hashtbl.create 64 in
  let i = ref 0 in
  while !i + 2 < String.length rgb do
    Hashtbl.replace seen (String.sub rgb !i 3) ();
    i := !i + 3
  done;
  Hashtbl.length seen

let reached m = function
  | Text s -> contains (Dos_machine.screen_text m) s
  | Graphics { mode; min_colors } -> Dos_machine.video_mode m = mode && colors m >= min_colors

let describe = function
  | Text s -> Printf.sprintf "text %S" s
  | Graphics { mode; min_colors } -> Printf.sprintf "mode %02Xh with %d+ colors" mode min_colors

let run_steps m n = ignore (Dos_machine.run_until m ~max_steps:n ~stop:(fun _ -> false))

(* 단계를 지난다. 맞췄으면 None, 못 맞췄으면 이유. *)
let run_stage m stage =
  List.iter
    (fun k -> Dos_machine.push_key m (key k); run_steps m after_key_steps)
    stage.press;
  let spent = ref 0 in
  while !spent < stage.budget && not (Dos_machine.exited m) && not (reached m stage.until) do
    run_steps m poll_steps;
    spent := !spent + poll_steps
  done;
  if reached m stage.until then None
  else if Dos_machine.exited m then
    Some (Printf.sprintf "the program exited (code %d) before %s" (Dos_machine.exit_code m)
            (describe stage.until))
  else Some (Printf.sprintf "no %s within %d steps" (describe stage.until) stage.budget)

let mount_dir m dir exe =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.iter (fun n ->
      let p = Filename.concat dir n in
      if (not (Sys.is_directory p)) && String.length n <= 12
         && String.uppercase_ascii n <> exe
      then
        match Dos_machine.mount_file m n (read_file p) with
        | Ok () -> ()
        | Error e -> failwith (Printf.sprintf "mount %s: %s" n e))

let run_game root g =
  let dir = Filename.concat root g.name in
  if not (Sys.file_exists dir) then begin
    Printf.printf "games smoke: %s 건너뜀 (폴더 없음)\n%!" g.name;
    true
  end
  else begin
    let m = Dos_machine.create () in
    mount_dir m dir g.exe;
    Dos_machine.load_exe ~name:g.exe m (read_file (Filename.concat dir g.exe));
    let unserved = ref 0 in
    Dos_machine.on_unserved m (fun _ ~vector:_ ~ax:_ -> incr unserved);
    let failure =
      try List.find_map (run_stage m) g.stages
      with Cpu86.Unsupported what -> Some ("unsupported: " ^ what)
    in
    match failure with
    | None ->
      Printf.printf "games smoke: %s ok (mode %02Xh, %d colors, %d unserved)\n%!" g.name
        (Dos_machine.video_mode m) (colors m) !unserved;
      true
    | Some why ->
      Printf.eprintf "FAIL %s: %s\n%!" g.name why;
      false
  end

let () =
  match Sys.getenv_opt "DOS_GAMES_DIR" with
  | None -> print_endline "games smoke: DOS_GAMES_DIR 없음, 건너뜀"
  | Some root ->
    let results = List.map (run_game root) games in
    if List.for_all Fun.id results then print_endline "games smoke: all passed"
    else exit 1
