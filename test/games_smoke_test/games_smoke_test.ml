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
  (* AA1.EXE 는 메모리가 모자라다는 글을 쓰고 끝나던 게임이다(헤더 maxalloc 을
     무시하고 메모리를 통째로 주던 때). 타이틀은 CGA 4색이다 *)
  { name = "arctic"; exe = "AA1.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x04; min_colors = 4 }; budget = 40_000_000 } ] };
  { name = "bio"; exe = "BMENACE1.EXE";
    stages = [
      { press = []; until = Text "Ready - Press a Key"; budget = 40_000_000 };
      { press = [ "space" ]; until = Graphics { mode = 0x0d; min_colors = 6 }; budget = 40_000_000 } ] };
  { name = "mars"; exe = "MARS1.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x04; min_colors = 4 }; budget = 40_000_000 } ] };
  { name = "dark"; exe = "DA1.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x0d; min_colors = 8 }; budget = 40_000_000 } ] };
  (* HH1.EXE 는 HARRY1.-0 을 EXEC 하는 런처다. 헤더가 블록을 작게 정해 둬서
     AH=4Ah 없이 EXEC 이 돼야 한다(통째로 주면 종료 코드 255 로 끝났다) *)
  { name = "harry"; exe = "HH1.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x13; min_colors = 32 }; budget = 40_000_000 } ] };
  (* 레이캐스팅 3D(모드 13h). 타이틀 뒤에 난이도 고르는 화면이 나온다 *)
  { name = "ken"; exe = "KEN.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x13; min_colors = 32 }; budget = 60_000_000 } ] };
  (* 텍스트 어드벤처. NOVA.COM 은 확장자만 COM 이고 MZ 서명이 있는 EXE 다 *)
  { name = "supernova"; exe = "NOVA.COM";
    stages = [
      { press = []; until = Text "Do you want Color or Black and white?"; budget = 20_000_000 };
      { press = [ "c" ]; until = Text "S U P E R N O V A"; budget = 40_000_000 } ] };
  { name = "titanic"; exe = "BEYOND.COM";
    stages = [
      { press = []; until = Text "Are you using a COLOR screen"; budget = 40_000_000 };
      { press = [ "y" ]; until = Text "Yes, I do have a color screen."; budget = 40_000_000 } ] };
  (* 인터럽트를 끈 채 INT 1Ah AH=0 으로 틱을 기다리는 지연 루프가 있다. 보류 IRQ0 를
     받지 못하면 모드 03h 에서 영영 돈다. 제목 화면은 모드 13h 다 *)
  { name = "arcy2"; exe = "ARCY2.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x13; min_colors = 32 }; budget = 60_000_000 } ] };
  (* 시작할 때 SGDT 의 여섯째 바이트로 286 과 386 을 가른다. SGDT 가 없으면
     "Processor:" 줄 앞에서 멈춘다. 프로세서 줄 다음 줄이 나오면 판별이 끝난 것이다 *)
  { name = "charlie"; exe = "CHARLIE.EXE";
    stages = [
      { press = []; until = Text "Display: VGA detected."; budget = 40_000_000 } ] };
]

(* 386 명령(66h 접두 등)을 쓰는 게임. 286 모델에서는 [Unsupported] 로 멈춘다.
   기본 모델이 386 이 되면 [games] 로 합친다(이슈 #204). *)
let games_386 = [
  (* PINBALL.EXE 는 첫 실행에 설정 메뉴를 띄운다: 사운드 카드 고르기(No Sound Card)
     → Exit and Save. 저장하면 게임이 모드 13h 인트로로 들어간다. 저장하지 않으면
     "Please run SETUP!" 로 끝난다 *)
  { name = "epicpinball"; exe = "PINBALL.EXE";
    stages = [
      { press = []; until = Text "Select Sound Card"; budget = 40_000_000 };
      { press = [ "enter"; "enter"; "down"; "down"; "enter" ];
        until = Graphics { mode = 0x13; min_colors = 3 }; budget = 100_000_000 } ] };
  (* 시작하자마자 66h 접두를 쓴다. 제목 메뉴는 모드 13h 에 100색이 넘는다 *)
  { name = "superworms"; exe = "SW.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x13; min_colors = 64 }; budget = 40_000_000 } ] };
  (* 인트로 애니메이션(OPEN2B.FLI)이 모드 13h 에 100색까지 올라간다 *)
  { name = "rotm"; exe = "ROTM.EXE";
    stages = [
      { press = []; until = Graphics { mode = 0x13; min_colors = 64 }; budget = 40_000_000 } ] };
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

(* 확장자가 COM 이어도 MZ 서명이 있으면 EXE 다(DOS 는 서명으로 고른다). *)
let load m name image =
  if String.length image >= 2 && (String.sub image 0 2 = "MZ" || String.sub image 0 2 = "ZM")
  then Dos_machine.load_exe ~name m image
  else Dos_machine.load_com ~name m image

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

(* 실행 파일도 마운트한다. 실기의 디스크에는 그 파일이 있고, 자기 파일을
   여는 프로그램이 있다(NOVA.COM 은 시작할 때 C:\NOVA.COM 을 연다). 빼면
   그 게임은 질문 화면이 뜨기 전에 종료 코드 0 으로 끝난다. *)
let mount_dir m dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.iter (fun n ->
      let p = Filename.concat dir n in
      if (not (Sys.is_directory p)) && String.length n <= 12
      then
        match Dos_machine.mount_file m n (read_file p) with
        | Ok () -> ()
        | Error e -> failwith (Printf.sprintf "mount %s: %s" n e))

let run_game ~model root g =
  let dir = Filename.concat root g.name in
  if not (Sys.file_exists dir) then begin
    Printf.printf "games smoke: %s 건너뜀 (폴더 없음)\n%!" g.name;
    true
  end
  else begin
    let m = Dos_machine.create ~model () in
    mount_dir m dir;
    load m g.exe (read_file (Filename.concat dir g.exe));
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
    let results =
      List.map (run_game ~model:Cpu86.I80286 root) games
      @ List.map (run_game ~model:Cpu86.I80386 root) games_386
    in
    if List.for_all Fun.id results then print_endline "games smoke: all passed"
    else exit 1
