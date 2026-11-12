(* 삼국지3(RTK3 DOS, KOEI) from a cold boot to the first turn of a new
   game: the KOEI logo, the copy-protection code, the title menu, the
   scenario, one player as 유비, the settings, and the strategy map at
   189년 봄 1월 with 평원 asking for a command. Then the game saves to slot
   1, and a second machine, booted with the save file the first one wrote,
   loads slot 1 from the title menu and comes back to the same turn. The
   loaded game then attacks 8. 북해: 유비, 관우 and 장비 go out, are placed
   on the battle map, and the battle is left to the AI (8. 위임) until the
   month ends and the war goes on into 189년 2월.

   Each stage waits for a picture, not for a step count. The pictures are
   regions of the 640x480 mode 12h frame, recorded once from this run; a
   region that never shows its digest means the game took another path,
   and the frame is written out to look at.

   The game is not in the repository. SAMGUK3_DIR names a directory with
   KOEI.COM and its files; without it the test is skipped. *)

let region x y w h = { Dos_scenario.x; y; w; h }

(* A stage budget is generous: the slowest stage (the opening skip, then
   MAIN loading its data) takes about 5M machine steps. *)
let wait_budget = 30_000_000
let key_budget = 30_000_000

let wait what r digest = Dos_scenario.Wait_for { what; region = r; digest; budget = wait_budget }
let press what keys = Dos_scenario.Press { what; keys; budget = key_budget }

(* The bottom message line, left of where the cursor blinks. A short
   question puts the cursor nearer, so it gets a narrower region. *)
let prompt_line = region 160 390 300 24
let short_prompt = region 160 390 180 24
let tiny_prompt = region 160 390 120 24

(* The battle map's side panel: the city fought over and the month. *)
let battle_panel = region 540 50 90 50

(* The AI fights a battle out day by day; ten days of one month took 160M
   machine steps. *)
let battle_budget = 400_000_000

let save_file = "SANGOKU3.SAV"

let to_title_menu =
  [ wait "KOEI 로고" (region 220 130 200 120) "418c2ef28ff2c7fb299ad234fe7bf09b";
    press "로고에서 오프닝 건너뛰기" [ "enter" ];
    (* The code for [孫李呂] in the manual is 10183. A key typed before
       this box is drawn lands in the code; a wrong code brings another
       question, and a second wrong one ends MAIN.EXE with "fatal error
       occurred." (measured). *)
    wait "보호 코드 [孫李呂]" (region 104 18 64 20) "3e0a63288c39ede1bda328640a6579da";
    press "보호 코드 10183" [ "1"; "0"; "1"; "8"; "3"; "enter" ];
    wait "마우스·키보드 고르기" (region 160 200 340 70) "2149360be7941fa5965f35e1a4dc0ded";
    press "키보드로" [ "enter" ];
    wait "타이틀 메뉴" (region 200 165 240 75) "f972cdb6e3cba6fcb06162d52a0c2171" ]

(* The question is typed out a letter at a time, and a key pressed before
   it ends only skips the typing: after a load, a 1 meant for 1. 군사 went
   there and the Enter after it fell on an empty prompt. The region ends at
   x 404; the text ends at 399 and the cursor starts at 408. *)
let first_turn =
  [ wait "189년 봄 1월" (region 16 392 120 24) "2ea57895fe7931423f3c4acb5f0e2a08";
    wait "평원 정보 (인구 370000, 금 2300, 병사 9500)" (region 20 165 210 215)
      "34a746fc60816308473894096042ec4c";
    wait "유비님, 6.평원에 명령을(0-9)?" (region 160 390 245 24)
      "220d20f86071aa3ead114ec923e9a892" ]

let new_game =
  to_title_menu
  @ [ press "1. 새로운 게임" [ "1"; "enter" ];
    wait "시나리오 목록" (region 170 150 290 140) "b2ef8071d3b11049ed75e296eafed109";
    press "시나리오 1 (189년)" [ "1"; "enter" ];
    wait "몇 명으로 게임하겠습니까" prompt_line "1db0914c7a404189ca81bfe0ef1ae127";
    press "한 명" [ "1"; "enter" ];
    wait "게임자 1 은 누구를" prompt_line "59a0ec12f23f76491d288a7cf9a47f31";
    press "3. 유비" [ "3"; "enter" ];
    wait "게임레벨" prompt_line "465ccbee3d0267db9f9d9bd706ead8a1";
    press "상급, 타국 전쟁 안 본다, 사실모드" [ "2"; "enter"; "2"; "enter"; "1"; "enter" ];
    wait "설정 요약" (region 160 120 300 135) "826e7ec6849ae80799a2f803d9d11dbb";
    press "모두 좋겠습니까 Y" [ "y"; "enter" ] ]
  @ first_turn

let save_to_slot_1 =
  [ press "9. 기능" [ "9"; "enter" ];
    wait "무엇을 하겠습니까(1-6)" short_prompt "6c938484c522fbe6510eca48870577fa";
    press "1. 중단" [ "1"; "enter" ];
    wait "어떻게 하겠습니까(1-3)" short_prompt "490ddc989a993e539739c692a0d341df";
    press "2. 저장" [ "2"; "enter" ];
    wait "어디에 저장하겠습니까(1-10)" short_prompt "22cdf360f3e7b3f4070bc2c8deb0759e";
    press "슬롯 1" [ "1"; "enter" ];
    wait "저장 뒤 어떻게 하겠습니까(1-3)" short_prompt "490ddc989a993e539739c692a0d341df" ]

(* The load list names what slot 1 holds: "1.189년 1월:유비 :평원:§1:상급". *)
let load_slot_1 =
  to_title_menu
  @ [ press "2. 데이터를 로드한다" [ "2"; "enter" ];
      wait "로드 목록 1번: 189년 1월 유비 평원 상급" (region 170 120 300 22)
        "adf0a36c876dd2ddc2118e94fdc829ce";
      press "슬롯 1" [ "1"; "enter" ] ]
  @ first_turn

let read path = In_channel.with_open_bin path In_channel.input_all

(* [save] replaces the save file the directory holds, as a player copying
   a save onto another machine would. *)
let boot ?save dir =
  let m = Dos_machine.create () in
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.iter (fun f ->
      let p = Filename.concat dir f in
      if not (Sys.is_directory p) then Dos_machine.mount_file m f (read p));
  Option.iter (Dos_machine.mount_file m save_file) save;
  Dos_machine.load_com m (read (Filename.concat dir "KOEI.COM"));
  m

let fail_with_frame m ~name message =
  let frame = Filename.concat (Filename.get_temp_dir_name ()) ("samguk3-" ^ name ^ ".ppm") in
  Out_channel.with_open_bin frame (fun oc -> output_string oc (Dos_machine.frame_ppm m));
  Printf.eprintf "FAIL samguk3 %s: %s\n  frame: %s\n%!" name message frame;
  exit 1

let play m ~name steps =
  let t0 = Unix.gettimeofday () in
  match Dos_scenario.run m steps with
  | Ok n -> Printf.printf "samguk3 %s: %d machine steps, %.1f s\n%!" name n (Unix.gettimeofday () -. t0)
  | Error f -> fail_with_frame m ~name (Dos_scenario.failure_to_string f)

let attack_bukhae =
  [ press "1. 군사" [ "1"; "enter" ];
    wait "군사: 무엇을 하겠습니까(1-9)" short_prompt "4a35704059cebfee719fd1676602baef";
    press "4. 전쟁" [ "4"; "enter" ];
    wait "어느 곳으로 쳐들어가겠습니까(1-60)" short_prompt "20bcdd7d1a3fab0fa90c0c704d8cb493";
    press "8. 북해" [ "8"; "enter" ];
    wait "누구를 보내겠습니까(1-3)" short_prompt "8321dbd4b2381db120db4694d427903b";
    (* Each officer, then his troops: 1. 보병. An empty Enter ends the list. *)
    press "유비·관우·장비, 모두 보병"
      [ "1"; "enter"; "1"; "enter"; "2"; "enter"; "1"; "enter"; "3"; "enter"; "1"; "enter"; "enter" ];
    wait "금을 얼마나 가지고 가겠습니까" short_prompt "27b7ed57d9a7267b2cd65b8b0cea1caf";
    press "금 0, 군량 20000" [ "0"; "enter"; "2"; "0"; "0"; "0"; "0"; "enter" ];
    wait "쳐들어가겠습니까(Y/N)" tiny_prompt "26c64e7311d8d83845b2e3c8648e8028";
    press "Y" [ "y" ];
    wait "전장: 8 북해, 189년 1월" battle_panel "996179b29797e5586bc4b48e62a668c2";
    wait "유비를 배치해 주세요" (region 300 360 200 40) "3c65fe8b74b996c2c264a13cae810509";
    (* 0 places the officer under the cursor; digits move it on the hex
       grid (2 down, 1 down-left). The cell below 관우 is not open. *)
    press "유비·관우·장비 배치" [ "0"; "2"; "0"; "1"; "0" ];
    wait "누가 군량을 가져 갑니까(1-3)" (region 110 300 300 30) "cc1b3b8810294397939f095e8a0eb07d";
    press "유비가 군량을" [ "1"; "enter" ];
    wait "전투 명령(1-9)" (region 325 290 170 55) "354891a0723d04960d63da6194d4caba";
    (* A battle menu opens on its digit alone; an Enter after it would close
       the menu again. *)
    press "8. 위임, Y" [ "8"; "y" ];
    Dos_scenario.Wait_for
      { what = "189년 2월, 북해 전쟁이 이어진다"; region = battle_panel;
        digest = "7f05bd71b88cd4d1b50185bab098cd8b"; budget = battle_budget } ]

let () =
  match Sys.getenv_opt "SAMGUK3_DIR" with
  | None -> print_endline "samguk3 scenario: SAMGUK3_DIR unset, skipped"
  | Some dir ->
    let first = boot dir in
    play first ~name:"new-game" (new_game @ save_to_slot_1);
    let original = read (Filename.concat dir save_file) in
    (match Dos_machine.read_mounted first save_file with
     | None -> fail_with_frame first ~name:"save" (save_file ^ " is not mounted")
     | Some saved when saved = original ->
       fail_with_frame first ~name:"save" (save_file ^ " did not change")
     | Some saved ->
       Printf.printf "samguk3 save: %s %d bytes, %s -> %s\n%!" save_file (String.length saved)
         (Digest.to_hex (Digest.string original)) (Digest.to_hex (Digest.string saved));
       play (boot ~save:saved dir) ~name:"load-and-battle" (load_slot_1 @ attack_bukhae))
