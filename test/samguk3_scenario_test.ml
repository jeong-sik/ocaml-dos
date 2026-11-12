(* 삼국지3(RTK3 DOS, KOEI) from a cold boot to the first turn of a new
   game: the KOEI logo, the copy-protection code, the title menu, the
   scenario, one player as 유비, the settings, and the strategy map at
   189년 봄 1월 with 평원 asking for a command.

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

(* The bottom message line, left of where the cursor blinks. *)
let prompt_line = region 160 390 300 24

let new_game =
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
    wait "타이틀 메뉴" (region 200 165 240 75) "f972cdb6e3cba6fcb06162d52a0c2171";
    press "1. 새로운 게임" [ "1"; "enter" ];
    wait "시나리오 목록" (region 170 150 290 140) "b2ef8071d3b11049ed75e296eafed109";
    press "시나리오 1 (189년)" [ "1"; "enter" ];
    wait "몇 명으로 게임하겠습니까" prompt_line "1db0914c7a404189ca81bfe0ef1ae127";
    press "한 명" [ "1"; "enter" ];
    wait "게임자 1 은 누구를" prompt_line "59a0ec12f23f76491d288a7cf9a47f31";
    press "3. 유비" [ "3"; "enter" ];
    wait "게임레벨" prompt_line "465ccbee3d0267db9f9d9bd706ead8a1";
    press "상급, 타국 전쟁 안 본다, 사실모드" [ "2"; "enter"; "2"; "enter"; "1"; "enter" ];
    wait "설정 요약" (region 160 120 300 135) "826e7ec6849ae80799a2f803d9d11dbb";
    press "모두 좋겠습니까 Y" [ "y"; "enter" ];
    wait "189년 봄 1월" (region 16 392 120 24) "2ea57895fe7931423f3c4acb5f0e2a08";
    wait "평원 정보 (인구 370000, 금 2300, 병사 9500)" (region 20 165 210 215)
      "34a746fc60816308473894096042ec4c" ]

let boot dir =
  let read path = In_channel.with_open_bin path In_channel.input_all in
  let m = Dos_machine.create () in
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.iter (fun f ->
      let p = Filename.concat dir f in
      if not (Sys.is_directory p) then Dos_machine.mount_file m f (read p));
  Dos_machine.load_com m (read (Filename.concat dir "KOEI.COM"));
  m

let () =
  match Sys.getenv_opt "SAMGUK3_DIR" with
  | None -> print_endline "samguk3 scenario: SAMGUK3_DIR unset, skipped"
  | Some dir ->
    let m = boot dir in
    let t0 = Unix.gettimeofday () in
    (match Dos_scenario.run m new_game with
     | Ok steps ->
       Printf.printf "samguk3 scenario: new game to 189년 1월 in %d machine steps, %.1f s\n"
         steps (Unix.gettimeofday () -. t0)
     | Error f ->
       let frame = Filename.concat (Filename.get_temp_dir_name ()) "samguk3-scenario.ppm" in
       Out_channel.with_open_bin frame (fun oc -> output_string oc (Dos_machine.frame_ppm m));
       Printf.eprintf "FAIL samguk3 scenario: %s\n  frame: %s\n%!"
         (Dos_scenario.failure_to_string f) frame;
       exit 1)
