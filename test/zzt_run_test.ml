(* 실게임 통합 검증 — ZZT 3.2(1991, Turbo Pascal)가 부팅해 보드를 그리고
   방향키로 플레이어가 움직이는지 본다. 이 하나가 CPU·BIOS·DOS·장치
   포트·로더를 한 번에 지나간다.

   게임 이미지는 저장소에 넣지 않는다(배포 조건). ZZT_DIR 이 가리키는
   자리에 ZZT.DAT·ZZT.CFG·TOWN.ZZT 가 있으면 돌고, 없으면 건너뛴다.
   실행 파일은 ZZT_EXE 로 따로 준다 — 원본은 LZEXE 로 눌려 있어서
   푼 것을 쓰는 게 보통이다. 기본값은 ZZT_DIR/ZZT.EXE. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let check_true name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* 타이틀 화면에서 게임 안까지 들어가는 키 차례 — 실측으로 정한 것이다.
   ESC, 'c'(색 고르기 통과), Enter, ↑, Enter, 스페이스, Enter, ↑, Enter,
   Enter. 굶주릴 때 하나씩 들어간다. *)
let intro_keys =
  [ 0x256b; 0x2e63; 0x1c0d; 0x1970; 0x1c0d; 0x3920; 0x1c0d; 0x1970; 0x1c0d;
    0x1c0d ]

let key_right = 0x4d00
let key_b = 0x3062          (* 'b' — 사이드바의 소리 켜기/끄기 *)
let key_s = 0x1f73          (* 's' — 저장 *)
let key_enter = 0x1c0d
let vram_base = 0xB8000
let player_glyph = 0x02
let player_attr = 0x1F              (* 파란 바탕 흰 글자 — 플레이어만 *)
let board_cols = 60                 (* 오른쪽은 사이드바다 *)

(* 보드에서 플레이어(스마일리)를 찾는다. 글자만 보면 안 된다 — 사이드바
   에도 같은 코드가 있다. 색까지 맞춰야 플레이어다. *)
let find_player m =
  let found = ref None in
  for r = 0 to 24 do
    for c = 0 to board_cols - 1 do
      let a = vram_base + (((r * 80) + c) * 2) in
      if !found = None
         && Dos_machine.mem_read m a = player_glyph
         && Dos_machine.mem_read m (a + 1) = player_attr
      then found := Some (c, r)
    done
  done;
  !found

let boot dir ~extra_keys ~steps =
  let m = Dos_machine.create () in
  (* ZZT.CFG 가 없으면 첫 실행 설정 화면이 떠서 키 차례가 어긋난다 *)
  List.iter
    (fun n ->
      let p = Filename.concat dir n in
      if Sys.file_exists p then Dos_machine.mount_file m n (read_file p))
    [ "ZZT.DAT"; "ZZT.CFG"; "TOWN.ZZT" ];
  let exe =
    match Sys.getenv_opt "ZZT_EXE" with
    | Some p -> p
    | None -> Filename.concat dir "ZZT.EXE"
  in
  Dos_machine.load_exe m (read_file exe);
  let keys =
    List.map (fun w -> { Dos_machine.word = w; not_before = 0 }) intro_keys
    @ extra_keys
  in
  ignore (Dos_machine.run_with_keys m ~max_steps:steps ~keys);
  m

(* ZZT 는 플레이어를 깜빡인다 — 한 시점의 화면만 보면 있는데도 없다고
   읽는다. 보일 때까지 조금 더 돌린다. 키는 남아 있지 않으니 그동안
   플레이어가 움직이지는 않는다. *)
let settle m =
  let seen = ref (find_player m) in
  let n = ref 0 in
  while !seen = None && !n < 400_000 do
    ignore (Dos_machine.step m);
    incr n;
    seen := find_player m
  done;
  !seen

let () =
  match Sys.getenv_opt "ZZT_DIR" with
  | None -> print_endline "zzt run: ZZT_DIR 없음, 건너뜀"
  | Some dir ->
    let m = boot dir ~extra_keys:[] ~steps:6_000_000 in
    let text = Dos_machine.screen_text m in
    let has needle =
      let n = String.length needle and h = String.length text in
      let rec go i = i + n <= h && (String.sub text i n = needle || go (i + 1)) in
      go 0
    in
    check_true "보드가 그려졌다" (has "The Town of ZZT");
    check_true "사이드바가 그려졌다" (has "Health:");
    let before = settle m in
    if before = None then
      prerr_endline ("화면:\n" ^ Dos_machine.screen_text_utf8 m);
    check_true "플레이어가 보드에 있다" (before <> None);
    (* 같은 부팅에 오른쪽 방향키를 하나 더 예약해 다시 돌린다 *)
    let m2 =
      boot dir
        ~extra_keys:[ { Dos_machine.word = key_right; not_before = 5_000_000 } ]
        ~steps:6_000_000
    in
    let after = settle m2 in
    let text2 = Dos_machine.screen_text m2 in
    let has2 needle =
      let n = String.length needle and h = String.length text2 in
      let rec go i = i + n <= h && (String.sub text2 i n = needle || go (i + 1)) in
      go 0
    in
    (* 첫 방향키는 멈춤을 풀고 그 한 번으로 움직인다 *)
    check_true "방향키가 멈춤을 풀었다" (not (has2 "Pausing"));
    (match (before, after) with
     | Some (c0, r0), Some (c1, r1) ->
       check "방향키가 플레이어를 한 칸 옮긴다 (열)" c1 (c0 + 1);
       check "행은 그대로" r1 r0
     | _ -> check_true "이동 전후 플레이어를 둘 다 찾았다" false);
    (* 메뉴 키가 게임 상태를 바꾸는가 — 사이드바의 안내가 뒤집힌다 *)
    let m3 =
      boot dir
        ~extra_keys:
          [ { Dos_machine.word = key_right; not_before = 5_000_000 };
            { Dos_machine.word = key_b; not_before = 6_000_000 };
            { Dos_machine.word = key_s; not_before = 7_000_000 };
            { Dos_machine.word = key_enter; not_before = 8_000_000 } ]
        ~steps:12_000_000
    in
    let text3 = Dos_machine.screen_text m3 in
    let has3 needle =
      let n = String.length needle and h = String.length text3 in
      let rec go i = i + n <= h && (String.sub text3 i n = needle || go (i + 1)) in
      go 0
    in
    check_true "메뉴 키 B 가 사이드바를 바꾼다" (has3 "Be noisy");
    (* 저장이 INT 21h 를 지나 마운트 표로 돌아왔는가 *)
    (match Dos_machine.read_mounted m3 "SAVED.SAV" with
     | None -> check_true "세이브 파일이 만들어졌다" false
     | Some data ->
       check_true "세이브가 비어 있지 않다" (String.length data > 1000);
       (* ZZT 3.x 월드 서명: FFFF 뒤에 보드 수 *)
       check_true "세이브가 ZZT 월드 형식이다"
         (String.length data > 2 && data.[0] = '\xff' && data.[1] = '\xff'));
    if !failed = 0 then print_endline "zzt run: all passed"
    else begin
      Printf.eprintf "zzt run: %d failures\n%!" !failed;
      exit 1
    end
