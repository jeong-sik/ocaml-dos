(* The scenario runner against a program small enough to know exactly:
   mode 13h, then forever read a key with INT 16h AH=00 and paint its ASCII
   code into the top-left pixel; 'q' exits. *)

let failed = ref 0

let check_true name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let paint_keys =
  String.concat ""
    [ "\xb8\x13\x00";     (* 0x100 mov ax,0013h *)
      "\xcd\x10";         (* 0x103 int 10h *)
      "\xb8\x00\xa0";     (* 0x105 mov ax,0A000h *)
      "\x8e\xc0";         (* 0x108 mov es,ax *)
      "\x30\xe4";         (* 0x10a loop: xor ah,ah *)
      "\xcd\x16";         (* 0x10c int 16h *)
      "\x26\xa2\x00\x00"; (* 0x10e mov es:[0],al *)
      "\x3c\x71";         (* 0x112 cmp al,'q' *)
      "\x75\xf4";         (* 0x114 jne loop *)
      "\xb8\x00\x4c";     (* 0x116 mov ax,4C00h *)
      "\xcd\x21" ]        (* 0x119 int 21h *)

let machine () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m paint_keys;
  m

let corner = { Dos_scenario.x = 0; y = 0; w = 1; h = 1 }
let budget = 1_000_000

(* What a region digest must be, computed from the frame without the
   runner: the MD5 of the region's rows of RGB bytes, top to bottom. *)
let expected_digest m (r : Dos_scenario.region) =
  let fw, _ = Dos_machine.frame_dims m in
  let rgb = Dos_machine.frame_rgb m in
  let rows =
    List.init r.h (fun i -> String.sub rgb ((((r.y + i) * fw) + r.x) * 3) (r.w * 3))
  in
  Digest.to_hex (Digest.string (String.concat "" rows))

let press what keys = Dos_scenario.Press { what; keys; budget }

let test_press_paints_and_region_reads_it () =
  let m = machine () in
  (match Dos_scenario.run m [ press "a" [ "a" ] ] with
   | Ok n -> check_true "a press spends machine steps" (n > 0)
   | Error f -> check_true (Dos_scenario.failure_to_string f) false);
  check_true "the key reached the guest" (Dos_machine.pixel m ~x:0 ~y:0 = Char.code 'a');
  check_true "the key was consumed" (not (Dos_state.key_pending m));
  check_true "the guest waits for the next key" (Dos_machine.kbd_waiting m);
  let block = { Dos_scenario.x = 1; y = 2; w = 3; h = 2 } in
  check_true "a 1x1 region digest is its RGB bytes"
    (Dos_scenario.region_digest m corner = Some (expected_digest m corner));
  check_true "a wider region digest takes its rows in order"
    (Dos_scenario.region_digest m block = Some (expected_digest m block))

let test_wait_for_seen_picture () =
  let m = machine () in
  ignore (Dos_scenario.run m [ press "b" [ "b" ] ]);
  let shown = expected_digest m corner in
  match
    Dos_scenario.run m
      [ Wait_for { what = "b painted"; region = corner; digest = shown; budget } ]
  with
  | Ok n -> check_true "a picture already shown needs no steps" (n = 0)
  | Error f -> check_true (Dos_scenario.failure_to_string f) false

(* The second step waits for a picture the program never draws; the runner
   reports that step, not the first, and stops within the budget. *)
let test_wait_for_times_out () =
  let m = machine () in
  let never = String.make 32 '0' in
  match
    Dos_scenario.run m
      [ press "c" [ "c" ];
        Wait_for { what = "never drawn"; region = corner; digest = never; budget } ]
  with
  | Ok _ -> check_true "an undrawn picture fails" false
  | Error f ->
    check_true "the failing step is the wait" (f.index = 1 && f.what = "never drawn");
    check_true "the reason names the budget"
      (String.starts_with ~prefix:"budget ran out" f.reason);
    check_true "the wait stops at its budget" (f.machine_steps <= budget + budget)

let test_bad_key_name () =
  let m = machine () in
  match Dos_scenario.run m [ press "typo" [ "no-such-key" ] ] with
  | Ok _ -> check_true "an unknown key name fails" false
  | Error f ->
    check_true "nothing ran for a bad name" (f.index = 0 && f.machine_steps = 0)

let test_exit_stops_the_run () =
  let m = machine () in
  let shown = String.make 32 'f' in
  match
    Dos_scenario.run m
      [ press "quit" [ "q" ];
        Wait_for { what = "after quit"; region = corner; digest = shown; budget } ]
  with
  | Ok _ -> check_true "a wait after exit fails" false
  | Error f ->
    check_true "the program exited" (Dos_machine.exited m);
    check_true "the failure says so"
      (f.index = 1 && String.starts_with ~prefix:"the program exited" f.reason)

let test_region_outside_frame () =
  let m = machine () in
  ignore (Dos_scenario.run m [ press "d" [ "d" ] ]);
  List.iter
    (fun r -> check_true "a region off the frame has no digest"
        (Dos_scenario.region_digest m r = None))
    [ { Dos_scenario.x = 319; y = 0; w = 2; h = 1 };
      { x = 0; y = 199; w = 1; h = 2 };
      { x = -1; y = 0; w = 1; h = 1 };
      { x = 0; y = 0; w = 0; h = 1 } ]

let () =
  test_press_paints_and_region_reads_it ();
  test_wait_for_seen_picture ();
  test_wait_for_times_out ();
  test_bad_key_name ();
  test_exit_stops_the_run ();
  test_region_outside_frame ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos scenario: all passed"
