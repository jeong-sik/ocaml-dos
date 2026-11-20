(* The --cpu flag of dosboot and dosplay: two exact values, nothing else. *)

let failed = ref 0

let check name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let () =
  check "286" (Cpu_flag.parse "286" = Ok Cpu86.I80286);
  check "386" (Cpu_flag.parse "386" = Ok Cpu86.I80386);
  List.iter
    (fun s -> check (Printf.sprintf "%S is refused" s) (Result.is_error (Cpu_flag.parse s)))
    [ ""; "86"; "186"; "486"; "386 "; " 386"; "I80386"; "0x386" ];
  (match Cpu_flag.parse "486" with
   | Error e -> check "the error names the flag and the value" (e = "bad --cpu 486 (want 286 or 386)")
   | Ok _ -> check "486 is refused" false);
  (* The model reaches the machine: 0F 84 (JE rel16) is a 386 instruction.
     mov al,5 ; cmp al,al ; je +2 ; mov al,1 ; mov [0200h],al ; int 20h
     leaves 5 at 0200h when the jump runs and is taken. *)
  let out_off = 0x200 in
  let stored model =
    let m = Dos_machine.create ~model () in
    Dos_machine.load_com m "\xb0\x05\x38\xc0\x0f\x84\x02\x00\xb0\x01\xa2\x00\x02\xcd\x20";
    match Dos_machine.run_until m ~max_steps:200 ~stop:(fun _ -> false) with
    | _ -> Some (Dos_state.rd8 m ((m.Dos_state.psp_seg lsl 4) + out_off))
    | exception Cpu86.Unsupported _ -> None
  in
  check "a 386 machine runs the jump and keeps AL" (stored Cpu86.I80386 = Some 5);
  check "a 286 machine does not leave 5 there" (stored Cpu86.I80286 <> Some 5);
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "cpu flag: all passed"
