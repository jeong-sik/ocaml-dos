(* The places a program tells an 80186 from an 80286 by, run on both
   models: what FLAGS bits 12-15 read back as, what PUSH SP pushes, and
   whether 0x0F is POP CS or the head of SMSW. Code at 0000:0000, stack at
   1000:0100. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %04x want %04x\n%!" name got want
  end

let cpu model code =
  let mem = Bytes.make (1024 * 1024) '\000' in
  String.iteri (fun i c -> Bytes.set mem i c) code;
  let read a = Char.code (Bytes.get mem (a land 0xfffff)) in
  let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff)) in
  let t = Cpu86.create ~model ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) () in
  Cpu86.set_seg t 1 0;
  Cpu86.set_ip t 0;
  Cpu86.set_seg t 2 0x1000;
  Cpu86.set_reg16 t 4 0x0100;
  t

let run t n = for _ = 1 to n do ignore (Cpu86.step t) done
let ax t = Cpu86.reg16 t 0
let bx t = Cpu86.reg16 t 3
let cx t = Cpu86.reg16 t 1

(* The test most CPU checks of the time run: clear bits 12-15 and read
   FLAGS back (an 8086 or 186 shows them set), then set them and read again
   (a 286 in real mode shows them clear). *)
let detect =
  String.concat ""
    [ "\x9c\x58";           (* pushf; pop ax *)
      "\x25\xff\x0f";       (* and ax,0FFFh *)
      "\x50\x9d";           (* push ax; popf *)
      "\x9c\x58";           (* pushf; pop ax *)
      "\x89\xc3";           (* mov bx,ax *)
      "\x0d\x00\xf0";       (* or ax,0F000h *)
      "\x50\x9d";           (* push ax; popf *)
      "\x9c\x59" ]          (* pushf; pop cx *)

let detect_steps = 13

let test_flags () =
  let t = cpu Cpu86.I80186 detect in
  run t detect_steps;
  check "186: bits 12-15 read set after clearing" (bx t land 0xF000) 0xF000;
  check "186: and after setting" (cx t land 0xF000) 0xF000;
  let t = cpu Cpu86.I80286 detect in
  run t detect_steps;
  check "286: bits 12-15 read clear after clearing" (bx t land 0xF000) 0;
  check "286: and after setting" (cx t land 0xF000) 0;
  check "286: bit 1 still reads set" (cx t land 0x0002) 0x0002;
  check "286: the flags word is the same either way" (Cpu86.flags t land 0xF000) 0

let test_push_sp () =
  List.iter
    (fun (name, code) ->
      let t = cpu Cpu86.I80186 code in
      run t 2;
      check ("186 " ^ name ^ " pushes the SP after the push") (ax t) 0x00FE;
      let t = cpu Cpu86.I80286 code in
      run t 2;
      check ("286 " ^ name ^ " pushes the SP before the push") (ax t) 0x0100;
      check ("286 " ^ name ^ " still moves SP") (Cpu86.reg16 t 4) 0x0100)
    [ ("push sp", "\x54\x58"); ("ff /6 sp", "\xff\xf4\x58") ]

let test_0f () =
  (* smsw ax: 0F 01 /4, modrm 11 100 000 *)
  let t = cpu Cpu86.I80286 "\x0f\x01\xe0" in
  run t 1;
  check "286: SMSW in real mode" (ax t) 0xFFF0;
  check "286: SMSW is three bytes" (Cpu86.dump_ip t) 3;
  (* On a 186, 0x0F pops CS: the word on the stack is 0. *)
  let t = cpu Cpu86.I80186 "\x0f" in
  Cpu86.set_seg t 1 0;
  run t 1;
  check "186: 0x0F is POP CS" (Cpu86.seg t 1) 0;
  check "186: POP CS is one byte" (Cpu86.dump_ip t) 1;
  let raised code =
    let t = cpu Cpu86.I80286 code in
    match Cpu86.step t with
    | _ -> false
    | exception Cpu86.Unsupported _ -> true
  in
  check "286: other 0F instructions are not there yet"
    (Bool.to_int (raised "\x0f\x00\xc0" && raised "\x0f\x01\xf0" && raised "\x0f\x06")) 1

(* Where INT 0 is taken from: the next instruction on a 186, the dividing
   instruction itself, prefix and all, on a 286. *)
let test_divide_fault () =
  List.iter
    (fun (name, code, next) ->
      List.iter
        (fun (model, want) ->
          let t = cpu model code in
          let at = ref (-1) in
          Cpu86.set_int_hook t (fun n -> if n = 0 then at := Cpu86.dump_ip t);
          ignore (Cpu86.step t);
          check (Printf.sprintf "%s: INT 0 from %s" name
                   (if model = Cpu86.I80286 then "286" else "186")) !at want)
        [ (Cpu86.I80186, next); (Cpu86.I80286, 0) ])
    [ ("cs: div bl by 0", "\x2e\xf6\xf3", 3); ("aam 0", "\xd4\x00", 2) ]

(* The ASCII and decimal adjusts where the two models part (SingleStepTests
   80286 37, 3F, 2F): the 286 adds or takes 106h from AX in one go, so the
   carry or borrow of AL±6 reaches AH, and its DAS keeps the borrow of
   AL-6 in CF. Each case: code, AX, AF set, then AX and CF on 186 and 286. *)
let test_bcd () =
  List.iter
    (fun (name, code, ax0, af, (ax186, cf186), (ax286, cf286)) ->
      List.iter
        (fun (model, label, want_ax, want_cf) ->
          let t = cpu model code in
          Cpu86.set_reg16 t 0 ax0;
          if af then Cpu86.set_flags t (Cpu86.flags t lor Cpu86.f_aux);
          run t 1;
          check (Printf.sprintf "%s %s: AX" label name) (ax t) want_ax;
          check (Printf.sprintf "%s %s: CF" label name)
            (Bool.to_int (Cpu86.flags t land Cpu86.f_carry <> 0)) want_cf)
        [ (Cpu86.I80186, "186", ax186, cf186); (Cpu86.I80286, "286", ax286, cf286) ])
    [ ("aaa FC", "\x37", 0xDFFC, false, (0xE002, 1), (0xE102, 1));
      ("aas 04 with AF", "\x3f", 0x1D04, true, (0x1C0E, 1), (0x1B0E, 1));
      ("das 00 with AF", "\x2f", 0x0000, true, (0x00FA, 0), (0x00FA, 1)) ]

let () =
  test_bcd ();
  test_divide_fault ();
  test_flags ();
  test_push_sp ();
  test_0f ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "cpu86 286: all passed"
