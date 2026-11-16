(* The places a program tells an 80186 from an 80286, and an 80286 from an
   80386, by, run on the models they part: what FLAGS bits 12-15 read back
   as, what PUSH SP pushes, whether 0x0F is POP CS or the head of SMSW, and
   the faults each takes (SingleStepTests/80286 and 80386). Code at
   0000:0000, stack at 1000:0100. *)

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

(* One step that may fault. [setup] runs before it; the result is the
   first interrupt the step took and the IP it was taken from, or None. *)
let fault_of model code setup =
  let t = cpu model code in
  setup t;
  let taken = ref None in
  Cpu86.set_int_hook t (fun n -> if !taken = None then taken := Some (n, Cpu86.dump_ip t));
  ignore (Cpu86.step t);
  (t, !taken)

let check_taken name got want =
  let show = function None -> "none" | Some (n, ip) -> Printf.sprintf "INT %d from %04x" n ip in
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %s want %s\n%!" name (show got) (show want)
  end

(* The faults a 286 takes where an 8086 or 186 carries on (SingleStepTests
   80286): a word at offset FFFF (INT 13), an instruction past 10 bytes
   (INT 13), an encoding the 286 leaves undefined (INT 6), and BOUND, which
   returns to itself (INT 5). A fault comes back to the first prefix with
   the registers as the instruction found them. *)
let test_faults () =
  let si_ffff t = Cpu86.set_reg16 t 6 0xFFFF in
  let nothing _ = () in
  (* cs: add [si],ax with SI=FFFF *)
  let t, got = fault_of Cpu86.I80286 "\x2e\x01\x04" si_ffff in
  check_taken "286 word at FFFF" got (Some (13, 0));
  check "286 word at FFFF leaves SI" (Cpu86.reg16 t 6) 0xFFFF;
  let _, got = fault_of Cpu86.I80186 "\x2e\x01\x04" si_ffff in
  check_taken "186 word at FFFF wraps" got None;
  (* leave with BP=FFFF: SP comes back from BP's value *)
  let t, got = fault_of Cpu86.I80286 "\xc9" (fun t -> Cpu86.set_reg16 t 5 0xFFFF) in
  check_taken "286 leave at FFFF" got (Some (13, 0));
  check "286 leave puts SP back" (Cpu86.reg16 t 4) 0x0100;
  (* pop [si] with SI=FFFF: the pop already moved SP *)
  let t, got = fault_of Cpu86.I80286 "\x8f\x04" si_ffff in
  check_taken "286 pop to FFFF" got (Some (13, 0));
  check "286 pop to FFFF keeps SP moved" (Cpu86.reg16 t 4) 0x0102;
  (* rep stosw from DI=FFFF, CX=3: DI moved once, CX taken twice *)
  let t, got =
    fault_of Cpu86.I80286 "\xf3\xab" (fun t -> Cpu86.set_reg16 t 7 0xFFFF; Cpu86.set_reg16 t 1 3)
  in
  check_taken "286 rep stosw at FFFF" got (Some (13, 0));
  check "286 rep stosw DI" (Cpu86.reg16 t 7) 0x0001;
  check "286 rep stosw CX" (Cpu86.reg16 t 1) 1;
  (* cmpsw reads ES:[DI] first: a fault there leaves SI where it was *)
  let t, got = fault_of Cpu86.I80286 "\xa7" (fun t -> Cpu86.set_reg16 t 7 0xFFFF) in
  check_taken "286 cmpsw at DI=FFFF" got (Some (13, 0));
  check "286 cmpsw leaves SI" (Cpu86.reg16 t 6) 0;
  check "286 cmpsw moved DI" (Cpu86.reg16 t 7) 0x0001;
  (* rol word [si],80h shifts by 0 but still reads; esc [si] reads a word *)
  let _, got = fault_of Cpu86.I80286 "\xc1\x04\x80" si_ffff in
  check_taken "286 rol by 0 at FFFF" got (Some (13, 0));
  let _, got = fault_of Cpu86.I80286 "\xd8\x04" si_ffff in
  check_taken "286 esc at FFFF" got (Some (13, 0));
  (* six prefixes and a five-byte far call: eleven bytes *)
  let long = "\x2e\x26\x2e\x36\x26\x3e\x9a\x00\x00\x00\x20" in
  let t, got = fault_of Cpu86.I80286 long nothing in
  check_taken "286 eleven bytes" got (Some (13, 0));
  check "286 eleven bytes push nothing" (Cpu86.reg16 t 4) 0x0100;
  let t, _ = fault_of Cpu86.I80186 long nothing in
  check "186 eleven bytes call" (Cpu86.seg t 1) 0x2000;
  (* lea ax,cx and mov cs,ax *)
  let _, got = fault_of Cpu86.I80286 "\x8d\xc1" nothing in
  check_taken "286 lea on a register" got (Some (6, 0));
  let _, got = fault_of Cpu86.I80286 "\x8e\xc8" nothing in
  check_taken "286 mov cs" got (Some (6, 0));
  let t, got = fault_of Cpu86.I80186 "\x8e\xc8" (fun t -> Cpu86.set_reg16 t 0 0x1234) in
  check_taken "186 mov cs" got None;
  check "186 mov cs loads CS" (Cpu86.seg t 1) 0x1234;
  (* bound ax,[0010h] with AX=5 above the upper bound 0 *)
  let bound = "\x62\x06\x10\x00" in
  let above t = Cpu86.set_reg16 t 0 5 in
  let _, got = fault_of Cpu86.I80286 bound above in
  check_taken "286 bound" got (Some (5, 0));
  let _, got = fault_of Cpu86.I80186 bound above in
  check_taken "186 bound" got (Some (5, 4))

(* What a 386 does in real mode that a 286 does not, each next to the 286
   result it replaces (SingleStepTests/80386 v1_ex_real_mode). *)
let test_386 () =
  let m386 = Cpu86.I80386 and m286 = Cpu86.I80286 in
  let t = cpu m386 detect in
  run t detect_steps;
  check "386: bits 12-15 read clear after clearing" (bx t land 0xF000) 0;
  check "386: IOPL and NT stick, bit 15 does not" (cx t land 0xF000) 0x7000;
  let t = cpu m386 "\x54\x58" in
  run t 2;
  check "386 push sp pushes the SP before the push" (ax t) 0x0100;
  let raised code =
    match Cpu86.step (cpu m386 code) with _ -> false | exception Cpu86.Unsupported _ -> true
  in
  check "386: 0x0F is not POP CS" (Bool.to_int (raised "\x0f\x06")) 1;
  (* mov fs,ax; mov bx,fs; then fs: mov al,[0010h] with DS on zeros *)
  let t = cpu m386 ("\x8e\xe0\x8c\xe3\x64\xa0\x10\x00" ^ String.make 8 '\x00' ^ "\x5a") in
  Cpu86.set_reg16 t 0 0;
  Cpu86.set_seg t 3 0x0100;
  run t 3;
  check "386: mov bx,fs" (bx t) 0;
  check "386: fs: reads through FS" (ax t land 0xff) 0x5a;
  let gs_read = "\x65\xa0\x10\x00" ^ String.make 12 '\x00' ^ "\x5a" in
  let t = cpu m386 gs_read in
  Cpu86.set_seg t 3 0x0100;
  Cpu86.set_seg t 4 0x0100;
  run t 1;
  check "386: gs: reads through GS" (ax t land 0xff) 0x5a;
  check "286: 65h is no prefix" (Bool.to_int (match Cpu86.step (cpu m286 gs_read) with
                                              | _ -> false | exception Cpu86.Unsupported _ -> true)) 1;
  let _, got = fault_of m386 "\x8e\xf0" (fun _ -> ()) in
  check_taken "386 mov sreg 6" got (Some (6, 0));
  (* 16- and 8-bit writes keep the upper half *)
  let t = cpu m386 "\xb8\xbc\x9a\xb4\x00" in
  Cpu86.set_reg32 t 0 0x12345678;
  run t 1;
  check "386: mov ax keeps EAX's upper half" (Cpu86.reg32 t 0) 0x12349abc;
  run t 1;
  check "386: mov ah keeps it too" (Cpu86.reg32 t 0) 0x123400bc;
  check "386: no snapshot form"
    (Bool.to_int (match Cpu86.save_state t with _ -> false | exception Invalid_argument _ -> true)) 1;
  let nothing _ = () and si_ffff t = Cpu86.set_reg16 t 6 0xFFFF in
  (* LOCK: lockable with memory runs, MOV or a register operand is INT 6 *)
  let _, got = fault_of m386 "\xf0\x00\x04" nothing in
  check_taken "386 lock add [si],al" got None;
  let _, got = fault_of m386 "\xf0\x88\x04" nothing in
  check_taken "386 lock mov [si],al" got (Some (6, 0));
  let _, got = fault_of m386 "\xf0\x80\x3c\x05" nothing in
  check_taken "386 lock cmp byte [si],5" got (Some (6, 0));
  let _, got = fault_of m386 "\xf0\x00\xd8" nothing in
  check_taken "386 lock add al,bl" got (Some (6, 0));
  let _, got = fault_of m286 "\xf0\x88\x04" nothing in
  check_taken "286 lock mov [si],al" got None;
  (* past FFFF through SS is INT 12, through DS INT 13 *)
  let sp_ffff t = Cpu86.set_reg16 t 4 0xFFFF in
  let _, got = fault_of m386 "\x58" sp_ffff in
  check_taken "386 pop ax at SP=FFFF" got (Some (12, 0));
  let _, got = fault_of m286 "\x58" sp_ffff in
  check_taken "286 pop ax at SP=FFFF" got (Some (13, 0));
  let bp_ffff t = Cpu86.set_reg16 t 5 0xFFFF in
  let _, got = fault_of m386 "\x01\x46\x00" bp_ffff in
  check_taken "386 add [bp+0],ax at FFFF" got (Some (12, 0));
  let _, got = fault_of m386 "\x2e\x01\x04" si_ffff in
  check_taken "386 cs: add [si],ax at FFFF" got (Some (13, 0));
  (* a faulting string step starts over: DI, SI and CX as before it *)
  let t, got =
    fault_of m386 "\xf3\xab" (fun t -> Cpu86.set_reg16 t 7 0xFFFF; Cpu86.set_reg16 t 1 3)
  in
  check_taken "386 rep stosw at FFFF" got (Some (13, 0));
  check "386 rep stosw DI" (Cpu86.reg16 t 7) 0xFFFF;
  check "386 rep stosw CX" (Cpu86.reg16 t 1) 3;
  let t, got = fault_of m386 "\xa5" (fun t -> Cpu86.set_reg16 t 7 0xFFFF) in
  check_taken "386 movsw at DI=FFFF" got (Some (13, 0));
  check "386 movsw leaves SI" (Cpu86.reg16 t 6) 0;
  (* POP r/m puts SP back; POPA keeps what it popped and puts SP back *)
  let t, _ = fault_of m386 "\x8f\x04" si_ffff in
  check "386 pop to FFFF puts SP back" (Cpu86.reg16 t 4) 0x0100;
  let popa_setup t =
    Cpu86.set_reg16 t 4 0xFFF9;
    List.iter (fun r -> Cpu86.set_reg16 t r 0x1111) [ 5; 6; 7 ]
  in
  let t, got = fault_of m386 "\x61" popa_setup in
  check_taken "386 popa at SP=FFF9" got (Some (12, 0));
  check "386 popa keeps the popped DI" (Cpu86.reg16 t 7) 0;
  check "386 popa puts SP back" (Cpu86.reg16 t 4) 0xFFF9;
  let t, got = fault_of m286 "\x61" popa_setup in
  check_taken "286 popa at SP=FFF9" got (Some (13, 0));
  check "286 popa puts DI back" (Cpu86.reg16 t 7) 0x1111;
  (* bound ax,[FFFDh], AX below the lower bound 0: the 386 still reads the
     upper bound at FFFF *)
  let low t = Cpu86.set_reg16 t 0 0x8000 in
  let _, got = fault_of m386 "\x62\x06\xfd\xff" low in
  check_taken "386 bound reads both" got (Some (13, 0));
  let _, got = fault_of m286 "\x62\x06\xfd\xff" low in
  check_taken "286 bound stops at the lower" got (Some (5, 0));
  (* HLT at CS:FFFF leaves EIP at 10000h; a NOP there makes the next
     fetch INT 13. CS F001 puts CS:FFFF at code byte 0Fh. *)
  let at_ffff byte model =
    let t = cpu model (String.make 0xF '\x00' ^ byte) in
    Cpu86.set_seg t 1 0xF001;
    Cpu86.set_ip t 0xFFFF;
    t
  in
  let t = at_ffff "\xf4" m386 in
  run t 1;
  check "386 hlt at FFFF: EIP" (Cpu86.dump_ip t) 0x10000;
  let t = at_ffff "\xf4" m286 in
  run t 1;
  check "286 hlt at FFFF: IP" (Cpu86.dump_ip t) 0;
  let t = at_ffff "\x90" m386 in
  run t 1;
  let taken = ref None in
  Cpu86.set_int_hook t (fun n -> taken := Some (n, Cpu86.dump_ip t));
  ignore (Cpu86.step t);
  check_taken "386 fetch past FFFF" !taken (Some (13, 0x10000));
  (* shl al,cl and shr al,cl by 16: the 386 sets CF as for 8 *)
  List.iter
    (fun (model, label, code, al, want) ->
      let t = cpu model code in
      Cpu86.set_reg16 t 0 al;
      Cpu86.set_reg16 t 1 16;
      run t 1;
      check (label ^ " by 16: CF") (Cpu86.flags t land Cpu86.f_carry) want)
    [ (m386, "386 shl al", "\xd2\xe0", 0x01, Cpu86.f_carry); (m286, "286 shl al", "\xd2\xe0", 0x01, 0);
      (m386, "386 shr al", "\xd2\xe8", 0x80, Cpu86.f_carry); (m286, "286 shr al", "\xd2\xe8", 0x80, 0) ];
  (* idiv bl: AX=81C1h by 7Ch overflows, but |AX|>>7 = 7Ch+80h and the low
     seven bits 3Fh are below 7Ch; B9DFh by 0Ch has 21h above 0Ch *)
  let idiv ax0 bl t = Cpu86.set_reg16 t 0 ax0; Cpu86.set_reg16 t 3 bl in
  List.iter
    (fun (model, label, ax_want, edge) ->
      let t, got = fault_of model "\xf6\xfb" (idiv 0x81c1 0x7c) in
      check_taken (label ^ " idiv edge") got edge;
      check (label ^ " idiv edge AX") (ax t) ax_want;
      let _, got = fault_of model "\xf6\xfb" (idiv 0xb9df 0x0c) in
      check_taken (label ^ " idiv past the edge") got (Some (0, 0));
      let _, got = fault_of model "\xf6\xfb" (idiv 0x81c1 0x84) in
      check_taken (label ^ " idiv edge with a positive quotient") got (Some (0, 0)))
    [ (m386, "386", 0xc180, None); (m286, "286", 0xc180, None) ];
  let _, got = fault_of Cpu86.I80186 "\xf6\xfb" (idiv 0x81c1 0x7c) in
  check_taken "186 idiv edge" got (Some (0, 2))

(* repz stops on a difference, repnz on a match: cmpsb over zeros *)
let test_rep_kind () =
  List.iter
    (fun (name, code, want_cx) ->
      let t = cpu Cpu86.I80286 code in
      Cpu86.set_seg t 0 0x2000;
      Cpu86.set_seg t 3 0x3000;
      Cpu86.set_reg16 t 1 3;
      run t 1;
      check (name ^ " CX") (cx t) want_cx)
    [ ("repz cmpsb over equal bytes", "\xf3\xa6", 0); ("repnz cmpsb over equal bytes", "\xf2\xa6", 2) ]

let () =
  test_rep_kind ();
  test_386 ();
  test_faults ();
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
