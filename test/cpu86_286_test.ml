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

let cpu_mem model code =
  let mem = Bytes.make (1024 * 1024) '\000' in
  String.iteri (fun i c -> Bytes.set mem i c) code;
  let read a = Char.code (Bytes.get mem (a land 0xfffff)) in
  let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff)) in
  let t = Cpu86.create ~model ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) () in
  Cpu86.set_seg t 1 0;
  Cpu86.set_ip t 0;
  Cpu86.set_seg t 2 0x1000;
  Cpu86.set_reg16 t 4 0x0100;
  (t, mem)

let cpu model code = fst (cpu_mem model code)

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

let nothing_386 _ = ()

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
  (* 0F 06 is CLTS on the 386: POP CS would move SP up by two *)
  let t = cpu m386 "\x0f\x06" in
  let sp_before = Cpu86.reg16 t 4 in
  run t 1;
  check "386: 0x0F is not POP CS" (Cpu86.reg16 t 4) sp_before;
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

(* 66h on the 386: the 32-bit width the prefix gives the instructions in
   80386.csv's 66 column and no other, and the places SingleStepTests/80386
   shows the 386 parting from a plain reading of the manual. The stack is
   1000:0100, so SS:x is byte 10000h + x. *)
let test_386_operand32 () =
  let m386 = Cpu86.I80386 in
  let r32 t n = Cpu86.reg32 t n in
  let stack = 0x10000 in
  let byte mem a = Char.code (Bytes.get mem a) in
  let dword mem a = byte mem a lor (byte mem (a + 1) lsl 8) lor (byte mem (a + 2) lsl 16)
                    lor (byte mem (a + 3) lsl 24) in
  let cf_zf = Cpu86.f_carry lor Cpu86.f_zero in
  (* mov eax,12345678h; add ebx,eax *)
  let t = cpu m386 "\x66\xb8\x78\x56\x34\x12\x66\x01\xc3" in
  Cpu86.set_reg32 t 3 0xedcba988;
  run t 2;
  check "o32 mov eax,imm32" (r32 t 0) 0x12345678;
  check "o32 add ebx,eax wraps at 2^32" (r32 t 3) 0;
  check "o32 add sets CF and ZF" (Cpu86.flags t land cf_zf) cf_zf;
  check "286: 66h is no prefix"
    (Bool.to_int (match Cpu86.step (cpu Cpu86.I80286 "\x66\x40") with
                  | _ -> false | exception Cpu86.Unsupported _ -> true)) 1;
  (* mov al,12h is a byte instruction: 66h leaves it one immediate byte *)
  let t = cpu m386 "\x66\xb0\x12\x66\x40" in
  Cpu86.set_reg32 t 0 0xffff0000;
  run t 2;
  check "o32 mov al,imm8 then inc eax" (r32 t 0) 0xffff0013;
  (* add ebx,-1 by 83 /0: the imm8 sign-extends to 32 bits *)
  let t = cpu m386 "\x66\x83\xc3\xff" in
  Cpu86.set_reg32 t 3 0x00010000;
  run t 1;
  check "o32 83 sign-extends" (r32 t 3) 0x0000ffff;
  (* mul ebx: FFFFFFFFh squared is FFFFFFFE_00000001h *)
  let t = cpu m386 "\x66\xf7\xe3" in
  Cpu86.set_reg32 t 0 0xffffffff;
  Cpu86.set_reg32 t 3 0xffffffff;
  run t 1;
  check "o32 mul EAX" (r32 t 0) 1;
  check "o32 mul EDX" (r32 t 2) 0xfffffffe;
  check "o32 mul CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  let divide edx eax ebx t =
    Cpu86.set_reg32 t 2 edx;
    Cpu86.set_reg32 t 0 eax;
    Cpu86.set_reg32 t 3 ebx
  in
  let t, got = fault_of m386 "\x66\xf7\xf3" (divide 1 5 2) in
  check_taken "o32 div 1_00000005h by 2" got None;
  check "o32 div quotient" (r32 t 0) 0x80000002;
  check "o32 div remainder" (r32 t 2) 1;
  let _, got = fault_of m386 "\x66\xf7\xf3" (divide 2 0 2) in
  check_taken "o32 div quotient past 32 bits" got (Some (0, 0));
  let t, got = fault_of m386 "\x66\xf7\xfb" (divide 0xffffffff 0xfffffff9 2) in
  check_taken "o32 idiv -7 by 2" got None;
  check "o32 idiv quotient" (r32 t 0) 0xfffffffd;
  check "o32 idiv remainder" (r32 t 2) 0xffffffff;
  (* imul ebx: -1 * -1 = 1 fits, so CF is clear *)
  let t = cpu m386 "\x66\xf7\xeb" in
  Cpu86.set_reg32 t 0 0xffffffff;
  Cpu86.set_reg32 t 3 0xffffffff;
  run t 1;
  check "o32 imul EDX:EAX" (r32 t 2 lxor r32 t 0) 1;
  check "o32 imul CF" (Cpu86.flags t land Cpu86.f_carry) 0;
  (* push eax; pop ebx: four bytes, SP by 4 *)
  let t, mem = cpu_mem m386 "\x66\x50\x66\x5b" in
  Cpu86.set_reg32 t 0 0xdeadbeef;
  run t 1;
  check "o32 push eax SP" (Cpu86.reg16 t 4) 0xfc;
  check "o32 push eax bytes" (dword mem (stack + 0xfc)) 0xdeadbeef;
  run t 1;
  check "o32 pop ebx" (r32 t 3) 0xdeadbeef;
  (* push es moves SP by 4 and writes one word (6606: two bytes written) *)
  let t, mem = cpu_mem m386 "\x66\x06" in
  Bytes.set mem (stack + 0xfe) '\x55';
  Bytes.set mem (stack + 0xff) '\x55';
  Cpu86.set_seg t 0 0x1234;
  run t 1;
  check "o32 push es SP" (Cpu86.reg16 t 4) 0xfc;
  check "o32 push es leaves the upper word" (dword mem (stack + 0xfc)) 0x55551234;
  (* pop es at SP=FFFE reads one word: no INT 12 (6607) *)
  let t, got = fault_of m386 "\x66\x07" (fun t -> Cpu86.set_reg16 t 4 0xfffe) in
  check_taken "o32 pop es at FFFE" got None;
  check "o32 pop es SP" (Cpu86.reg16 t 4) 2;
  (* pushad at SP=1Ah writes EDI first at FFFA, then INT 12 on ESI's FFFE;
     SP stays and EDI stays written (6660) *)
  let t, mem = cpu_mem m386 "\x66\x60" in
  let taken = ref None in
  Cpu86.set_int_hook t (fun n -> if !taken = None then taken := Some (n, Cpu86.dump_ip t));
  Cpu86.set_reg16 t 4 0x1a;
  Cpu86.set_reg32 t 7 0x7f3e2ee6;
  ignore (Cpu86.step t);
  check_taken "o32 pushad across the segment end" !taken (Some (12, 0));
  check "o32 pushad SP after the fault" (Cpu86.reg16 t 4) 0x1a;
  check "o32 pushad wrote EDI first" (dword mem (stack + 0xfffa)) 0x7f3e2ee6;
  check "o32 pushad did not write EAX" (dword mem (stack + 0x16)) 0;
  (* popad takes ESP's upper half from its slot (6661) *)
  let t, mem = cpu_mem m386 "\x66\x61" in
  Bytes.set mem (stack + 0x100 + 14) '\xcd';
  Bytes.set mem (stack + 0x100 + 15) '\xab';
  run t 1;
  check "o32 popad ESP" (r32 t 4) 0xabcd0120;
  (* jmp rel32: EIP past FFFFh is INT 13 from the prefix; back to 0 runs *)
  let _, got = fault_of m386 "\x66\xe9\x00\x00\x01\x00" nothing_386 in
  check_taken "o32 jmp past the CS limit" got (Some (13, 0));
  let t, got = fault_of m386 "\x66\xe9\xfa\xff\xff\xff" nothing_386 in
  check_taken "o32 jmp back" got None;
  check "o32 jmp back IP" (Cpu86.dump_ip t) 0;
  (* cwde; cdq *)
  let t = cpu m386 "\x66\x98\x66\x99" in
  Cpu86.set_reg32 t 0 0x12348000;
  run t 2;
  check "o32 cwde" (r32 t 0) 0xffff8000;
  check "o32 cdq" (r32 t 2) 0xffffffff;
  (* movsd: four bytes, SI and DI by 4 *)
  let t, mem = cpu_mem m386 "\x66\xa5" in
  Cpu86.set_seg t 0 0x2000;
  Cpu86.set_seg t 3 0x3000;
  Bytes.blit_string "\x01\x02\x03\x04" 0 mem 0x30000 4;
  run t 1;
  check "o32 movsd" (dword mem 0x20000) 0x04030201;
  check "o32 movsd SI" (Cpu86.reg16 t 6) 4;
  (* shl eax,4 by C1 *)
  let t = cpu m386 "\x66\xc1\xe0\x04" in
  Cpu86.set_reg32 t 0 0x18000001;
  run t 1;
  check "o32 shl eax,4" (r32 t 0) 0x80000010;
  check "o32 shl CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  (* cdq on a positive EAX whose bit 15 is set *)
  let t = cpu m386 "\x66\x99" in
  Cpu86.set_reg32 t 0 0x00008000;
  Cpu86.set_reg32 t 2 0xffffffff;
  run t 1;
  check "o32 cdq of 8000h" (r32 t 2) 0;
  (* idiv by a negative divisor: 7 / -2 = -3 r 1 *)
  let t, got = fault_of m386 "\x66\xf7\xfb" (divide 0 7 0xfffffffe) in
  check_taken "o32 idiv by -2" got None;
  check "o32 idiv by -2 quotient" (r32 t 0) 0xfffffffd;
  check "o32 idiv by -2 remainder" (r32 t 2) 1;
  (* a 32-bit shift count is five bits: shl eax,33 is shl eax,1 *)
  let t = cpu m386 "\x66\xc1\xe0\x21" in
  Cpu86.set_reg32 t 0 0x40000001;
  run t 1;
  check "o32 shl eax,33" (r32 t 0) 0x80000002;
  (* prefixes in any order and repeated: 66 66, 66 then FS, 66 then REP *)
  let t = cpu m386 "\x66\x66\xb8\x78\x56\x34\x12" in
  run t 1;
  check "o32 66 66 mov eax" (r32 t 0) 0x12345678;
  let t, mem = cpu_mem m386 "\x66\x64\xa1\x10\x00" in
  Cpu86.set_seg t 4 0x2000;
  Bytes.blit_string "\x11\x22\x33\x44" 0 mem 0x20010 4;
  run t 1;
  check "o32 fs: mov eax,[10h]" (r32 t 0) 0x44332211;
  let t, mem = cpu_mem m386 "\x66\xf3\xab" in
  Cpu86.set_seg t 0 0x2000;
  Cpu86.set_reg16 t 1 2;
  Cpu86.set_reg32 t 0 0xa1b2c3d4;
  run t 1;
  check "o32 rep stosd CX" (cx t) 0;
  check "o32 rep stosd second dword" (dword mem 0x20004) 0xa1b2c3d4;
  let _, got = fault_of m386 "\x66\xf0\x89\x04" nothing_386 in
  check_taken "o32 66 lock mov [si],eax" got (Some (6, 0));
  (* lodsd at SI=FFFEh crosses the segment end: INT 13 *)
  let _, got = fault_of m386 "\x66\xad" (fun t -> Cpu86.set_reg16 t 6 0xfffe) in
  check_taken "o32 lodsd at FFFE" got (Some (13, 0));
  (* enter with a display whose read crosses: EBP comes back whole (66C8) *)
  let t, got = fault_of m386 "\x66\xc8\x00\x00\x02" (fun t -> Cpu86.set_reg32 t 5 0x00010001) in
  check_taken "o32 enter display read past FFFF" got (Some (12, 0));
  check "o32 enter fault keeps EBP" (r32 t 5) 0x00010001;
  check "o32 enter fault keeps SP" (Cpu86.reg16 t 4) 0x100;
  (* call far ptr16:32 pushes CS and EIP as dwords *)
  let t, mem = cpu_mem m386 "\x66\x9a\x34\x12\x00\x00\x00\x20" in
  Cpu86.set_seg t 1 0;
  run t 1;
  check "o32 call far SP" (Cpu86.reg16 t 4) 0xf8;
  check "o32 call far pushed EIP" (dword mem (stack + 0xf8)) 8;
  check "o32 call far pushed CS" (dword mem (stack + 0xfc)) 0;
  check "o32 call far CS:IP" ((Cpu86.seg t 1 lsl 16) lor Cpu86.dump_ip t) 0x20001234;
  (* loop counts CX down first: CX=1 falls through *)
  let t = cpu m386 "\x66\xe2\xfd" in
  Cpu86.set_reg16 t 1 1;
  run t 1;
  check "o32 loop at CX=1 falls through" (Cpu86.dump_ip t) 3;
  (* mov eax,es clears EAX's upper half (668C) *)
  let t = cpu m386 "\x66\x8c\xc0" in
  Cpu86.set_reg32 t 0 0xffffffff;
  Cpu86.set_seg t 0 0x1234;
  run t 1;
  check "o32 mov eax,es" (r32 t 0) 0x1234;
  (* imul ebx: 10000h squared does not fit 32 bits *)
  let t = cpu m386 "\x66\xf7\xeb" in
  Cpu86.set_reg32 t 0 0x10000;
  Cpu86.set_reg32 t 3 0x10000;
  run t 1;
  check "o32 imul overflow EDX" (r32 t 2) 1;
  check "o32 imul overflow CF" (Cpu86.flags t land Cpu86.f_carry) Cpu86.f_carry;
  (* LOCK with 66h: add [si],eax runs, mov [si],eax is INT 6 *)
  let _, got = fault_of m386 "\xf0\x66\x01\x04" nothing_386 in
  check_taken "o32 lock add [si],eax" got None;
  let _, got = fault_of m386 "\xf0\x66\x89\x04" nothing_386 in
  check_taken "o32 lock mov [si],eax" got (Some (6, 0))

(* 67h: 32-bit offsets inside the 386's real-mode segments (SingleStepTests
   80386, the 67 files). Words at DS:0100h-01FEh hold their own offset; SS
   (1000h) and ES (2000h) hold the offset xor a tag, so a wrong segment or a
   wrong offset reads a different word. *)
let test_386_address32 () =
  let m386 = Cpu86.I80386 in
  let r32 t n = Cpu86.reg32 t n in
  let ss_tag = 0x5000 and es_tag = 0x6000 and es_seg = 0x2000 in
  let poke mem a v = Bytes.set mem a (Char.chr (v land 0xff)) in
  let poke16 mem a v = poke mem a v; poke mem (a + 1) (v lsr 8) in
  let byte mem a = Char.code (Bytes.get mem a) in
  let word mem a = byte mem a lor (byte mem (a + 1) lsl 8) in
  let fill mem =
    for k = 0 to 0x7f do
      let off = 0x100 + (2 * k) in
      poke16 mem off off;
      poke16 mem (0x10000 + off) (off lxor ss_tag);
      poke16 mem ((es_seg lsl 4) + off) (off lxor es_tag)
    done
  in
  let regs rs t = List.iter (fun (n, v) -> Cpu86.set_reg32 t n v) rs in
  let ax_after name code setup want =
    let t, mem = cpu_mem m386 code in
    fill mem;
    Cpu86.set_seg t 0 es_seg;
    setup t;
    run t 1;
    check ("a32 " ^ name) (ax t) want
  in
  ax_after "mov ax,[eax+ebx]" "\x67\x8b\x04\x18" (regs [ (0, 0x100); (3, 0x20) ]) 0x120;
  ax_after "mov ax,[eax+ebx*2]" "\x67\x8b\x04\x58" (regs [ (0, 0x100); (3, 0x10) ]) 0x120;
  ax_after "SIB without index scales the base" "\x67\x8b\x04\xe0" (regs [ (0, 0x24) ]) 0x120;
  ax_after "SIB disp32 only is not scaled" "\x67\x8b\x04\xe5\x20\x01\x00\x00" nothing_386 0x120;
  ax_after "mov ax,[120h]" "\x67\x8b\x05\x20\x01\x00\x00" nothing_386 0x120;
  ax_after "SIB [ebx+disp32]" "\x67\x8b\x04\x1d\x00\x01\x00\x00" (regs [ (3, 0x20) ]) 0x120;
  ax_after "disp8 sign-extends" "\x67\x8b\x40\xf0" (regs [ (0, 0x130) ]) 0x120;
  ax_after "offset wraps at 2^32" "\x67\x8b\x80\x20\x01\xff\xff" (regs [ (0, 0x10000) ]) 0x120;
  ax_after "[ebp+20h] is SS" "\x67\x8b\x45\x20" (regs [ (5, 0x100) ]) (0x120 lxor ss_tag);
  ax_after "[esp] is SS" "\x67\x8b\x04\x24" nothing_386 (0x100 lxor ss_tag);
  ax_after "[esp+ebx] is SS" "\x67\x8b\x04\x1c" (regs [ (3, 0x20) ]) (0x120 lxor ss_tag);
  ax_after "SIB [ebp+20h] is SS" "\x67\x8b\x44\x25\x20" (regs [ (5, 0x100) ]) (0x120 lxor ss_tag);
  ax_after "[ebp+ebx+20h] is SS" "\x67\x8b\x44\x1d\x20" (regs [ (5, 0xf0); (3, 0x10) ]) (0x120 lxor ss_tag);
  ax_after "index EBP stays DS" "\x67\x8b\x04\x2b" (regs [ (3, 0x100); (5, 0x20) ]) 0x120;
  ax_after "es: before 67h" "\x26\x67\x8b\x00" (regs [ (0, 0x120) ]) (0x120 lxor es_tag);
  ax_after "es: after 67h" "\x67\x26\x8b\x00" (regs [ (0, 0x120) ]) (0x120 lxor es_tag);
  ax_after "lea ax keeps the low word" "\x67\x8d\x04\x58" (regs [ (0, 0x12345); (3, 0x10) ]) 0x2365;
  let t, _ = cpu_mem m386 "\x66\x67\x8d\x04\x58" in
  regs [ (0, 0x12345); (3, 0x10) ] t;
  run t 1;
  check "a32 lea eax" (r32 t 0) 0x12365;
  let t, mem = cpu_mem m386 "\x66\x67\x8b\x04\x18" in
  fill mem;
  regs [ (0, 0x100); (3, 0x20) ] t;
  run t 1;
  check "a32 mov eax,[eax+ebx]" (r32 t 0) 0x01220120;
  (* mov ax,[120h] by moffs32, then inc ax: the 4 offset bytes are skipped *)
  let t, mem = cpu_mem m386 "\x67\xa1\x20\x01\x00\x00\x40" in
  fill mem;
  run t 2;
  check "a32 moffs32" (ax t) 0x121;
  (* the end byte past FFFFh faults: INT 13, or INT 12 by SS *)
  let mov_ax_eax = "\x67\x8b\x00" in
  let _, got = fault_of m386 mov_ax_eax (regs [ (0, 0x10120) ]) in
  check_taken "a32 [eax] past FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 mov_ax_eax (regs [ (0, 0xffff) ]) in
  check_taken "a32 word at FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\x8a\x00" (regs [ (0, 0xffff) ]) in
  check_taken "a32 byte at FFFFh" got None;
  let _, got = fault_of m386 "\x66\x67\x8b\x00" (regs [ (0, 0x10000) ]) in
  check_taken "a32 dword at 10000h" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\xac" (regs [ (6, 0x10000) ]) in
  check_taken "a32 lodsb at ESI 10000h" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\x8b\x45\x00" (regs [ (5, 0x10000) ]) in
  check_taken "a32 [ebp] past FFFFh" got (Some (12, 0));
  (* a far pointer or BOUND pair must fit whole: the second half does not wrap to 0 *)
  let _, got = fault_of m386 "\x67\xc4\x00" (regs [ (0, 0xfffe) ]) in
  check_taken "a32 les across FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\x62\x00" (regs [ (0, 0xfffe) ]) in
  check_taken "a32 bound across FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\xff\x18" (regs [ (0, 0xfffe) ]) in
  check_taken "a32 call far m16:16 across FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\xff\x28" (regs [ (0, 0xfffe) ]) in
  check_taken "a32 jmp far m16:16 across FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x66\x67\xff\x18" (regs [ (0, 0xfffc) ]) in
  check_taken "a32 call far m16:32 across FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x66\x67\x62\x00" (regs [ (0, 0xfffc) ]) in
  check_taken "a32 bound32 across FFFFh" got (Some (13, 0));
  (* movsw: the read moves ESI past FFFFh, the write faults, ESI comes back whole *)
  let t, got = fault_of m386 "\x67\xa5" (regs [ (6, 0xfffe); (7, 0xffff) ]) in
  check_taken "a32 movsw write past FFFFh" got (Some (13, 0));
  check "a32 movsw restarts ESI" (r32 t 6) 0xfffe;
  (* strings walk ESI/EDI and count ECX without a 16-bit wrap *)
  let t = cpu m386 "\x67\xac" in
  regs [ (6, 0xffff) ] t;
  run t 1;
  check "a32 lodsb ESI" (r32 t 6) 0x10000;
  let t = cpu m386 "\x67\xf3\xaa" in
  Cpu86.set_seg t 0 0x3000;
  regs [ (1, 0x10000); (7, 0) ] t;
  run t 1;
  check "a32 rep stosb ECX" (r32 t 1) 0;
  check "a32 rep stosb EDI" (r32 t 7) 0x10000;
  let t = cpu m386 "\x67\xe2\xfd" in
  regs [ (1, 0x10000) ] t;
  run t 1;
  check "a32 loop ECX" (r32 t 1) 0xffff;
  check "a32 loop jumps" (Cpu86.dump_ip t) 0;
  let t = cpu m386 "\x67\xe3\x02" in
  regs [ (1, 0x10000) ] t;
  run t 1;
  check "a32 jecxz on ECX" (Cpu86.dump_ip t) 3;
  let t = cpu m386 "\x66\x67\xe2\xfc" in
  regs [ (1, 0x10000) ] t;
  run t 1;
  check "a32 loopd ECX" (r32 t 1) 0xffff;
  let t = cpu m386 "\x66\x67\xe3\x02" in
  regs [ (1, 0x10000) ] t;
  run t 1;
  check "a32 jecxz with 66h" (Cpu86.dump_ip t) 4;
  let t, mem = cpu_mem m386 "\x26\x67\xac" in
  fill mem;
  Cpu86.set_seg t 0 es_seg;
  regs [ (6, 0x121) ] t;
  run t 1;
  check "a32 es: lodsb" (ax t land 0xff) ((0x120 lxor es_tag) lsr 8);
  let t, mem = cpu_mem m386 "\x67\xd7" in
  fill mem;
  regs [ (3, 0x100); (0, 0x20) ] t;
  run t 1;
  check "a32 xlat [ebx+al]" (ax t land 0xff) 0x20;
  let _, got = fault_of m386 "\x67\xd7" (regs [ (3, 0x10000) ]) in
  check_taken "a32 xlat EBX past FFFFh" got (Some (13, 0));
  (* pop [esp+2] counts from SP after the pop *)
  let t, mem = cpu_mem m386 "\x67\x8f\x44\x24\x02" in
  poke16 mem 0x10100 0xbeef;
  run t 1;
  check "a32 pop [esp+2] SP" (Cpu86.reg16 t 4) 0x102;
  check "a32 pop [esp+2] target" (word mem 0x10104) 0xbeef;
  let t, mem = cpu_mem m386 "\x66\x67\x8f\x44\x24\x02" in
  poke16 mem 0x10100 0xbeef;
  poke16 mem 0x10102 0xdead;
  run t 1;
  check "a32 pop dword [esp+2] target" (word mem 0x10106 lor (word mem 0x10108 lsl 16)) 0xdeadbeef;
  let t, got = fault_of m386 "\x67\x8f\x00" (regs [ (0, 0x10000) ]) in
  check_taken "a32 pop [eax] past FFFFh" got (Some (13, 0));
  check "a32 pop fault keeps SP" (Cpu86.reg16 t 4) 0x100;
  check "286: 67h is no prefix"
    (Bool.to_int (match Cpu86.step (cpu Cpu86.I80286 "\x67\x8b\x00") with
                  | _ -> false | exception Cpu86.Unsupported _ -> true)) 1

(* The 386's two-byte forms with 16-bit operands (SingleStepTests/80386 0F80-0F9F,
   0FA0/A1/A8/A9, 0FB6/B7/BE/BF and their 67h forms). *)
let test_386_0f () =
  let m386 = Cpu86.I80386 in
  let zf = Cpu86.f_zero in
  let ip_after code setup =
    let t = cpu m386 code in
    setup t;
    run t 1;
    Cpu86.dump_ip t
  in
  let with_zf t = Cpu86.set_flags t (Cpu86.flags t lor zf) in
  check "0f jz rel16 taken" (ip_after "\x0f\x84\x00\x10" with_zf) 0x1004;
  check "0f jz rel16 not taken" (ip_after "\x0f\x84\x00\x10" nothing_386) 4;
  check "0f jz rel16 wraps at 16 bits" (ip_after "\x0f\x84\xfd\xff" with_zf) 1;
  let t, mem = cpu_mem m386 "\x0f\x94\x06\x00\x01\x0f\x95\xc3" in
  Bytes.set mem 0x100 '\x55';
  with_zf t;
  Cpu86.set_reg16 t 3 0xffff;
  run t 2;
  check "0f setz [100h]" (Char.code (Bytes.get mem 0x100)) 1;
  check "0f setnz bl, upper byte stays" (bx t) 0xff00;
  let t, mem = cpu_mem m386 "\x0f\xb6\x06\x00\x01\x0f\xbe\x1e\x00\x01\x0f\xb7\xc9\x0f\xbf\xd1" in
  Bytes.set mem 0x100 '\x80';
  Cpu86.set_reg32 t 0 0x12345678;
  Cpu86.set_reg32 t 1 0xabcd1234;
  run t 4;
  check "0f movzx ax keeps EAX's upper half" (Cpu86.reg32 t 0) 0x12340080;
  check "0f movsx bx" (bx t) 0xff80;
  check "0f movzx cx,cx" (cx t) 0x1234;
  check "0f movsx dx,cx" (Cpu86.reg16 t 2) 0x1234;
  let t, mem = cpu_mem m386 "\x0f\xa0\x0f\xa9" in
  Cpu86.set_seg t 4 0x1234;
  Cpu86.set_seg t 5 0;
  run t 2;
  check "0f push fs writes FS" (Char.code (Bytes.get mem 0x100fe) lor (Char.code (Bytes.get mem 0x100ff) lsl 8)) 0x1234;
  check "0f pop gs" (Cpu86.seg t 5) 0x1234;
  check "0f push/pop SP" (Cpu86.reg16 t 4) 0x100;
  let _, got = fault_of m386 "\xf0\x0f\x84\x00\x00" nothing_386 in
  check_taken "0f lock jz is INT 6" got (Some (6, 0));
  let raised code =
    match Cpu86.step (cpu m386 code) with
    | _ -> false
    | exception Cpu86.Unsupported _ -> true
  in
  check "0f with 66h is not there yet" (Bool.to_int (raised "\x66\x0f\xb6\xc0")) 1;
  check "286: 0f 84 is not there" (Bool.to_int (match Cpu86.step (cpu Cpu86.I80286 "\x0f\x84\x00\x00") with
                                                | _ -> false | exception Cpu86.Unsupported _ -> true)) 1

(* The rest of the 386's 0Fh map with 16-bit operands: BT family, SHLD/SHRD,
   IMUL r16,r/m16, BSF/BSR, LSS/LFS/LGS and CLTS (SingleStepTests/80386 0FA3-0FBD,
   0F06 and their 67h forms). Words at DS:0100h. *)
let test_386_0f_bits () =
  let m386 = Cpu86.I80386 in
  let cf = Cpu86.f_carry and zf = Cpu86.f_zero in
  let word mem a = Char.code (Bytes.get mem a) lor (Char.code (Bytes.get mem (a + 1)) lsl 8) in
  let poke16 mem a v =
    Bytes.set mem a (Char.chr (v land 0xff));
    Bytes.set mem (a + 1) (Char.chr (v lsr 8))
  in
  let run_with code setup =
    let t, mem = cpu_mem m386 code in
    poke16 mem 0x100 0x0001;
    poke16 mem 0x102 0x8000;
    setup t;
    run t 1;
    (t, mem)
  in
  let regs rs t = List.iter (fun (n, v) -> Cpu86.set_reg16 t n v) rs in
  let carry t = Bool.to_int (Cpu86.flags t land cf <> 0) in
  let t, _ = run_with "\x0f\xa3\xc8" (regs [ (0, 0x0002); (1, 17) ]) in
  check "bt ax,cx takes the number mod 16" (carry t) 1;
  let t, _ = run_with "\x0f\xa3\x06\x00\x01" (regs [ (0, 31) ]) in
  check "bt [100h],ax moves a word up" (carry t) 1;
  let t, _ = run_with "\x0f\xa3\x06\x02\x01" (regs [ (0, 0xfff0) ]) in
  check "bt [102h],ax with -16 reads bit 0 of [100h]" (carry t) 1;
  let _, mem = run_with "\x0f\xab\x06\x00\x01" (regs [ (0, 4) ]) in
  check "bts [100h],ax" (word mem 0x100) 0x0011;
  let _, mem = run_with "\x0f\xb3\x06\x00\x01" (regs [ (0, 0) ]) in
  check "btr [100h],ax" (word mem 0x100) 0;
  let _, mem = run_with "\x0f\xbb\x06\x00\x01" (regs [ (0, 16 + 15) ]) in
  check "btc [100h],ax lands in [102h]" (word mem 0x102) 0;
  let t, _ = run_with "\x0f\xba\x26\x02\x01\x1f" nothing_386 in
  check "bt [102h],1Fh takes the immediate mod 16" (carry t) 1;
  let _, mem = run_with "\x0f\xba\x26\x00\x01\x01" nothing_386 in
  check "bt [100h],1 leaves the word" (word mem 0x100) 1;
  let _, got = fault_of m386 "\x0f\xba\x06\x00\x01\x00" nothing_386 in
  check_taken "0f ba /0 is INT 6" got (Some (6, 0));
  let lock_fault code = snd (fault_of m386 code nothing_386) in
  check_taken "lock bts [mem] runs" (lock_fault "\xf0\x0f\xab\x06\x00\x01") None;
  check_taken "lock btr [mem] runs" (lock_fault "\xf0\x0f\xb3\x06\x00\x01") None;
  check_taken "lock bts [mem],imm runs" (lock_fault "\xf0\x0f\xba\x2e\x00\x01\x01") None;
  check_taken "lock bt [mem] is INT 6" (lock_fault "\xf0\x0f\xa3\x06\x00\x01") (Some (6, 0));
  check_taken "lock bt [mem],imm is INT 6" (lock_fault "\xf0\x0f\xba\x26\x00\x01\x01") (Some (6, 0));
  check_taken "lock bts reg is INT 6" (lock_fault "\xf0\x0f\xab\xc8") (Some (6, 0));
  let t, _ = run_with "\x0f\xa4\xd8\x04" (regs [ (0, 0x1234); (3, 0xabcd) ]) in
  check "shld ax,bx,4" (ax t) 0x234a;
  check "shld ax,bx,4 CF" (carry t) 1;
  (* SDM: a 1-bit shift sets OF on a sign change (the data masks OF) *)
  let t, _ = run_with "\x0f\xa4\xd8\x01" (regs [ (0, 0x4000); (3, 0x8000) ]) in
  check "shld ax,bx,1 OF" (Bool.to_int (Cpu86.flags t land Cpu86.f_overflow <> 0)) 1;
  let t, _ = run_with "\x0f\xa5\xd8" (regs [ (0, 0x1234); (3, 0xabcd); (1, 0x24) ]) in
  check "shld ax,bx,cl takes CL mod 32" (ax t) 0x234a;
  let t, _ = run_with "\x0f\xa4\xd8\x12" (regs [ (0, 0); (3, 0x4088) ]) in
  check "shld ax,bx,18 rotates BX by 2" (ax t) 0x0221;
  check "shld ax,bx,18 CF" (carry t) 1;
  let t, _ = run_with "\x0f\xac\xd8\x04" (regs [ (0, 0x1234); (3, 0xabcd) ]) in
  check "shrd ax,bx,4" (ax t) 0xd123;
  check "shrd ax,bx,4 CF" (carry t) 0;
  let t, _ = run_with "\x0f\xac\xd8\x12" (regs [ (0, 0xffff); (3, 0x4089) ]) in
  check "shrd ax,bx,18 rotates BX by 2" (ax t) 0x5022;
  check "shrd ax,bx,18 CF" (carry t) 0;
  let t, _ = run_with "\x0f\xa4\xd8\x00" (regs [ (0, 0x1234); (3, 0xabcd) ]) in
  check "shld by 0 keeps AX" (ax t) 0x1234;
  let t, _ =
    run_with "\x0f\xa4\xd8\x00" (fun t ->
        regs [ (0, 0x1234); (3, 0xabcd) ] t;
        Cpu86.set_flags t (Cpu86.flags t lor cf lor zf))
  in
  check "shld by 0 keeps CF and ZF" (Cpu86.flags t land (cf lor zf)) (cf lor zf);
  let _, got = fault_of m386 "\x0f\xa4\x3d\x00" (regs [ (7, 0xffff) ]) in
  check_taken "shld [di],di,0 still reads the word at FFFFh" got (Some (13, 0));
  let t, _ = run_with "\x0f\xaf\xca" (regs [ (1, 0x100); (2, 0x100) ]) in
  check "imul cx,dx low word" (cx t) 0;
  check "imul cx,dx overflow" (carry t) 1;
  check "imul cx,dx OF" (Bool.to_int (Cpu86.flags t land Cpu86.f_overflow <> 0)) 1;
  let t, _ = run_with "\x6b\xc1\xfe" (regs [ (1, 3) ]) in
  check "6b imul ax,cx,-2 shares imul16" (ax t) 0xfffa;
  let t, _ = run_with "\x0f\xaf\xca" (regs [ (1, 0xffff); (2, 2) ]) in
  check "imul -1 by 2" (cx t) 0xfffe;
  check "imul -1 by 2 fits" (carry t) 0;
  let t, _ = run_with "\x0f\xaf\xca" (regs [ (1, 0x8000); (2, 1) ]) in
  check "imul -8000h by 1 fits" (carry t) 0;
  let t, _ = run_with "\x0f\xbc\xc3" (regs [ (3, 0x0050) ]) in
  check "bsf ax,bx" (ax t) 4;
  let t, _ = run_with "\x0f\xbd\xc3" (regs [ (3, 0x0050) ]) in
  check "bsr ax,bx" (ax t) 6;
  let t, _ = run_with "\x0f\xbc\xc3" (regs [ (0, 0x1234); (3, 0) ]) in
  check "bsf of 0 sets ZF" (Bool.to_int (Cpu86.flags t land zf <> 0)) 1;
  check "bsf of 0 keeps AX" (ax t) 0x1234;
  let t, _ =
    run_with "\x0f\xbc\xc3" (fun t ->
        regs [ (3, 1) ] t;
        Cpu86.set_flags t (Cpu86.flags t lor zf))
  in
  check "bsf of 1 clears ZF" (Bool.to_int (Cpu86.flags t land zf <> 0)) 0;
  let t, _ = run_with "\x0f\xb4\x06\x00\x01" nothing_386 in
  check "lfs ax,[100h] AX" (ax t) 1;
  check "lfs ax,[100h] FS" (Cpu86.seg t 4) 0x8000;
  let t, _ = run_with "\x0f\xb5\x06\x00\x01" nothing_386 in
  check "lgs GS" (Cpu86.seg t 5) 0x8000;
  let t, _ = run_with "\x0f\xb2\x06\x00\x01" nothing_386 in
  check "lss SS" (Cpu86.seg t 2) 0x8000;
  let _, got = fault_of m386 "\x0f\xb2\xc3" nothing_386 in
  check_taken "lss ax,bx is INT 6" got (Some (6, 0));
  (* 16-bit addresses wrap the second word to offset 0 (0FB4, C4, C5: the one
     test each at EA FFFEh runs); a word at FFFFh is INT 13 *)
  let t, _ = run_with "\x0f\xb4\x07" (regs [ (3, 0xfffe) ]) in
  check "lfs ax,[bx] at FFFEh takes FS from offset 0" (Cpu86.seg t 4) 0xb40f;
  let _, got = fault_of m386 "\x0f\xb4\x07" (regs [ (3, 0xfffd) ]) in
  check_taken "lfs ax,[bx] at FFFDh reads a word at FFFFh" got (Some (13, 0));
  let _, got = fault_of m386 "\x67\x0f\xb4\x03" (regs [ (3, 0xfffe) ]) in
  check_taken "a32 lfs across FFFFh" got (Some (13, 0));
  let t, _ = run_with "\x0f\x06" nothing_386 in
  check "clts is two bytes" (Cpu86.dump_ip t) 2

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
  test_386_operand32 ();
  test_386_address32 ();
  test_386_0f ();
  test_386_0f_bits ();
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
