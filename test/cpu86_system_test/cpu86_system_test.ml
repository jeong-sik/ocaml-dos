(* The 80386's system registers in real mode (RFC 0005 stage 3a): LGDT and
   LIDT, SMSW and LMSW, CLTS, MOV to and from CR0, CR2, CR3, DR0-DR7 and
   TR6, TR7, and what CR0.EM and TS do to ESC and WAIT. Writes that would turn
   on paging over tables that are not the first-megabyte identity map, or a
   breakpoint, stop the core with Unsupported and change nothing. Code at
   0000:0000, stack at 1000:0100, DS = 1000h after [set_ds]. Entering
   protected mode is in cpu86_protected_test. *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %x want %x\n%!" name got want
  end

let m386 = Cpu86.I80386

let cpu_mem code =
  let mem = Bytes.make (1024 * 1024) '\000' in
  String.iteri (fun i c -> Bytes.set mem i c) code;
  let read a = Char.code (Bytes.get mem (a land 0xfffff)) in
  let write a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff)) in
  let t =
    Cpu86.create ~model:m386 ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) ()
  in
  Cpu86.set_seg t 1 0;
  Cpu86.set_ip t 0;
  Cpu86.set_seg t 2 0x1000;
  Cpu86.set_reg16 t 4 0x0100;
  (t, mem)

let cpu code = fst (cpu_mem code)
let run t n = for _ = 1 to n do ignore (Cpu86.step t) done
let eax t = Cpu86.reg32 t 0
let ebx t = Cpu86.reg32 t 3
let ecx t = Cpu86.reg32 t 1
let esi t = Cpu86.reg32 t 6

(* [mov ax,1000h; mov ds,ax] *)
let set_ds = "\xb8\x00\x10\x8e\xd8"

(* [mov eax, imm32] *)
let mov_eax v =
  "\x66\xb8" ^ String.init 4 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

let sys t =
  match (Cpu86.save_state t).Cpu86.saved_386 with
  | Some x -> x
  | None -> failwith "an 80386 saves its system registers"

(* The first interrupt taken in [n] steps and the IP it was taken from. *)
let taken_in t n =
  let taken = ref None in
  Cpu86.set_int_hook t (fun v -> if !taken = None then taken := Some (v, Cpu86.dump_ip t));
  run t n;
  !taken

let check_taken name got want =
  if got <> want then begin
    incr failed;
    let show = function None -> "none" | Some (v, ip) -> Printf.sprintf "INT %d at %04x" v ip in
    Printf.eprintf "FAIL %s: got %s want %s\n%!" name (show got) (show want)
  end

(* The IP of the instruction the step stopped at, or None when it ran. *)
let stopped_at t =
  match Cpu86.step t with
  | _ -> None
  | exception Cpu86.Unsupported _ -> Some (Cpu86.dump_ip t)

let check_stopped name got want =
  if got <> want then begin
    incr failed;
    let show = function None -> "ran" | Some ip -> Printf.sprintf "Unsupported at %04x" ip in
    Printf.eprintf "FAIL %s: got %s want %s\n%!" name (show got) (show want)
  end

let bytes_at mem a n = List.init n (fun i -> Char.code (Bytes.get mem (a + i)))

(* LGDT and LIDT load limit and base; SGDT and SIDT read them back. A 16-bit
   operand loads a 24-bit base, a 66h operand the whole 32 bits. *)
let test_tables () =
  let image = "\x34\x12\x78\x56\x34\x12" in
  let load_store lgdt_code sgdt_code =
    let t, mem = cpu_mem (set_ds ^ lgdt_code ^ sgdt_code) in
    String.iteri (fun i c -> Bytes.set mem (0x10200 + i) c) image;
    run t 4;
    (t, bytes_at mem 0x10210 6)
  in
  let lgdt = "\x0f\x01\x16\x00\x02" and sgdt = "\x0f\x01\x06\x10\x02" in
  let t, stored = load_store lgdt sgdt in
  check "LGDT 16-bit: base" (sys t).Cpu86.saved_gdtr_base 0x345678;
  check "LGDT 16-bit: limit" (sys t).Cpu86.saved_gdtr_limit 0x1234;
  check "SGDT after LGDT 16-bit" (List.fold_left (fun a b -> (a lsl 8) lor b) 0 (List.rev stored))
    0x003456781234;
  let t, stored = load_store ("\x66" ^ lgdt) sgdt in
  check "LGDT 32-bit: base" (sys t).Cpu86.saved_gdtr_base 0x12345678;
  check "SGDT after LGDT 32-bit" (List.fold_left (fun a b -> (a lsl 8) lor b) 0 (List.rev stored))
    0x123456781234;
  let lidt = "\x0f\x01\x1e\x00\x02" and sidt = "\x0f\x01\x0e\x10\x02" in
  let t, stored = load_store lidt sidt in
  check "LIDT 16-bit: base" (sys t).Cpu86.saved_idtr_base 0x345678;
  check "LIDT 16-bit: limit" (sys t).Cpu86.saved_idtr_limit 0x1234;
  check "SIDT after LIDT" (List.fold_left (fun a b -> (a lsl 8) lor b) 0 (List.rev stored))
    0x003456781234;
  let t, _ = load_store ("\x66" ^ lidt) sidt in
  check "LIDT 32-bit: base" (sys t).Cpu86.saved_idtr_base 0x12345678;
  (* the GDTR and IDTR the 386 has after reset *)
  let t = cpu "\x90" in
  check "reset GDTR limit" (sys t).Cpu86.saved_gdtr_limit 0;
  check "reset IDTR limit" (sys t).Cpu86.saved_idtr_limit 0x3ff;
  check "reset IDTR base" (sys t).Cpu86.saved_idtr_base 0

(* A register operand is INT 6; six bytes past the segment end are INT 13 and
   leave the register as it was. *)
let test_table_faults () =
  List.iter
    (fun (name, code) ->
      check_taken name (taken_in (cpu code) 1) (Some (6, 0)))
    [ ("LGDT with a register", "\x0f\x01\xd0"); ("LIDT with a register", "\x0f\x01\xd8") ];
  let t = cpu "\x0f\x01\x16\xfc\xff" in
  check_taken "LGDT at FFFC" (taken_in t 1) (Some (13, 0));
  check "LGDT at FFFC kept the limit" (sys t).Cpu86.saved_gdtr_limit 0;
  check "LGDT at FFFC kept the base" (sys t).Cpu86.saved_gdtr_base 0

(* MOV to CR0 keeps the defined bits only. PG without PE is #GP, and PG over
   tables that are not the first-megabyte identity map stops the core before
   anything is written. *)
let test_cr0 () =
  let t = cpu (mov_eax 0x7ffffffe ^ "\x0f\x22\xc0" ^ "\x0f\x20\xc0") in
  run t 3;
  check "CR0 keeps MP EM TS ET" (eax t) 0x1e;
  let t = cpu (mov_eax 0x00000001 ^ "\x0f\x22\xc0") in
  run t 2;
  check "MOV CR0 with PE turns protected mode on" (sys t).Cpu86.saved_cr0 1;
  let t = cpu (mov_eax 0x80000000 ^ "\x0f\x22\xc0") in
  run t 1;
  check_taken "MOV CR0 with PG and no PE" (taken_in t 1) (Some (13, 6));
  check "MOV CR0 with PG and no PE left CR0" (sys t).Cpu86.saved_cr0 0;
  (* mov eax,-1 ; mov cr0,eax: PG over CR3 = 0, whose first PDE is not present *)
  let t = cpu (mov_eax 0xffffffff ^ "\x0f\x22\xc0") in
  run t 1;
  check_stopped "MOV CR0 with every bit" (stopped_at t) (Some 6);
  check "MOV CR0 with every bit left CR0" (sys t).Cpu86.saved_cr0 0

(* SMSW reads CR0, 32 bits into a 32-bit register, 16 bits otherwise; LMSW
   writes PE MP EM TS from a word. CLTS clears TS. *)
let test_msw () =
  let t = cpu "\xb8\x0c\x00\x0f\x01\xf0\x0f\x20\xc0" in
  run t 3;
  check "LMSW then MOV EAX,CR0" (eax t) 0xc;
  let t =
    cpu ("\xb8\x0c\x00\x0f\x01\xf0"
         ^ "\x66\xbb\x00\x00\xff\xff" ^ "\x0f\x01\xe3"
         ^ "\x66\xb9\x00\x00\xff\xff" ^ "\x0f\x01\xe1")
  in
  run t 4;
  check "SMSW bx keeps the upper half of EBX" (ebx t) 0xffff000c;
  run t 2;
  check "SMSW cx keeps the upper half of ECX" (ecx t) 0xffff000c;
  let t =
    cpu ("\xb8\x0c\x00\x0f\x01\xf0" ^ "\x66\xbb\x00\x00\xff\xff" ^ "\x66\x0f\x01\xe3")
  in
  run t 4;
  check "SMSW with 66h writes the whole register" (ebx t) 0xc;
  let t = cpu "\xb8\x01\x00\x0f\x01\xf0" in
  run t 2;
  check "LMSW with PE turns protected mode on" (sys t).Cpu86.saved_cr0 1;
  (* LMSW can set PE and cannot clear it *)
  let t = cpu "\xb8\x01\x00\x0f\x01\xf0\xb8\x00\x00\x0f\x01\xf0" in
  run t 4;
  check "LMSW cannot clear PE" (sys t).Cpu86.saved_cr0 1;
  let t = cpu "\xb8\x08\x00\x0f\x01\xf0\x0f\x06\x0f\x20\xc0" in
  run t 3;
  check "CLTS cleared TS" (sys t).Cpu86.saved_cr0 0;
  (* LMSW leaves the bits it does not own: ET, set through CR0, stays *)
  let t = cpu (mov_eax 0x10 ^ "\x0f\x22\xc0" ^ "\xb8\x08\x00\x0f\x01\xf0") in
  run t 4;
  check "LMSW keeps ET" (sys t).Cpu86.saved_cr0 0x18

(* CR2 and CR3 hold 32 bits. The mod field of the modrm byte is ignored: no
   displacement follows, and the register is the rm. CR1 and CR4 are INT 6. *)
let test_cr2_cr3 () =
  let t =
    cpu (mov_eax 0x12345678 ^ "\x0f\x22\xd0" ^ mov_eax 0x89abcdef ^ "\x0f\x22\xd8"
         ^ "\x0f\x20\xd3" ^ "\x0f\x20\xd9")
  in
  run t 6;
  check "MOV EBX,CR2" (ebx t) 0x12345678;
  check "MOV ECX,CR3" (ecx t) 0x89abcdef;
  let t = cpu ("\x66\xbe\xff\xff\xff\xff" ^ "\x0f\x20\x06" ^ "\xb8\x34\x12") in
  run t 3;
  check "mod 0 rm 6 reads into ESI" (esi t) 0;
  check "no displacement was fetched" (Cpu86.reg16 t 0) 0x1234;
  check_taken "MOV EAX,CR1" (taken_in (cpu "\x0f\x20\xc8") 1) (Some (6, 0));
  check_taken "MOV CR4,EAX" (taken_in (cpu "\x0f\x22\xe0") 1) (Some (6, 0))

(* DR0-DR3, DR6, DR7 hold what was written. A DR7 that enables a breakpoint
   and the reserved DR4, DR5 stop the core. TR6 and TR7 hold values; other
   test registers are INT 6. *)
let test_debug_and_test_registers () =
  let t =
    cpu (mov_eax 0xdeadbeef ^ "\x0f\x23\xc0" ^ "\x0f\x21\xc3"
         ^ mov_eax 0x0ff00ff0 ^ "\x0f\x23\xf0" ^ "\x0f\x21\xf1")
  in
  run t 6;
  check "DR0 round trip" (ebx t) 0xdeadbeef;
  check "DR6 round trip" (ecx t) 0x0ff00ff0;
  let t = cpu (mov_eax 0x400 ^ "\x0f\x23\xf8") in
  run t 2;
  check "DR7 without breakpoint enables" (sys t).Cpu86.saved_dr.(7) 0x400;
  let t = cpu (mov_eax 0x1 ^ "\x0f\x23\xf8") in
  run t 1;
  check_stopped "DR7 enabling a breakpoint" (stopped_at t) (Some 6);
  check "DR7 stayed" (sys t).Cpu86.saved_dr.(7) 0;
  check_stopped "MOV EAX,DR4" (stopped_at (cpu "\x0f\x21\xe0")) (Some 0);
  let t = cpu (mov_eax 0x11112222 ^ "\x0f\x26\xf0" ^ mov_eax 0x33334444 ^ "\x0f\x26\xf8"
               ^ "\x0f\x24\xf1" ^ "\x0f\x24\xfa") in
  run t 6;
  check "TR6 round trip" (ecx t) 0x11112222;
  check "TR7 round trip" (Cpu86.reg32 t 2) 0x33334444;
  check_taken "MOV TR3,EAX" (taken_in (cpu "\x0f\x26\xd8") 1) (Some (6, 0))

(* CR0.EM and TS send ESC to INT 7; MP and TS together send WAIT. Without
   them both run as before. *)
let test_coprocessor_traps () =
  let lmsw v = Printf.sprintf "\xb8%c\x00\x0f\x01\xf0" (Char.chr v) in
  let esc = "\xd8\xc0" and wait = "\x9b" in
  check_taken "ESC without EM or TS" (taken_in (cpu esc) 1) None;
  check_taken "ESC with EM" (taken_in (cpu (lmsw 4 ^ esc)) 3) (Some (7, 6));
  check_taken "ESC with TS" (taken_in (cpu (lmsw 8 ^ esc)) 3) (Some (7, 6));
  check_taken "ESC with MP only" (taken_in (cpu (lmsw 2 ^ esc)) 3) None;
  check_taken "WAIT without MP" (taken_in (cpu wait) 1) None;
  check_taken "WAIT with MP and TS" (taken_in (cpu (lmsw 10 ^ wait)) 3) (Some (7, 6));
  check_taken "WAIT with TS only" (taken_in (cpu (lmsw 8 ^ wait)) 3) None;
  check_taken "WAIT with MP only" (taken_in (cpu (lmsw 2 ^ wait)) 3) None

(* A saved 386 carries the system registers and gives them back; a state with
   CR0.PG with PE off or the wrong register counts is refused and changes
   nothing. *)
let test_state () =
  let program =
    set_ds ^ "\x0f\x01\x16\x00\x02" ^ mov_eax 0x12345678 ^ "\x0f\x22\xd0" ^ "\x0f\x23\xc0"
    ^ "\x0f\x26\xf0" ^ "\xb8\x04\x00\x0f\x01\xf0"
  in
  let t, mem = cpu_mem program in
  String.iteri (fun i c -> Bytes.set mem (0x10200 + i) c) "\x34\x12\x78\x56\x34\x12";
  run t 9;
  let s = Cpu86.save_state t in
  let t2 = cpu "\x90" in
  Cpu86.load_state t2 s;
  check "load_state gives back the saved state" (Bool.to_int (Cpu86.save_state t2 = s)) 1;
  check "CR2 survived" (sys t2).Cpu86.saved_cr2 0x12345678;
  check "CR0 survived" (sys t2).Cpu86.saved_cr0 0x4;
  check "GDTR base survived" (sys t2).Cpu86.saved_gdtr_base 0x345678;
  let x = sys t in
  let refused name saved_386 =
    let t3 = cpu "\x90" in
    let before = Cpu86.save_state t3 in
    (match Cpu86.load_state t3 { s with Cpu86.saved_386 = Some saved_386 } with
     | () -> check name 1 0
     | exception Invalid_argument _ -> ());
    check (name ^ ": nothing changed") (Bool.to_int (Cpu86.save_state t3 = before)) 1
  in
  refused "load_state with PG and no PE"
    { x with Cpu86.saved_cr0 = x.Cpu86.saved_cr0 lor 0x80000000 };
  refused "load_state with 5 segment bases" { x with Cpu86.saved_bases = Array.make 5 0 };
  refused "load_state with 7 segment limits" { x with Cpu86.saved_limits = Array.make 7 0 };
  refused "load_state with 7 debug registers" { x with Cpu86.saved_dr = Array.make 7 0 };
  refused "load_state with 3 test registers" { x with Cpu86.saved_tr = Array.make 3 0 }

let () =
  test_tables ();
  test_table_faults ();
  test_cr0 ();
  test_msw ();
  test_cr2_cr3 ();
  test_debug_and_test_registers ();
  test_coprocessor_traps ();
  test_state ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "cpu86 system registers: all passed"
