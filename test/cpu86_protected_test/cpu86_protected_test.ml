(* The 80386 in protected mode (RFC 0005 stage 3b): CR0.PE on and off, segment
   loads from the GDT and the LDT, the default operand, address and stack size
   from CS.D and SS.B, LLDT, LTR, SLDT, STR, VERR, VERW, LAR, LSL, ARPL, and the
   first-megabyte identity check CR0.PG makes. Loads protected mode answers with
   a fault, and every INT and fault in protected mode, stop the core with
   Unsupported until RFC 0005 3c and 4.

   Memory is 1MB. Code starts at 0000:0000 in real mode; the GDT is at 0800h, the
   LDT at 0A00h, the code of the protected part at 0100h. Selectors:

     08h flat 32-bit code        10h flat 32-bit data (B=1, 4GB)
     18h 16-bit code, 64KB       20h 16-bit data, base 10000h, 64KB
     28h LDT descriptor          30h available TSS at 5000h
     38h data, not present       40h data, expand-down
     48h data, DPL 3             50h conforming code
     58h execute-only code       60h read-only data
     68h call gate               70h 32-bit data, base 20000h, 64KB (B=1)
     78h 4GB data, base 12345678h   80h 64KB code, DPL 3
     88h 32-bit code, base 0, 512 bytes
     04h (LDT) data, base 20000h, 64KB *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %x want %x\n%!" name got want
  end

let check_bool name got want = check name (Bool.to_int got) (Bool.to_int want)

let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let le32 v = String.init 4 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

(* An 8-byte descriptor: [flags] is the upper nibble (G D/B L AVL). *)
let desc ~base ~limit ~access ~flags =
  le16 (limit land 0xffff) ^ le16 (base land 0xffff)
  ^ String.make 1 (Char.chr ((base lsr 16) land 0xff))
  ^ String.make 1 (Char.chr access)
  ^ String.make 1 (Char.chr ((flags lsl 4) lor ((limit lsr 16) land 0xf)))
  ^ String.make 1 (Char.chr ((base lsr 24) land 0xff))

let gdt_base = 0x800
let ldt_base = 0xa00
let gdtr_image = 0x7f0
let code32_at = 0x100
let tss_base = 0x5000

let granular_big = 0xc
let big = 0x4
let none = 0x0

let gdt =
  String.concat ""
    [ String.make 8 '\000';
      desc ~base:0 ~limit:0xfffff ~access:0x9a ~flags:granular_big;
      desc ~base:0 ~limit:0xfffff ~access:0x92 ~flags:granular_big;
      desc ~base:0 ~limit:0xffff ~access:0x9a ~flags:none;
      desc ~base:0x10000 ~limit:0xffff ~access:0x92 ~flags:none;
      desc ~base:ldt_base ~limit:0x1f ~access:0x82 ~flags:none;
      desc ~base:tss_base ~limit:0x67 ~access:0x89 ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0x12 ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0x96 ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0xf2 ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0x9e ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0x98 ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0x90 ~flags:none;
      desc ~base:0 ~limit:0xffff ~access:0x8c ~flags:none;
      desc ~base:0x20000 ~limit:0xffff ~access:0x92 ~flags:big;
      desc ~base:0x12345678 ~limit:0xfffff ~access:0x92 ~flags:granular_big;
      desc ~base:0 ~limit:0xffff ~access:0xfa ~flags:none;
      desc ~base:0 ~limit:0x1ff ~access:0x9a ~flags:big ]

let gdt_limit = String.length gdt - 1
let ldt = desc ~base:0x20000 ~limit:0xffff ~access:0x92 ~flags:none

let mem_size = 0x100000

let make code_real code_prot =
  let mem = Bytes.make mem_size '\000' in
  let put a s = String.iteri (fun i c -> Bytes.set mem (a + i) c) s in
  put 0 code_real;
  put code32_at code_prot;
  put gdt_base gdt;
  put ldt_base ldt;
  put gdtr_image (le16 gdt_limit ^ le32 gdt_base);
  let read a = Char.code (Bytes.get mem (a land (mem_size - 1))) in
  let write a v = Bytes.set mem (a land (mem_size - 1)) (Char.chr (v land 0xff)) in
  let t =
    Cpu86.create ~model:Cpu86.I80386 ~read ~write ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> ()) ()
  in
  Cpu86.set_seg t 1 0;
  Cpu86.set_ip t 0;
  Cpu86.set_seg t 2 0x1000;
  Cpu86.set_reg16 t 4 0x0100;
  (t, mem)

(* lgdt [gdtr_image]; mov eax,cr0; or al,1; mov cr0,eax; jmp 0008:code32_at *)
let enter_flat =
  "\x0f\x01\x16" ^ le16 gdtr_image ^ "\x0f\x20\xc0" ^ "\x0c\x01" ^ "\x0f\x22\xc0"
  ^ "\xea" ^ le16 code32_at ^ le16 0x08

let hlt = "\xf4"

(* The result of running until HLT or a stop. *)
type outcome = Halted | Stopped of string

let run t =
  let rec go n =
    if n > 2000 then Stopped "ran away"
    else if Cpu86.halted t then Halted
    else
      match Cpu86.step t with
      | _ -> go (n + 1)
      | exception Cpu86.Unsupported m -> Stopped m
  in
  go 0

let contains text part =
  let n = String.length text and m = String.length part in
  let rec at i = i + m <= n && (String.sub text i m = part || at (i + 1)) in
  at 0

let check_stopped name outcome part =
  match outcome with
  | Stopped m when contains m part -> ()
  | Stopped m ->
    incr failed;
    Printf.eprintf "FAIL %s: stopped with %S, want a message with %S\n%!" name m part
  | Halted ->
    incr failed;
    Printf.eprintf "FAIL %s: ran to HLT, want a stop with %S\n%!" name part

let check_halted name outcome =
  match outcome with
  | Halted -> ()
  | Stopped m ->
    incr failed;
    Printf.eprintf "FAIL %s: stopped with %S\n%!" name m

let sys t =
  match (Cpu86.save_state t).Cpu86.saved_386 with
  | Some x -> x
  | None -> failwith "an 80386 saves its system registers"

let eax t = Cpu86.reg32 t 0
let ebx t = Cpu86.reg32 t 3
let esp t = Cpu86.reg32 t 4

let mov_eax v = "\xb8" ^ le32 v      (* 32-bit code *)
let mov_ax_sel v = "\x66\xb8" ^ le16 v
let mov_sreg reg = "\x8e" ^ String.make 1 (Char.chr (0xc0 lor (reg lsl 3)))
let load_ds sel = mov_ax_sel sel ^ mov_sreg 3
let load_ss sel = mov_ax_sel sel ^ mov_sreg 2

let es = 0 and ss = 2 and ds = 3 and fs = 4

(* CR0.PE on: the part of the CPU a program sees. *)
let test_enter () =
  let t, _ = make enter_flat (mov_eax 0x12345678 ^ "\x66\xb8\x34\x12" ^ hlt) in
  check_halted "enter protected mode" (run t);
  check "PE is on" ((sys t).Cpu86.saved_cr0 land 1) 1;
  check "CS selector" (Cpu86.seg t 1) 0x08;
  let x = sys t in
  check "CS base" x.Cpu86.saved_bases.(1) 0;
  check "CS limit has G applied" x.Cpu86.saved_limits.(1) 0xffffffff;
  check_bool "CS.D" x.Cpu86.saved_code32 true;
  (* no prefix is 32 bits in 32-bit code, 66h is 16: mov eax,imm32 then mov ax,imm16 *)
  check "mov eax then mov ax" (eax t) 0x12341234

(* The caches stay as real mode left them until a segment is loaded, and a load
   fills base, limit and the size bits from the descriptor. *)
let test_segment_loads () =
  let t, mem =
    make enter_flat
      (load_ds 0x10 ^ load_ss 0x20 ^ mov_ax_sel 0x70 ^ mov_sreg 0 ^ mov_ax_sel 0x28
       ^ "\x0f\x00\xd0" ^ mov_ax_sel 0x04 ^ mov_sreg 4 ^ hlt)
  in
  ignore mem;
  check_halted "segment loads" (run t);
  let x = sys t in
  check "DS base" x.Cpu86.saved_bases.(ds) 0;
  check "DS limit" x.Cpu86.saved_limits.(ds) 0xffffffff;
  check "SS selector" (Cpu86.seg t ss) 0x20;
  check "SS base" x.Cpu86.saved_bases.(ss) 0x10000;
  check "SS limit" x.Cpu86.saved_limits.(ss) 0xffff;
  check_bool "SS.B is off for the 16-bit data segment" x.Cpu86.saved_stack32 false;
  check "ES base" x.Cpu86.saved_bases.(es) 0x20000;
  check "FS from the LDT: base" x.Cpu86.saved_bases.(fs) 0x20000;
  check "FS from the LDT: selector" (Cpu86.seg t fs) 0x04;
  (* a load through DS reaches base + offset *)
  let t, mem = make enter_flat (load_ss 0x70 ^ "\xbc\x00\x01\x00\x00" ^ mov_eax 0xdeadbeef ^ "\x50" ^ hlt) in
  check_halted "push through a based SS" (run t);
  check "pushed to base + ESP - 4" (Char.code (Bytes.get mem (0x200fc))) 0xef;
  check "ESP after the push" (esp t) 0xfc;
  check_bool "SS.B follows the 32-bit data segment" (sys t).Cpu86.saved_stack32 true

(* All four bytes of the base and the G bit reach the cache, and the RPL bits of
   a null selector do not make it a descriptor. *)
let test_descriptor_fields () =
  let t, _ = make enter_flat (load_ds 0x78 ^ hlt) in
  check_halted "load a descriptor with a high base" (run t);
  check "DS base has all four bytes" (sys t).Cpu86.saved_bases.(ds) 0x12345678;
  check "DS limit has G applied" (sys t).Cpu86.saved_limits.(ds) 0xffffffff;
  let t, _ = make enter_flat (mov_ax_sel 3 ^ mov_sreg 3 ^ hlt) in
  check_halted "null selector with RPL 3" (run t);
  check "DS limit after a null selector with RPL 3" (sys t).Cpu86.saved_limits.(ds)
    Cpu86.no_limit

(* A null selector loads into DS, ES, FS and GS and any access through it is out
   of range. *)
let test_null_selector () =
  let t, _ = make enter_flat (mov_ax_sel 0 ^ mov_sreg 3 ^ "\xa1\x00\x00\x00\x00" ^ hlt) in
  check_stopped "access through a null DS" (run t) "fault 13 in protected mode";
  check "DS limit is no_limit" (sys t).Cpu86.saved_limits.(ds) Cpu86.no_limit;
  check "DS base" (sys t).Cpu86.saved_bases.(ds) 0

(* The default sizes follow CS.D: 16-bit code (CS.D off) reads mov eax as a 66h
   instruction, and 67h flips the address size. *)
let test_code_sizes () =
  (* far jump from 32-bit code to the 16-bit code segment at 0200h *)
  let back16 = "\xea" ^ le32 0x200 ^ le16 0x18 in
  let t, mem =
    make enter_flat (load_ds 0x10 ^ back16)
  in
  let put a s = String.iteri (fun i c -> Bytes.set mem (a + i) c) s in
  (* 16-bit code: mov ax,1234h; 66 mov eax,... as 66 B8 imm32; mov bx,[0300h] *)
  put 0x200 ("\xb8\x34\x12" ^ "\x66\xbb" ^ le32 0xcafe0001 ^ hlt);
  check_halted "16-bit code segment" (run t);
  check "CS selector" (Cpu86.seg t 1) 0x18;
  check_bool "CS.D off" (sys t).Cpu86.saved_code32 false;
  check "mov ax,imm16 in 16-bit code" (eax t land 0xffff) 0x1234;
  check "66h mov ebx,imm32 in 16-bit code" (ebx t) 0xcafe0001;
  (* 67h in 32-bit code means a 16-bit address: [bx] with DS base 0 *)
  let t, mem = make enter_flat (load_ds 0x10 ^ mov_eax 0x00000300 ^ "\x66\x89\xc3" ^ "\x67\x8a\x07" ^ hlt) in
  Bytes.set mem 0x300 '\x5a';
  check_halted "67h in 32-bit code" (run t);
  check "mov al,[bx] reads through BX" (eax t land 0xff) 0x5a

(* The stack width follows SS.B. *)
let test_stack_width () =
  let mov_esp v = "\xbc" ^ le32 v in
  (* 16-bit stack, ESP = 20002h: two 16-bit pushes move SP only and SP wraps at
     64KB, so ESP keeps its upper half *)
  let t, mem =
    make enter_flat (load_ss 0x20 ^ mov_esp 0x20002 ^ "\x66\x50\x66\x50" ^ hlt)
  in
  check_halted "pushes on a 16-bit stack" (run t);
  check "ESP keeps its upper half, SP wrapped" (esp t) 0x2fffe;
  ignore mem;
  (* the same pushes of a dword on a 32-bit flat stack: ESP crosses 64KB *)
  let t, mem = make enter_flat (load_ss 0x10 ^ mov_esp 0x30000 ^ mov_eax 0x11223344 ^ "\x50" ^ hlt) in
  check_halted "push on a 32-bit stack" (run t);
  check "ESP not wrapped" (esp t) 0x2fffc;
  check "the dword is at ESP" (Char.code (Bytes.get mem 0x2fffc)) 0x44;
  (* a word push and pop on the same 32-bit stack *)
  let t, mem = make enter_flat (load_ss 0x10 ^ mov_esp 0x30000 ^ mov_eax 0x11223344 ^ "\x66\x50" ^ hlt) in
  check_halted "word push on a 32-bit stack" (run t);
  check "ESP drops by two without wrapping" (esp t) 0x2fffe;
  check "the word is at ESP" (Char.code (Bytes.get mem 0x2fffe)) 0x44;
  let t, mem = make enter_flat (load_ss 0x10 ^ mov_esp 0x2fffe ^ "\x66\x5b" ^ hlt) in
  Bytes.set mem 0x2fffe '\x66';
  Bytes.set mem 0x2ffff '\x55';
  check_halted "word pop on a 32-bit stack" (run t);
  check "ESP rises by two" (esp t) 0x30000;
  check "BX is the word" (ebx t land 0xffff) 0x5566;
  (* LEAVE on a 32-bit stack takes EBP whole: both operand sizes *)
  let leave_test name leave pop_bytes =
    let t, mem = make enter_flat (load_ss 0x10 ^ mov_esp 0x50000 ^ "\xbd" ^ le32 0x30000 ^ leave ^ hlt) in
    String.iteri (fun i c -> Bytes.set mem (0x30000 + i) c) (le32 0x55667788);
    check_halted name (run t);
    check (name ^ ": ESP") (esp t) (0x30000 + pop_bytes);
    check (name ^ ": EBP") (Cpu86.reg32 t 5 land (if pop_bytes = 2 then 0xffff else 0xffffffff))
      (if pop_bytes = 2 then 0x7788 else 0x55667788)
  in
  leave_test "LEAVE 32-bit" "\xc9" 4;
  leave_test "LEAVE 16-bit" "\x66\xc9" 2;
  (* on a 16-bit stack the same push wraps SP, not ESP *)
  let t, mem = make enter_flat (load_ss 0x20 ^ mov_esp 0x30000 ^ mov_eax 0x11223344 ^ "\x50" ^ hlt) in
  check_halted "push on a 16-bit stack" (run t);
  check "SP wrapped, ESP upper half kept" (esp t) 0x3fffc;
  check "the dword is at SS base + SP" (Char.code (Bytes.get mem 0x1fffc)) 0x44;
  (* POP SS to a descriptor of the other width raises the stack pointer by the
     width it had before the load (test386 POST 9): ESP keeps its upper half *)
  let t, mem = make enter_flat (load_ss 0x20 ^ mov_esp 0x2fffc ^ "\x17" ^ hlt) in
  String.iteri (fun i c -> Bytes.set mem (0x1fffc + i) c) (le32 0x70);
  check_halted "POP SS to a 32-bit stack" (run t);
  check "ESP after POP SS" (esp t) 0x20000;
  check "SS selector" (Cpu86.seg t ss) 0x70;
  check_bool "SS.B on" (sys t).Cpu86.saved_stack32 true

(* Loads protected mode answers with a fault stop the core, and the stop names
   what was wrong. The cache of the register being loaded stays as it was. *)
let test_stops () =
  let stops name body part =
    let t, _ = make enter_flat (body ^ hlt) in
    check_stopped name (run t) part
  in
  let sel_into reg sel = mov_ax_sel sel ^ mov_sreg reg in
  stops "null selector into SS" (sel_into ss 0) "null selector into SS";
  stops "selector beyond the GDT" (sel_into ds 0x90) "beyond its table";
  stops "LDT selector with no LDT" (sel_into ds 0x04) "beyond its table";
  stops "not present" (sel_into ds 0x38) "not present";
  stops "system descriptor into DS" (sel_into ds 0x28) "system descriptor";
  stops "call gate into DS" (sel_into ds 0x68) "system descriptor";
  stops "expand-down segment" (sel_into ds 0x40) "expand-down";
  stops "execute-only code into DS" (sel_into ds 0x58) "execute-only";
  stops "code segment into SS" (sel_into ss 0x08) "code segment";
  stops "read-only data into SS" (sel_into ss 0x60) "read-only";
  stops "RPL above DPL" (sel_into ds 0x13) "privilege level";
  stops "far jump to a data segment" ("\xea" ^ le32 0x120 ^ le16 0x10) "data segment";
  stops "far jump with RPL 3" ("\xea" ^ le32 0x120 ^ le16 0x0b) "privilege level";
  stops "far jump to DPL 3 code" ("\xea" ^ le32 0x120 ^ le16 0x80) "privilege level";
  stops "SS with DPL 3" (sel_into ss 0x48) "privilege level";
  (* a DPL 3 data segment is reachable with RPL 3 *)
  let t, _ = make enter_flat (sel_into ds 0x4b ^ hlt) in
  check_halted "DPL 3 data with RPL 3" (run t);
  (* a stopped load leaves the register as it was *)
  let t, _ = make enter_flat (sel_into ds 0x10 ^ sel_into ds 0x38 ^ hlt) in
  ignore (run t);
  check "DS keeps its selector after a stopped load" (Cpu86.seg t ds) 0x10;
  check "DS keeps its limit after a stopped load" (sys t).Cpu86.saved_limits.(ds) 0xffffffff;
  (* INT and faults go through the IDT, which is RFC 0005 3c *)
  stops "INT in protected mode" "\xcd\x21" "int 21 in protected mode";
  stops "divide error in protected mode" (mov_eax 0 ^ "\xf7\xf0") "fault 0 in protected mode";
  stops "single-step trap in protected mode"
    (mov_eax 0x100 ^ "\x50\x9d\x90") "single-step trap in protected mode"

(* CS limit bounds instruction fetch, and EIP goes past 64KB in a 4GB code
   segment. *)
let test_fetch_limit () =
  let t, mem = make enter_flat (mov_eax 0x20000 ^ "\xff\xe0") in
  Bytes.set mem 0x20000 '\xf4';
  check_halted "jump past 64KB in flat code" (run t);
  check "EIP after the HLT" (Cpu86.dump_ip t) 0x20001;
  (* 64KB code segment: a NOP at FFFFh is the last byte, the next fetch is #GP *)
  let t, mem = make enter_flat ("\xea" ^ le32 0xffff ^ le16 0x18) in
  Bytes.set mem 0xffff '\x90';
  check_stopped "fetch past the CS limit" (run t) "fault 13 in protected mode";
  check "EIP wound back to the NOP" (Cpu86.dump_ip t) 0x10000;
  (* 512-byte 32-bit code segment: the fetch after the last byte stops, well below 64KB *)
  let t, mem = make enter_flat ("\xea" ^ le32 0x1ff ^ le16 0x88) in
  Bytes.set mem 0x1ff '\x90';
  check_stopped "fetch past a 512-byte CS limit" (run t) "fault 13 in protected mode";
  check "EIP wound back to the next instruction" (Cpu86.dump_ip t) 0x200;
  (* a far jump checks its offset against the limit of the code segment it loads:
     from 16-bit code into flat code past 64KB *)
  let t, mem = make enter_flat ("\xea" ^ le32 0x200 ^ le16 0x18) in
  String.iteri (fun i c -> Bytes.set mem (0x200 + i) c) ("\x66\xea" ^ le32 0x20000 ^ le16 0x08);
  Bytes.set mem 0x20000 '\xf4';
  check_halted "far jump from 16-bit code to flat code past 64KB" (run t);
  check "EIP after the HLT" (Cpu86.dump_ip t) 0x20001

(* LLDT, LTR, SLDT and STR. LTR marks the TSS descriptor busy in the GDT. *)
let test_ldt_and_task_register () =
  let t, mem =
    make enter_flat
      (mov_ax_sel 0x28 ^ "\x0f\x00\xd0" ^ "\x0f\x00\xc1" ^ mov_ax_sel 0x30
       ^ "\x0f\x00\xd8" ^ "\x0f\x00\xcb" ^ hlt)
  in
  check_halted "LLDT, SLDT, LTR, STR" (run t);
  let x = sys t in
  check "SLDT" (Cpu86.reg32 t 1 land 0xffff) 0x28;
  check "STR" (ebx t land 0xffff) 0x30;
  check "LDTR base" x.Cpu86.saved_ldt_base ldt_base;
  check "LDTR limit" x.Cpu86.saved_ldt_limit 0x1f;
  check "TR base" x.Cpu86.saved_task_base tss_base;
  check "TR limit" x.Cpu86.saved_task_limit 0x67;
  check "TSS descriptor is busy" (Char.code (Bytes.get mem (gdt_base + 0x30 + 5))) 0x8b;
  (* a null selector empties the LDT *)
  let t, _ = make enter_flat (mov_ax_sel 0x28 ^ "\x0f\x00\xd0" ^ mov_ax_sel 0 ^ "\x0f\x00\xd0" ^ hlt) in
  check_halted "LLDT with a null selector" (run t);
  check "LDTR selector" (sys t).Cpu86.saved_ldt_selector 0;
  check "LDTR limit" (sys t).Cpu86.saved_ldt_limit 0;
  let stops name body part =
    let t, _ = make enter_flat (body ^ hlt) in
    check_stopped name (run t) part
  in
  stops "LLDT of a code selector" (mov_ax_sel 0x08 ^ "\x0f\x00\xd0") "not an LDT descriptor";
  stops "LLDT of a TSS" (mov_ax_sel 0x30 ^ "\x0f\x00\xd0") "not an LDT descriptor";
  stops "LLDT of an LDT entry" (mov_ax_sel 0x04 ^ "\x0f\x00\xd0") "names an LDT entry";
  stops "LTR of an LDT descriptor" (mov_ax_sel 0x28 ^ "\x0f\x00\xd8") "not an available TSS";
  stops "LTR of a busy TSS"
    (mov_ax_sel 0x30 ^ "\x0f\x00\xd8" ^ mov_ax_sel 0x30 ^ "\x0f\x00\xd8")
    "not an available TSS";
  stops "LTR of a null selector" (mov_ax_sel 0 ^ "\x0f\x00\xd8") "null selector";
  (* in real mode these are INT 6 *)
  List.iter
    (fun (name, code) ->
      let t, _ = make code "" in
      let taken = ref None in
      Cpu86.set_int_hook t (fun v -> if !taken = None then taken := Some (v, Cpu86.dump_ip t));
      ignore (Cpu86.step t);
      check name (match !taken with Some (6, 0) -> 1 | _ -> 0) 1)
    [ ("SLDT in real mode", "\x0f\x00\xc0"); ("LLDT in real mode", "\x0f\x00\xd0");
      ("LTR in real mode", "\x0f\x00\xd8"); ("VERR in real mode", "\x0f\x00\xe0");
      ("LAR in real mode", "\x0f\x02\xc3"); ("LSL in real mode", "\x0f\x03\xc3");
      ("ARPL in real mode", "\x63\xc3") ]

let zf t = Cpu86.flags t land Cpu86.f_zero <> 0

(* VERR, VERW, LAR, LSL and ARPL set ZF and never fault. *)
let test_queries () =
  let after body =
    let t, _ = make enter_flat (body ^ hlt) in
    check_halted "query" (run t);
    t
  in
  let verr sel = mov_ax_sel sel ^ "\x0f\x00\xe0" in
  let verw sel = mov_ax_sel sel ^ "\x0f\x00\xe8" in
  check_bool "VERR data" (zf (after (verr 0x10))) true;
  check_bool "VERR readable code" (zf (after (verr 0x08))) true;
  check_bool "VERR execute-only code" (zf (after (verr 0x58))) false;
  check_bool "VERR conforming code" (zf (after (verr 0x50))) true;
  check_bool "VERR conforming code with RPL 3" (zf (after (verr 0x53))) true;
  check_bool "VERR system descriptor" (zf (after (verr 0x28))) false;
  check_bool "VERR null" (zf (after (verr 0))) false;
  check_bool "VERR beyond the table" (zf (after (verr 0x90))) false;
  check_bool "VERR RPL above DPL" (zf (after (verr 0x13))) false;
  check_bool "VERR DPL 3 data with RPL 3" (zf (after (verr 0x4b))) true;
  check_bool "VERW data" (zf (after (verw 0x10))) true;
  check_bool "VERW read-only data" (zf (after (verw 0x60))) false;
  check_bool "VERW code" (zf (after (verw 0x08))) false;
  let lar sel = mov_eax 0xffffffff ^ mov_ax_sel sel ^ "\x66\x89\xc3" ^ "\x0f\x02\xc3" in
  let t = after (lar 0x10) in
  check_bool "LAR data: ZF" (zf t) true;
  check "LAR flat data: access rights and flag nibble" (eax t) 0x00c09200;
  let t = after (lar 0x68) in
  check "LAR call gate" (eax t) 0x00008c00;
  let t = after (lar 0x38) in
  check "LAR not present data still reads" (eax t) 0x00001200;
  let t = after (lar 0x90) in
  check_bool "LAR beyond the table: ZF" (zf t) false;
  check "LAR beyond the table leaves EAX" (eax t) 0xffff0090;
  let t = after (lar 0x13) in
  check_bool "LAR RPL above DPL: ZF" (zf t) false;
  let t = after (mov_eax 0xffffffff ^ mov_ax_sel 0x10 ^ "\x66\x89\xc3" ^ "\x66\x0f\x02\xc3") in
  check "LAR with 66h writes AX only" (eax t) 0xffff9200;
  let lsl_ sel = mov_eax 0 ^ mov_ax_sel sel ^ "\x66\x89\xc3" ^ "\x0f\x03\xc3" in
  let t = after (lsl_ 0x10) in
  check "LSL flat data" (eax t) 0xffffffff;
  let t = after (lsl_ 0x20) in
  check "LSL 64KB data" (eax t) 0xffff;
  let t = after (lsl_ 0x68) in
  check_bool "LSL call gate: ZF" (zf t) false;
  let t = after (lsl_ 0x28) in
  check "LSL of the LDT" (eax t) 0x1f;
  (* ARPL raises the destination RPL to the source RPL *)
  let arpl dest src = "\x66\xbb" ^ le16 dest ^ mov_ax_sel src ^ "\x63\xc3" in
  let t = after (arpl 0x08 0x03) in
  check_bool "ARPL raises: ZF" (zf t) true;
  check "ARPL raises BX" (ebx t land 0xffff) 0x0b;
  let t = after (arpl 0x0b 0x01) in
  check_bool "ARPL keeps: ZF" (zf t) false;
  check "ARPL keeps BX" (ebx t land 0xffff) 0x0b

(* Clearing PE returns to real mode: segment loads write the base again and the
   limits and size bits stay as protected mode left them. *)
let test_return_to_real_mode () =
  let to16 = "\xea" ^ le32 0x200 ^ le16 0x18 in
  let t, mem = make enter_flat (load_ds 0x10 ^ to16) in
  let put a s = String.iteri (fun i c -> Bytes.set mem (a + i) c) s in
  put 0x200 ("\x0f\x20\xc0" ^ "\x24\xfe" ^ "\x0f\x22\xc0" ^ "\xea" ^ le16 0x300 ^ le16 0);
  put 0x300 ("\xb8\x34\x12" ^ "\x8e\xd8" ^ hlt);
  check_halted "back to real mode" (run t);
  let x = sys t in
  check "PE is off" (x.Cpu86.saved_cr0 land 1) 0;
  check "DS selector" (Cpu86.seg t ds) 0x1234;
  check "DS base is selector * 16" x.Cpu86.saved_bases.(ds) 0x12340;
  check "DS limit stays 4GB" x.Cpu86.saved_limits.(ds) 0xffffffff;
  check "CS base" x.Cpu86.saved_bases.(1) 0;
  check "CS limit stays 64KB" x.Cpu86.saved_limits.(1) 0xffff;
  check_bool "CS.D off" x.Cpu86.saved_code32 false

(* PG: the first megabyte must be one PDE and 256 PTEs mapping linear to physical. *)
let test_paging () =
  let directory = 0x1000 and table = 0x2000 in
  let set_cr3 dir = "\x66\xb8" ^ le32 dir ^ "\x0f\x22\xd8" in
  let enter_paged =
    set_cr3 directory ^ "\x0f\x01\x16" ^ le16 gdtr_image ^ "\x0f\x20\xc0" ^ "\x66\x0d"
    ^ le32 0x80000001 ^ "\x0f\x22\xc0" ^ "\xea" ^ le16 code32_at ^ le16 0x08
  in
  let build ?(pde = table lor 3) ?(pte = fun i -> (i * 0x1000) lor 3) code =
    let t, mem = make enter_paged code in
    let put a s = String.iteri (fun i c -> Bytes.set mem (a + i) c) s in
    put directory (le32 pde);
    put 0x3000 (le32 (table lor 3));
    for i = 0 to 255 do put (table + (i * 4)) (le32 (pte i)) done;
    (t, mem)
  in
  let t, mem = build (load_ds 0x10 ^ "\xa1\x00\x03\x00\x00" ^ hlt) in
  Bytes.set mem 0x300 '\x77';
  check_halted "identity paging is accepted" (run t);
  check "CR0 has PG and PE" ((sys t).Cpu86.saved_cr0 land 0x80000001) 0x80000001;
  check "the access reads the physical address" (eax t land 0xff) 0x77;
  let stops name (t, _) part = check_stopped name (run t) part in
  stops "PDE not present" (build ~pde:0 hlt) "PDE 0 of CR3=00001000 is not present";
  stops "PTE not present" (build ~pte:(fun i -> if i = 7 then 0 else (i * 0x1000) lor 3) hlt)
    "PTE 7 is not present";
  stops "PTE maps another frame"
    (build ~pte:(fun i -> if i = 5 then 0x9003 else (i * 0x1000) lor 3) hlt)
    "PTE 5 maps 00009000, not the identity 00005000";
  stops "last PTE maps another frame"
    (build ~pte:(fun i -> if i = 255 then 0x1003 else (i * 0x1000) lor 3) hlt)
    "PTE 255 maps 00001000, not the identity 000ff000";
  (* the check names the stop and leaves CR0 alone *)
  let t, _ = build ~pde:0 hlt in
  ignore (run t);
  check "a refused PG leaves CR0 without PG" ((sys t).Cpu86.saved_cr0 land 0x80000000) 0;
  (* PG over a table that is the identity map after the guest rebuilt it is accepted;
     CR3 writes while PG is on are checked too *)
  let t, _ = build (mov_eax 0x3000 ^ "\x0f\x22\xd8" ^ hlt) in
  check_halted "CR3 to another identity directory" (run t);
  check "CR3 changed" (sys t).Cpu86.saved_cr3 0x3000;
  let t, _ = build (mov_eax 0x4000 ^ "\x0f\x22\xd8" ^ hlt) in
  check_stopped "CR3 to a directory with no PDE" (run t) "PDE 0 of CR3=00004000";
  check "CR3 stays" (sys t).Cpu86.saved_cr3 0x1000;
  (* PG with PE off, and PE off with PG on, are #GP *)
  let t, _ = make ("\x66\xb8" ^ le32 0x80000000 ^ "\x0f\x22\xc0") "" in
  let taken = ref None in
  Cpu86.set_int_hook t (fun v -> if !taken = None then taken := Some (v, Cpu86.dump_ip t));
  ignore (Cpu86.step t);
  ignore (Cpu86.step t);
  check "PG without PE is INT 13" (match !taken with Some (13, 6) -> 1 | _ -> 0) 1;
  let t, _ = build (mov_eax 0xfffffffe ^ "\x0f\x22\xc0") in
  check_stopped "PE off with PG on" (run t) "fault 13 in protected mode"

(* A saved 386 in protected mode gives back every cache and its EIP. *)
let test_state_round_trip () =
  let t, _ =
    make enter_flat
      (load_ds 0x10 ^ load_ss 0x20 ^ mov_ax_sel 0x28 ^ "\x0f\x00\xd0" ^ mov_ax_sel 0x30
       ^ "\x0f\x00\xd8" ^ mov_ax_sel 0x04 ^ mov_sreg 4 ^ mov_eax 0x20000 ^ "\xff\xe0")
  in
  (match run t with
   | Stopped _ | Halted -> ());
  let s = Cpu86.save_state t in
  check "EIP is saved whole" (s.Cpu86.saved_ip land 0xffff0000) 0x20000;
  let t2, _ = make "" "" in
  Cpu86.load_state t2 s;
  check_bool "load_state gives back the saved state" (Cpu86.save_state t2 = s) true;
  check "EIP survives" (Cpu86.dump_ip t2) (Cpu86.dump_ip t);
  let refused name x =
    let t3, _ = make "" "" in
    match Cpu86.load_state t3 { s with Cpu86.saved_386 = Some x } with
    | () -> check name 1 0
    | exception Invalid_argument _ -> ()
  in
  let x = sys t in
  refused "PG over a real-mode CR0" { x with Cpu86.saved_cr0 = 0x80000000 }

let () =
  test_enter ();
  test_segment_loads ();
  test_descriptor_fields ();
  test_null_selector ();
  test_code_sizes ();
  test_stack_width ();
  test_stops ();
  test_fetch_limit ();
  test_ldt_and_task_register ();
  test_queries ();
  test_return_to_real_mode ();
  test_paging ();
  test_state_round_trip ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end else print_endline "cpu86 protected mode: all passed"
