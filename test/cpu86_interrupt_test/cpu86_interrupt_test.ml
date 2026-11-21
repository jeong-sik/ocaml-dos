(* The 80386 delivering INT n, faults and the single-step trap through the IDT in
   protected mode (RFC 0005 stage 3c-2b), and IRET coming back. There is no real-chip
   data for protected mode: the expected values are read off the 386 PRM (INT and IRET
   Operation, 9.4-9.10), not measured.

   Memory is 1MB. The real-mode part at 0000h loads GDTR and IDTR and enters protected
   mode, the protected part is at 0100h, the GDT at 0800h (image at 07f0h), the IDT image
   at 07e0h and the IDT at 1000h. Vector v has a handler at 2000h + v * 20h that is a single
   HLT unless a test writes its own, so a halted core says which handler ran: EIP stands
   one byte past it. The stack top is 9000h in a flat data segment.

     08h flat 32-bit code           10h flat data
     18h 64KB code, D=0             20h 64KB data, B=0
     28h conforming code, DPL 3     30h non-conforming code, DPL 3
     38h code, not present          40h data, not present
     48h 32-bit code, 512 bytes     50h LDT descriptor (a system type)
     78h and above are beyond the GDT *)

let failed = ref 0

let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %x want %x\n%!" name got want
  end

let le16 v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let le32 v = String.init 4 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))

let desc ~base ~limit ~access ~flags =
  le16 (limit land 0xffff) ^ le16 (base land 0xffff)
  ^ String.make 1 (Char.chr ((base lsr 16) land 0xff))
  ^ String.make 1 (Char.chr access)
  ^ String.make 1 (Char.chr ((flags lsl 4) lor ((limit lsr 16) land 0xf)))
  ^ String.make 1 (Char.chr ((base lsr 24) land 0xff))

let gdt_base = 0x800
let gdtr_image = 0x7f0
let idtr_image = 0x7e0
let idt_base = 0x1000
let code32_at = 0x100
let stack_top = 0x9000
let scratch = 0x8000
let mem_size = 0x100000
let granular_big = 0xc
let big = 0x4

let gdt =
  String.concat ""
    [ String.make 8 '\000';
      desc ~base:0 ~limit:0xfffff ~access:0x9a ~flags:granular_big;
      desc ~base:0 ~limit:0xfffff ~access:0x92 ~flags:granular_big;
      desc ~base:0 ~limit:0xffff ~access:0x9a ~flags:0;
      desc ~base:0 ~limit:0xffff ~access:0x92 ~flags:0;
      desc ~base:0 ~limit:0xfffff ~access:0xfe ~flags:granular_big;
      desc ~base:0 ~limit:0xfffff ~access:0xfa ~flags:granular_big;
      desc ~base:0 ~limit:0xfffff ~access:0x1a ~flags:granular_big;
      desc ~base:0 ~limit:0xfffff ~access:0x12 ~flags:granular_big;
      desc ~base:0 ~limit:0x1ff ~access:0x9a ~flags:big;
      desc ~base:0 ~limit:0xff ~access:0x82 ~flags:0 ]

let sel_code32 = 0x08 and sel_data = 0x10 and sel_conforming3 = 0x28
let sel_code3 = 0x30 and sel_code_absent = 0x38 and sel_data_absent = 0x40
let sel_code_small = 0x48 and sel_system = 0x50
let sel_beyond = 0x78

(* An IDT gate. Access 8Eh is a 386 interrupt gate, 8Fh a 386 trap gate, 86h and 87h the
   286 ones (a 16-bit frame), 85h a task gate; clearing 80h makes the gate not present. *)
let gate ?(high = 0) ~selector ~offset ~access () =
  le16 (offset land 0xffff) ^ le16 selector ^ "\000" ^ String.make 1 (Char.chr access)
  ^ le16 (high lor ((offset lsr 16) land 0xffff))

let interrupt_gate32 = 0x8e and trap_gate32 = 0x8f and interrupt_gate16 = 0x86
let trap_gate16 = 0x87 and task_gate = 0x85 and absent_interrupt_gate32 = 0x0e

let handler_at v = 0x2000 + (v * 0x20)
let gate_to ?(selector = sel_code32) ~access v = gate ~selector ~offset:(handler_at v) ~access ()
let default_gate v = gate_to ~access:interrupt_gate32 v
let absent v = gate_to ~access:absent_interrupt_gate32 v
let idt_entries = 0x80

let hlt = "\xf4"

(* real mode: lgdt; lidt; mov eax,cr0; or al,1; mov cr0,eax; jmp 0008:code32_at *)
let enter =
  "\x0f\x01\x16" ^ le16 gdtr_image ^ "\x0f\x01\x1e" ^ le16 idtr_image
  ^ "\x0f\x20\xc0" ^ "\x0c\x01" ^ "\x0f\x22\xc0" ^ "\xea" ^ le16 code32_at ^ le16 sel_code32

(* mov ax,10h; mov ss,ax; mov ds,ax; mov esp,stack_top: the 66h prefix is 16-bit in 32-bit code *)
let prologue = "\x66\xb8" ^ le16 sel_data ^ "\x8e\xd0\x8e\xd8" ^ "\xbc" ^ le32 stack_top
let body_at = code32_at + String.length prologue

let make ?(real = enter) ?(gates = []) ?(handlers = []) ?(idt_limit_entries = idt_entries) body =
  let mem = Bytes.make mem_size '\000' in
  let put a s = String.iteri (fun i c -> Bytes.set mem (a + i) c) s in
  put 0 real;
  put code32_at (prologue ^ body);
  put gdt_base gdt;
  put gdtr_image (le16 (String.length gdt - 1) ^ le32 gdt_base);
  put idtr_image (le16 ((idt_limit_entries * 8) - 1) ^ le32 idt_base);
  for v = 0 to idt_entries - 1 do
    put (idt_base + (v * 8)) (match List.assoc_opt v gates with Some g -> g | None -> default_gate v);
    put (handler_at v) (match List.assoc_opt v handlers with Some h -> h | None -> hlt)
  done;
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

type outcome = Halted | Stopped of string

let run t =
  let rec go n =
    if n > 5000 then Stopped "ran away"
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

let check_halted name = function
  | Halted -> ()
  | Stopped m -> incr failed; Printf.eprintf "FAIL %s: stopped with %S\n%!" name m

let check_stopped name outcome part =
  match outcome with
  | Stopped m when contains m part -> ()
  | Stopped m ->
    incr failed;
    Printf.eprintf "FAIL %s: stopped with %S, want a message with %S\n%!" name m part
  | Halted -> incr failed; Printf.eprintf "FAIL %s: ran to HLT, want a stop with %S\n%!" name part

let esp t = Cpu86.reg32 t 4
let eax t = Cpu86.reg32 t 0
let ebx t = Cpu86.reg32 t 3
let dword mem a = Int32.to_int (Bytes.get_int32_le mem a) land 0xffffffff
let word mem a = Bytes.get_uint16_le mem a

let flag_if = 0x200 and flag_tf = 0x100 and flag_nt = 0x4000 and flag_vm = 0x20000

(* Which handler ran: a halted core stands one byte past its HLT. -1 is none. *)
let vector_run t =
  let ip = Cpu86.dump_ip t - 1 in
  if ip >= 0x2000 && (ip - 0x2000) mod 0x20 = 0 then (ip - 0x2000) / 0x20 else -1

let nop = "\x90"
let sti = "\xfb"
let pushfd = "\x9c"
let popfd = "\x9d"
let iretd = "\xcf"
let int_n n = "\xcd" ^ String.make 1 (Char.chr n)
let mov_eax v = "\xb8" ^ le32 v
let mov_ebx v = "\xbb" ^ le32 v
let mov_esp v = "\xbc" ^ le32 v
let push_imm v = "\x68" ^ le32 v
let or_stack_with v = "\x81\x0c\x24" ^ le32 v        (* or dword [esp], v *)
let add_stack_dword n = "\x83\x04\x24" ^ String.make 1 (Char.chr n)
let add_esp n = "\x83\xc4" ^ String.make 1 (Char.chr n)
let mov_mem_dword addr v = "\xc7\x05" ^ le32 addr ^ le32 v
let mov_ax_sel sel = "\x66\xb8" ^ le16 sel
let mov_ds_ax = "\x8e\xd8"
let mov_ss_ax = "\x8e\xd0"

(* mov eax,0 ; div eax: the divide is the second instruction, five bytes in *)
let divide_by_zero = mov_eax 0 ^ "\xf7\xf0"
let divide_at = body_at + 5

(* mov eax,1 ; bound eax,[scratch]: the bounds there are 0..0, so the bound faults *)
let out_of_bounds = mov_eax 1 ^ "\x62\x05" ^ le32 scratch
let bound_at = body_at + 5

(* ---------- the frame ---------- *)

(* INT n through a 32-bit interrupt gate: EFLAGS, CS and EIP pushed in that order (the next
   instruction is the return address), then IF, TF and NT cleared. *)
let test_interrupt_gate () =
  let t, mem = make (sti ^ int_n 0x40 ^ mov_eax 1) in
  check_halted "INT through an interrupt gate" (run t);
  check "handler" (vector_run t) 0x40;
  check "ESP after three dwords" (esp t) (stack_top - 12);
  check "return EIP is the next instruction" (dword mem (esp t)) (body_at + 3);
  check "pushed CS" (dword mem (esp t + 4)) sel_code32;
  check "pushed EFLAGS has IF" (dword mem (esp t + 8) land flag_if) flag_if;
  check "pushed EFLAGS has the reserved bit 1" (dword mem (esp t + 8) land 2) 2;
  check "an interrupt gate clears IF" (Cpu86.flags t land flag_if) 0;
  check "CS" (Cpu86.seg t 1) sel_code32;
  check "the instruction after INT did not run: EAX still holds the prologue's selector" (eax t) sel_data

(* A trap gate leaves IF alone. Both clear NT, and the pushed image keeps it. *)
let test_trap_gate () =
  let set_nt = sti ^ pushfd ^ or_stack_with flag_nt ^ popfd ^ int_n 0x41 in
  let t, mem = make ~gates:[ (0x41, gate_to ~access:trap_gate32 0x41) ] set_nt in
  check_halted "INT through a trap gate" (run t);
  check "handler" (vector_run t) 0x41;
  check "IF stays on" (Cpu86.flags t land flag_if) flag_if;
  check "NT is cleared" (Cpu86.flags t land flag_nt) 0;
  check "the pushed image keeps NT" (dword mem (esp t + 8) land flag_nt) flag_nt;
  let t, mem = make set_nt in
  check_halted "INT through an interrupt gate with NT set" (run t);
  check "an interrupt gate clears IF" (Cpu86.flags t land flag_if) 0;
  check "an interrupt gate clears NT" (Cpu86.flags t land flag_nt) 0;
  check "the pushed image keeps IF" (dword mem (esp t + 8) land flag_if) flag_if

(* TF set by POPFD traps after the next instruction: vector 1, returning to the one after. *)
let test_single_step () =
  let body = sti ^ pushfd ^ or_stack_with flag_tf ^ popfd ^ nop ^ nop in
  let t, mem = make body in
  check_halted "single-step trap" (run t);
  check "vector 1" (vector_run t) 1;
  check "TF is cleared in the handler" (Cpu86.flags t land flag_tf) 0;
  check "IF is cleared by the interrupt gate" (Cpu86.flags t land flag_if) 0;
  check "the pushed image has TF" (dword mem (esp t + 8) land flag_tf) flag_tf;
  check "returns to the instruction after the one that ran" (dword mem (esp t)) (body_at + 11);
  let t, _ = make ~gates:[ (1, gate_to ~access:trap_gate32 1) ] body in
  check_halted "single-step trap through a trap gate" (run t);
  check "a trap gate keeps IF" (Cpu86.flags t land flag_if) flag_if

(* A 16-bit (286) gate pushes FLAGS, CS and IP as words and takes only the low offset word. *)
let test_gate_widths () =
  let t, mem =
    make ~gates:[ (0x44, gate ~high:0xdead ~selector:sel_code32 ~offset:(handler_at 0x44)
                     ~access:interrupt_gate16 ()) ]
      (sti ^ int_n 0x44)
  in
  check_halted "INT through a 16-bit interrupt gate" (run t);
  check "the high offset word of a 16-bit gate is ignored" (vector_run t) 0x44;
  check "ESP after three words" (esp t) (stack_top - 6);
  check "IP" (word mem (esp t)) (body_at + 3);
  check "CS" (word mem (esp t + 2)) sel_code32;
  check "FLAGS has IF" (word mem (esp t + 4) land flag_if) flag_if;
  check "a 16-bit interrupt gate clears IF" (Cpu86.flags t land flag_if) 0;
  let t, _ = make ~gates:[ (0x45, gate_to ~access:trap_gate16 0x45) ] (sti ^ int_n 0x45) in
  check_halted "INT through a 16-bit trap gate" (run t);
  check "a 16-bit trap gate keeps IF" (Cpu86.flags t land flag_if) flag_if;
  check "ESP after three words" (esp t) (stack_top - 6);
  let t, _ = make (int_n 0x46) in
  check_halted "INT through a 32-bit gate" (run t);
  check "ESP after three dwords" (esp t) (stack_top - 12);
  (* the RPL of the gate's selector is dropped: CPL stays 0 *)
  let t, mem =
    make ~gates:[ (0x47, gate_to ~selector:(sel_code32 lor 3) ~access:interrupt_gate32 0x47) ]
      (int_n 0x47)
  in
  check_halted "a gate selector with an RPL" (run t);
  check "CS has no RPL in the handler" (Cpu86.seg t 1) sel_code32;
  check "the pushed CS is the old one" (dword mem (esp t + 4)) sel_code32

(* ---------- INT 3 and INTO ---------- *)

let test_int3_into () =
  let t, mem = make "\xcc" in
  check_halted "INT 3" (run t);
  check "vector 3" (vector_run t) 3;
  check "returns after the one-byte INT 3" (dword mem (esp t)) (body_at + 1);
  (* mov al,7fh ; add al,1 sets OF *)
  let t, mem = make ("\xb0\x7f\x04\x01" ^ "\xce") in
  check_halted "INTO with OF" (run t);
  check "vector 4" (vector_run t) 4;
  check "returns after INTO" (dword mem (esp t)) (body_at + 5);
  let t, _ = make ("\xce" ^ mov_eax 0x55 ^ hlt) in
  check_halted "INTO without OF" (run t);
  check "INTO without OF falls through" (eax t) 0x55;
  check "no handler ran" (vector_run t) (-1)

(* ---------- error codes ---------- *)

(* [body] raises a fault whose handler runs. The handler's frame has the error code on top
   of EIP, CS and EFLAGS. *)
let faulted name ?gates ?handlers ?idt_limit_entries body ~vector ~code ~return_ip =
  let t, mem = make ?gates ?handlers ?idt_limit_entries body in
  check_halted name (run t);
  check (name ^ ": handler") (vector_run t) vector;
  check (name ^ ": error code") (dword mem (esp t)) code;
  check (name ^ ": return EIP") (dword mem (esp t + 4)) return_ip;
  check (name ^ ": pushed CS") (dword mem (esp t + 8)) sel_code32;
  check (name ^ ": ESP after four dwords") (esp t) (stack_top - 16)

let df = 8 and np = 11 and ss = 12 and gp = 13

(* A broken gate for INT n. The INT is the faulting instruction, a software origin (EXT is
   0), so an IDT error code is vector * 8 + 2 and a selector error code is the selector
   with its RPL dropped (386 PRM INT Operation). *)
let test_idt_errors () =
  let at_int = body_at in
  let int40 = int_n 0x40 in
  let idt_code = (0x40 * 8) + 2 in
  let broken name gate_bytes ~vector ~code =
    faulted name ~gates:[ (0x40, gate_bytes) ] int40 ~vector ~code ~return_ip:at_int
  in
  faulted "vector beyond the IDT limit" ~idt_limit_entries:0x40 int40 ~vector:gp ~code:idt_code
    ~return_ip:at_int;
  let t, _ = make ~idt_limit_entries:0x41 int40 in
  check_halted "the last vector inside the IDT limit" (run t);
  check "the last vector is delivered" (vector_run t) 0x40;
  List.iter
    (fun (name, access) ->
       broken name (gate_to ~access 0x40) ~vector:gp ~code:idt_code)
    [ ("a data descriptor in the IDT", 0x92); ("an LDT descriptor in the IDT", 0x82);
      ("a TSS in the IDT", 0x89); ("a call gate in the IDT", 0x8c);
      ("the type comes before presence", 0x0c) ];
  broken "a gate that is not present" (absent 0x40) ~vector:np ~code:idt_code;
  broken "a null handler selector" (gate_to ~selector:0 ~access:interrupt_gate32 0x40)
    ~vector:gp ~code:0;
  broken "a handler selector beyond the GDT, RPL dropped"
    (gate_to ~selector:(sel_beyond lor 3) ~access:interrupt_gate32 0x40) ~vector:gp ~code:sel_beyond;
  broken "a handler that is a data segment"
    (gate_to ~selector:sel_data ~access:interrupt_gate32 0x40) ~vector:gp ~code:sel_data;
  broken "a handler that is a system descriptor"
    (gate_to ~selector:sel_system ~access:interrupt_gate32 0x40) ~vector:gp ~code:sel_system;
  broken "a handler code segment that is not present"
    (gate_to ~selector:(sel_code_absent lor 3) ~access:interrupt_gate32 0x40)
    ~vector:np ~code:sel_code_absent;
  broken "a non-conforming handler with DPL above CPL"
    (gate_to ~selector:sel_code3 ~access:interrupt_gate32 0x40) ~vector:gp ~code:sel_code3;
  broken "a handler EIP beyond its code segment limit"
    (gate_to ~selector:sel_code_small ~access:interrupt_gate32 0x40) ~vector:gp ~code:0;
  (* a conforming code segment is taken whatever its DPL (PRM: "conforming OR DPL = CPL").
     확인 필요: 현대 Intel SDM 이 DPL 이 CPL 보다 큰 conforming 을 막는지 읽지 못했다. *)
  let t, _ =
    make ~gates:[ (0x40, gate_to ~selector:sel_conforming3 ~access:interrupt_gate32 0x40) ] int40
  in
  check_halted "a conforming handler" (run t);
  check "the conforming handler runs" (vector_run t) 0x40;
  check "CS" (Cpu86.seg t 1) sel_conforming3

(* A fault a program did not ask for (EXT is 1): the first fault is a benign #BR, so a fault
   in its delivery is handled on its own (PRM 9.8.8, Table 9-4). *)
let test_processor_event_errors () =
  let idt_code = (5 * 8) + 2 + 1 in
  let broken name gate_bytes ~vector ~code =
    faulted name ~gates:[ (5, gate_bytes) ] out_of_bounds ~vector ~code ~return_ip:bound_at
  in
  broken "#BR into a gate that is not present" (absent 5) ~vector:np ~code:idt_code;
  broken "#BR into a data descriptor" (gate_to ~access:0x92 5) ~vector:gp ~code:idt_code;
  broken "#BR into a null handler selector" (gate_to ~selector:0 ~access:interrupt_gate32 5)
    ~vector:gp ~code:1;
  broken "#BR into a handler selector beyond the GDT"
    (gate_to ~selector:(sel_beyond lor 3) ~access:interrupt_gate32 5) ~vector:gp
    ~code:(sel_beyond lor 1);
  broken "#BR into a handler that is not present"
    (gate_to ~selector:sel_code_absent ~access:interrupt_gate32 5) ~vector:np
    ~code:(sel_code_absent lor 1)

(* INT n pushes no error code, even for the vectors a fault gives one (8, 10-14). A fault
   pushes it when it has one (#DF 8, #TS 10, #NP 11, #SS 12, #GP 13) and does not for the
   others (#DE 0, #BR 5). *)
let test_error_code_vectors () =
  List.iter
    (fun v ->
       let t, _ = make (int_n v) in
       check_halted (Printf.sprintf "INT %d" v) (run t);
       check (Printf.sprintf "INT %d handler" v) (vector_run t) v;
       check (Printf.sprintf "INT %d pushes no error code" v) (esp t) (stack_top - 12))
    [ 8; 10; 11; 12; 13; 14 ];
  faulted "#NP from a segment load" (mov_ax_sel sel_data_absent ^ mov_ds_ax) ~vector:np
    ~code:sel_data_absent ~return_ip:(body_at + 4);
  faulted "#SS from a stack segment load" (mov_ax_sel sel_data_absent ^ mov_ss_ax) ~vector:ss
    ~code:sel_data_absent ~return_ip:(body_at + 4);
  faulted "#GP from a selector beyond the GDT" (mov_ax_sel sel_beyond ^ mov_ds_ax) ~vector:gp
    ~code:sel_beyond ~return_ip:(body_at + 4);
  faulted "#GP(0) from a null SS" (mov_ax_sel 0 ^ mov_ss_ax) ~vector:gp ~code:0
    ~return_ip:(body_at + 4);
  let no_code name body ~vector ~return_ip =
    let t, mem = make body in
    check_halted name (run t);
    check (name ^ ": handler") (vector_run t) vector;
    check (name ^ ": no error code") (esp t) (stack_top - 12);
    check (name ^ ": return EIP") (dword mem (esp t)) return_ip
  in
  no_code "#DE" divide_by_zero ~vector:0 ~return_ip:divide_at;
  no_code "#BR" out_of_bounds ~vector:5 ~return_ip:bound_at

(* ---------- double fault ---------- *)

(* A fault while delivering a fault: two contributory ones (#DE, #TS, #NP, #SS, #GP) make a
   #DF with error code 0, a benign first one does not (PRM 9.8.8 Tables 9-3 and 9-4). A
   fault while delivering the #DF is a shutdown, which the core reports as Unsupported. *)
let test_double_fault () =
  faulted "#DE into a gate that is not present" ~gates:[ (0, absent 0) ] divide_by_zero
    ~vector:df ~code:0 ~return_ip:divide_at;
  faulted "#BR, then #NP, whose gate is not present either" ~gates:[ (5, absent 5); (np, absent np) ]
    out_of_bounds ~vector:df ~code:0 ~return_ip:bound_at;
  let t, _ = make ~gates:[ (0, absent 0); (df, absent df) ] divide_by_zero in
  check_stopped "a fault delivering the #DF" (run t) "shutdown";
  let t, _ =
    make ~gates:[ (0, absent 0); (df, gate_to ~selector:0 ~access:interrupt_gate32 df) ]
      divide_by_zero
  in
  check_stopped "a null #DF handler selector" (run t) "shutdown"

(* ---------- stack room ---------- *)

let test_stack_room () =
  (* 12 bytes of room is exactly an INT frame *)
  let t, _ = make (mov_esp 12 ^ int_n 0x40) in
  check_halted "ESP 12" (run t);
  check "ESP 12: delivered" (vector_run t) 0x40;
  check "ESP 12: the stack is used up" (esp t) 0;
  (* ESP 10 is short of it: #SS(0), delivered through a 16-bit gate that needs 8 bytes *)
  let t, mem =
    make ~gates:[ (ss, gate_to ~access:interrupt_gate16 ss) ] (mov_esp 10 ^ int_n 0x40)
  in
  check_halted "ESP 10" (run t);
  check "ESP 10: the #SS handler" (vector_run t) ss;
  check "ESP 10: four words" (esp t) 2;
  check "ESP 10: error code 0" (word mem (esp t)) 0;
  check "ESP 10: return IP is the INT" (word mem (esp t + 2)) (body_at + 5);
  (* no room for the #SS frame either, nor for the #DF one *)
  let t, _ = make (mov_esp 4 ^ int_n 0x40) in
  check_stopped "ESP 4" (run t) "shutdown"

let test_task_gate () =
  let t, _ = make ~gates:[ (0x42, gate ~selector:0x30 ~offset:0 ~access:task_gate ()) ] (int_n 0x42) in
  check_stopped "a task gate" (run t) "task gate"

(* ---------- IRET ---------- *)

let test_iret_pairs () =
  let marker = mov_mem_dword scratch 0x1234 in
  (* INT n and IRETD *)
  let t, mem = make ~handlers:[ (0x40, marker ^ iretd) ] (sti ^ int_n 0x40 ^ mov_eax 0x55 ^ hlt) in
  check_halted "INT n and IRETD" (run t);
  check "ran past the INT" (eax t) 0x55;
  check "the handler ran" (dword mem scratch) 0x1234;
  check "ESP restored" (esp t) stack_top;
  check "IF restored" (Cpu86.flags t land flag_if) flag_if;
  check "CS restored" (Cpu86.seg t 1) sel_code32;
  (* a fault with no error code returns to the faulting instruction: the handler skips the DIV *)
  let t, _ =
    make ~handlers:[ (0, add_stack_dword 2 ^ iretd) ] (divide_by_zero ^ mov_ebx 0x77 ^ hlt)
  in
  check_halted "#DE handler skips the DIV" (run t);
  check "ran past the DIV" (ebx t) 0x77;
  check "ESP restored after #DE" (esp t) stack_top;
  (* a fault with an error code: the handler drops the code, skips the 2-byte MOV DS,AX *)
  let t, _ =
    make ~handlers:[ (np, add_esp 4 ^ add_stack_dword 2 ^ iretd) ]
      (mov_ax_sel sel_data_absent ^ mov_ds_ax ^ mov_eax 0x77 ^ hlt)
  in
  check_halted "#NP handler drops the code and skips the load" (run t);
  check "ran past the load" (eax t) 0x77;
  check "DS kept its old value" (Cpu86.seg t 3) sel_data;
  check "ESP restored after #NP" (esp t) stack_top;
  (* a 16-bit gate returns with a 16-bit IRET (66h IRETD) *)
  let t, mem =
    make ~gates:[ (0x44, gate_to ~access:interrupt_gate16 0x44) ]
      ~handlers:[ (0x44, marker ^ "\x66" ^ iretd) ] (sti ^ int_n 0x44 ^ mov_eax 0x66 ^ hlt)
  in
  check_halted "a 16-bit gate and IRET" (run t);
  check "ran past the INT" (eax t) 0x66;
  check "the 16-bit handler ran" (dword mem scratch) 0x1234;
  check "ESP restored after the 16-bit frame" (esp t) stack_top;
  check "IF restored after the 16-bit frame" (Cpu86.flags t land flag_if) flag_if;
  (* a hand-built frame returns the same way *)
  let target = body_at + 13 in
  let t, _ = make (push_imm 0x202 ^ "\x6a\x08" ^ push_imm target ^ iretd ^ mov_eax 0x99 ^ hlt) in
  check_halted "IRETD with a hand-built frame" (run t);
  check "IRETD jumped to the frame's EIP" (eax t) 0x99;
  check "ESP restored" (esp t) stack_top

(* IRET with NT set is a task return, and a popped EFLAGS with VM set is a return to
   virtual-8086 mode (PRM IRET Operation). Both are stage 6, so the core stops. *)
let test_iret_refusals () =
  let set_nt = pushfd ^ or_stack_with flag_nt ^ popfd in
  let t, _ = make (set_nt ^ iretd) in
  check_stopped "IRETD with NT set" (run t) "NT";
  let t, _ = make (set_nt ^ "\x66" ^ iretd) in
  check_stopped "IRET with NT set" (run t) "NT";
  let t, _ = make (push_imm (flag_vm lor 2) ^ "\x6a\x08" ^ push_imm 0x2000 ^ iretd) in
  check_stopped "IRETD with VM in the popped EFLAGS" (run t) "virtual-8086";
  (* real mode is not touched: NT set before the IRETD, and VM in the image, change nothing *)
  let real =
    "\xb8\x02\x40\x50\x9d"                         (* mov ax,4002h; push ax; popf *)
    ^ "\x66" ^ push_imm (flag_vm lor flag_nt lor 2)
    ^ "\x66\x6a\x00" ^ "\x66" ^ push_imm 0x40 ^ "\x66\xcf"
  in
  let t, _ = make ~real:(real ^ String.make (0x40 - String.length real) '\x90' ^ hlt) nop in
  for _ = 1 to 3 do ignore (Cpu86.step t) done;
  check "real mode: NT is set" (Cpu86.flags t land flag_nt) flag_nt;
  check_halted "real mode IRETD" (run t);
  check "real mode IRETD returned to 0000:0040" (Cpu86.dump_ip t) 0x41

let () =
  test_interrupt_gate ();
  test_trap_gate ();
  test_single_step ();
  test_gate_widths ();
  test_int3_into ();
  test_idt_errors ();
  test_processor_event_errors ();
  test_error_code_vectors ();
  test_double_fault ();
  test_stack_room ();
  test_task_gate ();
  test_iret_pairs ();
  test_iret_refusals ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end else print_endline "cpu86 interrupts: all passed"
