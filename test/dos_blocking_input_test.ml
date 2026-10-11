(* Execute real COM instructions through the installed INT hook. An empty
   blocking read cannot reach the following store, marker, or exit. *)

let check name condition = if not condition then failwith name

let word n = String.init 2 (fun i -> Char.chr ((n lsr (8 * i)) land 0xff))
let mov_ax n = "\xb8" ^ word n
let store off = "\xa3" ^ word off
let exit = "\xb8\x00\x4c\xcd\x21"
let marker = "\xb2\x58\xb4\x02\xcd\x21"
let bios_read ah = "\xb4" ^ String.make 1 (Char.chr ah) ^ "\xcd\x16"
let run m n = Dos_machine.run_until m ~max_steps:n ~stop:(fun _ -> false)
let read_word m off = Dos_state.rd16 m (0x10000 + off)

let machine code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  m

let save m = match Dos_snapshot.save m with
  | Ok s -> s | Error e -> failwith (Dos_snapshot.save_error_to_string e)

let restore s = match Dos_snapshot.restore s with
  | Ok m -> m | Error e -> failwith (Dos_snapshot.error_to_string e)

let test_bios_blocking ah =
  let m = machine (bios_read ah ^ store 0x200 ^ marker ^ exit) in
  let report = run m 100 in
  check "empty read stays live" (not (Dos_machine.exited m));
  check "empty read cannot print marker" ((Dos_machine.screen_text m).[0] = ' ');
  check "caller stays at decoded return IP" (Cpu86.dump_ip (Dos_machine.cpu_of m) = 0x104);
  check "no fabricated AX result" (Cpu86.reg16 (Dos_machine.cpu_of m) 0 = ah lsl 8);
  check "idle waits consume bounded budget" (report.machine_steps = 100);
  check "idle waits are not guest instructions" (report.instructions = 2);
  check "wait ends only on budget" (report.stop_reason = Dos_machine.Budget_exhausted);
  check "wait advances device clock" (report.elapsed_cycles > 0);
  check "waiting does not use HLT" (not (Dos_machine.halted m));
  let saved = save m in
  let resumed = restore saved in
  check "wait snapshot roundtrip" (save resumed = saved);
  List.iter (fun current ->
      Dos_machine.push_key current 0x1051;
      Dos_machine.push_key current 0x1352;
      ignore (run current 100);
      check "key completes and exits" (Dos_machine.exited current);
      check "exact key returned" (read_word current 0x200 = 0x1051);
      check "marker runs after input" ((Dos_machine.screen_text current).[0] = 'X');
      check "one read consumes one key" (Dos_state.peek_key current = 0x1352);
      check "completed continuation removed" (current.Dos_state.input_continuations = []))
    [m; resumed];
  check "restored wait has identical outcome" (save m = save resumed)

let test_polling () =
  let code = bios_read 0x01 ^ store 0x200 ^ bios_read 0x11 ^ store 0x202
             ^ bios_read 0x00 ^ store 0x204 ^ exit in
  let m = machine code in
  Dos_machine.push_key m 0x1051;
  ignore (run m 100);
  check "polls then read exit" (Dos_machine.exited m);
  List.iter (fun off -> check "poll peeks without consuming" (read_word m off = 0x1051))
    [0x200; 0x202; 0x204];
  check "only blocking read consumed the key" (not (Dos_state.key_pending m));
  List.iter (fun ah ->
      let m = machine (bios_read ah ^ marker ^ exit) in
      ignore (run m 100);
      check "empty BIOS poll returns" (Dos_machine.exited m);
      check "poll never creates a continuation" (m.Dos_state.input_continuations = []))
    [0x01; 0x11]

let test_irq_wait () =
  let m = machine (bios_read 0 ^ store 0x200 ^ marker ^ exit) in
  ignore (run m 2);
  (* IRQ handler increments a memory counter, then IRET. It cannot complete
     the waiting read, even if a key arrives before that IRET. *)
  String.iteri (fun i c -> Dos_state.wr8 m (0x10400 + i) (Char.code c))
    "\x2e\xfe\x06\x00\x03\xcf";
  Dos_state.wr16 m 0x20 0x400;
  Dos_state.wr16 m 0x22 0x1000;
  m.Dos_state.pending_irq0 <- true;
  ignore (Dos_machine.step_result m);
  check "IRQ executes while waiting" (Dos_state.rd8 m 0x10300 = 1);
  check "IRQ did not discard wait" (List.length m.Dos_state.input_continuations = 1);
  let saved = save m in
  let resumed = restore saved in
  check "snapshot inside IRQ carries underlying wait" (save resumed = saved);
  List.iter (fun current ->
      Dos_machine.push_key current 0x1051;
      ignore (Dos_machine.step_result current); (* IRET *)
      check "IRQ does not consume the waiting key" (Dos_state.key_pending current);
      check "IRET reaches suspended boundary" (Cpu86.dump_ip (Dos_machine.cpu_of current) = 0x104);
      ignore (run current 100);
      check "IRQ wait resumes with the key" (read_word current 0x200 = 0x1051);
      check "IRQ wait reaches exit" (Dos_machine.exited current)) [m; resumed];
  check "IRQ snapshot resumes identically" (save m = save resumed)

let test_old_vector_chain () =
  let m = machine (bios_read 0 ^ store 0x200 ^ marker ^ exit) in
  let old_off = Dos_state.rd16 m (0x16 * 4) in
  let old_seg = Dos_state.rd16 m (0x16 * 4 + 2) in
  Dos_state.wr16 m 0x10500 old_off;
  Dos_state.wr16 m 0x10502 old_seg;
  (* inc cs:[300h]; pushf; call far cs:[500h]; iret *)
  String.iteri (fun i c -> Dos_state.wr8 m (0x10400 + i) (Char.code c))
    "\x2e\xfe\x06\x00\x03\x9c\x2e\xff\x1e\x00\x05\xcf";
  Dos_state.wr16 m (0x16 * 4) 0x400;
  Dos_state.wr16 m (0x16 * 4 + 2) 0x1000;
  ignore (run m 100);
  check "old vector does not reenter guest hook" (Dos_state.rd8 m 0x10300 = 1);
  check "old-vector read blocks caller" (not (Dos_machine.exited m));
  check "host wait enables timer interrupts" (Dos_state.interrupts_enabled m);
  let m = restore (save m) in
  Dos_machine.push_key m 0x1051;
  ignore (run m 100);
  check "hook chain completes once" (Dos_state.rd8 m 0x10300 = 1 && Dos_machine.exited m);
  check "hook chain returns key" (read_word m 0x200 = 0x1051)

let exec_through_old_vector ~carry () =
  (* Keep the parent's real stack within its resized allocation. *)
  let code = "\xbc\xfe\x0f\xbb\x00\x01\xb4\x4a\xcd\x21"
             ^ mov_ax 0x4b00 ^ "\xba\x00\x06\xbb\x20\x06"
             ^ (if carry then "\xf9" else "\xf8") ^ "\xcd\x21"
             ^ store 0x702 ^ "\x9c\x58" ^ store 0x700 ^ exit in
  let m = machine code in
  let old_off = Dos_state.rd16 m (0x21 * 4) in
  let old_seg = Dos_state.rd16 m (0x21 * 4 + 2) in
  Dos_state.wr16 m 0x10500 old_off;
  Dos_state.wr16 m 0x10502 old_seg;
  (* A tail chain preserves the interrupt's single owned return frame. *)
  String.iteri (fun i c -> Dos_state.wr8 m (0x10400 + i) (Char.code c))
    "\x2e\xff\x2e\x00\x05";
  String.iteri (fun i c -> Dos_state.wr8 m (0x10600 + i) (Char.code c)) "CHILD.COM\000";
  Dos_state.wr16 m (0x21 * 4) 0x400;
  Dos_state.wr16 m (0x21 * 4 + 2) 0x1000;
  m

let test_exec_return_frame () =
  List.iter (fun child_exit -> List.iter (fun carry ->
      let m = exec_through_old_vector ~carry () in
      let child = "\xa1\x02\x00" ^ store 0x700 ^ child_exit in
      Dos_machine.mount_file m "CHILD.COM" child;
      let report = Dos_machine.run_until m ~max_steps:100
          ~stop:(fun current -> current.Dos_state.psp_seg <> 0x1000) in
      check "old-vector EXEC starts child" (report.stop_reason = Dos_machine.Stop_requested);
      let child_base = m.Dos_state.psp_seg lsl 4 in
      check "EXEC never writes flags into child PSP"
        (Dos_state.rd16 m (child_base + 2) = Dos_dos.conv_mem_top);
      let saved = save m in
      let resumed = restore saved in
      check "EXEC return-frame ownership survives snapshot" (save resumed = saved);
      List.iter (fun current ->
          ignore (run current 100);
          check "child exit returns to parent" (Dos_machine.exited current);
          check "EXEC success clears caller CF through IRET"
            (read_word current 0x700 land Cpu86.f_carry = 0);
          check "child observed its intact PSP"
            (Dos_state.rd16 current (child_base + 0x700) = Dos_dos.conv_mem_top)) [m; resumed];
      check "EXEC snapshot resumes identically" (save m = save resumed)) [false; true])
    [mov_ax 0x4c2a ^ "\xcd\x21";
     "\xba\x20\x00" ^ mov_ax 0x312a ^ "\xcd\x21";
     "\xba\x00\x02\xcd\x27"];
  let missing = exec_through_old_vector ~carry:false () in
  ignore (run missing 100);
  check "same-frame EXEC failure returns CF" (read_word missing 0x700 land Cpu86.f_carry <> 0);
  check "same-frame EXEC failure returns DOS error" (read_word missing 0x702 = 2)

let test_deferred_trap () =
  let m = machine (bios_read 0 ^ store 0x200 ^ exit) in
  ignore (run m 1); (* MOV AH,0 precedes the single-stepped INT. *)
  let cpu = Dos_machine.cpu_of m in
  Cpu86.set_flags cpu ((Cpu86.flags cpu lor Cpu86.f_trap) land lnot Cpu86.f_interrupt);
  ignore (run m 1);
  check "wait admits IRQ with caller IF clear" (Dos_state.interrupts_enabled m);
  String.iteri (fun i c -> Dos_state.wr8 m (0x10400 + i) (Char.code c))
    "\x2e\xfe\x06\x00\x03\xcf";
  Dos_state.wr16 m 4 0x400;
  Dos_state.wr16 m 6 0x1000;
  Dos_machine.push_key m 0x1051;
  ignore (Dos_machine.step_result m);
  check "deferred trap precedes caller store" (read_word m 0x200 = 0);
  check "deferred trap enters debugger" (Cpu86.dump_ip cpu = 0x400);
  check "trap return is immediately after INT"
    (Dos_state.rd16 m (Dos_state.physical (Cpu86.seg cpu 2) (Cpu86.reg16 cpu 4)) = 0x104);
  ignore (Dos_machine.step_result m); (* debugger counter *)
  ignore (Dos_machine.step_result m); (* IRET *)
  check "wait does not leak enabled IF to caller" (not (Dos_state.interrupts_enabled m));
  check "caller TF restored" (Cpu86.flags cpu land Cpu86.f_trap <> 0);
  check "debugger handled the completed INT once" (Dos_state.rd8 m 0x10300 = 1)

let () =
  List.iter test_bios_blocking [0x00; 0x10];
  test_polling ();
  test_irq_wait ();
  test_old_vector_chain ();
  test_exec_return_frame ();
  test_deferred_trap ();
  print_endline "blocking input: all passed"
