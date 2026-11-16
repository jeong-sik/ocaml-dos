(* Blocking DOS input is exercised through guest INT instructions. No test
   invokes the service handler directly or reissues an unfinished read. *)

let check name condition = if not condition then failwith name
let word n = String.init 2 (fun i -> Char.chr ((n lsr (8 * i)) land 0xff))
let mov_ax n = "\xb8" ^ word n
let mov_dx n = "\xba" ^ word n
let mov_cx n = "\xb9" ^ word n
let store off = "\xa3" ^ word off
let int21 = "\xcd\x21"
let exit = mov_ax 0x4c00 ^ int21
let marker = "\xb2\x58\xb4\x02" ^ int21
let run m = ignore (Dos_machine.run_until m ~max_steps:100 ~stop:(fun _ -> false))
let byte m off = Dos_state.rd8 m (0x10000 + off)
let read_word m off = Dos_state.rd16 m (0x10000 + off)
let bytes m off n = String.init n (fun i -> Char.chr (byte m (off + i)))
let write m off value = Dos_state.wr8 m (0x10000 + off) value
let screen_prefix m n = String.sub (Dos_machine.screen_text m) 0 n
let machine code = let m = Dos_machine.create () in Dos_machine.load_com m code; m
let save m = match Dos_snapshot.save m with
  | Ok s -> s | Error e -> failwith (Dos_snapshot.save_error_to_string e)
let restore s = match Dos_snapshot.restore s with
  | Ok m -> m | Error e -> failwith (Dos_snapshot.error_to_string e)

let test_byte ah =
  let m = machine (mov_ax ((ah lsl 8) lor 0xa5) ^ int21 ^ store 0x200 ^ marker ^ exit) in
  run m;
  check "empty DOS read blocks marker/exit" (not (Dos_machine.exited m) && screen_prefix m 1 = " ");
  check "empty read does not fabricate AL" (Cpu86.reg8 (Dos_machine.cpu_of m) 0 = 0xa5);
  let resumed = restore (save m) in
  List.iter (fun current ->
      Dos_machine.type_string current "QR";
      run current;
      check "DOS byte read exits after one key" (Dos_machine.exited current);
      check "DOS byte is delivered once" (byte current 0x200 = Char.code 'Q');
      check "second key remains queued" (Dos_state.peek_key current land 0xff = Char.code 'R');
      check "only AH01 echoes" (screen_prefix current (if ah = 1 then 2 else 1)
          = if ah = 1 then "QX" else "X")) [m; resumed];
  check "byte wait snapshot resumes identically" (save m = save resumed)

let test_extended_and_polls () =
  let m = machine (mov_ax 0x0700 ^ int21 ^ store 0x200
                   ^ mov_ax 0x0b00 ^ int21 ^ store 0x202
                   ^ mov_ax 0x0800 ^ int21 ^ store 0x204 ^ exit) in
  run m;
  Dos_machine.push_key m 0x5000;
  run m;
  check "extended key prefix returned" (byte m 0x200 = 0);
  check "DOS status sees pending scan without consuming" (byte m 0x202 = 0xff);
  check "next DOS byte read returns pending scan" (byte m 0x204 = 0x50);
  check "extended key consumed exactly once" (m.Dos_state.ext_scan_pending = 0 && not (Dos_state.key_pending m));
  let empty = machine (mov_dx 0xff ^ mov_ax 0x0600 ^ int21 ^ "\x9c\x58" ^ store 0x200
                       ^ mov_ax 0x0b00 ^ int21 ^ store 0x202 ^ marker ^ exit) in
  run empty;
  check "DOS06/0B remain nonblocking" (Dos_machine.exited empty && screen_prefix empty 1 = "X");
  check "DOS06 reports empty via ZF" (read_word empty 0x200 land Cpu86.f_zero <> 0);
  check "DOS0B reports no byte" (byte empty 0x202 = 0);
  let peek = machine (mov_ax 0x0b00 ^ int21 ^ store 0x200
                      ^ mov_ax 0x0b00 ^ int21 ^ store 0x202
                      ^ mov_ax 0x0800 ^ int21 ^ store 0x204 ^ exit) in
  Dos_machine.type_string peek "Q";
  run peek;
  check "DOS0B peeks without consuming" (byte peek 0x200 = 0xff && byte peek 0x202 = 0xff
                                         && byte peek 0x204 = Char.code 'Q')

let test_flush_once () =
  let m = machine (mov_ax 0x0800 ^ int21 ^ store 0x200
                   ^ mov_ax 0x0c08 ^ int21 ^ store 0x202 ^ marker ^ exit) in
  Dos_machine.push_key m 0x5000;
  Dos_machine.type_string m "old";
  run m;
  check "AH0C clears pending scan and queued typeahead"
    (byte m 0x200 = 0 && m.Dos_state.ext_scan_pending = 0 && not (Dos_state.key_pending m));
  check "AH0C subfunction remains blocked" (not (Dos_machine.exited m));
  let resumed = restore (save m) in
  List.iter (fun current ->
      Dos_machine.type_string current "Q";
      run current;
      check "resume does not flush the arriving key" (Dos_machine.exited current && byte current 0x202 = Char.code 'Q'))
    [m; resumed];
  check "flush wait snapshot resumes identically" (save m = save resumed)

let test_private_nonblocking_flags () =
  (* OR clears ZF before a far call through the saved old DOS vector. The
     delegated AH06 poll sets it, and the ROM IRET must return that result. *)
  let m = machine (mov_dx 0xff ^ mov_ax 0x0c06 ^ "\x09\xc0"
                   ^ "\x9c\xff\x1e\x00\x05\x9c\x58" ^ store 0x200 ^ exit) in
  Dos_state.wr16 m 0x10500 (Dos_state.rd16 m (0x21 * 4));
  Dos_state.wr16 m 0x10502 (Dos_state.rd16 m (0x21 * 4 + 2));
  run m;
  check "private AH0C/06 stays nonblocking" (Dos_machine.exited m);
  check "private AH0C returns the delegated poll ZF" (read_word m 0x200 land Cpu86.f_zero <> 0)

let line_machine fn capacity =
  let m = machine (mov_dx 0x300 ^ mov_ax fn ^ int21 ^ marker ^ exit) in
  write m 0x300 capacity;
  write m 0x301 2; (* Existing template count must not become new input. *)
  write m (0x302 + capacity) 0xa5;
  m

let test_line fn =
  let m = line_machine fn 5 in
  if fn = 0x0c0a then Dos_machine.type_string m "old";
  run m;
  check "line read initially blocks" (not (Dos_machine.exited m));
  Dos_machine.type_string m "a\tb";
  run m;
  check "partial line echoes once" (screen_prefix m 9 = "a       b");
  check "partial line cannot run caller" (not (Dos_machine.exited m));
  let saved = save m in
  let resumed = restore saved in
  check "partial line and echo widths survive snapshot" (save resumed = saved);
  List.iter (fun current ->
      Dos_machine.type_string current "\b\bbcdef";
      run current;
      check "backspace erases byte and tab echo" (screen_prefix current 9 = "abcd     ");
      check "full buffer still waits for CR" (not (Dos_machine.exited current));
      Dos_machine.type_string current "\bz\r";
      run current;
      check "CR completes line" (Dos_machine.exited current);
      check "line count excludes CR" (byte current 0x301 = 4);
      check "line bytes preserve edit and limit" (bytes current 0x302 5 = "abcz\r");
      check "buffer limit does not overwrite guard" (byte current 0x307 = 0xa5);
      check "AH0A echoes CR without adding LF" (screen_prefix current 4 = "Xbcz")) [m; resumed];
  check "partial line replay is identical" (save m = save resumed)

let test_line_limits () =
  let zero = line_machine 0x0a00 0 in
  run zero;
  check "zero-capacity read returns without input" (Dos_machine.exited zero && byte zero 0x301 = 2);
  let one = line_machine 0x0a00 1 in
  Dos_machine.type_string one "ab";
  run one;
  check "one-byte buffer reserves room for CR" (not (Dos_machine.exited one));
  Dos_machine.type_string one "\r";
  run one;
  check "one-byte buffer returns empty line plus CR" (byte one 0x301 = 0 && byte one 0x302 = 0x0d
                                                       && byte one 0x303 = 0xa5)

let console_read length destination =
  "\xbb\x00\x00" ^ mov_cx length ^ mov_dx destination ^ mov_ax 0x3f00 ^ int21

let test_console_read () =
  let m = machine (console_read 1 0x300 ^ store 0x200
                   ^ console_read 5 0x301 ^ store 0x202 ^ marker ^ exit) in
  write m 0x300 0xa5;
  run m;
  check "console read blocks empty input" (not (Dos_machine.exited m));
  Dos_machine.type_string m "ab";
  run m;
  check "short request still waits for complete cooked line"
    (not (Dos_machine.exited m) && byte m 0x300 = 0xa5 && screen_prefix m 2 = "ab");
  let partial = restore (save m) in
  List.iter (fun current ->
      Dos_machine.type_string current "\r";
      ignore (Dos_machine.step_result current);
      check "first read returns one byte and retains rest" (byte current 0x300 = Char.code 'a'
          && current.Dos_state.console_pending = "b\r\n")) [m; partial];
  let resumed = restore (save m) in
  List.iter (fun current ->
      run current;
      check "remaining line completes without another key" (Dos_machine.exited current);
      check "cooked read counts requested prefix and remainder" (read_word current 0x200 = 1 && read_word current 0x202 = 3);
      check "cooked line preserves CR/LF across reads" (bytes current 0x300 4 = "ab\r\n");
      check "remaining line consumed once" (current.Dos_state.console_pending = "")) [m; partial; resumed];
  check "both console snapshot cuts resume identically" (save m = save partial && save m = save resumed);
  let zero = machine (console_read 0 0x300 ^ store 0x200 ^ exit) in
  run zero;
  check "zero-length read does not block" (Dos_machine.exited zero && read_word zero 0x200 = 0);
  let eof = machine (console_read 5 0x300 ^ store 0x200 ^ exit) in
  Dos_machine.type_string eof "\026\r";
  run eof;
  check "cooked Ctrl-Z line returns EOF" (Dos_machine.exited eof && read_word eof 0x200 = 0)

let () =
  List.iter test_byte [0x01; 0x07; 0x08];
  test_extended_and_polls ();
  test_flush_once ();
  test_private_nonblocking_flags ();
  List.iter test_line [0x0a00; 0x0c0a];
  test_line_limits ();
  test_console_read ();
  print_endline "DOS console input: all passed"
