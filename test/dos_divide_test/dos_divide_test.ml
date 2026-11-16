(* A division by zero on the machine's 286: DOS's own INT 0 ends the
   program, and a program's INT 0 handler finds the DIV in its frame. *)

let failed = ref 0

let check name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine image =
  let m = Dos_machine.create () in
  Dos_machine.load_com m image;
  ignore (Dos_machine.run_until m ~max_steps:10_000 ~stop:(fun _ -> false));
  m

let contains s sub =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let test_dos_handler () =
  (* xor bl,bl; div bl; mov ax,4C07h; int 21h *)
  let m = machine "\x30\xdb\xf6\xf3\xb8\x07\x4c\xcd\x21" in
  check "DOS ends the program" (Dos_machine.exited m);
  check "with exit code 0, not the program's own" (Dos_machine.exit_code m = 0);
  check "and says so on the console" (contains (Dos_machine.screen_text m) "Divide overflow")

(* The handler stores the return IP its frame holds, steps it past the
   two-byte DIV, and returns; the program then exits with code 5. *)
let test_program_handler () =
  let code =
    String.concat ""
      [ "\xb8\x00\x25";           (* 0x100 mov ax,2500h *)
        "\xba\x14\x01";           (* 0x103 mov dx,0114h *)
        "\xcd\x21";               (* 0x106 int 21h *)
        "\x30\xdb";               (* 0x108 xor bl,bl *)
        "\xf6\xf3";               (* 0x10a div bl *)
        "\xb8\x05\x4c";           (* 0x10c mov ax,4C05h *)
        "\xcd\x21";               (* 0x10f int 21h *)
        "\x90\x90\x90";           (* 0x111 *)
        "\x55";                   (* 0x114 push bp *)
        "\x89\xe5";               (* 0x115 mov bp,sp *)
        "\x8b\x46\x02";           (* 0x117 mov ax,[bp+2] *)
        "\x2e\xa3\x26\x01";       (* 0x11a mov cs:[0126h],ax *)
        "\x83\x46\x02\x02";       (* 0x11e add word [bp+2],2 *)
        "\x5d";                   (* 0x122 pop bp *)
        "\xcf";                   (* 0x123 iret *)
        "\x90\x90";               (* 0x124 *)
        "\x00\x00" ]              (* 0x126 the IP the frame held *)
  in
  let m = machine code in
  let psp = m.Dos_state.psp_seg lsl 4 in
  check "the program's handler let it go on" (Dos_machine.exited m && Dos_machine.exit_code m = 5);
  check "the frame held the DIV itself" (Dos_state.rd16 m (psp + 0x126) = 0x10a)

let () =
  test_dos_handler ();
  test_program_handler ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos divide: all passed"
