(* test386.asm (github.com/barotto/test386.asm) against the bare I80386,
   RFC 0005 stage 0.

   The ROM is not in this repository (GPL-3.0); TEST386_BIN names a 64KB
   build. Without it the test is skipped. The ROM sits at physical F0000h,
   the CPU starts at F000:FFF0 as after a reset, and the ROM reports progress
   by writing POST codes to port 190h. A failed check jumps to the ROM's
   error routine, which halts; the last POST code names the test. 0xFF is
   the end.

   INT n and faults go through the real-mode IVT the ROM sets up, the way the
   chip does it: FLAGS, CS, IP pushed, IF and TF cleared.

   What passes at this stage: every real-mode POST (00h-06h), then POST 08h,
   where the protected-mode tests begin and the CPU stops at the first
   instruction it does not have yet (a 32-bit LGDT). A halt, a run out of
   steps, or a stop before POST 08h fails. Later stages raise
   [expected_prefix] as the CPU learns protected mode. *)

let rom_size = 0x10000
let rom_base = 0xF0000
let memory_size = 0x100000
let post_port = 0x190
let reset_cs = 0xF000
let reset_ip = 0xFFF0
let post_done = 0xFF
let step_limit = 50_000_000
let reg_sp = 4
let seg_cs = 1
let seg_ss = 2
let ivt_entry_bytes = 4
let unconnected_port = 0xFF

(* The POST codes test386 writes in order up to where this CPU stops. 07h is
   not used by test386. *)
let expected_prefix = [ 0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06; 0x08 ]

type stop =
  | Halted of string     (* the error routine, or a HLT nobody expected *)
  | Step_limit of string
  | Unsupported of string

let rec is_prefix p l =
  match p, l with
  | [], _ -> true
  | _ :: _, [] -> false
  | x :: p', y :: l' -> x = y && is_prefix p' l'

(* [true] when this stage's expectation holds. *)
let passes posts stop =
  match List.rev posts, stop with
  | last :: _, _ when last = post_done -> true
  | _, (Halted _ | Step_limit _) -> false
  | _, Unsupported _ -> is_prefix expected_prefix posts

let run_rom rom =
  let mem = Bytes.make memory_size '\000' in
  Bytes.blit_string rom 0 mem rom_base rom_size;
  (* A20 is closed: addresses past 1MB wrap. The ROM range ignores writes. *)
  let addr a = a land (memory_size - 1) in
  let read a = Char.code (Bytes.get mem (addr a)) in
  let write a v =
    let a = addr a in
    if a < rom_base then Bytes.set mem a (Char.chr (v land 0xFF))
  in
  let posts = ref [] in
  let port_out p v = if p = post_port then posts := (v land 0xFF) :: !posts in
  let cpu =
    Cpu86.create ~model:Cpu86.I80386 ~read ~write
      ~port_in:(fun _ -> unconnected_port) ~port_out ()
  in
  let read16 a = read a lor (read (a + 1) lsl 8) in
  let push16 v =
    let sp = (Cpu86.reg16 cpu reg_sp - 2) land 0xFFFF in
    Cpu86.set_reg16 cpu reg_sp sp;
    let ss = Cpu86.seg cpu seg_ss in
    write (Cpu86.physical cpu ~seg:ss ~off:sp) (v land 0xFF);
    write (Cpu86.physical cpu ~seg:ss ~off:((sp + 1) land 0xFFFF)) (v lsr 8)
  in
  let deliver n =
    let flags = Cpu86.flags cpu in
    push16 flags;
    push16 (Cpu86.seg cpu seg_cs);
    push16 (Cpu86.dump_ip cpu);
    Cpu86.set_flags cpu (flags land lnot (Cpu86.f_interrupt lor Cpu86.f_trap));
    Cpu86.set_ip cpu (read16 (n * ivt_entry_bytes));
    Cpu86.set_seg cpu seg_cs (read16 ((n * ivt_entry_bytes) + 2))
  in
  Cpu86.set_int_hook cpu deliver;
  Cpu86.set_seg cpu seg_cs reset_cs;
  Cpu86.set_ip cpu reset_ip;
  let where n =
    Printf.sprintf "%04x:%04x after %d steps" (Cpu86.seg cpu seg_cs) (Cpu86.dump_ip cpu) n
  in
  let rec go n =
    if Cpu86.halted cpu then Halted (where n)
    else if n >= step_limit then Step_limit (where n)
    else
      match Cpu86.step cpu with
      | _ -> go (n + 1)
      | exception Cpu86.Unsupported m -> Unsupported (Printf.sprintf "%s, %s" m (where n))
  in
  let stop = go 0 in
  (List.rev !posts, stop)

let describe = function
  | Halted at -> "HLT at " ^ at
  | Step_limit at -> "step limit at " ^ at
  | Unsupported m -> "Unsupported " ^ m

let () =
  match Sys.getenv_opt "TEST386_BIN" with
  | None -> print_endline "cpu86 test386: TEST386_BIN unset, skipped"
  | Some path ->
    let rom = In_channel.with_open_bin path In_channel.input_all in
    if String.length rom <> rom_size then begin
      Printf.eprintf "cpu86 test386: %s is %d bytes, want %d\n" path (String.length rom) rom_size;
      exit 1
    end;
    let posts, stop = run_rom rom in
    Printf.printf "cpu86 test386: POST %s\n  %s\n"
      (String.concat " " (List.map (Printf.sprintf "%02X") posts))
      (describe stop);
    if not (passes posts stop) then exit 1
