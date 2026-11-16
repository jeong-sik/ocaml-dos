(* INT 21h AH=34h 와 INDOS — MS-DOS 4.0 MISC.ASM $GET_INDOS_FLAG,
   CONST2.ASM SWAP_START(ErrorMode 다음 INDOS), DISP.ASM 의 INC/DEC INDOS. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w n = String.init 2 (fun i -> Char.chr ((n lsr (8 * i)) land 0xff))
let psp = 0x10000
let quit = "\xb8\x00\x4c\xcd\x21"
let sda = 0x0070
let in_dos_phys = (sda * 16) + 1
let error_mode_phys = sda * 16
let seen = 0x300
let unseen = 0xEE
let handler_at = 0x400

let machine code =
  let m = Dos_machine.create () in
  Dos_machine.load_com m code;
  m

let run m n = ignore (Dos_machine.run_until m ~max_steps:n ~stop:(fun _ -> false))

let save m = match Dos_snapshot.save m with
  | Ok s -> s | Error e -> failwith (Dos_snapshot.save_error_to_string e)
let restore s = match Dos_snapshot.restore s with
  | Ok m -> m | Error e -> failwith (Dos_snapshot.error_to_string e)

(* ---------- AH=34h ---------- *)

let () =
  let probe carry at =
    (if carry then "\xf9" else "\xf8") ^ "\xb8\x77\x34" ^ "\xcd\x21" ^ "\x9c"
    ^ "\x89\x1e" ^ w at ^ "\x8c\x06" ^ w (at + 2) ^ "\xa3" ^ w (at + 4) ^ "\x8f\x06" ^ w (at + 6)
  in
  let m = machine (probe true 0x200 ^ probe false 0x210 ^ quit) in
  run m 1000;
  check "34h program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let word a = Dos_state.rd16 m (psp + a) in
  List.iter
    (fun (at, cf) ->
      let name = Printf.sprintf "34h CF=%d" cf in
      check (name ^ ": BX") (word at) 1;
      check (name ^ ": ES") (word (at + 2)) sda;
      check (name ^ ": AX") (word (at + 4)) 0x3477;
      check (name ^ ": CF kept") (word (at + 6) land 1) cf)
    [ (0x200, 1); (0x210, 0) ];
  check "INDOS outside a call" (Dos_state.rd8 m in_dos_phys) 0;
  check "ErrorMode" (Dos_state.rd8 m error_mode_phys) 0

(* ---------- 기다리는 동안 IRQ0 가 본 INDOS ----------

   처리기: push ds; push ax; mov ax,0070h; mov ds,ax; mov al,[0001];
   cs: mov [0300],al; pop ax; pop ds; iret *)
let handler =
  "\x1e\x50" ^ "\xb8" ^ w sda ^ "\x8e\xd8" ^ "\xa0" ^ w 1 ^ "\x2e\xa2" ^ w seen
  ^ "\x58\x1f\xcf"

let irq_during_wait read =
  let m = machine (read ^ "\xa3" ^ w 0x200 ^ quit) in
  run m 2;
  String.iteri (fun i c -> Dos_state.wr8 m (psp + handler_at + i) (Char.code c)) handler;
  Dos_state.wr8 m (psp + seen) unseen;
  Dos_state.wr16 m 0x20 handler_at;
  Dos_state.wr16 m 0x22 (psp / 16);
  m.Dos_state.pending_irq0 <- true;
  run m 30;
  m

let finish name m =
  Dos_machine.push_key m 0x1E61;
  run m 1000;
  check (name ^ ": exits") (Bool.to_int (Dos_machine.exited m)) 1;
  check (name ^ ": INDOS after") (Dos_state.rd8 m in_dos_phys) 0

let () =
  let dos_read = "\xb4\x01\xcd\x21" in
  let m = irq_during_wait dos_read in
  check "DOS 01h wait: still waiting" (Bool.to_int (Dos_machine.exited m)) 0;
  check "DOS 01h wait: IRQ saw INDOS" (Dos_state.rd8 m (psp + seen)) 1;
  check "DOS 01h wait: INDOS" (Dos_state.rd8 m in_dos_phys) 1;
  let resumed = restore (save m) in
  check "snapshot: INDOS" (Dos_state.rd8 resumed in_dos_phys) 1;
  (* 이 기능 전에 찍은 스냅숏: 기다리는 읽기는 있는데 바이트는 0 이다.
     INDOS 는 기다리는 읽기에서 나오니, 되살릴 때 다시 쓴다. *)
  let old = restore (save m) in
  Dos_state.wr8 old in_dos_phys 0;
  let rederived = restore (save old) in
  check "old snapshot: INDOS re-derived" (Dos_state.rd8 rederived in_dos_phys) 1;
  finish "DOS 01h" m;
  check "DOS 01h: AL" (Dos_state.rd8 m (psp + 0x200)) 0x61;
  finish "DOS 01h restored" resumed;
  let m = irq_during_wait "\xb4\x00\xcd\x16" in
  check "BIOS wait: IRQ saw INDOS" (Dos_state.rd8 m (psp + seen)) 0;
  check "BIOS wait: INDOS" (Dos_state.rd8 m in_dos_phys) 0;
  finish "BIOS 00h" m

let () =
  if !failed = 0 then print_endline "dos in_dos: all passed"
  else begin
    Printf.eprintf "dos in_dos: %d failures\n%!" !failed;
    exit 1
  end
