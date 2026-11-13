(* 받지 못한 INT 호출의 관찰자 — Dos_machine.on_unserved. 기능 갈래가 없는
   호스트 서비스와, 프로그램이 부른 INT 가 ROM IRET 스텁으로 간 경우만
   알리고, 기계 동작은 그대로여야 한다. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let quit = "\xb8\x00\x4c\xcd\x21"
let int_ ~ax v = "\xb8" ^ w ax ^ "\xcd" ^ String.make 1 (Char.chr v)
let handler_at = 0x0200                     (* COM 오프셋: iret 한 바이트 *)
(* 처리기를 IRET 스텁과 같은 오프셋 0020h 로 가리킨다 — 세그먼트까지 봐야
   스텁이 아닌 것을 안다. 101E:0020 = 1000:0200. *)
let stub_off = 0x20
let hook_2f =
  "\x1e" ^ "\xb8" ^ w (0x1000 + ((handler_at - stub_off) / 16)) ^ "\x8e\xd8"
  ^ "\xba" ^ w stub_off ^ "\xb8\x2f\x25\xcd\x21" ^ "\x1f"

let program =
  let body =
    String.concat ""
      [ int_ ~ax:0x5200 0x21;               (* 이 기계에 없는 DOS 기능 *)
        int_ ~ax:0x3000 0x21;               (* 있는 기능 *)
        int_ ~ax:0xFE00 0x10;
        int_ ~ax:0x0E41 0x10;               (* 있는 기능 *)
        int_ ~ax:0xFF00 0x16;
        int_ ~ax:0xFF00 0x1A;
        int_ ~ax:0x0000 0x11;               (* 있는 기능 *)
        int_ ~ax:0xC000 0x15;               (* 지원 안 함으로 답하는 기능 *)
        int_ ~ax:0x00FF 0x33;
        int_ ~ax:0x1600 0x2F;               (* IRET 스텁 *)
        hook_2f;
        int_ ~ax:0x1600 0x2F;               (* 이제 프로그램의 처리기 *)
        quit ]
  in
  body ^ String.make (handler_at - 0x100 - String.length body) '\x90' ^ "\xcf"

let kind_code = function Dos_state.No_function -> 0 | Dos_state.Iret_stub -> 1

let () =
  let run observe =
    let m = Dos_machine.create () in
    let seen = ref [] in
    if observe then
      Dos_machine.on_unserved m (fun kind ~vector ~ax -> seen := (kind, vector, ax) :: !seen);
    Dos_machine.load_com m program;
    (* 곧바로 IRQ0: ROM 의 INT 8 처리기가 INT 1Ch(IRET 스텁)를 부른다 *)
    m.Dos_state.pending_irq0 <- true;
    ignore (Dos_machine.run_until m ~max_steps:10_000 ~stop:(fun _ -> false));
    (m, List.rev !seen)
  in
  let m, seen = run true in
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  check "IRQ0 ran" (Dos_machine.tick_count m) 1;
  let want =
    [ (Dos_state.No_function, 0x21, 0x5200);
      (Dos_state.No_function, 0x10, 0xFE00);
      (Dos_state.No_function, 0x16, 0xFF00);
      (Dos_state.No_function, 0x1A, 0xFF00);
      (Dos_state.No_function, 0x15, 0xC000);
      (Dos_state.No_function, 0x33, 0x00FF);
      (Dos_state.Iret_stub, 0x2F, 0x1600) ]
  in
  check "event count" (List.length seen) (List.length want);
  List.iteri
    (fun i ((k, v, ax), (wk, wv, wax)) ->
      let name = Printf.sprintf "event %d" i in
      check (name ^ " kind") (kind_code k) (kind_code wk);
      check (name ^ " vector") v wv;
      check (name ^ " AX") ax wax)
    (List.combine (List.filteri (fun i _ -> i < List.length want) seen)
       (List.filteri (fun i _ -> i < List.length seen) want));
  (* 관찰자가 있어도 없어도 기계는 같은 곳에 닿는다 *)
  let quiet, _ = run false in
  check "same screen" (Dos_machine.screen_digest quiet) (Dos_machine.screen_digest m);
  check "same exit" (Dos_machine.exit_code quiet) (Dos_machine.exit_code m)

let () =
  if !failed = 0 then print_endline "dos unserved: all passed"
  else begin
    Printf.eprintf "dos unserved: %d failures\n%!" !failed;
    exit 1
  end
