(* INT 15h — RBIL INT 15/AH=88h(rb-1529), 4Fh(rb-1387), C0h(rb-1594).
   이 기계는 1MB 라 확장 메모리가 0 이고, 모르는 기능은 AH=86h·CF=1 이다.
   게스트가 벡터를 가로채 옛 벡터로 넘겨도 CF 가 호출자에게 가야 한다. *)

let failed = ref 0
let check name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d (0x%x) want %d (0x%x)\n%!" name got got want want
  end

let w v = String.init 2 (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let psp = 0x10000
let out = 0x7000
let old_vector = 0x6000
let handler_at = 0x0600
let bx_in = 0x1234
let cf = 1
let quit = "\xb8\x00\x4c\xcd\x21"

(* AX·BX·FLAGS 를 out+6k 에 둔다 *)
let call k ~ax ~carry =
  let at = out + (6 * k) in
  "\xb8" ^ w ax ^ "\xbb" ^ w bx_in ^ (if carry then "\xf9" else "\xf8") ^ "\xcd\x15\x9c"
  ^ "\xa3" ^ w at ^ "\x89\x1e" ^ w (at + 2) ^ "\x58\xa3" ^ w (at + 4)

(* 옛 벡터를 저장하고 처리기 "jmp far cs:[old_vector]" 를 건다 *)
let hook =
  "\xb8\x15\x35\xcd\x21" ^ "\x89\x1e" ^ w old_vector ^ "\x8c\x06" ^ w (old_vector + 2)
  ^ "\xb8\x15\x25\xba" ^ w handler_at ^ "\xcd\x21"

let program =
  let body =
    String.concat ""
      [ call 0 ~ax:0x8800 ~carry:true;
        call 1 ~ax:0x4F1E ~carry:false;
        call 2 ~ax:0xC000 ~carry:false;
        call 3 ~ax:0x86AA ~carry:false;
        call 4 ~ax:0x0000 ~carry:false;
        hook;
        call 5 ~ax:0x8800 ~carry:true;
        call 6 ~ax:0xC000 ~carry:false;
        quit ]
  in
  body ^ String.make (handler_at - 0x100 - String.length body) '\x90'
  ^ "\x2e\xff\x2e" ^ w old_vector

let () =
  let m = Dos_machine.create () in
  Dos_machine.load_com m program;
  ignore (Dos_machine.run_until m ~max_steps:10_000 ~stop:(fun _ -> false));
  check "program exits" (Bool.to_int (Dos_machine.exited m)) 1;
  let r k i = Dos_state.rd16 m (psp + out + (6 * k) + (2 * i)) in
  let expect name k ~ax ~carry =
    check (name ^ ": AX") (r k 0) ax;
    check (name ^ ": BX kept") (r k 1) bx_in;
    check (name ^ ": CF") (r k 2 land cf) carry
  in
  expect "88h no extended memory" 0 ~ax:0 ~carry:0;
  expect "4Fh keeps the scan code" 1 ~ax:0x4F1E ~carry:cf;
  expect "C0h unsupported" 2 ~ax:0x8600 ~carry:cf;
  expect "86h unsupported, AL kept" 3 ~ax:0x86AA ~carry:cf;
  expect "00h unsupported" 4 ~ax:0x8600 ~carry:cf;
  expect "hooked 88h" 5 ~ax:0 ~carry:0;
  expect "hooked C0h" 6 ~ax:0x8600 ~carry:cf

let () =
  if !failed = 0 then print_endline "dos int15: all passed"
  else begin
    Printf.eprintf "dos int15: %d failures\n%!" !failed;
    exit 1
  end
