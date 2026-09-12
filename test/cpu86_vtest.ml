(* SingleStepTests/8086 (실칩 Intel P80C86A-2 생성) 상태 기반 검증.
   각 케이스: 초기 레지스터/플래그/RAM 세팅 → 명령 1개 → final regs/ram 비교.
   프리페치 큐와 사이클은 모델 밖 — 비교하지 않는다.

   판정을 셋으로 나눈다:
     pass   전부 일치
     fail   레지스터·IP·세그먼트·RAM 이 다르거나, Intel 이 값을 정의한
            플래그가 다르다. 이건 고쳐야 할 결함이다.
     undef  Intel 문서가 "정의되지 않음" 이라고 적은 플래그만 다르다.
            실칩은 값을 내지만 프로그램이 기대면 안 되는 자리다.

   실행: SST8086_DIR(기본 /tmp/sst8086/v1), SST_FILES(콤마 목록, 기본 00).
   fail 이 하나라도 있으면 종료 코드 1. *)

let dir = try Sys.getenv "SST8086_DIR" with Not_found -> "/tmp/sst8086/v1"

(* 남은 차이를 적어 둔 래칫. 적힌 수를 넘으면 실패, 밑돌면 알려준다. *)
(* dune 는 runtest 로 돌 때만 이 파일을 빌드 디렉터리에 복사한다.
   실행 파일을 직접 부르는 경우도 있으니 소스 자리도 같이 본다. *)
let baseline_path =
  try Sys.getenv "SST_BASELINE"
  with Not_found ->
    let exe_dir = Filename.dirname Sys.argv.(0) in
    let candidates =
      [ Filename.concat exe_dir "sst8086-baseline.txt";
        Filename.concat exe_dir "../../../test/sst8086-baseline.txt";
        "test/sst8086-baseline.txt" ]
    in
    (match List.find_opt Sys.file_exists candidates with
     | Some p -> p
     | None -> List.hd candidates)

let baseline =
  let tbl = Hashtbl.create 8 in
  (try
     let ic = open_in baseline_path in
     (try
        while true do
          let line = String.trim (input_line ic) in
          if line <> "" && line.[0] <> '#' then
            match String.split_on_char ' ' line with
            | [ name; n ] -> Hashtbl.replace tbl name (int_of_string n)
            | _ -> ()
        done
      with End_of_file -> ());
     close_in ic
   with Sys_error _ -> ());
  tbl

let allowed base = try Hashtbl.find baseline base with Not_found -> 0
let files = try Sys.getenv "SST_FILES" with Not_found -> "00"

let mem = Bytes.make (1024 * 1024) '\000'
let read_mem a = Char.code (Bytes.get mem (a land 0xfffff))
let write_mem a v = Bytes.set mem (a land 0xfffff) (Char.chr (v land 0xff))

let reg_index = function
  | "ax" -> Some 0 | "cx" -> Some 1 | "dx" -> Some 2 | "bx" -> Some 3
  | "sp" -> Some 4 | "bp" -> Some 5 | "si" -> Some 6 | "di" -> Some 7
  | _ -> None

let seg_index = function
  | "es" -> Some 0 | "cs" -> Some 1 | "ss" -> Some 2 | "ds" -> Some 3
  | _ -> None

(* Intel 8086 매뉴얼이 "undefined" 로 적은 플래그. 명령마다 다르다 —
   MUL 은 CF·OF 만 정의하고, DIV 는 여섯 개 전부를 정의하지 않는다.
   그룹2 는 회전량이 1 일 때만 OF 를 정의하고 AF 는 늘 미정의다. *)
let undefined_flags base =
  let cf = Cpu86.f_carry and pf = Cpu86.f_parity and af = Cpu86.f_aux in
  let zf = Cpu86.f_zero and sf = Cpu86.f_sign and ov = Cpu86.f_overflow in
  match base with
  | "F6.4" | "F6.5" | "F7.4" | "F7.5" -> sf lor zf lor af lor pf
  | "F6.6" | "F6.7" | "F7.6" | "F7.7" -> cf lor ov lor sf lor zf lor af lor pf
  | "27" | "2F" -> ov                       (* daa/das: OF 만 미정의 *)
  | "37" | "3F" -> ov lor sf lor zf lor pf  (* aaa/aas: CF·AF 만 정의 *)
  | "D4" | "D5" -> af lor cf lor ov         (* aam/aad: SF·ZF·PF 만 정의 *)
  | _ ->
    if String.length base >= 2 && base.[0] = 'D' && base.[1] >= '0'
       && base.[1] <= '3'
    then (if base.[1] <= '1' then af else af lor ov)
    else 0

type verdict = Pass | Undef_only of string list | Fail of string list

let apply_state t regs ram =
  let open Yojson.Safe.Util in
  List.iter
    (fun (k, v) ->
      let n = to_int v in
      match reg_index k with
      | Some i -> Cpu86.set_reg16 t i n
      | None ->
        (match seg_index k with
         | Some s -> Cpu86.set_seg t s n
         | None ->
           if k = "ip" then Cpu86.set_ip t n
           else if k = "flags" then Cpu86.set_flags t n
           else ()))
    (to_assoc (member "regs" regs));
  List.iter (fun cell ->
      match to_list cell with
      | [ a; v ] -> write_mem (to_int a) (to_int v)
      | _ -> ())
    (to_list (member "ram" ram))

let flag_names =
  [ (Cpu86.f_carry, "CF"); (Cpu86.f_parity, "PF"); (Cpu86.f_aux, "AF");
    (Cpu86.f_zero, "ZF"); (Cpu86.f_sign, "SF"); (Cpu86.f_trap, "TF");
    (Cpu86.f_interrupt, "IF"); (Cpu86.f_direction, "DF");
    (Cpu86.f_overflow, "OF") ]

let compare_final t regs ram ~undef =
  let open Yojson.Safe.Util in
  let hard = ref [] and soft = ref [] in
  List.iter
    (fun (k, v) ->
      let want = to_int v in
      if k = "flags" then begin
        let got = Cpu86.flags t in
        List.iter
          (fun (bit, name) ->
            if (got land bit) <> (want land bit) then begin
              let d =
                Printf.sprintf "%s: got=%d want=%d" name
                  (if got land bit <> 0 then 1 else 0)
                  (if want land bit <> 0 then 1 else 0)
              in
              if bit land undef <> 0 then soft := d :: !soft
              else hard := d :: !hard
            end)
          flag_names
      end
      else begin
        let got =
          match reg_index k with
          | Some i -> Cpu86.reg16 t i
          | None ->
            (match seg_index k with
             | Some s -> Cpu86.seg t s
             | None -> if k = "ip" then Cpu86.dump_ip t else -1)
        in
        if got <> want then
          hard := Printf.sprintf "%s: got=%04x want=%04x" k got want :: !hard
      end)
    (to_assoc (member "regs" regs));
  List.iter (fun cell ->
      match to_list cell with
      | [ a; v ] ->
        let a = to_int a and v = to_int v in
        if read_mem a <> v then
          hard := Printf.sprintf "ram[%x]: got=%02x want=%02x"
                    a (read_mem a) v :: !hard
      | _ -> ())
    (to_list (member "ram" ram));
  match (!hard, !soft) with
  | [], [] -> Pass
  | [], s -> Undef_only s
  | h, s -> Fail (h @ s)

(* 실칩처럼 IVT 로 인터럽트를 전달한다: flags/cs/ip 를 밀고 벡터로
   점프한 뒤 IF·TF 를 끈다. 케이스가 그 스택 프레임까지 비교하므로
   훅이 없으면 INT·INTO·0 나눗셈이 전부 미구현으로 샌다. *)
let install_int_hook t =
  Cpu86.set_int_hook t (fun v ->
      let flags = Cpu86.flags t in
      let push w =
        Cpu86.set_reg16 t 4 ((Cpu86.reg16 t 4 - 2) land 0xffff);
        let a = ((Cpu86.seg t 2 lsl 4) + Cpu86.reg16 t 4) land 0xfffff in
        write_mem a (w land 0xff);
        write_mem ((a + 1) land 0xfffff) ((w lsr 8) land 0xff)
      in
      push flags;
      push (Cpu86.seg t 1);
      push (Cpu86.dump_ip t);
      Cpu86.set_flags t
        (flags land lnot Cpu86.f_interrupt land lnot Cpu86.f_trap);
      let rd a = read_mem a lor (read_mem (a + 1) lsl 8) in
      Cpu86.set_ip t (rd (v * 4));
      Cpu86.set_seg t 1 (rd (v * 4 + 2)))

let run_file base =
  let path = Filename.concat dir (base ^ ".json.gz") in
  let ic = Unix.open_process_in ("gzcat " ^ Filename.quote path) in
  let json = Yojson.Safe.from_channel ic in
  ignore (Unix.close_process_in ic);
  let cases = Yojson.Safe.Util.to_list json in
  let undef = undefined_flags base in
  let total = ref 0 and pass = ref 0 and fail = ref 0 and soft = ref 0 in
  let unsup = ref 0 in
  let fail_samples = ref [] in
  List.iter (fun case ->
      incr total;
      let open Yojson.Safe.Util in
      let name = to_string (member "name" case) in
      Bytes.fill mem 0 (1024 * 1024) '\000';
      let t =
        (* 실칩 스위트는 8086 세대를 재는 것이므로 모델을 못 박는다 —
           186 이 가져간 opcode 자리는 여기선 거울 명령이다. *)
        Cpu86.create ~model:Cpu86.I8086 ~read:read_mem ~write:write_mem
          ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) ()
      in
      install_int_hook t;
      apply_state t (member "initial" case) (member "initial" case);
      (try
         ignore (Cpu86.step t);
         match compare_final t (member "final" case) (member "final" case)
                 ~undef with
         | Pass -> incr pass
         | Undef_only _ -> incr soft
         | Fail bad ->
           incr fail;
           if List.length !fail_samples < 4 then
             fail_samples := (name, bad) :: !fail_samples
       with Cpu86.Unsupported _ -> incr unsup))
    cases;
  let budget = allowed base in
  Printf.printf "%s: total=%d pass=%d fail=%d undef=%d unsupported=%d%s\n%!"
    base !total !pass !fail !soft !unsup
    (if budget > 0 then Printf.sprintf " (래칫 %d)" budget else "");
  if !fail > budget then
    List.iter (fun (name, bad) ->
        Printf.eprintf "  FAIL %s — %s\n%!" name (String.concat "; " bad))
      (List.rev !fail_samples);
  if !fail < budget then
    Printf.eprintf
      "  %s: 실패 %d < 래칫 %d — sst8086-baseline.txt 를 조이세요\n%!"
      base !fail budget;
  (max 0 (!fail - budget), !unsup)

let () =
  (* 데이터가 없으면(예: CI) 이 스위트는 스킵 — runtest 는 그린으로
     통과시킨다. 로컬에 /tmp/sst8086 가 있으면 전 opcode 검증이 돈다. *)
  if not (Sys.file_exists (Filename.concat dir "00.json.gz")) then
    print_endline "cpu86 vtest: dataset missing, skipped"
  else begin
    let bad = ref 0 and missing = ref 0 in
    List.iter
      (fun f ->
        let fl, un = run_file f in
        bad := !bad + fl;
        missing := !missing + un)
      (String.split_on_char ',' files);
    if !bad > 0 || !missing > 0 then begin
      Printf.eprintf "cpu86 vtest: %d fail, %d unsupported\n%!" !bad !missing;
      exit 1
    end
  end
