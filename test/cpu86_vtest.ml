(* SingleStepTests/8086 (실칩 Intel P80C86A-2 생성) 상태 기반 검증.
   각 케이스: 초기 레지스터/플래그/RAM 세팅 → 명령 1개 → final regs/ram 비교.
   프리페치 큐와 사이클은 모델 밖 — 비교하지 않는다.

   실행: SST8086_DIR(기본 /tmp/sst8086/v1), SST_FILES(콤마 목록, 기본 00).
   판정: pass / fail(값 불일치) / unsupported(미구현 opcode). *)

let dir = try Sys.getenv "SST8086_DIR" with Not_found -> "/tmp/sst8086/v1"
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

let compare_final t regs ram =
  let open Yojson.Safe.Util in
  let mismatches = ref [] in
  List.iter
    (fun (k, v) ->
      let want = to_int v in
      let got =
        match reg_index k with
        | Some i -> Cpu86.reg16 t i
        | None ->
          (match seg_index k with
           | Some s -> Cpu86.seg t s
           | None ->
             if k = "ip" then Cpu86.dump_ip t
             else if k = "flags" then Cpu86.flags t
             else -1)
      in
      if got <> want then
        mismatches := Printf.sprintf "%s: got=%04x want=%04x" k got want
                      :: !mismatches)
    (to_assoc (member "regs" regs));
  List.iter (fun cell ->
      match to_list cell with
      | [ a; v ] ->
        let a = to_int a and v = to_int v in
        if read_mem a <> v then
          mismatches := Printf.sprintf "ram[%x]: got=%02x want=%02x"
                          a (read_mem a) v :: !mismatches
      | _ -> ())
    (to_list (member "ram" ram));
  !mismatches

let run_file base =
  let path = Filename.concat dir (base ^ ".json.gz") in
  let ic = Unix.open_process_in ("gzcat " ^ Filename.quote path) in
  let json = Yojson.Safe.from_channel ic in
  ignore (Unix.close_process_in ic);
  let cases = Yojson.Safe.Util.to_list json in
  let total = ref 0 and pass = ref 0 and fail = ref 0 and unsup = ref 0 in
  let fail_samples = ref [] in
  let one_filter = try Some (Sys.getenv "SST_ONE") with Not_found -> None in
  List.iter (fun case ->
      incr total;
      let open Yojson.Safe.Util in
      let name = to_string (member "name" case) in
      let dbg = one_filter = Some name in
      Bytes.fill mem 0 (1024 * 1024) '\000';
      let t =
        Cpu86.create ~read:read_mem ~write:write_mem
          ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ())
      in
      apply_state t (member "initial" case) (member "initial" case);
      if dbg then begin
        let init = member "initial" case in
        Printf.eprintf "DBG %s\n  initial regs: %s\n  initial ram: %s\n  final: %s\n%!"
          name
          (Yojson.Safe.to_string (member "regs" init))
          (Yojson.Safe.to_string (member "ram" init))
          (Yojson.Safe.to_string (member "final" case))
      end;
      (try
         ignore (Cpu86.step t);
         let bad = compare_final t (member "final" case) (member "final" case) in
         if bad = [] then incr pass
         else begin
           incr fail;
           if List.length !fail_samples < 6 then begin
             fail_samples := (name, bad) :: !fail_samples;
             if one_filter = None || one_filter = Some name then begin
               let init = member "initial" case in
               let dump_regs = Yojson.Safe.to_string (member "regs" init) in
               let dump_ram = Yojson.Safe.to_string (member "ram" init) in
               let dump_fin = Yojson.Safe.to_string (member "final" case) in
               let dump_ours =
                 Printf.sprintf "bx=%04x si=%04x ds=%04x bh=%02x mem[ds:bx+si]=%02x"
                   (Cpu86.reg16 t 3) (Cpu86.reg16 t 6) (Cpu86.seg t 3)
                   (Cpu86.reg8 t 7)
                   (read_mem (((Cpu86.seg t 3 lsl 4) + Cpu86.reg16 t 3 + Cpu86.reg16 t 6) land 0xfffff))
               in
               Printf.eprintf "FAILDET %s\n  init.regs=%s\n  init.ram=%s\n  final=%s\n  ours: %s\n%!"
                 name dump_regs dump_ram dump_fin dump_ours
             end
           end
         end
       with Cpu86.Unsupported _ -> incr unsup))
    cases;
  Printf.printf "%s: total=%d pass=%d fail=%d unsupported=%d\n%!"
    base !total !pass !fail !unsup;
  List.iter (fun (name, bad) ->
      Printf.eprintf "  FAIL %s — %s\n%!" name (String.concat "; " bad))
    (List.rev !fail_samples)

let () =
  (* 데이터가 없으면(예: CI) 이 스위트는 스킵 — runtest 는 그린으로
     통과시킨다. 로컬에 /tmp/sst8086 가 있으면 전 opcode 검증이 돈다. *)
  if not (Sys.file_exists (Filename.concat dir "00.json.gz")) then
    print_endline "cpu86 vtest: dataset missing, skipped"
  else List.iter run_file (String.split_on_char ',' files)
