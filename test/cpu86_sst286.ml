(* SingleStepTests/80286, real mode (made on a Harris N80C286-12), against
   the I80286 model. Each test sets registers and RAM, runs one
   instruction and the HALT after it, and compares every register and the
   bytes the test lists. Bits the file's RMSK masks leave out are the ones
   the chip leaves undefined; they are not compared, and neither are those
   bits of the flags an exception pushed (EXCP).

   SST80286_DIR names the directory of .MOO.gz files (default
   /tmp/sst80286/v1_real_mode), SST_FILES a comma list of their names
   without the extension (default 00). A file's failures may not exceed
   the count sst80286-baseline.txt allows; the exit code is 1 if they do.
   With no data the suite is skipped. *)

let dir = try Sys.getenv "SST80286_DIR" with Not_found -> "/tmp/sst80286/v1_real_mode"
let files = try Sys.getenv "SST_FILES" with Not_found -> "00"

let baseline_path =
  let exe_dir = Filename.dirname Sys.argv.(0) in
  let candidates =
    [ Filename.concat exe_dir "sst80286-baseline.txt";
      Filename.concat exe_dir "../../../test/sst80286-baseline.txt";
      "test/sst80286-baseline.txt" ]
  in
  match List.find_opt Sys.file_exists candidates with Some p -> p | None -> List.hd candidates

let baseline =
  let tbl = Hashtbl.create 8 in
  (match open_in baseline_path with
   | ic ->
     (try
        while true do
          let line = String.trim (input_line ic) in
          if line <> "" && line.[0] <> '#' then
            match String.split_on_char ' ' line with
            | [ name; n ] -> Hashtbl.replace tbl name (int_of_string n)
            | _ -> failwith ("bad baseline line: " ^ line)
        done
      with End_of_file -> ());
     close_in ic
   | exception Sys_error _ -> ());
  tbl

let allowed name = Option.value (Hashtbl.find_opt baseline name) ~default:0

(* The tests assume 16 MB of RAM, and a 286 in real mode reaches past
   1 MB (FFFF:FFFF is 10FFEFh). *)
let mem_size = 16 * 1024 * 1024
let mem = Bytes.make mem_size '\000'
let addr_mask = mem_size - 1
let read_mem a = Char.code (Bytes.get mem (a land addr_mask))

(* Every address written, so a test can put the memory back to zeroes for
   the next one, whatever it wrote. *)
let written = ref []

let write_mem a v =
  let a = a land addr_mask in
  written := a :: !written;
  Bytes.set mem a (Char.chr (v land 0xff))

let clear_written () =
  List.iter (fun a -> Bytes.set mem a '\000') !written;
  written := []

let get t = function
  | Moo.Ax -> Cpu86.reg16 t 0 | Cx -> Cpu86.reg16 t 1 | Dx -> Cpu86.reg16 t 2
  | Bx -> Cpu86.reg16 t 3 | Sp -> Cpu86.reg16 t 4 | Bp -> Cpu86.reg16 t 5
  | Si -> Cpu86.reg16 t 6 | Di -> Cpu86.reg16 t 7
  | Es -> Cpu86.seg t 0 | Cs -> Cpu86.seg t 1 | Ss -> Cpu86.seg t 2 | Ds -> Cpu86.seg t 3
  | Ip -> Cpu86.dump_ip t | Flags -> Cpu86.flags t

let set t r v =
  match r with
  | Moo.Ax -> Cpu86.set_reg16 t 0 v | Cx -> Cpu86.set_reg16 t 1 v
  | Dx -> Cpu86.set_reg16 t 2 v | Bx -> Cpu86.set_reg16 t 3 v
  | Sp -> Cpu86.set_reg16 t 4 v | Bp -> Cpu86.set_reg16 t 5 v
  | Si -> Cpu86.set_reg16 t 6 v | Di -> Cpu86.set_reg16 t 7 v
  | Es -> Cpu86.set_seg t 0 v | Cs -> Cpu86.set_seg t 1 v
  | Ss -> Cpu86.set_seg t 2 v | Ds -> Cpu86.set_seg t 3 v
  | Ip -> Cpu86.set_ip t v | Flags -> Cpu86.set_flags t v

let reg_name = function
  | Moo.Ax -> "ax" | Bx -> "bx" | Cx -> "cx" | Dx -> "dx" | Cs -> "cs" | Ss -> "ss"
  | Ds -> "ds" | Es -> "es" | Sp -> "sp" | Bp -> "bp" | Si -> "si" | Di -> "di"
  | Ip -> "ip" | Flags -> "flags"

(* The IVT delivers an interrupt as the chip does: flags, CS and IP go on
   the stack, IF and TF go off, CS:IP comes from the vector. *)
let install_int_hook t =
  Cpu86.set_int_hook t (fun v ->
      let flags = Cpu86.flags t in
      let push w =
        Cpu86.set_reg16 t 4 ((Cpu86.reg16 t 4 - 2) land 0xffff);
        let a = (Cpu86.seg t 2 lsl 4) + Cpu86.reg16 t 4 in
        write_mem a (w land 0xff);
        write_mem (a + 1) ((w lsr 8) land 0xff)
      in
      push flags;
      push (Cpu86.seg t 1);
      push (Cpu86.dump_ip t);
      Cpu86.set_flags t (flags land lnot Cpu86.f_interrupt land lnot Cpu86.f_trap);
      let rd a = read_mem a lor (read_mem (a + 1) lsl 8) in
      Cpu86.set_ip t (rd (v * 4));
      Cpu86.set_seg t 1 (rd ((v * 4) + 2)))

(* The flags Intel's manuals call undefined after each instruction — the
   same for the 8086 and the 80286. MUL and IMUL define only CF and OF,
   DIV and IDIV none, the shifts and rotates leave AF undefined and OF too
   unless the count is 1. A program must not read these, so a difference
   there is counted apart from a failure. The table is cpu86_vtest's. *)
let undefined_flags name =
  let cf = Cpu86.f_carry and pf = Cpu86.f_parity and af = Cpu86.f_aux in
  let zf = Cpu86.f_zero and sf = Cpu86.f_sign and ov = Cpu86.f_overflow in
  match name with
  | "F6.4" | "F6.5" | "F7.4" | "F7.5" | "69" | "6B" -> sf lor zf lor af lor pf
  | "F6.6" | "F6.7" | "F7.6" | "F7.7" -> cf lor ov lor sf lor zf lor af lor pf
  | "27" | "2F" -> ov
  | "37" | "3F" -> ov lor sf lor zf lor pf
  | "D4" | "D5" -> af lor cf lor ov
  | _ ->
    if String.length name >= 2 && (name.[0] = 'D' || name.[0] = 'C') && name.[1] >= '0'
       && name.[1] <= '3' && name <> "C2" && name <> "C3"
    then (if name.[0] = 'D' && name.[1] <= '1' then af else af lor ov)
    else 0

(* A REP string instruction runs at most 127 times here, a step each. *)
let step_limit = 1_000

let all_regs = Array.to_list Moo.reg_bits

type verdict = Pass | Undefined_only | Fail of string list

let run_test ~file_masks ~undefined (test : Moo.test) =
  List.iter (fun (a, v) -> write_mem a v) test.initial.ram;
  let t =
    Cpu86.create ~model:Cpu86.I80286 ~read:read_mem ~write:write_mem
      ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) ()
  in
  install_int_hook t;
  List.iter (fun (r, v) -> set t r v) test.initial.regs;
  let rec go n = if n < step_limit && not (Cpu86.halted t) then (ignore (Cpu86.step t); go (n + 1)) in
  go 0;
  let mask r =
    match List.assoc_opt r test.masks with
    | Some m -> m
    | None -> Option.value (List.assoc_opt r file_masks) ~default:0xffff
  in
  let wrong = ref [] and undefined_differ = ref false in
  List.iter
    (fun r ->
      let want =
        match List.assoc_opt r test.final.regs with
        | Some v -> v
        | None -> Option.value (List.assoc_opt r test.initial.regs) ~default:0
      in
      let m = mask r and got = get t r in
      let m_defined = match r with Moo.Flags -> m land lnot undefined | _ -> m in
      if got land m_defined <> want land m_defined then
        wrong := Printf.sprintf "%s=%04x want %04x" (reg_name r) got want :: !wrong
      else if got land m <> want land m then undefined_differ := true)
    all_regs;
  let flags_mask = mask Flags land lnot undefined in
  List.iter
    (fun (a, v) ->
      let m =
        match test.exception_taken with
        | Some (_, f) when a = f -> flags_mask land 0xff
        | Some (_, f) when a = f + 1 -> (flags_mask lsr 8) land 0xff
        | Some _ | None -> 0xff
      in
      if read_mem a land m <> v land m then
        wrong := Printf.sprintf "[%x]=%02x want %02x" a (read_mem a) v :: !wrong)
    test.final.ram;
  clear_written ();
  match !wrong with
  | [] -> if !undefined_differ then Undefined_only else Pass
  | wrong ->
    let init =
      String.concat " "
        (List.map (fun (r, v) -> Printf.sprintf "%s=%04x" (reg_name r) v) test.initial.regs)
    in
    let exc =
      match test.exception_taken with Some (v, _) -> Printf.sprintf " [int %d]" v | None -> ""
    in
    Fail (List.rev wrong @ [ "from " ^ init ^ exc ])

let read_gz path =
  let ic = Unix.open_process_args_in "gzip" [| "gzip"; "-dc"; path |] in
  let s = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> s
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> failwith ("gzip failed on " ^ path)

type outcome = { name : string; total : int; failed : int; undefined : int; unsupported : int }

let run_file name =
  let file = Moo.parse (read_gz (Filename.concat dir (name ^ ".MOO.gz"))) in
  let failed = ref 0 and undefined_only = ref 0 and unsupported = ref 0 and samples = ref [] in
  let undefined = undefined_flags name in
  List.iter
    (fun (test : Moo.test) ->
      match run_test ~file_masks:file.file_masks ~undefined test with
      | Pass -> ()
      | Undefined_only -> incr undefined_only
      | Fail wrong ->
        incr failed;
        if List.length !samples < 3 then samples := (test.name, wrong) :: !samples
      | exception Cpu86.Unsupported what ->
        incr unsupported;
        if List.length !samples < 3 then samples := (test.name, [ "unsupported " ^ what ]) :: !samples;
        clear_written ())
    file.tests;
  let o =
    { name; total = List.length file.tests; failed = !failed; undefined = !undefined_only;
      unsupported = !unsupported }
  in
  let budget = allowed name in
  Printf.printf "%s: total=%d fail=%d undef=%d unsupported=%d%s\n%!" name o.total o.failed
    o.undefined o.unsupported
    (if budget > 0 then Printf.sprintf " (allowed %d)" budget else "");
  if o.failed + o.unsupported > budget then
    List.iter
      (fun (n, wrong) -> Printf.eprintf "  FAIL %s: %s\n%!" n (String.concat ", " wrong))
      (List.rev !samples);
  if o.failed + o.unsupported < budget then
    Printf.eprintf "  %s: %d < allowed %d; lower it in sst80286-baseline.txt\n%!" name
      (o.failed + o.unsupported) budget;
  o

let () =
  if not (Sys.file_exists (Filename.concat dir "00.MOO.gz")) then
    print_endline "cpu86 sst286: dataset missing, skipped"
  else begin
    let over =
      List.fold_left
        (fun acc name ->
          let o = run_file name in
          acc + max 0 (o.failed + o.unsupported - allowed name))
        0 (String.split_on_char ',' files)
    in
    if over > 0 then begin
      Printf.eprintf "cpu86 sst286: %d over the baseline\n%!" over;
      exit 1
    end
  end
