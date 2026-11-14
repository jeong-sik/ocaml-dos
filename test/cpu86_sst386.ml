(* SingleStepTests/80386, real mode (made on an Intel 386EX), against the
   I80386 model. Each test sets the 32-bit registers and RAM, runs one
   instruction and the HALT after it, and compares the registers and the
   bytes the test lists. Bits the test's RM32 masks leave out are the ones
   the chip leaves undefined; they are not compared.

   What is not compared, and why:
   - EFLAGS above bit 15. The 386EX's SMM dump fills the reserved bits 18-31
     with ones, and RF·VM (16·17) are not modeled in real mode.
   - CR0, CR3, DR6, DR7: the real-mode instructions these files test do not
     change them, and the model has none of them yet.

   SST80386_DIR names the directory of .MOO.gz files (default
   /tmp/sst80386/v1_ex_real_mode), SST_FILES a comma list of their names
   without the extension (default 00). The tests revocation_list.txt (next
   to that directory) names are skipped. A file's failures may not exceed
   the count sst80386-baseline.txt allows; the exit code is 1 if they do.
   With no data the suite is skipped. *)

let dir = try Sys.getenv "SST80386_DIR" with Not_found -> "/tmp/sst80386/v1_ex_real_mode"
let files = try Sys.getenv "SST_FILES" with Not_found -> "00"

let read_lines path =
  match open_in path with
  | ic ->
    let rec go acc =
      match input_line ic with
      | line -> go (String.trim line :: acc)
      | exception End_of_file -> close_in ic; List.rev acc
    in
    go []
  | exception Sys_error _ -> []

let baseline_path =
  let exe_dir = Filename.dirname Sys.argv.(0) in
  let candidates =
    [ Filename.concat exe_dir "sst80386-baseline.txt";
      Filename.concat exe_dir "../../../test/sst80386-baseline.txt";
      "test/sst80386-baseline.txt" ]
  in
  match List.find_opt Sys.file_exists candidates with Some p -> p | None -> List.hd candidates

let baseline =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun line ->
      if line <> "" && line.[0] <> '#' then
        match String.split_on_char ' ' line with
        | [ name; n ] -> Hashtbl.replace tbl name (int_of_string n)
        | _ -> failwith ("bad baseline line: " ^ line))
    (read_lines baseline_path);
  tbl

let allowed name = Option.value (Hashtbl.find_opt baseline name) ~default:0

let revoked =
  List.filter
    (fun l -> l <> "" && l.[0] <> '#')
    (read_lines (Filename.concat (Filename.dirname dir) "revocation_list.txt"))

(* The tests assume 16 MB of RAM; real mode reaches 10FFEFh. *)
let mem_size = 16 * 1024 * 1024
let mem = Bytes.make mem_size '\000'
let addr_mask = mem_size - 1
let read_mem a = Char.code (Bytes.get mem (a land addr_mask))

let written = ref []

let write_mem a v =
  let a = a land addr_mask in
  written := a :: !written;
  Bytes.set mem a (Char.chr (v land 0xff))

let clear_written () =
  List.iter (fun a -> Bytes.set mem a '\000') !written;
  written := []

(* The registers this runner sets and compares. *)
type place = Gpr of int | Seg of int | Ip | Flags | Not_modeled

let place = function
  | Moo.R32.Eax -> Gpr 0 | Ecx -> Gpr 1 | Edx -> Gpr 2 | Ebx -> Gpr 3
  | Esp -> Gpr 4 | Ebp -> Gpr 5 | Esi -> Gpr 6 | Edi -> Gpr 7
  | Es -> Seg 0 | Cs -> Seg 1 | Ss -> Seg 2 | Ds -> Seg 3 | Fs -> Seg 4 | Gs -> Seg 5
  | Eip -> Ip | Eflags -> Flags
  | Cr0 | Cr3 | Dr6 | Dr7 -> Not_modeled

let reg_name = function
  | Moo.R32.Cr0 -> "cr0" | Cr3 -> "cr3" | Eax -> "eax" | Ebx -> "ebx" | Ecx -> "ecx"
  | Edx -> "edx" | Esi -> "esi" | Edi -> "edi" | Ebp -> "ebp" | Esp -> "esp"
  | Cs -> "cs" | Ds -> "ds" | Es -> "es" | Fs -> "fs" | Gs -> "gs" | Ss -> "ss"
  | Eip -> "eip" | Eflags -> "eflags" | Dr6 -> "dr6" | Dr7 -> "dr7"

let flags_compared = 0xffff
let segment_bits = 0xffff

let set t r v =
  match place r with
  | Gpr n -> Cpu86.set_reg32 t n v
  | Seg n -> Cpu86.set_seg t n v
  | Ip -> Cpu86.set_ip t v
  | Flags -> Cpu86.set_flags t v
  | Not_modeled -> ()

(* The value to compare and the bits of it that count. *)
let get t r =
  match place r with
  | Gpr n -> Some (Cpu86.reg32 t n, 0xffffffff)
  | Seg n -> Some (Cpu86.seg t n, segment_bits)
  | Ip -> Some (Cpu86.dump_ip t, 0xffffffff)
  | Flags -> Some (Cpu86.flags t, flags_compared)
  | Not_modeled -> None

(* The IVT delivers an interrupt as the chip does in real mode: CS:IP comes
   from the vector, flags, CS and IP go on the stack as words, IF and TF go
   off. The vector is read before the pushes: with SS=0 and SP=8 the pushes
   land on vectors 0-1, and the 386 still takes the old vector (F6.6 div
   byte [ss:bp+di+6AE2h], 62 bound bx,[fs:bx-48h] with SS=1). *)
let install_int_hook t =
  Cpu86.set_int_hook t (fun v ->
      let rd a = read_mem a lor (read_mem (a + 1) lsl 8) in
      let new_ip = rd (v * 4) and new_cs = rd ((v * 4) + 2) in
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
      Cpu86.set_ip t new_ip;
      Cpu86.set_seg t 1 new_cs)

(* The flags Intel's manuals call undefined after each instruction, as in
   cpu86_sst286: a difference there is counted apart from a failure. *)
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

type verdict = Pass | Undefined_only | Fail of string list

let run_test ~file_masks ~undefined (test : Moo.test) =
  List.iter (fun (a, v) -> write_mem a v) test.initial.ram;
  let t =
    Cpu86.create ~model:Cpu86.I80386 ~read:read_mem ~write:write_mem
      ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) ()
  in
  install_int_hook t;
  List.iter (fun (r, v) -> set t r v) test.initial.regs32;
  let rec go n = if n < step_limit && not (Cpu86.halted t) then (ignore (Cpu86.step t); go (n + 1)) in
  go 0;
  let mask r =
    match List.assoc_opt r test.masks32 with
    | Some m -> m
    | None -> Option.value (List.assoc_opt r file_masks) ~default:0xffffffff
  in
  let wrong = ref [] and undefined_differ = ref false in
  Array.iter
    (fun r ->
      match get t r with
      | None -> ()
      | Some (got, bits) ->
        let want =
          match List.assoc_opt r test.final.regs32 with
          | Some v -> v
          | None -> Option.value (List.assoc_opt r test.initial.regs32) ~default:0
        in
        let m = mask r land bits in
        let m_defined = match r with Moo.R32.Eflags -> m land lnot undefined | _ -> m in
        if got land m_defined <> want land m_defined then
          wrong := Printf.sprintf "%s=%x want %x" (reg_name r) got (want land bits) :: !wrong
        else if got land m <> want land m then undefined_differ := true)
    Moo.R32.bits;
  let flags_mask = mask Moo.R32.Eflags land lnot undefined in
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
        (List.filter_map
           (fun (r, v) ->
             match place r with
             | Not_modeled -> None
             | Gpr _ | Seg _ | Ip | Flags -> Some (Printf.sprintf "%s=%x" (reg_name r) v))
           test.initial.regs32)
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

type outcome = {
  name : string; total : int; failed : int; undefined : int; unsupported : int; skipped : int;
}

let run_file name =
  let file = Moo.parse (read_gz (Filename.concat dir (name ^ ".MOO.gz"))) in
  let failed = ref 0 and undefined_only = ref 0 and unsupported = ref 0 in
  let skipped = ref 0 and samples = ref [] in
  let undefined = undefined_flags name in
  List.iter
    (fun (test : Moo.test) ->
      let is_revoked =
        match test.hash with Some h -> List.mem h revoked | None -> false
      in
      if is_revoked then incr skipped
      else
        match run_test ~file_masks:file.file_masks32 ~undefined test with
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
      unsupported = !unsupported; skipped = !skipped }
  in
  let budget = allowed name in
  Printf.printf "%s: total=%d fail=%d undef=%d unsupported=%d%s%s\n%!" name o.total o.failed
    o.undefined o.unsupported
    (if o.skipped > 0 then Printf.sprintf " revoked=%d" o.skipped else "")
    (if budget > 0 then Printf.sprintf " (allowed %d)" budget else "");
  if o.failed + o.unsupported > budget then
    List.iter
      (fun (n, wrong) -> Printf.eprintf "  FAIL %s: %s\n%!" n (String.concat ", " wrong))
      (List.rev !samples);
  if o.failed + o.unsupported < budget then
    Printf.eprintf "  %s: %d < allowed %d; lower it in sst80386-baseline.txt\n%!" name
      (o.failed + o.unsupported) budget;
  o

let () =
  if not (Sys.file_exists (Filename.concat dir "00.MOO.gz")) then
    print_endline "cpu86 sst386: dataset missing, skipped"
  else begin
    let over =
      List.fold_left
        (fun acc name ->
          let o = run_file name in
          acc + max 0 (o.failed + o.unsupported - allowed name))
        0 (String.split_on_char ',' files)
    in
    if over > 0 then begin
      Printf.eprintf "cpu86 sst386: %d over the baseline\n%!" over;
      exit 1
    end
  end
