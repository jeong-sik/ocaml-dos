(* 보호 모드에서만 도는 386 명령: LLDT·SLDT·LTR·STR·VERR·VERW(0F 00), LAR·LSL(0F 02·03),
   ARPL(63). 실모드에서는 모두 INT 6 이다(386 PRM: "Interrupt 6 in real-address mode").
   특권 수준은 링 0 하나만 안다([Cpu86_segment.outside_ring_zero]): CPL 은 0 이고 셀렉터의
   RPL 이 접근을 가른다. clock 수는 386 PRM 표에서 기억한 값이다(확인 필요: 원문은 못
   읽었다). *)

open Cpu86_state
open Cpu86_segment
open Cpu86_fault
open Cpu86_decode

let require_protected_mode t =
  if t.cr0 land cr0_pe = 0 then fault Invalid_opcode

(* ---------- 셀렉터를 올리고 내리기 ---------- *)

(* 시스템 기술자의 형식 니블(386 PRM 6.1). *)
let type_tss_available_286 = 0x1
let type_ldt = 0x2
let type_tss_busy_286 = 0x3
let type_call_gate_286 = 0x4
let type_task_gate = 0x5
let type_tss_available = 0x9
let type_tss_busy = 0xb
let type_call_gate = 0xc

let busy_bit = 0x2
let access_byte_offset = 5

let stop t ctx what =
  unsupported t ctx
    (Printf.sprintf "%s (protection checks and faults: RFC 0005 3c and 4)" what)

let store_selector t rm w selector =
  match rm, w with
  | Reg n, Cpu86_system.Dword -> put32 t n selector
  | (Reg _ | Mem _ | Mem32 _), _ -> op_write t 16 rm selector

(* LLDT: 셀렉터는 GDT 의 LDT 기술자를 가리키거나 널이다. 널이면 LDT 는 비어 있다. *)
let load_ldt t ctx selector =
  if selector land selector_table_indicator <> 0 then
    stop t ctx (Printf.sprintf "lldt %04x names an LDT entry" selector);
  match read_descriptor t selector with
  | Null_selector ->
    t.ldt.sys_selector <- selector;
    t.ldt.sys_base <- 0;
    t.ldt.sys_limit <- 0
  | Beyond_table -> stop t ctx (Printf.sprintf "lldt %04x is beyond the GDT" selector)
  | Found d ->
    if not d.desc_present then
      stop t ctx (Printf.sprintf "lldt %04x is not present" selector)
    else
      match d.desc_kind with
      | System ty when ty = type_ldt ->
        t.ldt.sys_selector <- selector;
        t.ldt.sys_base <- d.desc_base;
        t.ldt.sys_limit <- d.desc_limit
      | System _ | Data _ | Code _ ->
        stop t ctx (Printf.sprintf "lldt %04x is not an LDT descriptor" selector)

(* LTR: 사용할 수 있는 TSS 를 가리켜야 하고 바쁨(busy)으로 표시한다. 표시는 GDT 메모리에
   쓴다. *)
let load_task_register t ctx selector =
  if selector land selector_table_indicator <> 0 then
    stop t ctx (Printf.sprintf "ltr %04x names an LDT entry" selector);
  match read_descriptor t selector with
  | Null_selector -> stop t ctx "ltr with a null selector"
  | Beyond_table -> stop t ctx (Printf.sprintf "ltr %04x is beyond the GDT" selector)
  | Found d ->
    if not d.desc_present then
      stop t ctx (Printf.sprintf "ltr %04x is not present" selector)
    else
      match d.desc_kind with
      | System ty when ty = type_tss_available || ty = type_tss_available_286 ->
        let entry = t.gdtr.table_base + (selector land selector_index_mask) in
        t.write ((entry + access_byte_offset) land t.addr_mask) (d.desc_access lor busy_bit);
        t.task.sys_selector <- selector;
        t.task.sys_base <- d.desc_base;
        t.task.sys_limit <- d.desc_limit
      | System _ | Data _ | Code _ ->
        stop t ctx (Printf.sprintf "ltr %04x is not an available TSS" selector)

(* ---------- 닿는지 묻는 명령 ---------- *)

(* [selector] 의 기술자가 링 0 에서 닿는가: 적합(conforming) 코드는 늘 닿고, 그 밖은 DPL 이
   RPL 이상이다. *)
let reachable selector d =
  match d.desc_kind with
  | Code { conforming = true; _ } -> true
  | Code { conforming = false; _ } | Data _ | System _ ->
    d.desc_dpl >= selector land selector_rpl_mask

let lookup_reachable t selector =
  match read_descriptor t selector with
  | Found d when reachable selector d -> Some d
  | Found _ | Null_selector | Beyond_table -> None

let verify_read t selector =
  match lookup_reachable t selector with
  | Some { desc_kind = Data _; _ } -> true
  | Some { desc_kind = Code { readable; _ }; _ } -> readable
  | Some { desc_kind = System _; _ } | None -> false

let verify_write t selector =
  match lookup_reachable t selector with
  | Some { desc_kind = Data { writable; _ }; _ } -> writable
  | Some { desc_kind = Code _ | System _; _ } | None -> false

(* 0F 00: /0 SLDT, /1 STR, /2 LLDT, /3 LTR, /4 VERR, /5 VERW. /6·/7 은 없다. *)
let group6 t ctx w =
  require_protected_mode t;
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let cycles_reg_or_mem reg mem = match rm with Reg _ -> reg | Mem _ | Mem32 _ -> mem in
  match regf with
  | 0 -> store_selector t rm w t.ldt.sys_selector; cycles_reg_or_mem 2 2
  | 1 -> store_selector t rm w t.task.sys_selector; cycles_reg_or_mem 23 27
  | 2 -> load_ldt t ctx (op_read t 16 rm); cycles_reg_or_mem 20 24
  | 3 -> load_task_register t ctx (op_read t 16 rm); cycles_reg_or_mem 23 27
  | 4 -> t.zf <- verify_read t (op_read t 16 rm); cycles_reg_or_mem 10 11
  | 5 -> t.zf <- verify_write t (op_read t 16 rm); cycles_reg_or_mem 15 16
  | _ -> unsupported t ctx (Printf.sprintf "0f 00 /%d" regf)

(* LAR·LSL 이 받는 시스템 기술자의 형식(386 PRM). LAR 은 TSS·LDT·호출 게이트·작업 게이트를,
   LSL 은 TSS·LDT 를 받는다. 코드·데이터 세그먼트는 둘 다 받는다. *)
let lar_accepts = function
  | System ty ->
    ty = type_tss_available_286 || ty = type_ldt || ty = type_tss_busy_286
    || ty = type_call_gate_286 || ty = type_task_gate || ty = type_tss_available
    || ty = type_tss_busy || ty = type_call_gate
  | Data _ | Code _ -> true

let lsl_accepts = function
  | System ty ->
    ty = type_tss_available_286 || ty = type_ldt || ty = type_tss_busy_286
    || ty = type_tss_available || ty = type_tss_busy
  | Data _ | Code _ -> true

(* LAR 이 올리는 값: 접근 권한 바이트는 bit 8-15 에, 32 비트 피연산자면 플래그 니블(G D/B
   L AVL)이 bit 20-23 에 간다(확인 필요: 386 PRM 의 마스크는 못 읽었다). *)
let access_rights_shift = 8
let flags_shift = 20

(* LAR r, r/m16 과 LSL r, r/m16: 닿고 형식이 맞으면 ZF=1 이고 목적지를 쓴다. 아니면 ZF=0 이고
   목적지는 그대로다. 결함은 없다. *)
let query_descriptor t ctx w ~accepts ~value =
  require_protected_mode t;
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let selector = op_read t 16 rm in
  (match lookup_reachable t selector with
   | Some d when accepts d.desc_kind ->
     t.zf <- true;
     (match w with
      | Cpu86_system.Word -> set_reg16 t regf (value d w)
      | Cpu86_system.Dword -> put32 t regf (value d w))
   | Some _ | None -> t.zf <- false);
  match rm with Reg _ -> 15 | Mem _ | Mem32 _ -> 16

let load_access_rights t ctx w =
  query_descriptor t ctx w ~accepts:lar_accepts ~value:(fun d w ->
      let rights = d.desc_access lsl access_rights_shift in
      match w with
      | Cpu86_system.Word -> rights
      | Cpu86_system.Dword -> rights lor (d.desc_flags lsl flags_shift))

let load_segment_limit t ctx w =
  query_descriptor t ctx w ~accepts:lsl_accepts ~value:(fun d w ->
      match w with
      | Cpu86_system.Word -> d.desc_limit land 0xffff
      | Cpu86_system.Dword -> d.desc_limit)

(* ARPL r/m16, r16: 목적지의 RPL 이 원천의 RPL 보다 작으면 원천 것으로 올리고 ZF=1, 아니면
   ZF=0. 피연산자는 66h 와 상관없이 16 비트다. *)
let arpl t ctx =
  require_protected_mode t;
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  let dest = op_read t 16 rm in
  let source = reg16 t regf in
  if dest land selector_rpl_mask < source land selector_rpl_mask then begin
    op_write t 16 rm
      ((dest land lnot selector_rpl_mask) lor (source land selector_rpl_mask));
    t.zf <- true
  end else t.zf <- false;
  match rm with Reg _ -> 20 | Mem _ | Mem32 _ -> 21
