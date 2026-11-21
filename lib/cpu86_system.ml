(* 386 의 시스템 명령: 기술자 표 레지스터(LGDT·LIDT·SGDT·SIDT), 기계 상태 워드
   (LMSW·SMSW), CLTS, 제어·디버그·시험 레지스터로의 MOV. CR0.PE 를 켜는 쓰기는
   보호 모드로 들어가고, 끄는 쓰기는 실모드로 돌아간다. CR0.PG 를 켜는 쓰기는 첫
   1MB 가 항등 매핑인지 확인한다([check_identity_map]). *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode

type width = Word | Dword

(* SGDT·SIDT·LGDT·LIDT 가 오가는 바이트: 한계 2, 기준 3, 마지막 1. *)
let descriptor_table_bytes = 6
let descriptor_table_base_mask_16bit_operand = 0xffffff
let top_byte_shift = 24
let byte_mask = 0xff

(* SGDT·SIDT: 한계, 기준의 하위 16비트, 기준의 위 8비트와 마지막 바이트를 세
   워드로 쓴다. 6 바이트가 세그먼트 끝을 넘으면 하나도 쓰기 전에 결함이다(386
   PRM: "Interrupt 13 if any part of the operand would lie outside of the
   effective address space from 0 to 0FFFFH"). *)
let store_descriptor_table t seg off table ~top_byte =
  within t seg off descriptor_table_bytes;
  write16 t seg off table.table_limit;
  write16 t seg (off + 2) (table.table_base land 0xffff);
  write16 t seg (off + 4)
    (((table.table_base lsr 16) land byte_mask) lor (top_byte lsl 8))

(* LGDT·LIDT: 읽기가 모두 끝난 뒤에 쓴다 — 도중에 결함이 나도 표 레지스터는
   그대로다. 16비트 피연산자는 기준의 위 8비트를 0 으로 싣는다(386 PRM LGDT:
   피연산자 크기가 16 비트면 24 비트 기준을 싣는다). *)
let load_descriptor_table t seg off table w =
  within t seg off descriptor_table_bytes;
  let limit = read16 t seg off in
  let base_low = read16 t seg (off + 2) in
  let base_high = read16 t seg (off + 4) in
  let base = base_low lor (base_high lsl 16) in
  table.table_limit <- limit;
  table.table_base <-
    (match w with
     | Word -> base land descriptor_table_base_mask_16bit_operand
     | Dword -> base)

(* 물리 주소의 더블워드. 표 메모리를 읽는다. *)
let read_physical32 t addr =
  let byte i = t.read ((addr + i) land t.addr_mask) in
  let b0 = byte 0 in
  let b1 = byte 1 in
  let b2 = byte 2 in
  let b3 = byte 3 in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let page_frame_mask = 0xfffff000
let page_bytes = 0x1000
let page_entry_bytes = 4
let page_entry_present = 0x1

(* 첫 1MB 의 페이지 수: PDE 하나가 가리키는 표의 앞 256 칸. *)
let first_megabyte_pages = 256

(* WORKAROUND: test386 POST 8 이 PG 를 켜기 전에 첫 1MB 를 선형=물리로 매핑해 두고
   (PDE 하나, PTE 256 개) 그 매핑으로 POST 9 이후를 돈다. 페이징이 5 단계라 그 모양만
   확인하고 이후 선형 주소를 그대로 물리 주소로 쓴다. 게스트가 표 메모리를 고쳐도
   반영하지 않는다. 근본 해결: 5단계 페이징 *)
let check_identity_map t ctx ~cr3 =
  let stop what =
    unsupported t ctx (Printf.sprintf "CR0.PG on: %s (page tables, RFC 0005 5)" what)
  in
  let pde = read_physical32 t (cr3 land page_frame_mask) in
  if pde land page_entry_present = 0 then
    stop (Printf.sprintf "PDE 0 of CR3=%08x is not present" cr3);
  let table = pde land page_frame_mask in
  for i = 0 to first_megabyte_pages - 1 do
    let pte = read_physical32 t (table + (i * page_entry_bytes)) in
    if pte land page_entry_present = 0 then stop (Printf.sprintf "PTE %d is not present" i);
    if pte land page_frame_mask <> i * page_bytes then
      stop (Printf.sprintf "PTE %d maps %08x, not the identity %08x" i
              (pte land page_frame_mask) (i * page_bytes))
  done

(* CR0 에 [v] 를 쓴다. 정의된 비트만 남긴다. PG 는 PE 가 켜져 있어야 하고(아니면 #GP),
   PG 를 새로 켜는 쓰기는 CR3 의 표를 읽어 보고 항등이 아니면 쓰기 전에 멈춘다. PE 를
   끄면 실모드로 돌아간다: 세그먼트 캐시는 그대로고 이후 적재는 base 만 쓴다. *)
let write_cr0 t ctx v =
  let v = v land cr0_defined in
  if v land cr0_pg <> 0 then begin
    if v land cr0_pe = 0 then fault vector_general_protection;
    if t.cr0 land cr0_pg = 0 then check_identity_map t ctx ~cr3:t.cr3
  end;
  t.cr0 <- v

(* 0F 01 의 /0 SGDT, /1 SIDT, /2 LGDT, /3 LIDT, /4 SMSW, /6 LMSW. 메모리 피연산자만
   받는 넷의 레지스터 형태는 INT 6 이다(386 PRM "Interrupt 6 if the operand is a
   register"). SMSW 는 32 비트 레지스터에 CR0 전체를 쓰고(test386 의 `smsw eax`
   와 `mov ebx, cr0` 가 같아야 한다), 메모리와 16 비트 레지스터에는 아래 워드만
   쓴다. LMSW 는 CR0 의 아래 네 비트(PE·MP·EM·TS)만 바꾸고 PE 는 켜기만 한다
   (386 PRM: LMSW 는 PE 를 켤 수 있고 끌 수 없다). /5·/7 은 없다
   (INVLPG 는 486). clock 수는 386 PRM 표에서 기억한 값이다(확인 필요: 원문은
   못 읽었다). *)
let group7 t ctx w =
  let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
  match (regf, rm, w) with
  | 0, (Mem (seg, off) | Mem32 (seg, off)), _ ->
    store_descriptor_table t seg off t.gdtr ~top_byte:((t.gdtr.table_base lsr top_byte_shift) land byte_mask);
    9
  | 1, (Mem (seg, off) | Mem32 (seg, off)), _ ->
    store_descriptor_table t seg off t.idtr ~top_byte:((t.idtr.table_base lsr top_byte_shift) land byte_mask);
    9
  | 2, (Mem (seg, off) | Mem32 (seg, off)), _ ->
    load_descriptor_table t seg off t.gdtr w;
    11
  | 3, (Mem (seg, off) | Mem32 (seg, off)), _ ->
    load_descriptor_table t seg off t.idtr w;
    11
  | (0 | 1 | 2 | 3), Reg _, _ -> fault vector_invalid_opcode
  | 4, Reg n, Dword -> put32 t n t.cr0; 2
  | 4, Reg _, Word -> op_write t 16 rm (t.cr0 land 0xffff); 2
  | 4, (Mem _ | Mem32 _), _ -> op_write t 16 rm (t.cr0 land 0xffff); 3
  | 6, _, _ ->
    let v = op_read t 16 rm in
    write_cr0 t ctx
      (((t.cr0 land lnot msw_loadable) lor (v land msw_loadable)) lor (t.cr0 land cr0_pe));
    (match rm with Reg _ -> 10 | Mem _ | Mem32 _ -> 13)
  | _ ->
    let form = match w with Word -> "386" | Dword -> "386, 66h" in
    unsupported t ctx (Printf.sprintf "0f 01 /%d (%s)" regf form)

(* CLTS: CR0 의 TS 를 지운다. *)
let clts t =
  t.cr0 <- t.cr0 land lnot cr0_ts;
  5

(* MOV 제어·디버그·시험 레지스터: modrm 의 mod 는 무시되고(변위를 가져오지
   않는다) reg 필드가 레지스터 번호, rm 이 범용 레지스터다. 피연산자는 66h 와
   상관없이 32 비트다. 없는 번호는 INT 6 이다: 386 의 제어 레지스터는 CR0·CR2·CR3
   이고 시험 레지스터는 TR6·TR7 이다. DR4·DR5 는 386 PRM 이 예약이라 했고
   실칩이 무엇을 하는지 모른다(확인 필요) — 멈춘다. *)
let register_operands t =
  let byte = fetch8 t in
  ((byte lsr 3) land 7, byte land 7)

let debug_breakpoint_enables = 0xff

let debug_register_slot t ctx n =
  match n with
  | 4 | 5 -> unsupported t ctx (Printf.sprintf "mov dr%d (reserved)" n)
  | _ -> n

let test_register_slot n =
  if n < test_register_first then fault vector_invalid_opcode;
  n - test_register_first

let move_from_special t ctx op2 =
  let n, r = register_operands t in
  let v =
    match op2 with
    | 0x20 ->
      (match n with
       | 0 -> t.cr0
       | 2 -> t.cr2
       | 3 -> t.cr3
       | _ -> fault vector_invalid_opcode)
    | 0x21 -> t.dr.(debug_register_slot t ctx n)
    | _ -> t.tr.(test_register_slot n)
  in
  put32 t r v;
  match op2 with 0x20 -> 6 | 0x21 -> if n < 4 then 22 else 14 | _ -> 12

(* CR0 에 쓰는 길은 모두 [write_cr0] 를 지난다. DR7 의 L0-G3 비트는 디버그 중단점을
   켠다. 이 코어는 중단점을 걸지 않아 그 비트를 켜는 쓰기는 멈춘다. *)
let move_to_special t ctx op2 =
  let n, r = register_operands t in
  let v = get32 t r in
  match op2 with
  | 0x22 ->
    (match n with
     | 0 -> write_cr0 t ctx v; 10
     | 2 -> t.cr2 <- v; 4
     | 3 ->
       if t.cr0 land cr0_pg <> 0 then check_identity_map t ctx ~cr3:v;
       t.cr3 <- v;
       4
     | _ -> fault vector_invalid_opcode)
  | 0x23 ->
    let slot = debug_register_slot t ctx n in
    if slot = 7 && v land debug_breakpoint_enables <> 0 then
      unsupported t ctx "mov dr7 enables a breakpoint";
    t.dr.(slot) <- v;
    if n < 4 then 22 else 16
  | _ ->
    t.tr.(test_register_slot n) <- v;
    12
