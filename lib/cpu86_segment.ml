(* 세그먼트 레지스터 적재. 실모드는 base 를 셀렉터×16 으로 쓴다. 보호 모드(CR0.PE)는
   셀렉터가 GDT 나 LDT 의 기술자를 고르고, 그 base·limit·D·B 를 레지스터의 캐시에
   쓴다. 캐시한 뒤 표 메모리를 고쳐도 캐시는 그대로다.

   형식·존재·표 밖 셀렉터처럼 보호 모드가 결함으로 받는 적재는 선택자가 실린 결함을
   던진다([Cpu86_fault.fault]). 링을 넘는 적재와 확장 하향 세그먼트, CS 에 싣는 시스템
   기술자(호출 게이트·TSS)는 결함이 아니라 아직 없는 기능이라 4 단계(RFC 0005)까지
   [Unsupported] 로 멈춘다 — 조용히 받아 주지 않는다. *)

open Cpu86_state
open Cpu86_fault

(* ---------- 기술자 ---------- *)

(* 기술자의 종류. S 비트가 꺼진 것은 시스템 기술자(TSS·LDT·게이트)이고 값은 형식
   니블이다. S 가 켜진 것은 비트 3 이 코드(1)와 데이터(0)를 가른다. *)
type kind =
  | Data of { writable : bool; expand_down : bool }
  | Code of { readable : bool; conforming : bool }
  | System of int

type descriptor = {
  desc_base : int;
  desc_limit : int;  (** 마지막 바이트의 오프셋. G 비트가 켜지면 4KB 단위가 적용돼 있다 *)
  desc_big : bool;  (** D(코드)·B(스택) 비트 *)
  desc_access : int;  (** 5번째 바이트: P DPL S 형식 *)
  desc_flags : int;  (** 위 니블: G D/B L AVL *)
  desc_kind : kind;
  desc_dpl : int;
  desc_present : bool;
}

let access_present = 0x80
let access_code_or_data = 0x10
let access_executable = 0x08
let access_writable_or_readable = 0x02
let access_expand_down_or_conforming = 0x04
let flag_granularity = 0x8
let flag_big = 0x4

(* [low] 는 기술자의 아래 네 바이트, [high] 는 위 네 바이트(리틀 엔디안 더블워드).
   limit 은 아래 16 비트와 [high] 의 bit 16-19, base 는 아래 워드의 위 16 비트와
   [high] 의 bit 0-7·24-31 이다. *)
let parse ~low ~high =
  let access = (high lsr 8) land 0xff in
  let flags = (high lsr 20) land 0xf in
  let raw_limit = (low land 0xffff) lor (high land 0xf0000) in
  let desc_limit =
    if flags land flag_granularity <> 0 then (raw_limit lsl 12) lor 0xfff else raw_limit
  in
  let desc_base = (low lsr 16) lor ((high land 0xff) lsl 16) lor (high land 0xff000000) in
  let desc_kind =
    if access land access_code_or_data = 0 then System (access land 0xf)
    else if access land access_executable = 0 then
      Data { writable = access land access_writable_or_readable <> 0;
             expand_down = access land access_expand_down_or_conforming <> 0 }
    else
      Code { readable = access land access_writable_or_readable <> 0;
             conforming = access land access_expand_down_or_conforming <> 0 }
  in
  { desc_base; desc_limit; desc_big = flags land flag_big <> 0; desc_access = access;
    desc_flags = flags; desc_kind; desc_dpl = (access lsr 5) land 3;
    desc_present = access land access_present <> 0 }

(* ---------- 표에서 읽기 ---------- *)

(* 셀렉터의 bit 2(TI)가 GDT(0)와 LDT(1)를 가른다. bit 0-1 은 RPL 이다. *)
let selector_table_indicator = 0x4
let selector_index_mask = 0xfff8
let selector_null_mask = 0xfffc
let descriptor_bytes = 8

type lookup =
  | Null_selector  (** TI=0 이고 인덱스 0. RPL 은 본다 *)
  | Beyond_table  (** 기술자 여덟 바이트가 표의 limit 안에 들지 않는다 *)
  | Found of descriptor

(* 표 메모리는 선형 = 물리로 읽는다. 페이징이 켜진 때는 [Cpu86_system] 이 첫 1MB 가 항등
   매핑인지 확인해 둔 뒤라 같은 값이다(5 단계 전까지의 한계). *)
let read_descriptor t selector =
  if selector land selector_null_mask = 0 then Null_selector
  else begin
    let table_base, table_limit =
      if selector land selector_table_indicator = 0 then
        (t.gdtr.table_base, t.gdtr.table_limit)
      else (t.ldt.sys_base, t.ldt.sys_limit)
    in
    let offset = selector land selector_index_mask in
    if offset + descriptor_bytes - 1 > table_limit then Beyond_table
    else begin
      let byte i = t.read ((table_base + offset + i) land t.addr_mask) in
      let b0 = byte 0 in
      let b1 = byte 1 in
      let b2 = byte 2 in
      let b3 = byte 3 in
      let b4 = byte 4 in
      let b5 = byte 5 in
      let b6 = byte 6 in
      let b7 = byte 7 in
      let low = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
      let high = b4 lor (b5 lsl 8) lor (b6 lsl 16) lor (b7 lsl 24) in
      Found (parse ~low ~high)
    end
  end

(* ---------- 적재 ---------- *)

let segment_names = [| "ES"; "CS"; "SS"; "DS"; "FS"; "GS" |]

let stop t what =
  raise (Unsupported (Printf.sprintf "%s @%04x:%04x (privilege levels and gates: RFC 0005 4)"
                        what t.segs.(sreg_cs) t.ip))

let store t n selector d =
  t.segs.(n) <- selector;
  t.bases.(n) <- d.desc_base;
  t.limits.(n) <- d.desc_limit;
  if n = sreg_cs then t.code32 <- d.desc_big;
  if n = sreg_ss then t.stack32 <- d.desc_big

let selector_rpl_mask = 0x3

(* CPL 이 0 일 때 [n] 에 [selector] 로 [d] 를 싣는 것이 링을 넘는지. *)
let outside_ring_zero n selector d =
  let rpl = selector land selector_rpl_mask in
  if n = sreg_cs || n = sreg_ss then rpl <> 0 || d.desc_dpl <> 0 else rpl > d.desc_dpl

(* 오류 코드가 되는 선택자: 위 14 비트(TI 포함). RPL 자리는 오류 코드에서 EXT·IDT 비트다
   (386 PRM 9.7 "Error Code"). *)
let selector_error_code selector = selector land selector_null_mask

(* 보호 모드의 적재. 쓰기는 모든 검사를 지난 마지막에 한다 — 던진 결함이나 멈춘 적재가
   반쯤 쓰인 캐시를 남기지 않는다.

   결함과 오류 코드와 검사 순서는 386 PRM 17 장의 MOV(Sreg)·JMP·RET·IRET 의 Operation 에서
   읽었다. 종류가 틀리면 #GP(선택자), 그다음에 없음(SS 는 #SS(선택자), 나머지는
   #NP(선택자))이다. 널 CS·SS 와 표 밖 선택자는 각각 #GP(0)·#GP(선택자)다.

   특권 수준은 링 0 하나만 안다(CPL 은 늘 0): CS·SS 는 RPL 과 DPL 이 0 이어야 하고,
   데이터 세그먼트는 RPL 이 DPL 이하여야 한다. 그 밖은 링을 넘는 적재라 4 단계 전에는
   멈춘다. 확인 필요: PRM 의 MOV 는 SS·DS 류의 RPL 불일치도 #GP(선택자)로 적고 conforming
   코드를 DS 에 싣는 것도 #GP 로 읽히게 적는다. 3b 가 받아 온 동작은 그대로 두었다. *)
let load_protected t n selector =
  let name = segment_names.(n) in
  let code = selector_error_code selector in
  let not_present () =
    if n = sreg_ss then fault (Stack_fault code) else fault (Segment_not_present code)
  in
  match read_descriptor t selector with
  | Null_selector ->
    (* MOV(Sreg): "IF selector is null THEN #GP(0)" 는 SS 만이고 DS·ES·FS·GS 는 널을 받는다.
       far JMP·RET·IRET 의 널 CS 도 #GP(0) 다. *)
    if n = sreg_cs || n = sreg_ss then fault (General_protection 0)
    else begin
      t.segs.(n) <- selector;
      t.bases.(n) <- 0;
      t.limits.(n) <- no_limit
    end
  | Beyond_table ->
    (* MOV(Sreg)·JMP·RET: "Selector index must be within its descriptor table limits
       else #GP(selector)" *)
    fault (General_protection code)
  | Found d ->
    (match d.desc_kind with
     | System ty ->
       (* 데이터·스택 레지스터: "AR byte must indicate data or readable code segment else
          #GP(selector)"(MOV). CS 의 시스템 기술자는 호출 게이트·TSS 일 수 있어(JMP) 결함이
          아니라 아직 없는 기능이다. *)
       if n = sreg_cs then
         stop t (Printf.sprintf "selector %04x into CS is a system descriptor (type %x): a gate or a TSS"
                   selector ty)
       else fault (General_protection code)
     | Data { writable; expand_down } ->
       if n = sreg_cs then fault (General_protection code) (* JMP·RET: 코드 세그먼트여야 한다 *)
       else if n = sreg_ss && not writable then
         fault (General_protection code) (* MOV: "must indicate a writable data segment" *)
       else if outside_ring_zero n selector d then
         stop t (Printf.sprintf "selector %04x into %s crosses a privilege level" selector name)
       else if not d.desc_present then not_present ()
       else if expand_down then
         stop t (Printf.sprintf "selector %04x into %s is an expand-down segment" selector name)
       else store t n selector d
     | Code { readable; _ } ->
       if n = sreg_ss then fault (General_protection code) (* MOV: 쓸 수 있는 데이터여야 한다 *)
       else if n <> sreg_cs && not readable then
         fault (General_protection code) (* MOV: "data or readable code segment" *)
       else if outside_ring_zero n selector d then
         stop t (Printf.sprintf "selector %04x into %s crosses a privilege level" selector name)
       else if not d.desc_present then not_present ()
       else store t n selector d)

(* 세그먼트 레지스터를 싣는 모든 명령(MOV·POP·LDS 류·far JMP/CALL/RET·IRET)이 이것을
   지난다. 8086-286 의 CR0 는 늘 0 이라 실모드 적재다. 실모드 쪽은 [set_real_seg] 와 같은
   두 줄을 바로 쓴다: 모듈 사이 호출은 인라인되지 않아서 한 번 더 부르면 삼국지3 5백만
   스텝의 호스트 명령 수가 0.2% 가량 는다. *)
let set_seg t n v =
  if t.cr0 land cr0_pe = 0 then begin
    let selector = v land 0xffff in
    t.segs.(n) <- selector;
    t.bases.(n) <- selector lsl 4
  end else load_protected t n (v land 0xffff)

(* CS 의 limit 이 [selector] 를 싣고 난 뒤 갖게 될 값. 먼 점프·호출·복귀는 목표 오프셋을
   새 CS 의 limit 으로 따지는데, 그 검사가 스택 쓰기와 세그먼트 적재보다 앞서 선다.
   실모드 적재는 limit 을 건드리지 않아 지금 값이다. 적재가 멈출 셀렉터는 적재가
   스스로 멈추니 지금 값을 돌려준다. *)
let code_limit_after t selector =
  if t.cr0 land cr0_pe = 0 then t.limits.(sreg_cs)
  else
    match read_descriptor t (selector land 0xffff) with
    | Found d -> d.desc_limit
    | Null_selector | Beyond_table -> t.limits.(sreg_cs)
