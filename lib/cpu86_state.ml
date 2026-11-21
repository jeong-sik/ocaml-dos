(* 코어의 상태: 모델, 레지스터, 플래그, 명령 하나의 문맥, 스냅숏 값. *)

exception Unsupported of string

(* 어느 세대의 실리콘을 흉내내는가. 8086 과 80186 은 같은 opcode 자리에
   서로 다른 명령을 놓았다 — 0x60-0x6F, 0xC0/0xC1, 0xC8/0xC9, 그룹2 의
   reg=6 이 그렇다. 어느 쪽이 맞는지는 코어가 정할 수 없고 흉내낼 칩이
   정한다. 실칩 스위트는 [I8086] 로 돈다.

   80286 은 실모드에서 186 과 같은 명령을 돌린다. 프로그램이 둘을 가르는
   자리만 다르다: FLAGS 의 bit12-15 는 읽으면 0 이고, PUSH SP 는 줄기 전
   SP 를 밀고, 0x0F 는 POP CS 가 아니라 두 바이트 명령(SMSW 등)의 머리다.
   1990년대 DOS 게임이 이것으로 "286 이상" 을 확인한다. 나눗셈 오류가
   나눗셈 자신으로 돌아가는 것도 286 부터다.

   80386 은 286 의 실모드 위에 32 비트 레지스터의 위 절반, FS·GS, 실모드에서도
   쓸 수 있는 FLAGS bit12-14(IOPL·NT)를 더한다. 게임은 bit12-14 가 남는지로
   "386 이상" 을 가른다. 32 비트 명령과 0x0F 확장은 아직 없다(RFC 0001 2·3
   단계). *)
type model = I8086 | I80186 | I80286 | I80386

(* 286 실모드의 MSW: 예약 비트 4-15 는 1, PE·MP·EM·TS 는 0. *)
let msw_real_mode_286 = 0xFFF0

(* SGDT·SIDT 는 기술자 표 레지스터를 6바이트로 쓴다: 한계(16비트), 기준의 하위
   24비트, 마지막 바이트. 286 은 마지막 바이트에 FFh 를 쓰고 386 은 기준의 위
   8비트를 쓴다. 게임은 이 바이트로 286 과 386 을 가른다(`push sp` 로 286 이상을
   확인한 다음 단계: Charlie the Duck 은 `inc bh` 가 0 이면 286).

   근거: Intel 80386 Programmer's Reference Manual, SGDT/SIDT 의 Compatibility
   Note — "The 80286 stores 1's in these upper bits, whereas the 80386 stores
   0's if the operand-size attribute is 16 bits. These bits were specified as
   undefined by the SGDT/SIDT instructions in the iAPX 286 Programmer's
   Reference Manual." 즉 FFh 는 286 매뉴얼이 정한 값이 아니라 칩이 보이는
   동작이다. 386 이 16비트 피연산자에서도 기준 32비트를 다 쓴다는 보고는
   OS/2 Museum "SGDT/SIDT Fiction and Reality" 에 있다. 이 코어는 피연산자 크기를
   보지 않고 늘 48비트를 쓴다(확인 필요: 실칩 386 은 못 봤다). LGDT·LIDT 가
   16비트 피연산자로 기준의 위 8비트를 0 으로 싣기 때문에, 16비트 명령만 쓰는
   프로그램은 어느 쪽 해석에서도 0 을 읽는다.

   리셋 값: IDTR 은 386 PRM 10.1 "Processor State After Reset" 이 "IDTR: base =0
   limit =03FFH" 라고 적었다(실모드 IVT 와 같다). 286 도 같다고 보았다(확인 필요:
   286 매뉴얼은 못 읽었다). GDTR 은 같은 절이 "All registers not mentioned above
   are undefined" 라서 값이 정해져 있지 않다 — 0 으로 둔다. *)
let gdtr_reset_limit = 0x0000
let idtr_reset_limit = 0x03FF
let descriptor_table_reset_base = 0
let descriptor_table_top_byte_286 = 0xFF

(* 기술자 표 레지스터 하나: GDTR·IDTR. *)
type descriptor_table = { mutable table_base : int; mutable table_limit : int }

(* 셀렉터로 고르는 시스템 세그먼트 레지스터 하나: LDTR·TR. 셀렉터와 그 기술자에서
   읽은 base·limit 을 들고 있다(캐시). 적재한 뒤 표 메모리를 고쳐도 캐시는 그대로다. *)
type system_segment = {
  mutable sys_selector : int;
  mutable sys_base : int;
  mutable sys_limit : int;
}

(* LDTR·TR 의 리셋 값. 386 PRM 10.1 은 리셋 뒤 두 레지스터를 적지 않는다(확인
   필요). 널 셀렉터와 빈 표로 둔다. *)
let system_segment_reset () = { sys_selector = 0; sys_base = 0; sys_limit = 0 }

(* CR0 의 비트. PE(보호 모드), MP·EM·TS(코프로세서), ET, PG(페이징). 386 PRM 10.1
   은 리셋 뒤 CR0 의 ET 가 80387 이 있을 때만 켜진다고 적었다. 이 기계에는
   코프로세서가 없어 모두 0 이다(확인 필요: 같은 절의 그림 10-2 는 못 읽었다).
   5-30 은 예약이라 쓰기를 받지 않고 0 으로 읽는다(확인 필요: 실칩이 무엇으로
   읽는지는 못 봤다). *)
let cr0_pe = 0x00000001
let cr0_mp = 0x00000002
let cr0_em = 0x00000004
let cr0_ts = 0x00000008
let cr0_et = 0x00000010
let cr0_pg = 0x80000000
let cr0_defined = cr0_pe lor cr0_mp lor cr0_em lor cr0_ts lor cr0_et lor cr0_pg

let cr0_reset = 0

(* 386 의 32 비트 시스템 레지스터(CR2·CR3·DR·TR, 기술자 표 기준)가 담는 폭. *)
let register32_mask = 0xffffffff

(* LMSW 가 쓰는 CR0 의 아래 네 비트: PE MP EM TS. *)
let msw_loadable = cr0_pe lor cr0_mp lor cr0_em lor cr0_ts

(* 디버그 레지스터 DR0-DR7 과 시험 레지스터 TR6·TR7 의 수. *)
let debug_register_count = 8
let test_register_first = 6
let test_register_count = 2

let f_carry = 0x001
let f_parity = 0x004
let f_aux = 0x010
let f_zero = 0x040
let f_sign = 0x080
let f_trap = 0x100
let f_interrupt = 0x200
let f_direction = 0x400
let f_overflow = 0x800

(* 주소선 수. 8086·186 은 20개라 FFFF:0010 이 0 으로 감기고, 286 은
   24개라 실모드에서도 10FFEFh 까지 닿는다. 1MB 에서 감는 것은 보드가 할
   일이다(Dos_a20 의 마스크를 Dos_machine 의 메모리 접근이 씌운다). 386DX 는
   32개다. *)
let address_mask = function
  | I8086 | I80186 -> 0xfffff
  | I80286 -> 0xffffff
  | I80386 -> 0xffffffff

(* 명령 하나의 최대 길이. 286 은 접두 바이트까지 10 바이트를 넘으면 INT 13
   이다(SingleStepTests/80286 9A·EA: 접두 6 개 + 5 바이트). 8086·186 은
   접두 바이트를 몇 개든 받는다. 386 은 15 바이트다(Intel 80386 Programmer's
   Reference Manual, 1986). *)
let max_length = function I8086 | I80186 -> max_int | I80286 -> 10 | I80386 -> 15

(* 세그먼트 레지스터 수: ES CS SS DS, 386 은 FS GS 까지. *)
let segment_count = function I8086 | I80186 | I80286 -> 4 | I80386 -> 6

(* ES CS SS DS: 모든 모델에 있는 세그먼트. *)
let classic_segments = segment_count I8086

(* 실모드 세그먼트의 한계: 마지막으로 닿을 수 있는 오프셋. 세그먼트가 64KB 라
   FFFFh 다. [t.limits] 의 모든 칸이 이 값으로 시작한다. *)
let real_mode_limit = 0xffff

(* 널 셀렉터를 실은 DS·ES·FS·GS 의 limit. 오프셋 0 의 한 바이트도 한계 밖이다. *)
let no_limit = -1

(* 세그먼트 레지스터 번호: [segs] 의 칸이고 modrm reg 필드(8C·8E)의 값이다. *)
let sreg_es = 0
let sreg_cs = 1
let sreg_ss = 2
let sreg_ds = 3
let sreg_fs = 4
let sreg_gs = 5

(* 명령 앞에 붙는 바이트의 종류. [Rep true] 는 rep/repz, [Rep false] 는
   repnz 다. 386 만 64h·65h 를 FS·GS override 로, 66h·67h 를 피연산자·주소
   크기 바꾸기로 읽는다. [prefix_table] 이
   모델마다 256 칸으로 펼쳐 두어 바이트 하나를 한 번 찾아 가른다 — 함수로
   가르면 삼국지3 3천만 스텝의 기계어 명령이 0.75% 는다. *)
type prefix = Not_prefix | Segment of int | Lock | Rep of bool | Operand_size | Address_size

let prefix_of_byte model b =
  match b with
  | 0x26 -> Segment sreg_es
  | 0x2e -> Segment sreg_cs
  | 0x36 -> Segment sreg_ss
  | 0x3e -> Segment sreg_ds
  | 0x64 | 0x65 ->
    (match model with
     | I80386 -> Segment (if b = 0x64 then sreg_fs else sreg_gs)
     | I8086 | I80186 | I80286 -> Not_prefix)
  | 0x66 ->
    (match model with
     | I80386 -> Operand_size
     | I8086 | I80186 | I80286 -> Not_prefix)
  | 0x67 ->
    (match model with
     | I80386 -> Address_size
     | I8086 | I80186 | I80286 -> Not_prefix)
  | 0xf0 -> Lock
  | 0xf2 -> Rep false
  | 0xf3 -> Rep true
  | _ -> Not_prefix

let byte_values = 256
let prefix_table model = Array.init byte_values (prefix_of_byte model)

(* 범용 레지스터 위 절반의 칸 수. 386 만 있다. *)
let upper_halves = function I8086 | I80186 | I80286 -> 0 | I80386 -> 8

(* FLAGS bit12-14(IOPL 두 비트, NT). 386 은 실모드에서도 POPF·IRET 로 쓴다. *)
let iopl_nt_bits = 0x7000
let no_fetch_end = -1

(* 페치 뒤 IP 가 도는 폭. 8086-286 은 FFFF 다음이 0 이다. 386 의 EIP 는 32
   비트라 CS 의 마지막 바이트를 가져오면 limit+1 이 되고, 그 자리를 가져오려 하면
   CS 한계 밖이라 INT 13 이다(CS 의 limit+1 이 [fetch_end] 를 막는다).
   SingleStepTests/80386 의 ret·jmp·call·iret 이 FFFF 의 HLT 로 가면 끝난 뒤
   EIP 가 10000h 다. 한계가 FFFFFFFFh 인 CS 의 마지막 바이트 다음은 100000000h
   라 33 비트를 담는다. *)
let fetch_ip_mask = function I8086 | I80186 | I80286 -> 0xffff | I80386 -> 0x1ffffffff

(* 메모리 접근의 오프셋을 자르는 폭. 8086·186 은 오프셋이 64KB 에서 감긴다. 286 은
   오프셋 FFFF 의 워드가 한계 밖이라 접근 전에 결함이고, 386 은 오프셋이 32 비트다
   (한계가 FFFFh 보다 큰 보호 모드 세그먼트). *)
let offset_mask = function
  | I8086 | I80186 | I80286 -> 0xffff
  | I80386 -> 0xffffffff

type t = {
  model : model;
  addr_mask : int;  (** [address_mask model] — 메모리 접근마다 쓰는 파생값 *)
  offset_mask : int;  (** [offset_mask model] — 메모리 접근마다 쓰는 파생값 *)
  mutable regs : int array;  (** AX CX DX BX SP BP SI DI *)
  regs_hi : int array;
      (** 386 의 EAX…EDI 위 16 비트. 16 비트 쓰기는 [regs] 만 바꾸니 위
          절반이 저절로 남는다. 다른 모델은 빈 배열이다 *)
  mutable segs : int array;  (** ES CS SS DS, 386 은 FS GS 까지 *)
  bases : int array;
      (** [segs] 칸마다 세그먼트 베이스. 주소를 세는 곳은 셀렉터×16 대신 이
          칸을 읽는다. [set_seg] 가 셀렉터×16 으로 쓴다 *)
  limits : int array;
      (** [segs] 칸마다 마지막으로 닿을 수 있는 오프셋. 만들 때 [real_mode_limit]
          이고, 실모드 적재는 쓰지 않는다. 널 셀렉터를 실은 DS·ES·FS·GS 는 -1 이라
          어느 접근도 한계 밖이다 *)
  mutable code32 : bool;
      (** CS 의 D 비트. 켜지면 접두 없는 명령의 피연산자·주소 크기가 32 비트다.
          실모드 적재는 쓰지 않는다 *)
  mutable stack32 : bool;
      (** SS 의 B 비트. 켜지면 스택 포인터가 SP 대신 ESP 다. 실모드 적재는 쓰지
          않는다 *)
  mutable ip : int;
  mutable cf : bool;
  mutable pf : bool;
  mutable af : bool;
  mutable zf : bool;
  mutable sf : bool;
  mutable tf : bool;
  mutable intf : bool;
  mutable df : bool;
  mutable of_ : bool;
  mutable iopl_nt : int;  (** FLAGS bit12-14. 386 만 쓰고, 다른 모델은 늘 0 *)
  mutable cr0 : int;
      (** 386 의 CR0. [cr0_defined] 의 비트만 든다. 다른 모델은 리셋 값 그대로다 *)
  mutable cr2 : int;  (** 페이지 결함 주소. 페이징(RFC 0005 5 단계)이 쓴다 *)
  mutable cr3 : int;  (** 페이지 디렉터리 주소. 페이징이 쓴다 *)
  gdtr : descriptor_table;
  idtr : descriptor_table;
  ldt : system_segment;  (** LDTR *)
  task : system_segment;  (** TR *)
  dr : int array;  (** DR0-DR7: 쓴 값을 들고 있을 뿐 중단점은 걸지 않는다 *)
  tr : int array;  (** TR6·TR7: 쓴 값을 들고 있을 뿐이다 *)
  mutable halted : bool;
  mutable cycles : int;
  mutable int_hook : (int -> unit) option;
  fetch_limit : int;  (** [max_length model] — 명령 하나가 넘을 수 없는 바이트 수 *)
  mutable fetch_end : int;
      (** 이 IP 에서 바이트를 가져오면 명령이 [fetch_limit] 를 넘는다. 286 은
          명령마다 정하고, 길이 제한이 없는 8086·186 은 IP 가 될 수 없는 -1 *)
  fetch_ip_mask : int;  (** [fetch_ip_mask model] *)
  prefixes : prefix array;  (** [prefix_table model] *)
  undo_regs : int array;  (** [atomic] 이 결함 때 되돌릴 범용 레지스터 *)
  undo_segs : int array;  (** 그때의 세그먼트 레지스터 *)
  undo_bases : int array;  (** 그때의 세그먼트 베이스 *)
  undo_hi : int array;  (** 그때의 범용 레지스터 위 절반(386) *)
  read : int -> int;
  write : int -> int -> unit;
  port_in : int -> int;
  port_out : int -> int -> unit;
}

(* 세그먼트 레지스터 [sreg] 의 베이스에 [off] 를 더한 버스 주소. *)
let physical t ~sreg ~off = (t.bases.(sreg) + off) land t.addr_mask

let create ?(model = I80186) ~read ~write ~port_in ~port_out () =
  {
    model;
    addr_mask = address_mask model;
    offset_mask = offset_mask model;
    regs = Array.make 8 0;
    regs_hi = Array.make (upper_halves model) 0;
    segs = Array.make (segment_count model) 0;
    bases = Array.make (segment_count model) 0;
    limits = Array.make (segment_count model) real_mode_limit;
    code32 = false;
    stack32 = false;
    ip = 0;
    cf = false; pf = false; af = false; zf = false; sf = false;
    tf = false; intf = true; df = false; of_ = false;
    iopl_nt = 0;
    cr0 = cr0_reset; cr2 = 0; cr3 = 0;
    gdtr = { table_base = descriptor_table_reset_base; table_limit = gdtr_reset_limit };
    idtr = { table_base = descriptor_table_reset_base; table_limit = idtr_reset_limit };
    ldt = system_segment_reset ();
    task = system_segment_reset ();
    dr = Array.make debug_register_count 0;
    tr = Array.make test_register_count 0;
    halted = false;
    cycles = 0;
    int_hook = None;
    fetch_limit = max_length model;
    fetch_end = no_fetch_end;
    fetch_ip_mask = fetch_ip_mask model;
    prefixes = prefix_table model;
    undo_regs = Array.make 8 0;
    undo_segs = Array.make classic_segments 0;
    undo_bases = Array.make classic_segments 0;
    undo_hi = Array.make (upper_halves model) 0;
    read; write; port_in; port_out;
  }

let halted t = t.halted

(* HLT 해제 — 인터럽트 도착로 깨운다(8086 계약: HLT 는 (N)INTR 로 해제). *)
let wake t = t.halted <- false
let cycles t = t.cycles
let set_int_hook t f = t.int_hook <- Some f

let reg16 t n = t.regs.(n)
let set_reg16 t n v = t.regs.(n) <- v land 0xffff

let no_upper_half what =
  invalid_arg (Printf.sprintf "Cpu86.%s: only the 80386 has 32-bit registers" what)

(* 386 의 32 비트 레지스터. [get32]·[put32] 는 모델을 보지 않는다 — 66h 가
   붙은 명령만 부르고, 66h 는 386 에서만 접두다. *)
let get32 t n = (t.regs_hi.(n) lsl 16) lor t.regs.(n)

let put32 t n v =
  t.regs.(n) <- v land 0xffff;
  t.regs_hi.(n) <- (v lsr 16) land 0xffff

(* 스택 포인터: SS 의 B 비트가 켜지면 ESP, 아니면 SP. 386 이 아닌 모델은 B 가
   늘 꺼져 있어 [regs_hi] 를 건드리지 않는다. 호출하는 곳은 스택 명령이다. *)
let stack_mask t = if t.stack32 then 0xffffffff else 0xffff

let stack_pointer t = if t.stack32 then get32 t 4 else t.regs.(4)

let set_stack_pointer t v =
  if t.stack32 then put32 t 4 v else t.regs.(4) <- v land 0xffff

(* SP·ESP 를 [n] 바이트 올린다(RET imm16 이 인자를 버린다). *)
let drop_stack t n = set_stack_pointer t ((stack_pointer t + n) land stack_mask t)

(* ENTER·LEAVE 의 프레임 포인터: 스택이 32 비트면 EBP, 아니면 BP. *)
let frame_pointer t = if t.stack32 then get32 t 5 else t.regs.(5)

let set_frame_pointer t v =
  if t.stack32 then put32 t 5 v else t.regs.(5) <- v land 0xffff

let reg32 t n =
  match t.model with
  | I80386 -> get32 t n
  | I8086 | I80186 | I80286 -> no_upper_half "reg32"

let set_reg32 t n v =
  match t.model with
  | I80386 -> put32 t n v
  | I8086 | I80186 | I80286 -> no_upper_half "set_reg32"

let reg8 t n =
  let full = t.regs.(n land 3) in
  if n < 4 then full land 0xff else full lsr 8

let set_reg8 t n v =
  let b = v land 0xff in
  let i = n land 3 in
  t.regs.(i) <-
    (if n < 4 then (t.regs.(i) land 0xff00) lor b
     else (t.regs.(i) land 0x00ff) lor (b lsl 8))

let seg t n = t.segs.(n)

(* 실모드의 세그먼트 적재: 셀렉터와 base(셀렉터×16)만 쓴다. limit·D·B 는 그대로다 —
   보호 모드에서 돌아온 386 이 FFFFh 보다 큰 limit 을 들고 실모드를 도는 것이
   실칩 동작이다(Intel 80386 PRM 의 실모드 복귀 절은 돌아가기 전에 64KB limit 으로
   다시 싣게 한다). 세그먼트 레지스터를 쓰는 곳은 이것, 보호 모드 적재
   ([Cpu86_segment.set_seg]), [Cpu86_fault.atomic] 의 되돌림, [load_state] 다. *)
let set_real_seg t n v =
  let selector = v land 0xffff in
  t.segs.(n) <- selector;
  t.bases.(n) <- selector lsl 4

let dump_ip t = t.ip
let bus_read t a = t.read a
let bus_write t a v = t.write a v
let set_ip t v = t.ip <- v land 0xffff

(* FLAGS 워드의 bit1 은 항상 1 이다. bits12-15 는 8086·186 에서 읽으면
   항상 1(실칩 SingleStepTests 실측 — 계산 비트만 돌려주면 상위 니블이
   0 으로 나와 전 add 그룹이 fail), 286 실모드에서는 항상 0 이다(IOPL·NT
   를 실모드에서 세울 수 없다). 386 은 bit15 만 0 이고 bit12-14 는
   [iopl_nt] 가 든다. *)
let fixed_flags = function I8086 | I80186 -> 0xF002 | I80286 | I80386 -> 0x0002

let flags t =
  let b cond bit = if cond then bit else 0 in
  fixed_flags t.model lor t.iopl_nt
  lor b t.cf f_carry lor b t.pf f_parity lor b t.af f_aux
  lor b t.zf f_zero lor b t.sf f_sign lor b t.tf f_trap
  lor b t.intf f_interrupt lor b t.df f_direction
  lor b t.of_ f_overflow

let set_flags t v =
  let test bit = v land bit <> 0 in
  t.cf <- test f_carry; t.pf <- test f_parity; t.af <- test f_aux;
  t.zf <- test f_zero; t.sf <- test f_sign; t.tf <- test f_trap;
  t.intf <- test f_interrupt; t.df <- test f_direction; t.of_ <- test f_overflow;
  match t.model with
  | I80386 -> t.iopl_nt <- v land iopl_nt_bits
  | I8086 | I80186 | I80286 -> ()

(* 하위 8비트의 1 개수가 짝수면 PF. 8086 은 PF 만 8비트 폭으로 본다. *)
let parity_even v =
  let n = ref (v land 0xff) in
  let count = ref 0 in
  while !n <> 0 do
    count := !count + (!n land 1);
    n := !n lsr 1
  done;
  !count land 1 = 0

(* 메모리 피연산자를 만드는 규칙: 주소 크기와 세그먼트 override. 67h 는 386
   에서만 [A32] 쪽을 만든다. 둘을 한 값에 두는 것은 둘 다 메모리 피연산자를
   만드는 곳(modrm, 문자열, XLAT, moffs)만 보기 때문이다 — 그곳이 주소 크기를
   빼먹으면 컴파일러가 갈래가 빠졌다고 잡는다. *)
type addressing =
  | A16           (** 16 비트 주소, 기본 세그먼트 *)
  | A16_seg of int (** 16 비트 주소, 이 세그먼트 레지스터 *)
  | A32           (** 32 비트 주소(67h), 기본 세그먼트 *)
  | A32_seg of int (** 32 비트 주소(67h), 이 세그먼트 레지스터 *)

let address32 = function A32 | A32_seg _ -> true | A16 | A16_seg _ -> false

let segment_for addr ~default =
  match addr with
  | A16_seg s | A32_seg s -> s
  | A16 | A32 -> default

(* 명령 하나의 문맥. 접두를 다 읽고 나면 바뀌지 않는다. [base_ip] 는 첫
   접두 바이트의 IP 다 — 결함과 Unsupported 가 그 자리로 되돌린다. [addr] 는
   메모리 피연산자의 규칙([addressing]), [rep] 은 Some true(rep/repz)·Some
   false(repnz). LOCK 은 접두를 읽는 동안 [Cpu86_decode.check_lock] 이 따지고
   끝난다. [opcode] 는 opcode 바이트이고, 386 에서 66h 가 붙어 폭이 32 비트가
   되는 명령이면 [operand32_form] 이 더해져 있다. *)
type ctx = { base_ip : int; opcode : int; addr : addressing; rep : bool option }

(* [ctx.opcode] 의 bit 8. 필드 대신 opcode 값에 싣는 것은 16 비트 명령이 이
   표시를 따질 일이 없어서다: [step] 의 match 에서 0xFF 를 넘는 값은 마지막
   갈래에만 닿는다. 삼국지3 3천만 스텝의 기계어 명령이 66h 전 24.06G 이고,
   bool 필드·접두 인자·갈래 검사로 들고 다니면 24.33G, 이렇게 실으면 24.15G
   다. *)
let operand32_form = 0x100
let opcode_byte = 0xff

(* [t.ip] already passed the opcode and modrm bytes [decode_prefixes]/the
   dispatch consumed while deciding this instruction is unsupported. Left
   there, the next [step] would resume mid instruction and run whatever
   bytes follow as a fresh one -- real hardware instead re-faults on the
   same instruction (#UD points at it, not past it). Rewinding to
   [ctx.base_ip] here gives the same guarantee: nothing runs until a
   caller does something about the fault. *)
let unsupported t ctx what =
  t.ip <- ctx.base_ip;
  raise (Unsupported (Printf.sprintf "%s @%04x:%04x (opcode %02x)"
            what (seg t 1) ctx.base_ip ctx.opcode))

(* ---------- saved state ---------- *)

type saved_386 = {
  saved_regs_hi : int array;
  saved_fs : int;
  saved_gs : int;
  saved_cr0 : int;
  saved_cr2 : int;
  saved_cr3 : int;
  saved_gdtr_base : int;
  saved_gdtr_limit : int;
  saved_idtr_base : int;
  saved_idtr_limit : int;
  saved_dr : int array;
  saved_tr : int array;
  saved_bases : int array;
  saved_limits : int array;
  saved_code32 : bool;
  saved_stack32 : bool;
  saved_ldt_selector : int;
  saved_ldt_base : int;
  saved_ldt_limit : int;
  saved_task_selector : int;
  saved_task_base : int;
  saved_task_limit : int;
}

type saved = {
  saved_regs : int array;
  saved_segs : int array;
  saved_ip : int;
  saved_flags : int;
  saved_halted : bool;
  saved_cycles : int;
  saved_386 : saved_386 option;
}

let model t = t.model

(* The full pattern is the point: a field added to [t] fails the build here
   until a snapshot either carries it or says why it does not. *)
let save_state t =
  let { model (* fixed by whoever creates the CPU; picks what [saved_386] holds *);
        addr_mask = _; offset_mask = _ (* derived from the model *);
        regs; segs; ip; regs_hi; bases; limits; code32; stack32; ldt; task;
        iopl_nt = _ (* in the flags word, [flags t] *);
        cr0; cr2; cr3; gdtr; idtr; dr; tr;
        cf = _; pf = _; af = _; zf = _; sf = _; tf = _; intf = _; df = _;
        of_ = _ (* the nine flags travel as one word, [flags t] *);
        halted; cycles;
        fetch_limit = _ (* derived from the model *);
        fetch_end = _; fetch_ip_mask = _; prefixes = _ (* derived from the model *);
        undo_regs = _; undo_segs = _; undo_bases = _; undo_hi = _
        (* scratch for the step in progress; a snapshot falls between steps *);
        int_hook = _; read = _; write = _; port_in = _; port_out = _
        (* closures: the machine that owns this CPU wires them *) } = t
  in
  let saved_386 =
    match model with
    | I80386 ->
      Some { saved_regs_hi = Array.copy regs_hi; saved_fs = segs.(sreg_fs);
             saved_gs = segs.(sreg_gs);
             saved_cr0 = cr0; saved_cr2 = cr2; saved_cr3 = cr3;
             saved_gdtr_base = gdtr.table_base; saved_gdtr_limit = gdtr.table_limit;
             saved_idtr_base = idtr.table_base; saved_idtr_limit = idtr.table_limit;
             saved_dr = Array.copy dr; saved_tr = Array.copy tr;
             saved_bases = Array.copy bases; saved_limits = Array.copy limits;
             saved_code32 = code32; saved_stack32 = stack32;
             saved_ldt_selector = ldt.sys_selector; saved_ldt_base = ldt.sys_base;
             saved_ldt_limit = ldt.sys_limit;
             saved_task_selector = task.sys_selector; saved_task_base = task.sys_base;
             saved_task_limit = task.sys_limit }
    | I8086 | I80186 | I80286 -> None
  in
  { saved_regs = Array.copy regs; saved_segs = Array.sub segs 0 classic_segments;
    saved_ip = ip; saved_flags = flags t; saved_halted = halted; saved_cycles = cycles;
    saved_386 }

(* Everything is checked before anything is written: a refused state leaves
   the CPU as it was. *)
let load_state t s =
  if Array.length s.saved_regs <> Array.length t.regs
     || Array.length s.saved_segs <> classic_segments then
    invalid_arg "Cpu86.load_state: 8 registers and 4 segments";
  (match t.model, s.saved_386 with
   | I80386, Some x ->
     if Array.length x.saved_regs_hi <> upper_halves I80386 then
       invalid_arg "Cpu86.load_state: 8 upper halves";
     if Array.length x.saved_dr <> debug_register_count
        || Array.length x.saved_tr <> test_register_count then
       invalid_arg "Cpu86.load_state: 8 debug registers and 2 test registers";
     if Array.length x.saved_bases <> segment_count I80386
        || Array.length x.saved_limits <> segment_count I80386 then
       invalid_arg "Cpu86.load_state: 6 segment bases and 6 segment limits";
     if x.saved_cr0 land cr0_pg <> 0 && x.saved_cr0 land cr0_pe = 0 then
       invalid_arg "Cpu86.load_state: CR0.PG is on with CR0.PE off"
   | I80386, None -> invalid_arg "Cpu86.load_state: an 80386 needs its upper halves, FS and GS"
   | (I8086 | I80186 | I80286), Some _ ->
     invalid_arg "Cpu86.load_state: 80386 state for an older model"
   | (I8086 | I80186 | I80286), None -> ());
  (match s.saved_386 with
   | Some x ->
     Array.iteri (fun i v -> t.regs_hi.(i) <- v land 0xffff) x.saved_regs_hi;
     Array.iteri (fun i v -> t.segs.(i) <- v land 0xffff) s.saved_segs;
     t.segs.(sreg_fs) <- x.saved_fs land 0xffff;
     t.segs.(sreg_gs) <- x.saved_gs land 0xffff;
     Array.iteri (fun i v -> t.bases.(i) <- v land register32_mask) x.saved_bases;
     Array.iteri (fun i v -> t.limits.(i) <- max no_limit (min v register32_mask))
       x.saved_limits;
     t.code32 <- x.saved_code32;
     t.stack32 <- x.saved_stack32;
     t.ldt.sys_selector <- x.saved_ldt_selector land 0xffff;
     t.ldt.sys_base <- x.saved_ldt_base land register32_mask;
     t.ldt.sys_limit <- max no_limit (min x.saved_ldt_limit register32_mask);
     t.task.sys_selector <- x.saved_task_selector land 0xffff;
     t.task.sys_base <- x.saved_task_base land register32_mask;
     t.task.sys_limit <- max no_limit (min x.saved_task_limit register32_mask);
     t.cr0 <- x.saved_cr0 land cr0_defined;
     t.cr2 <- x.saved_cr2 land register32_mask;
     t.cr3 <- x.saved_cr3 land register32_mask;
     t.gdtr.table_base <- x.saved_gdtr_base land register32_mask;
     t.gdtr.table_limit <- x.saved_gdtr_limit land 0xffff;
     t.idtr.table_base <- x.saved_idtr_base land register32_mask;
     t.idtr.table_limit <- x.saved_idtr_limit land 0xffff;
     Array.iteri (fun i v -> t.dr.(i) <- v land register32_mask) x.saved_dr;
     Array.iteri (fun i v -> t.tr.(i) <- v land register32_mask) x.saved_tr
   | None -> ());
  Array.iteri (fun i v -> set_reg16 t i v) s.saved_regs;
  (match s.saved_386 with
   | Some _ -> ()
   | None -> Array.iteri (fun i v -> set_real_seg t i v) s.saved_segs);
  t.ip <- s.saved_ip land t.fetch_ip_mask;
  set_flags t s.saved_flags;
  t.halted <- s.saved_halted;
  t.cycles <- s.saved_cycles
