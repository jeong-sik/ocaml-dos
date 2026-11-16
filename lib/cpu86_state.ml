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
   나눗셈 자신으로 돌아가는 것도 286 부터다. *)
type model = I8086 | I80186 | I80286

(* 286 실모드의 MSW: 예약 비트 4-15 는 1, PE·MP·EM·TS 는 0. *)
let msw_real_mode_286 = 0xFFF0

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
   24개라 실모드에서도 10FFEFh 까지 닿는다. 1MB 에서 감는 것은 286 보드의
   A20 게이트가 할 일이다(Dos_machine 의 메모리가 감는다). *)
let address_mask = function I8086 | I80186 -> 0xfffff | I80286 -> 0xffffff

(* 명령 하나의 최대 길이. 286 은 접두 바이트까지 10 바이트를 넘으면 INT 13
   이다(SingleStepTests/80286 9A·EA: 접두 6 개 + 5 바이트). 8086·186 은
   접두 바이트를 몇 개든 받는다. *)
let max_length = function I8086 | I80186 -> max_int | I80286 -> 10
let no_fetch_end = -1

type t = {
  model : model;
  addr_mask : int;  (** [address_mask model] — 메모리 접근마다 쓰는 파생값 *)
  mutable regs : int array;  (** AX CX DX BX SP BP SI DI *)
  mutable segs : int array;  (** ES CS SS DS *)
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
  mutable halted : bool;
  mutable cycles : int;
  mutable int_hook : (int -> unit) option;
  fetch_limit : int;  (** [max_length model] — 명령 하나가 넘을 수 없는 바이트 수 *)
  mutable fetch_end : int;
      (** 이 IP 에서 바이트를 가져오면 명령이 [fetch_limit] 를 넘는다. 286 은
          명령마다 정하고, 길이 제한이 없는 8086·186 은 IP 가 될 수 없는 -1 *)
  undo_regs : int array;  (** [atomic] 이 결함 때 되돌릴 범용 레지스터 *)
  undo_segs : int array;  (** 그때의 세그먼트 레지스터 *)
  read : int -> int;
  write : int -> int -> unit;
  port_in : int -> int;
  port_out : int -> int -> unit;
}

let physical t ~seg ~off = ((seg lsl 4) + off) land t.addr_mask

let create ?(model = I80186) ~read ~write ~port_in ~port_out () =
  {
    model;
    addr_mask = address_mask model;
    regs = Array.make 8 0;
    segs = Array.make 4 0;
    ip = 0;
    cf = false; pf = false; af = false; zf = false; sf = false;
    tf = false; intf = true; df = false; of_ = false;
    halted = false;
    cycles = 0;
    int_hook = None;
    fetch_limit = max_length model;
    fetch_end = no_fetch_end;
    undo_regs = Array.make 8 0;
    undo_segs = Array.make 4 0;
    read; write; port_in; port_out;
  }

let halted t = t.halted

(* HLT 해제 — 인터럽트 도착로 깨운다(8086 계약: HLT 는 (N)INTR 로 해제). *)
let wake t = t.halted <- false
let cycles t = t.cycles
let set_int_hook t f = t.int_hook <- Some f

let reg16 t n = t.regs.(n)
let set_reg16 t n v = t.regs.(n) <- v land 0xffff

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
let set_seg t n v = t.segs.(n) <- v land 0xffff
let dump_ip t = t.ip
let set_ip t v = t.ip <- v land 0xffff

(* FLAGS 워드의 bit1 은 항상 1 이다. bits12-15 는 8086·186 에서 읽으면
   항상 1(실칩 SingleStepTests 실측 — 계산 비트만 돌려주면 상위 니블이
   0 으로 나와 전 add 그룹이 fail), 286 실모드에서는 항상 0 이다(IOPL·NT
   를 실모드에서 세울 수 없다). *)
let fixed_flags = function I8086 | I80186 -> 0xF002 | I80286 -> 0x0002

let flags t =
  let b cond bit = if cond then bit else 0 in
  fixed_flags t.model
  lor b t.cf f_carry lor b t.pf f_parity lor b t.af f_aux
  lor b t.zf f_zero lor b t.sf f_sign lor b t.tf f_trap
  lor b t.intf f_interrupt lor b t.df f_direction
  lor b t.of_ f_overflow

let set_flags t v =
  let test bit = v land bit <> 0 in
  t.cf <- test f_carry; t.pf <- test f_parity; t.af <- test f_aux;
  t.zf <- test f_zero; t.sf <- test f_sign; t.tf <- test f_trap;
  t.intf <- test f_interrupt; t.df <- test f_direction; t.of_ <- test f_overflow

(* 하위 8비트의 1 개수가 짝수면 PF. 8086 은 PF 만 8비트 폭으로 본다. *)
let parity_even v =
  let n = ref (v land 0xff) in
  let count = ref 0 in
  while !n <> 0 do
    count := !count + (!n land 1);
    n := !n lsr 1
  done;
  !count land 1 = 0

(* 명령 하나의 문맥. 접두를 다 읽고 나면 바뀌지 않는다. [base_ip] 는 첫
   접두 바이트의 IP 다 — 결함과 Unsupported 가 그 자리로 되돌린다. [ovr] 는
   세그먼트 override, [rep] 은 Some true(rep/repz)·Some false(repnz). *)
type ctx = { base_ip : int; opcode : int; ovr : int option; rep : bool option }

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

type saved = {
  saved_regs : int array;
  saved_segs : int array;
  saved_ip : int;
  saved_flags : int;
  saved_halted : bool;
  saved_cycles : int;
}

(* The full pattern is the point: a field added to [t] fails the build here
   until a snapshot either carries it or says why it does not. *)
let save_state t =
  let { model = _ (* fixed by whoever creates the CPU *);
        addr_mask = _ (* derived from the model *);
        regs; segs; ip;
        cf = _; pf = _; af = _; zf = _; sf = _; tf = _; intf = _; df = _;
        of_ = _ (* the nine flags travel as one word, [flags t] *);
        halted; cycles;
        fetch_limit = _ (* derived from the model *);
        fetch_end = _; undo_regs = _; undo_segs = _
        (* scratch for the step in progress; a snapshot falls between steps *);
        int_hook = _; read = _; write = _; port_in = _; port_out = _
        (* closures: the machine that owns this CPU wires them *) } = t
  in
  { saved_regs = Array.copy regs; saved_segs = Array.copy segs; saved_ip = ip;
    saved_flags = flags t; saved_halted = halted; saved_cycles = cycles }

let load_state t s =
  if Array.length s.saved_regs <> 8 || Array.length s.saved_segs <> 4 then
    invalid_arg "Cpu86.load_state: 8 registers and 4 segments";
  Array.iteri (fun i v -> set_reg16 t i v) s.saved_regs;
  Array.iteri (fun i v -> set_seg t i v) s.saved_segs;
  set_ip t s.saved_ip;
  set_flags t s.saved_flags;
  t.halted <- s.saved_halted;
  t.cycles <- s.saved_cycles
