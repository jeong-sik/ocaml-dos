(* 보호 모드의 인터럽트 전달(RFC 0005 3c-2b). INT n·INT 3·INTO, 결함, 단일 스텝 트랩이
   IDT 의 게이트를 읽어 핸들러로 간다. 실모드는 호스트가 IVT 로 받는다([Cpu86_fault.do_int]).

   규칙은 386 PRM 의 INT 명령 Operation(PROTECTED-MODE, TRAP-GATE-OR-INTERRUPT-GATE,
   INTERRUPT-TO-SAME-PRIVILEGE-LEVEL), 9.5-9.10 에서 읽었다. 실칩 데이터는 없다.

   특권 수준은 링 0 하나만 안다(CPL 은 늘 0). 그래서 같은 특권 수준으로 가는 전달만
   만들었다. 소프트웨어 INT 의 게이트 DPL 검사("DPL < CPL")는 걸릴 수 없고, 바깥 링에서
   안쪽 링으로 가는 전달(TSS 의 스택 바꾸기)과 작업 게이트는 4·6 단계다. *)

open Cpu86_state
open Cpu86_segment
open Cpu86_fault
open Cpu86_decode

(* ---------- IDT 의 게이트 ---------- *)

type gate_size = Gate16 | Gate32

type gate_type =
  | Interrupt_gate of gate_size  (** 핸들러에 들어가며 IF 를 지운다 *)
  | Trap_gate of gate_size
  | Task_gate

type gate = {
  gate_type : gate_type;
  gate_selector : int;
  gate_offset : int;  (** 16 비트 게이트는 아래 워드만 쓴다 *)
}

let idt_entry_bytes = 8

(* 기술자의 5번째 바이트: P DPL S 형식. 게이트는 S=0 이다(386 PRM 6.3.1 Table 6-1 의 형식
   5·6·7·E·F). *)
let access_type_mask = 0xf
let type_task_gate = 0x5
let type_interrupt_gate_16 = 0x6
let type_trap_gate_16 = 0x7
let type_interrupt_gate_32 = 0xe
let type_trap_gate_32 = 0xf

(* ---------- 오류 코드 ---------- *)

(* 소프트웨어 INT(INT n·INT 3·INTO)와 프로세서가 일으킨 사건(결함·트랩)을 가른다.
   오류 코드의 EXT 비트가 다르다. 확인 필요: 386 PRM 9.7 은 EXT 를 "event external to the
   program" 에 켠다고만 적고 결함을 전달하다 난 결함은 적지 않았다. 앞선 결함의 전달 중이면
   켜는 것은 현대 Intel SDM 의 문구(Vol.3 오류 코드 절)를 따랐고 원문을 읽지는 못했다. *)
type origin = Software_int | Processor_event

let ext_bit = function Software_int -> 0 | Processor_event -> 1
let idt_bit = 0x2

(* IDT 를 가리키는 오류 코드: 벡터 × 8 + IDT 비트 + EXT(386 PRM 9.7, INT 의 Operation
   "#GP(vector number * 8+2+EXT)"). *)
let idt_error_code ~vector ~origin = (vector lsl 3) lor idt_bit lor ext_bit origin

(* 선택자를 가리키는 오류 코드: 선택자의 위 14 비트 + EXT("#GP(selector+EXT)"). *)
let selector_error_code_ext ~selector ~origin = selector_error_code selector lor ext_bit origin

(* ---------- 게이트 읽기 ---------- *)

(* PRM: 벡터는 IDT 한계 안이어야 하고(아니면 #GP(IDT 코드)), 기술자는 인터럽트·트랩·작업
   게이트여야 하며(아니면 #GP(IDT 코드)), 있어야 한다(아니면 #NP(IDT 코드)). 소프트웨어 INT 의
   "gate DPL < CPL" 은 CPL 이 0 이라 걸리지 않는다. *)
let read_gate t ~vector ~origin =
  let code = idt_error_code ~vector ~origin in
  let offset = vector * idt_entry_bytes in
  if offset + idt_entry_bytes - 1 > t.idtr.table_limit then fault (General_protection code);
  let byte i = t.read ((t.idtr.table_base + offset + i) land t.addr_mask) in
  let b0 = byte 0 in
  let b1 = byte 1 in
  let b2 = byte 2 in
  let b3 = byte 3 in
  let b5 = byte 5 in
  let b6 = byte 6 in
  let b7 = byte 7 in
  let offset_low = b0 lor (b1 lsl 8) and gate_selector = b2 lor (b3 lsl 8) in
  let offset_high = b6 lor (b7 lsl 8) in
  let access = b5 in
  if access land access_code_or_data <> 0 then fault (General_protection code);
  let gate_type, gate_offset =
    match access land access_type_mask with
    | ty when ty = type_task_gate -> (Task_gate, offset_low)
    | ty when ty = type_interrupt_gate_16 -> (Interrupt_gate Gate16, offset_low)
    | ty when ty = type_trap_gate_16 -> (Trap_gate Gate16, offset_low)
    | ty when ty = type_interrupt_gate_32 -> (Interrupt_gate Gate32, offset_low lor (offset_high lsl 16))
    | ty when ty = type_trap_gate_32 -> (Trap_gate Gate32, offset_low lor (offset_high lsl 16))
    | _ -> fault (General_protection code)
  in
  if access land access_present = 0 then fault (Segment_not_present code);
  { gate_type; gate_selector; gate_offset }

(* ---------- 핸들러로 들어가기 ---------- *)

(* 게이트가 가리키는 코드 세그먼트(PRM INT, TRAP-GATE-OR-INTERRUPT-GATE): 널이면
   #GP(EXT), 표 밖이거나 코드가 아니면 #GP(선택자+EXT), 없으면 #NP(선택자+EXT). 같은 링에서
   conforming 이거나 DPL 이 CPL 과 같은 세그먼트만 받는다. DPL 이 CPL 보다 큰 non-conforming
   은 #GP(선택자+EXT)이고 작은 것은 안쪽 링으로 가는 전달(4 단계)인데 CPL 이 0 이라 없다. *)
let handler_code t ~selector ~origin =
  let code = selector_error_code_ext ~selector ~origin in
  match read_descriptor t selector with
  | Null_selector -> fault (General_protection (ext_bit origin))
  | Beyond_table -> fault (General_protection code)
  | Found d ->
    (match d.desc_kind with
     | Data _ | System _ -> fault (General_protection code)
     | Code { conforming; _ } ->
       if not d.desc_present then fault (Segment_not_present code)
       else if (not conforming) && d.desc_dpl <> 0 then fault (General_protection code)
       else d)

(* 같은 링 전달의 프레임: EFLAGS, CS, EIP 와 오류 코드(32 비트 게이트는 더블워드, 16 비트
   게이트는 워드). 확인 필요: PRM INTERRUPT-TO-SAME-PRIVILEGE-LEVEL 은 스택이 "10 바이트"(32
   비트)·"6 바이트"(16 비트)를 받아야 한다고 적는데, 32 비트 프레임은 12 바이트다(IRET 의
   RETURN-SAME-LEVEL 은 12 바이트). 실제 프레임 크기로 따진다. *)
let frame_bytes size ~error_code =
  let word = match size with Gate16 -> 2 | Gate32 -> 4 in
  match error_code with None -> 3 * word | Some _ -> 4 * word

(* 쓰기 전에 스택이 프레임을 받는지: 0 아래로 내려가면 #SS(0). 쓴 것이 없어야 결함이 명령을
   되돌릴 수 있다. push 는 0 아래로 내려가면 4GB 위로 감아 한계를 넘지 않아서 이 검사가 따로
   필요하다. 한계는 push 가 쓸 때 본다. 첫 push 가 가장 높은 바이트를 쓰니 한계를 넘는
   스택은 아무것도 쓰기 전에 걸린다. *)
let require_stack_room t bytes =
  if bytes > stack_pointer t then fault (Stack_fault 0)

let push t size v =
  match size with Gate16 -> push16 t (v land 0xffff) | Gate32 -> push32 t v

let enter_handler t gate size ~clears_if ~error_code ~origin ~return_ip =
  let d = handler_code t ~selector:gate.gate_selector ~origin in
  require_stack_room t (frame_bytes size ~error_code);
  if gate.gate_offset > d.desc_limit then fault (General_protection 0);
  let eflags = flags t and cs = t.segs.(sreg_cs) in
  push t size eflags;
  push t size cs;
  push t size return_ip;
  (match error_code with Some c -> push t size c | None -> ());
  store t sreg_cs (gate.gate_selector land lnot selector_rpl_mask) d;
  t.ip <- gate.gate_offset;
  if clears_if then t.intf <- false;
  t.tf <- false;
  t.iopl_nt <- t.iopl_nt land lnot f_nested_task

(* [Fault] 는 쓰기 전에만 던진다. [return_ip] 는 핸들러가 IRET 으로 돌아올 오프셋이다. *)
let deliver t ~vector ~error_code ~origin ~return_ip =
  let gate = read_gate t ~vector ~origin in
  match gate.gate_type with
  | Interrupt_gate size ->
    enter_handler t gate size ~clears_if:true ~error_code ~origin ~return_ip
  | Trap_gate size ->
    enter_handler t gate size ~clears_if:false ~error_code ~origin ~return_ip
  | Task_gate ->
    raise (Unsupported (Printf.sprintf "vector %d is a task gate (RFC 0005 6) @%04x:%04x"
                          vector (seg t 1) t.ip))

(* ---------- 진입점 ---------- *)

(* 같은 특권 수준의 INT 는 386 PRM INT 의 클록 표에서 "pm=59" 다. 결함·트랩은 표가 없어
   같은 값을 쓴다(확인 필요). *)
let protected_delivery_cycles = 59

(* INT n·INT 3·INTO. 게이트가 틀려 [Fault] 가 나면 명령이 일으킨 결함이라 호출한 쪽이 INT 명령
   자리로 되돌려 결함으로 전달한다. 돌아올 자리는 다음 명령이다. *)
let software t ~vector =
  deliver t ~vector ~error_code:None ~origin:Software_int ~return_ip:t.ip;
  protected_delivery_cycles

(* 전염성 결함(0·10-13)과 무해한 결함의 구분(386 PRM 9.8.8 Table 9-3). 페이지 결함은
   5 단계에서 더한다. *)
type fault_class = Benign | Contributory

let class_of_fault = function
  | Divide_error | Segment_not_present _ | Stack_fault _ | General_protection _ -> Contributory
  | Bound_exceeded | Invalid_opcode | Device_not_available -> Benign

let vector_double_fault = 8

(* 이중 결함 핸들러로 가는 중에 결함이 나면 프로세서가 셧다운한다(PRM 9.8.8). 이 코어는
   멈춘다. 오류 코드는 늘 0 이다. *)
let deliver_double_fault t ~return_ip =
  try
    deliver t ~vector:vector_double_fault ~error_code:(Some 0) ~origin:Processor_event ~return_ip
  with Fault _ ->
    raise (Unsupported (Printf.sprintf "shutdown: a fault while delivering a double fault @%04x:%04x"
                          (seg t 1) return_ip))

(* 프로세서가 일으킨 사건(결함, 단일 스텝 트랩)의 전달. 전달 중 둘째 결함이 나면 PRM
   Table 9-4 대로 둘이 모두 전염성일 때만 이중 결함이고, 아니면 둘째 결함을 대신 전달한다
   ("handled serially"). 둘째 결함의 돌아올 자리도 같은 [return_ip] 다. *)
let rec deliver_event t ~vector ~error_code ~klass ~return_ip =
  try deliver t ~vector ~error_code ~origin:Processor_event ~return_ip
  with Fault second ->
    (match klass, class_of_fault second with
     | Contributory, Contributory -> deliver_double_fault t ~return_ip
     | Benign, Benign | Benign, Contributory | Contributory, Benign ->
       deliver_fault t second ~return_ip)

and deliver_fault t f ~return_ip =
  deliver_event t ~vector:(vector_of_fault f) ~error_code:(error_code_of_fault f)
    ~klass:(class_of_fault f) ~return_ip

(* 단일 스텝 트랩은 벡터 1 이고 무해한 사건이다(Table 9-3 의 Debug exceptions). *)
let vector_debug = 1

let deliver_trap t ~return_ip =
  deliver_event t ~vector:vector_debug ~error_code:None ~klass:Benign ~return_ip
