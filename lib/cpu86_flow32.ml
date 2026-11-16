(* 386 의 66h 형태 가운데 흐름과 스택: 32 비트 EIP 로 가는 점프·far call·RET·
   IRETD, LOOP, ENTER, PUSHAD·POPAD, 세그먼트 레지스터의 PUSH·POP. 실모드 스택은
   16 비트라 SP 만 움직인다. 데이터 명령과 [dispatch] 는 [Cpu86_exec32] 에 있다. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode
open Cpu86_alu

(* 32 비트 EIP 로 간다. 실모드 CS 한계가 FFFFh 라 그 위는 그 명령 자신의 INT 13
   이고 EIP 와 스택은 바뀌지 않는다. SingleStepTests/80386 66C3 에서 FFFFh 위
   EIP 를 꺼낸 128 건이 모두 그렇다. 점프는 같은 규칙으로 둔다 — 66E9 의 rel32 는
   ±64K 안이라 그런 점프가 데이터에 없다. *)
let check_target target =
  let target = target land dword_mask in
  if target > 0xffff then fault vector_segment_overrun;
  target

let jump_rel t d = t.ip <- check_target (t.ip + d)

(* [atomic] 은 아래 16 비트만 되돌린다. ENTER 의 디스플레이 복사는 EBP 를 4 씩
   줄이며 읽어 위 절반까지 바꾸는데, 그 읽기가 넘쳐 결함이 나면 EBP 전체가
   명령 앞 값이다(SingleStepTests/80386 66C8 의 결함 128 건). 다른 32 비트 스택
   명령은 결함 전에 위 절반을 건드리지 않아 [atomic] 으로 된다. *)
let atomic32 t f =
  let hi = Array.copy t.regs_hi in
  atomic t (fun () ->
      try f () with Fault _ as e -> Array.blit hi 0 t.regs_hi 0 (Array.length hi); raise e)

(* 세그먼트 레지스터의 32 비트 PUSH·POP 은 SP 를 4 움직이고 워드 하나만
   오간다. 위 워드는 쓰지도 읽지도 않아 SP=FFFEh 의 POP 도 결함이 아니다
   (SingleStepTests/80386 6606·6607: 쓴 바이트가 둘, SP=FFFE 의 POP 36 건). *)
let push_sreg t sreg =
  let sp = (t.regs.(4) - 4) land 0xffff in
  write16 t sreg_ss sp (seg t sreg);
  t.regs.(4) <- sp

let pop_sreg t sreg =
  let v = read16 t sreg_ss t.regs.(4) in
  set_seg t sreg v;
  t.regs.(4) <- (t.regs.(4) + 4) land 0xffff

let far_call t off sg =
  atomic t (fun () ->
      let target = check_target off in
      push32 t (seg t sreg_cs);
      push32 t t.ip;
      set_seg t sreg_cs sg;
      t.ip <- target;
      13)

let far_jump t off sg =
  let target = check_target off in
  set_seg t sreg_cs sg;
  t.ip <- target

let enter32 t =
  let size = fetch16 t in
  let nesting = fetch8 t land 0x1f in
  atomic32 t (fun () ->
      push32 t (get32 t 5);
      let frame = t.regs.(4) in
      if nesting > 0 then begin
        for _ = 1 to nesting - 1 do
          put32 t 5 ((get32 t 5) - 4);
          push32 t (read32 t sreg_ss t.regs.(5))
        done;
        push32 t frame
      end;
      put32 t 5 frame;
      set_reg16 t 4 ((t.regs.(4) - size) land 0xffff);
      19)

(* 16 비트 스택의 POPAD 는 ESP 자리를 버리지 않는다: SP 는 32 만큼 오르고 ESP
   의 위 16 비트는 그 자리 값의 위 16 비트가 된다(SingleStepTests/80386 6661,
   2,500 건 모두). *)
let popad t =
  restore_sp_on_fault t (fun () ->
      put32 t 7 (pop32 t);
      put32 t 6 (pop32 t);
      put32 t 5 (pop32 t);
      let esp_slot = pop32 t in
      t.regs_hi.(4) <- esp_slot lsr 16;
      put32 t 3 (pop32 t);
      put32 t 2 (pop32 t);
      put32 t 1 (pop32 t);
      put32 t 0 (pop32 t);
      25)

(* PUSHAD 는 EAX 를 SP-4 에, EDI 를 SP-32 에 두는데 쓰기는 EDI 부터 위로
   한다. 세그먼트 끝을 넘는 첫 자리에서 INT 12 이고, 그 아래에 이미 쓴
   더블워드는 남는다. SP 는 그대로다(SingleStepTests/80386 6660, SP 가 32
   보다 작은 8 건). *)
let pushad_slots = 8
let pushad t =
  let sp = t.regs.(4) in
  let esp = get32 t 4 in
  let value = function 4 -> esp | n -> get32 t n in
  for n = pushad_slots - 1 downto 0 do
    write32 t sreg_ss ((sp - (4 * (n + 1))) land 0xffff) (value n)
  done;
  t.regs.(4) <- (sp - (4 * pushad_slots)) land 0xffff;
  36

(* 카운터는 주소 크기를 따른다: 67h 면 ECX, 아니면 CX. *)
let loop32 t ctx =
  let d = sx8 (fetch8 t) in
  let wide = address32 ctx.addr in
  let cx = if wide then (get32 t 1 - 1) land offset32_mask else (t.regs.(1) - 1) land 0xffff in
  let take =
    match ctx.opcode with
    | 0xe0 -> cx <> 0 && not t.zf
    | 0xe1 -> cx <> 0 && t.zf
    | _ -> cx <> 0
  in
  if take then jump_rel t d;
  if wide then put32 t 1 cx else t.regs.(1) <- cx;
  5

let ret_near t n =
  atomic t (fun () ->
      t.ip <- check_target (pop32 t);
      t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
      10)

let ret_far t n =
  atomic t (fun () ->
      let target = check_target (pop32 t) in
      let sg = pop32 t land 0xffff in
      set_seg t sreg_cs sg;
      t.ip <- target;
      t.regs.(4) <- (t.regs.(4) + n) land 0xffff;
      18)

let iretd t =
  atomic t (fun () ->
      let target = check_target (pop32 t) in
      let sg = pop32 t land 0xffff in
      let fl = pop32 t in
      set_seg t sreg_cs sg;
      t.ip <- target;
      set_flags t (fl land 0xffff);
      8)
