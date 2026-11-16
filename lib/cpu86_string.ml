(* 문자열 명령 MOVS·CMPS·STOS·LODS·SCAS·INS·OUTS 와 rep 접두. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode
open Cpu86_alu

(* string 명령의 단위. 정수 태그 대신 합타입이라 새 단위를 더할 때
   포인터 전진 규칙과 실행 본체 양쪽에서 컴파일러가 누락을 잡는다. *)
type string_unit = Movs | Cmps | Stos | Lods | Scas | Ins | Outs

(* string ops: DS:[SI] → ES:[DI], DF 가 방향. rep 은 CX 카운터 —
   cmps/scas 는 repz/repnz 의 ZF 판정으로 조기 종료한다. [width] 는 8·16·32
   비트이고 32 는 386 의 66h 형태다. 67h(A32)면 포인터는 ESI·EDI, 카운터는
   ECX 이고 오프셋은 감기지 않는다. *)
let string_op t ctx width (unit_kind : string_unit) =
  let d = (width / 8) * (if t.df then -1 else 1) in
  let wide = address32 ctx.addr in
  let adv r =
    if wide then put32 t r (get32 t r + d)
    else t.regs.(r) <- (t.regs.(r) + d) land 0xffff
  in
  let at_ptr sreg r = if wide then Mem32 (sreg, get32 t r) else Mem (sreg, t.regs.(r)) in
  let count () = if wide then get32 t 1 else t.regs.(1) in
  let count_down () =
    if wide then put32 t 1 (get32 t 1 - 1)
    else t.regs.(1) <- t.regs.(1) - 1
  in
  (* 포인터는 그 명령이 쓰는 것만 전진한다 — movs/cmps=SI·DI 둘 다,
     stos/scas=DI 만, lods=SI 만. (둘 다 전진시키면 lodsb+stosw 조합이
     반복당 DI+3 — ZZT 화면 stride-3 오염의 뿌리, 실측.)
     [guard r] 는 레지스터 r 이 짚는 접근 바로 앞에 선다. 286 이 그
     접근에서 세그먼트 끝을 넘으면 그 포인터는 이미 움직였고 뒤 접근은
     일어나지 않는다. REP 이면 CX 는 이 반복에서 [cx_taken] 번 줄어
     있다: 읽기는 1, 쓰기는 다음 반복 몫까지 2, CMPS 의 첫 읽기는 0
     이다(SingleStepTests/80286 A5 A7 AB AD AF 6D 6F 의 INT 13 사례
     전부). 레지스터를 되돌리지 않는 결함으로 던진다. 접근마다 예외
     처리기를 세우지 않으려고 접근 전에 따진다.
     386 은 그 반복을 처음부터 다시 한다: SI·DI·CX 가 반복 앞 값이다
     (SingleStepTests/80386 A5 A7 AB AD AF 6D 6F). [restart] 가 되돌린다. *)
  (* 더블워드와 67h 는 386 에만 있고, 386 의 [guard] 는 접근 자신과 같은
     결함만 내니 따질 것이 없다. *)
  let crosses off = width = 16 && overruns t off in
  let read opd = if width = 32 then op_read32 t opd else op_read t width opd in
  let write opd v = if width = 32 then op_write32 t opd v else op_write t width opd v in
  let acc () = if width = 32 then get32 t 0 else op_read t width (Reg 0) in
  let guard r sreg ~cx_taken =
    if crosses t.regs.(r) then begin
      (match t.model with
       | I80386 -> ()
       | I8086 | I80186 | I80286 ->
         adv r;
         (match ctx.rep with
          | Some _ -> t.regs.(1) <- (t.regs.(1) - cx_taken) land 0xffff
          | None -> ()));
      fault (overrun_vector t sreg)
    end
  in
  let si_sreg = segment_for ctx.addr ~default:sreg_ds in
  let read_si ~cx_taken =
    guard 6 si_sreg ~cx_taken;
    let v = read (at_ptr si_sreg 6) in
    adv 6; v
  in
  let read_di ~cx_taken =
    guard 7 sreg_es ~cx_taken;
    let v = read (at_ptr sreg_es 7) in
    adv 7; v
  in
  let write_di v =
    guard 7 sreg_es ~cx_taken:2;
    write (at_ptr sreg_es 7) v;
    adv 7
  in
  let run_once () =
    match unit_kind with
    | Movs -> write_di (read_si ~cx_taken:1)
    | Cmps ->
      (* 286 은 ES:[DI] 를 먼저 읽는다 — DI 에서 넘친 결함은 SI 를 그대로
         두고, SI 에서 넘친 결함은 DI 도 옮겨 둔다. 8086·186 은 SI 부터
         읽는 순서를 그대로 둔다. *)
      let src, dst =
        match t.model with
        | I80286 | I80386 ->
          let dst = read_di ~cx_taken:0 in
          (read_si ~cx_taken:1, dst)
        | I8086 | I80186 ->
          let src = read_si ~cx_taken:1 in
          (src, read_di ~cx_taken:1)
      in
      ignore (alu_result t 7 src dst width)
    | Stos -> write_di (acc ())
    | Lods -> write (Reg 0) (read_si ~cx_taken:1)
    | Scas ->
      let a = acc () in
      ignore (alu_result t 7 a (read_di ~cx_taken:1) width)
    | Ins (* 186: ES:[DI] <- 포트 DX *) ->
      let p = reg16 t 2 in
      write_di
        (match width with
         | 8 -> t.port_in p
         | 16 -> port_in16 t p
         | _ -> port_in32 t p)
    | Outs (* 186: 포트 DX <- DS:[SI] *) ->
      let p = reg16 t 2 and v = read_si ~cx_taken:1 in
      (match width with
       | 8 -> t.port_out p v
       | 16 -> port_out16 t p v
       | _ -> port_out32 t p v)
  in
  let restart f =
    match t.model with
    | I80386 ->
      let esi = get32 t 6 and edi = get32 t 7 in
      (try f () with
       | Fault _ as e -> put32 t 6 esi; put32 t 7 edi; raise e)
    | I8086 | I80186 | I80286 -> f ()
  in
  let run_once () = restart run_once in   (* 반복 하나 = 다시 할 수 있는 단위 *)
  match ctx.rep with
  | None -> run_once ()
  | Some want_zf ->
    if count () = 0 then ()  (* CX=0: 실행 없음 *)
    else begin
      let continue_ = ref true in
      while !continue_ && count () <> 0 do
        run_once ();
        count_down ();
        (* 비교 계열만 ZF 로 조기 종료한다 *)
        (match unit_kind with
         | Cmps | Scas -> continue_ := (t.zf = want_zf)
         | Movs | Stos | Lods | Ins | Outs -> ())
      done
    end
