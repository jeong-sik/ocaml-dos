(* 386 의 두 바이트 명령: 0Fh 뒤의 바이트가 정한다. 16 비트 피연산자 형태만
   있다 — 66h 가 붙으면 [Cpu86_decode.operand_sized] 가 0Fh 에 표시를 실어
   [Cpu86_exec32] 로 보내고, 거기서는 아직 [Unsupported] 다. 주소 크기(67h)는
   [ctx.addr] 로 modrm 이 따른다. RFC 0001 3 단계를 데이터 파일 단위로 더한다:
   지금은 Jcc rel16, SETcc, MOVZX·MOVSX, FS·GS 의 PUSH·POP 이다. *)

open Cpu86_state
open Cpu86_decode

let jcc_near_first = 0x80
let jcc_near_last = 0x8f
let setcc_first = 0x90
let setcc_last = 0x9f
let condition_field = 0xf

let sx16_of_byte v = if v >= 0x80 then v lor 0xff00 else v

(* 16 비트 피연산자의 근거리 점프는 IP 를 16 비트에서 감는다 — 70h-7Fh 와
   같다. *)
let jump_near16 t d = t.ip <- (t.ip + d) land 0xffff

let dispatch t ctx =
  let op2 = fetch8 t in
  match op2 with
  | b when b >= jcc_near_first && b <= jcc_near_last ->
    let d = fetch16 t in
    if condition t (b land condition_field) then begin jump_near16 t d; 8 end else 4
  | b when b >= setcc_first && b <= setcc_last ->
    let _, _, rm, _ = decode_modrm t ~addr:ctx.addr in
    op_write t 8 rm (Bool.to_int (condition t (b land condition_field)));
    4
  | 0xa0 -> push16 t (seg t sreg_fs); 10
  | 0xa1 -> set_seg t sreg_fs (pop16 t); 8
  | 0xa8 -> push16 t (seg t sreg_gs); 10
  | 0xa9 -> set_seg t sreg_gs (pop16 t); 8
  (* MOVZX·MOVSX r16: B6/BE 는 바이트를 넓히고, B7/BF 는 16 비트 피연산자에서
     워드를 그대로 옮긴다. *)
  | 0xb6 | 0xb7 | 0xbe | 0xbf ->
    let _, regf, rm, _ = decode_modrm t ~addr:ctx.addr in
    let v =
      match op2 with
      | 0xb6 -> op_read t 8 rm
      | 0xbe -> sx16_of_byte (op_read t 8 rm)
      | _ -> op_read t 16 rm
    in
    set_reg16 t regf v;
    3
  | _ -> unsupported t ctx (Printf.sprintf "0f %02x (386)" op2)
