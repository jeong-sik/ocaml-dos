(* 8086 코어. 명령 집합 전체 중 M0 범위만: 상태·modrm·ALU 8종·mov·
   inc/dec·push/pop·jcc/jmp·hlt. 미구현 opcode 는 Unsupported 로 죽는다 —
   조용한 오동작(0xFF 반환 등)이 아니라 예외가 하네스에 다음에 구현할
   명령을 알려준다.

   레지스터 번호는 인코딩 그대로 (mli 문서 참조). 세그먼트 기본값:
   BP 기반 유효주소는 SS, 나머지는 DS (8086 하드웨어 배선).

   모듈: [Cpu86_state] 상태·레지스터·플래그, [Cpu86_fault] 결함과
   인터럽트, [Cpu86_decode] 페치·접두·modrm, [Cpu86_alu] 산술,
   [Cpu86_string] 문자열 명령, [Cpu86_exec] 명령 디스패치. 이 파일은
   바깥에 보이는 이름만 모은다. *)

include Cpu86_state

let idle = Cpu86_exec.idle
let step = Cpu86_exec.step
