# ocaml-dos

8086 리얼모드 CPU와 DOS 인터럽트 표면의 OCaml 구현. ocaml-msx의 계약을
그대로 잇는다: 코어는 순수하고 결정론적이며, 시간·파일 IO는 호출자가
소유한다.

목표: 턴제 DOS 게임(삼국지 III)을 타이틀 화면까지 부트하는 것. MASC의
기계 Lane 패턴(msx-retro-mania와 같은 `masc_dos_*` 표면)으로 서버에
내장되는 것이 종착점이다.

## 마일스톤

| | 내용 | 상태 |
|---|---|---|
| M0 | 8086 상태·modrm·ALU 8종·mov·inc/dec·push/pop·jcc/jmp·hlt | 완료 |
| M1 | 나머지 명령(call/ret, 그룹 80-83/FE/FF, string, mul/div, shift), 텍스트 비디오(0xB800), INT 10h/16h/21h 최소, COM 로더 | |
| M2 | MZ EXE 로더, INT 21h 파일 표면, VGA Mode 13h | |
| M3 | 실게임 부트 (삼국지 III 타이틀) | |

미구현 opcode는 `Cpu86.Unsupported` 예외로 죽는다 — 하네스가 그 게임이
필요로 하는 다음 명령을 아는 관측 자료다 (조용한 오동작 방지).

## 빌드와 테스트

```sh
dune build
dune runtest --force
```
