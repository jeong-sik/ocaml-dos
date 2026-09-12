# ocaml-dos

8086/186 리얼모드 CPU 와 DOS 기계의 OCaml 구현. 코어는 순수하고
결정론적이다 — 시간과 파일 IO 는 호출자가 소유한다.

목표는 1990년대 턴제 DOS 게임을 실제로 돌리고, 바깥 프로그램이 그것을
관측하고 조작할 수 있게 하는 것이다. ZZT 3.2(1991, Turbo Pascal)가
보드를 그리고, 방향키로 움직이고, 메뉴 키가 먹고, 게임 저장까지 된다 —
저장한 73KB 짜리 월드 파일을 하네스가 그대로 꺼낼 수 있다.

## 마일스톤

| | 내용 | 상태 |
|---|---|---|
| M0 | 8086 상태·modrm·ALU·mov·inc/dec·push/pop·jcc/jmp·hlt | 완료 |
| M1 | 명령 집합 완성 — call/ret, 그룹 80-83/FE/FF, string+rep, mul/div, shift | 완료 |
| M2 | MZ EXE 로더, INT 21h 파일 표면, VGA Mode 13h | 완료 |
| M3 | 실게임 — ZZT 보드 렌더, 방향키 이동, 메뉴 키, 세이브 파일 | 완료 |
| M4 | 삼국지 III 타이틀 | 이미지 확보 대기 |

## CPU 정확성

실칩(Intel P80C86A-2)에서 뽑은 [SingleStepTests/8086](https://github.com/SingleStepTests/8086)
64만 6천 케이스로 잰다. 판정을 셋으로 나눈다.

| | 케이스 | 뜻 |
|---|---|---|
| pass | 599,993 | 레지스터·메모리·플래그가 전부 일치 |
| undef | 41,180 | Intel 이 미정의로 둔 플래그만 다르다 |
| fail | 4,827 | 고쳐야 할 차이 — 전부 div/idiv 트랩 프레임 |

남은 4,827 은 나눗셈이 0 나눗셈이나 몫 넘침으로 트랩할 때 인터럽트
프레임에 밀리는 플래그다. 나눗셈이 성공하는 경로는 전부 일치한다.
`test/sst8086-baseline.txt` 가 이 수를 붙잡고 있어서, 넘으면 테스트가
깨진다.

```sh
# 데이터셋을 /tmp/sst8086/v1 에 풀어 두고
ALL=$(ls /tmp/sst8086/v1/*.json.gz | xargs -n1 basename | sed 's/\.json\.gz$//' | paste -sd, -)
SST_FILES="$ALL" dune exec --root . test/cpu86_vtest.exe
```

데이터셋이 없으면 이 스위트는 건너뛴다(CI 가 그렇다).

### 어느 칩을 흉내내는가

8086 과 80186 은 같은 opcode 자리에 다른 명령을 놓았다. `Cpu86.create`
의 `?model` 이 어느 쪽인지 정한다.

| opcode | I8086 | I80186 |
|---|---|---|
| 0x60-0x6F | 0x70-0x7F(jcc) 의 거울 | pusha/popa/push imm/imul/bound/ins/outs |
| 0xC0/0xC1 | ret imm16 / ret | shift rm,imm8 |
| 0xC8/0xC9 | retf imm16 / retf | enter / leave |
| 그룹2 reg=6 | SETMO/SETMOC | shl |

기본값은 `I80186` — 1990년대 게임은 286 이상에서 돌았고 Borland
컴파일러가 186 명령을 낸다. 실칩 검증 스위트만 `I8086` 으로 돈다.

8087 코프로세서는 없다. ESC(0xD8-0xDF)는 피연산자만 읽고 지나가며,
INT 11h 장비 워드의 코프로세서 비트도 0 이라 게스트가 소프트웨어
부동소수 경로를 고른다.

## 기계가 주는 것

- **BIOS**: INT 10h(모드·커서·스크롤·문자·DAC·폰트·문자열), 11h 장비,
  12h 메모리, 16h 키보드, 1Ah 시각, 33h 마우스
- **DOS**: INT 21h — 콘솔 입출력, 파일 핸들(open/create/read/write/
  seek/close/dup), FCB, findfirst/findnext, 메모리 할당(48h/49h/4Ah),
  날짜·시각, 벡터 읽기·쓰기, IOCTL, DOS 버전 5.0
- **장치 포트**: PIT 타이머(분주비로 틱 속도가 바뀐다), PIC 마스크,
  VGA DAC(0x3C8/0x3C9), CRTC, CGA 상태·모드, PC 스피커, 조이스틱
- **화면**: 텍스트 80x25 와 VGA 13h. RGB·PPM 으로 내보내고, 코드 페이지
  437 을 UTF-8 로 옮긴 텍스트도 준다

### 계약

- **결정론**: 같은 이미지에 같은 키를 같은 순서로 넣으면 같은 화면이
  나온다. 시각도 난수도 호스트에서 오지 않는다 — 날짜·시각은
  `set_clock` 이 정한 기준시각에 CPU 사이클로 환산한 경과를 더해 만들고,
  타이머 틱과 재주사 비트도 사이클에서 나온다. 라이브러리는 환경 변수를
  읽지 않는다.
- **파일**: 하네스가 마운트한 것만 보인다. 게스트가 쓴 내용은 파일을
  닫을 때 마운트 표로 돌아가므로 같은 세션 안에서 저장하고 다시 열 수
  있다. 호스트 디스크에는 나가지 않는다.
- **메모리**: 실기처럼 프로그램이 남은 전부를 갖는다. AH=4Ah 로 제
  블록을 줄여야 AH=48h 이 성공한다.
- **미구현 명령**: `Cpu86.Unsupported` 예외로 죽는다. 조용한 오동작
  대신, 그 게임이 다음에 필요로 하는 것을 알려주는 관측 자료다.

## 빌드와 테스트

```sh
dune build
dune runtest --force
```

실게임 통합 검증은 게임 파일이 있을 때만 돈다.

```sh
ZZT_DIR=/path/to/zzt ZZT_EXE=/path/to/ZZT.EXE dune exec --root . test/zzt_run_test.exe
```

## 게임 돌리기

```sh
dune exec --root . bin/dosboot.exe -- \
  --exe ZZT.EXE --mount ZZT.DAT=ZZT.DAT --mount TOWN.ZZT=TOWN.ZZT \
  --steps 6000000 --utf8 \
  --keys 256b,2e63,1c0d,1970,1c0d,3920,1c0d,1970,1c0d,1c0d,4d00@5000000
```

`--keys` 의 한 항목은 `(스캔 코드 lsl 8) lor ASCII` 를 16진수로 쓴 것이고,
`@STEP` 은 그 스텝 전에는 넣지 않는다는 예약이다. 키를 미리 다 밀어
넣으면 앞선 메뉴의 "아무 키나" 루프가 전부 먹어 치우기 때문에, 게임이
특정 상태가 된 다음에 들어가야 하는 키는 `@` 로 묶는다. 글자만 칠
때는 `--type`.

진단은 `--trace N`(N 스텝마다 CS:IP), `--int-trace`(INT 명령),
`--dump ADDR,LEN`(메모리), `--save NAME=PATH`(게스트가 쓴 파일 꺼내기).

게임 이미지는 저장소에 넣지 않는다.

## 바깥에서 몰기

```ocaml
let m = Dos_machine.create () in
Dos_machine.mount_file m "TOWN.ZZT" board_bytes;
Dos_machine.load_exe m exe_bytes;
ignore (Dos_machine.run_with_keys m ~max_steps:6_000_000
          ~keys:[ { Dos_machine.word = 0x1c0d; not_before = 0 } ]);
print_string (Dos_machine.screen_text_utf8 m);
Dos_machine.push_key m 0x4d00;                 (* → *)
ignore (Dos_machine.run_until m ~max_steps:100_000 ~stop:(fun _ -> false));
let png_source = Dos_machine.frame_rgb m
```

`Dos_machine.kbd_waiting` 이 true 면 게스트가 입력을 기다리다 굶은
것이다 — 그때 키를 넣으면 된다. `run_with_keys` 가 그 정책을 그대로
담고 있다.

## 지금 못 하는 것

- 나눗셈이 트랩할 때 인터럽트 프레임에 밀리는 플래그 4,827 건이 실칩과
  다르다. 실칩의 나눗셈 미세동작(자리 옮김-빼기 루프)을 그대로 옮겨야
  좁혀진다.
- 그래픽은 VGA 13h 만 그린다. CGA/EGA 모드(4·5·6·0Dh·10h·12h)를 세우면
  모드 번호는 기억하지만 화면은 여전히 텍스트 VRAM 으로 그려진다.
  지금 어느 모드인지는 `video_mode` 로 알 수 있다.
- 8087 코프로세서가 없다. 소리도 내지 않는다 — 스피커 포트의 상태만
  `speaker_on` 으로 보인다.
- 디렉터리가 없다. `mkdir`/`chdir` 은 성공한 척하지 않고 "경로 없음" 으로
  답한다.
- 스냅샷과 복원이 없다. 리플레이나 분기 실행이 필요해지면 그때 만든다.

## 구조

| 모듈 | 맡은 것 |
|---|---|
| `Cpu86` | 8086/186 코어. 메모리와 포트는 콜백으로 바깥에 있다 |
| `Dos_state` | 기계 상태와 기본기 — 화면·커서·키 링·인터럽트 전달·시계 |
| `Dos_ports` | 하드웨어 포트 — PIT, PIC, DAC, CRTC, 스피커 |
| `Dos_bios` | ROM·IVT·BDA 와 INT 10h/11h/12h/16h/1Ah/33h |
| `Dos_dos` | INT 21h/20h — 파일, 콘솔, 메모리, 날짜 |
| `Dos_render` | 화면을 텍스트와 RGB 로 |
| `Dos_machine` | 배선·로더와 바깥에 보이는 얼굴 |
