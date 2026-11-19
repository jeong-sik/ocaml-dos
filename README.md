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
| M4 | 삼국지 III — 콜드 부팅에서 KOEI 로고 → 카피프로텍션 → 타이틀 메뉴 → 시나리오 1·유비 → 설정 → **첫 턴(189년 봄 1월, 평원 명령 대기)** 까지 가고, 게임 안에서 슬롯 1 에 저장한 뒤 그 세이브 파일을 넣은 새 기계가 타이틀 메뉴에서 불러와 같은 턴으로 돌아온다. 불러온 판에서 8. 북해로 쳐들어가 유비·관우·장비를 전장에 배치하고 전투를 AI 에 맡기면, 열흘 싸운 뒤 189년 2월로 넘어가 전쟁이 이어진다. 모두 `test/samguk3_scenario_test/samguk3_scenario_test.ml` 이 화면 조건으로 재현한다. 남은 관문: 통일 — 엔딩을 그리는 END.EXE 가 여는 `ENDSTIL.DAT` 가 지금 가진 사본에 없다 | 진행 |

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
`test/cpu86_vtest/sst8086-baseline.txt` 가 이 수를 붙잡고 있어서, 넘으면 테스트가
깨진다.

```sh
# 데이터셋을 /tmp/sst8086/v1 에 풀어 두고
ALL=$(ls /tmp/sst8086/v1/*.json.gz | xargs -n1 basename | sed 's/\.json\.gz$//' | paste -sd, -)
SST_FILES="$ALL" dune exec --root . test/cpu86_vtest/cpu86_vtest.exe
```

데이터셋이 없으면 이 스위트는 건너뛴다(CI 가 그렇다).

`I80286` 은 Harris N80C286-12 에서 뽑은
[SingleStepTests/80286](https://github.com/SingleStepTests/80286) 실모드
327 파일, 147만 8천 케이스로 잰다. 형식은 JSON 이 아니라
[MOO](https://github.com/dbalsom/moo) 라 `test/sst/moo.ml` 이 읽는다. fail 은
2 이고 사유는 `test/sst/sst80286-baseline.txt` 에 있다: IVT 바이트가 빠진
테스트 하나, SP=BP=FFFF 인 ENTER 하나다.

```sh
# 데이터셋을 /tmp/sst80286/v1_real_mode 에 받아 두고(약 310MB)
ALL=$(ls /tmp/sst80286/v1_real_mode/*.MOO.gz | xargs -n1 basename | sed 's/\.MOO\.gz$//' | paste -sd, -)
SST_FILES="$ALL" dune exec --root . test/sst/cpu86_sst286.exe
```

`I80386` 은 Intel 386EX 에서 뽑은
[SingleStepTests/80386](https://github.com/SingleStepTests/80386) 실모드로
잰다. 실모드 941 파일, 175만 8,700 케이스를 모두 돌린다. 0Fh 로 시작하지
않는 780 파일(66h·67h 가 없는 325, 66h 194, 67h 261)과 0Fh 161 파일(66h
없는 97, 660F·67660F 64)이다.
fail 은 baseline 의 4 건뿐이다. 67A5·67AB·6766A5·6766AB 에서 rep 문자열이
칩이 이미 가져온 자기 코드를 덮는 경우라, 프리페치 큐가 없는 이 모델로는
맞출 수 없다(#159). 포트 22h·23h 는 CPU 가 아니라
386EX 칩 안 주변장치 레지스터(REMAPCFG)라, 러너가 시험 기계에서 읽힌 값
7Fh·42h 를 돌려준다. 다른 포트는 FFh 다. 데이터셋 옆 `revocation_list.txt`
가 적은 테스트는 건너뛴다.

```sh
# 저장소를 /tmp/sst80386 에 받아 두고(v1_ex_real_mode 약 580MB)
ALL=$(ls /tmp/sst80386/v1_ex_real_mode/*.MOO.gz | xargs -n1 basename | sed 's/\.MOO\.gz$//' | paste -sd, -)
SST_FILES="$ALL" dune exec --root . test/sst/cpu86_sst386.exe
```

보호 모드(RFC 0005)는 [test386.asm](https://github.com/barotto/test386.asm)
ROM 으로 잰다. ROM 은 GPL-3.0 이라 저장소에 없고, `TEST386_BIN` 이 64KB
빌드를 가리키면 `test/cpu86_test386` 이 돈다(없으면 건너뛴다). 러너는 맨
`I80386` 에 1MB 를 붙이고 ROM 을 F0000h 에 둔 뒤 F000:FFF0 에서 시작한다.
ROM 이 포트 190h 에 쓰는 POST 코드를 모은다. 지금은 실모드 POST 00h-06h 를
지나 08h 의 32 비트 LGDT(`66 0F 01`)에서 멈추는 것이 기대값이다. 실패한
시험은 ROM 의 error 자리에서 HLT 하므로 테스트가 깨진다.

```sh
# NASM 으로 빌드한 test386.bin 이 있을 때
TEST386_BIN=/path/to/test386.bin dune build --root . @test/cpu86_test386/runtest --force
```

### 어느 칩을 흉내내는가

8086 과 80186 은 같은 opcode 자리에 다른 명령을 놓았다. `Cpu86.create`
의 `?model` 이 어느 쪽인지 정한다.

| opcode | I8086 | I80186 |
|---|---|---|
| 0x60-0x6F | 0x70-0x7F(jcc) 의 거울 | pusha/popa/push imm/imul/bound/ins/outs |
| 0xC0/0xC1 | ret imm16 / ret | shift rm,imm8 |
| 0xC8/0xC9 | retf imm16 / retf | enter / leave |
| 그룹2 reg=6 | SETMO/SETMOC | shl |

`I80286` 은 실모드에서 `I80186` 과 같은 명령을 돌리고, 프로그램이 CPU 를
가르는 자리만 다르다.

| | I8086·I80186 | I80286 |
|---|---|---|
| FLAGS bit12-15 를 읽으면 | 1 | 0 |
| PUSH SP 가 미는 값 | 줄어든 SP | 줄기 전 SP |
| 0x0F | POP CS | 두 바이트 명령의 머리 — SMSW(0F 01 /4)만 있고 0xFFF0 을 준다 |
| 주소선 | 20개, FFFF:FFFF 는 0FFEFh | 24개, FFFF:FFFF 는 10FFEFh(1MB 에서 감는 일은 기계의 A20 몫) |
| AAA·AAS | AL 과 AH 를 따로 고친다 | AX 에 106h 를 더하고 뺀다 |
| DAS | AL-6 의 빌림은 CF 에 남지 않는다 | 남는다 |
| 오프셋 FFFF 의 워드 | 오프셋 0 으로 감는다 | INT 13 |
| 10 바이트를 넘는 명령(접두 포함) | 그대로 돈다 | INT 13 |
| LEA·LES·LDS·BOUND·CALL/JMP FAR 의 레지스터 피연산자, MOV CS, 8C·8E 의 reg 4-7, 8F·C6·C7 의 reg≠0 | 별칭으로 돌거나 Unsupported | INT 6 |
| BOUND 범위 밖 | INT 5 가 다음 명령으로 돌아간다 | BOUND 자신으로 돌아간다 |

기계(`Dos_machine`)는 기본으로 `I80286` 으로 돈다. 1990년대 게임은 286 이상을
요구하고 위 자리로 확인한다. 루카스아츠의 Monkey Island 2 와 Day of the
Tentacle 도 최소 사양이 286 이다. 실칩 검증 스위트만 `I8086` 으로 돈다.

286 의 결함은 명령을 일어나지 않은 것으로 돌린다. IP 는 첫 접두
바이트로, 레지스터는 명령 시작 때 값으로 돌아간다. 실칩이 먼저 고쳐 둔
레지스터는 그대로 둔다. POP r/m 은 쓰기에서 넘쳐도 SP 를 올린 채고, 문자열
명령은 넘친 접근의 포인터를 이미 옮겼다. REP 의 CX 도 그 반복에서 읽기면
1, 쓰기면 2 줄어 있다.

`I80386` 은 286 의 실모드 위에 이것이 다르다.

| | I80286 | I80386 |
|---|---|---|
| 범용 레지스터 | 16 비트 | 32 비트(`reg32`), 16·8 비트 쓰기는 위 절반을 둔다 |
| FS·GS | 없다(64h·65h 는 Unsupported) | 접두 64h·65h, MOV sreg 4·5 |
| FLAGS bit12-14 | 0 | POPF·IRET 로 쓴 값이 남는다(bit15 는 0) |
| LOCK | 버스 신호로만 쓴다 | 받지 않는 명령이나 레지스터 피연산자면 INT 6 |
| SS 로 오프셋 FFFF 의 워드 | INT 13 | INT 12 |
| FFFF 의 마지막 바이트 뒤 | IP 가 0 으로 감는다 | EIP 10000h, 다음 페치가 INT 13 |
| 결함 난 문자열 명령 | 포인터·CX 를 옮겨 둔다 | 그 반복 앞 값으로 둔다 |
| 결함 난 POP r/m·POPA | SP 를 올린 채 / 모두 되돌린다 | SP 만 되돌린다(POPA 가 꺼낸 값은 남는다) |
| BOUND 하한 밖 | 상한을 읽지 않는다 | 상한도 읽는다(FFFF 면 INT 13) |
| 바이트 SHL·SHR 를 16·24 번 | CF=0 | 8 번일 때의 CF |
| 명령 길이 | 10 바이트 | 15 바이트 |
| 66h·67h | opcode 자리(Unsupported) | 피연산자·주소 크기 접두 |

66h 는 [80386.csv](https://github.com/SingleStepTests/80386/blob/main/80386.csv)
의 `66` 열이 고른 한 바이트 opcode 155 개를 32 비트 피연산자로 돌린다.
67h 는 주소를 32 비트로 센다(SIB·disp32, 문자열의 ESI·EDI·ECX). 실모드
한계는 FFFFh 그대로라, 끝 바이트가 그 위인 접근은 INT 13 이고 SS 면 INT 12
다. 설명서를 글자 그대로 읽은 것과 데이터가 다른 자리는 이렇다.

| 명령 | 실칩 |
|---|---|
| 66h PUSH/POP sreg | SP 는 4 움직이고 워드 하나만 오간다 |
| 66h POPAD | ESP 위 16 비트를 스택의 ESP 자리에서 가져온다 |
| 66h PUSHAD | EDI 부터 위로 쓰고, 세그먼트 끝을 넘는 첫 자리에서 INT 12 다 |
| 66h RET | FFFFh 위 EIP 는 INT 13 이다 |
| 67h SIB 인덱스 없음(4) | 배율이 베이스에 곱해진다 |

`dosboot --cpu 386` 이면 기계가 `I80386` 으로 돈다. 32 비트 레지스터·FS·GS·
0Fh 명령을 쓰는 게임이 돌고, 스냅숏도 386 상태를 담는다. 보호 모드는 아직
없다(RFC 0005).

바이트 IDIV 는 286·386 모두 몫이 음수이고 |피젯수| >> 7 이 |젯수| + 80h
이며 |피젯수| 의 아래 7 비트가 |젯수| 보다 작으면 넘쳐도 INT 0 이 아니다.
AL=80h 이고 AH 는 그 7 비트에 피젯수 부호를 붙인 값이다. 두 스위트의 이
조건 10 건이 모두 그렇다.

나눗셈 오류(DIV·IDIV·AAM 0)도 286 에서 결함이라 INT 0 이 그 나눗셈
자신으로 돌아간다. 그래서 INT 0 은 DOS 가 받는다. MS-DOS 2.0 의 DIVOV
(`CTRLC.ASM`)처럼 콘솔에 "Divide overflow" 를 쓰고 프로그램을 종료 코드
0 으로 끝낸다. DOS 는 이 종료의 사유를 Ctrl-C 로 남기지만(AH=4Dh 의
AH=1), 이 기계는 사유를 따로 두지 않아 0 으로 답한다.

8087 코프로세서는 없다. ESC(0xD8-0xDF)는 피연산자만 읽고(286 은 워드로)
지나가며,
INT 11h 장비 워드의 코프로세서 비트도 0 이라 게스트가 소프트웨어
부동소수 경로를 고른다.

## 기계가 주는 것

- **BIOS**: INT 10h(모드·커서·스크롤·문자·CGA 팔레트·DAC·폰트·문자열), 11h 장비,
  12h 메모리, 15h 시스템(88h 확장 메모리 0KB, 83h·86h 는 RTC 주기 인터럽트로
  977µs 씩 세는 기다리기, 모르는 기능은 CF=1·AH=86h),
  16h 키보드, 1Ah 시각, 33h 마우스(커서는 실기 드라이버처럼 화면 메모리에
  그리고, 숨기거나 옮기기 전에 밑을 되돌린다. 기본 화살표와 그리는 방법은
  DOSBox-X mouse.cpp 를 따른다. 하드웨어 텍스트 커서(0Ah BX≠0)는 그리지
  않는다). F000:FFFE 의 모델 바이트는 FCh(AT)다. ROM F000:FA6E 에 IBM
  BIOS 표준 자리의 8x8 글꼴(0x20-0x7E)이 실려 있다 — 그래픽 모드에서
  글자를 직접 찍는 프로그램이 이 표를 읽는다(삼국지3 MAIN 실측). 바이트의
  최상위 비트가 왼쪽 점인 IBM 순서다. 쓰는 장치가 없는 IRQ 줄(벡터
  0Ah-0Fh)의 기본 루틴은 EOI 를 보내고 IRET 한다(SeaBIOS handle_hwpic1).
  텍스트 화면 그림은 CP437 256자를
  모두 그린다 — 상자 선·블록·악센트 글자는 font8x8(Public Domain)에서,
  웃는 얼굴·카드 무늬·화살표 같은 40자는 이 저장소에서 그렸다
- **그래픽 모드의 글자**: INT 10h AH=09h·0Ah·0Eh·13h 와 DOS 출력이
  그래픽 모드에서는 글꼴 표의 점을 화면에 찍는다. 글꼴은 INT 43h 가
  가리키는 표에서 찾는다. CGA 모드의 128 이상 글자는 INT 1Fh 표에서
  찾는다. 프로그램이 벡터를 바꾸면 그 글꼴로 그린다. AH=09h 는 BL 이
  색이고 bit7 이면 XOR 이다. 256색 모드의 바탕은 BH 다. AH=0Ah 는 모드의
  가장 밝은 색을 쓴다. 480줄 모드(11h·12h)는 8x16 글자 30행, 350줄은
  8x14, 200줄은 8x8 이다. 8x14·8x16 표는 VGA ROM 글꼴이 아니라 8x8
  글꼴을 세로로 늘린 것이라 모양이 다르다. AH=06h·07h 스크롤은 점을
  옮긴다. AH=0Ch 도 bit7 이면 XOR 한다(256색 모드 제외). AH=08h(글자
  읽기)는 그래픽 모드에서 화면을 읽지 않는다
- **텍스트 모드의 글자 속성**: AH=0Eh 와 DOS 출력은 글자만 바꾸고 칸의
  색은 그대로 둔다. 마지막 줄에서 넘치면 새 줄은 커서 칸의 색으로
  채운다. AH=09h·13h 는 색을 쓴다. 부팅한 화면은 회색 공백(07h)이다
- **DOS**: INT 21h — 콘솔 입출력, 파일 핸들(open/create/read/write/
  seek/close/dup, 6Ch 확장 열기, 0~4번 CON·AUX·PRN도 같은 표에 있어 46h로 파일에
  돌리고 닫을 수 있다. AUX·PRN에 실제 바이트를 읽고 쓰면 포트와 INT 24h가 없어
  `Unsupported`로 멈춘다. 위치는 32비트로 돈다. 0바이트 쓰기는 파일 길이를
  위치에 맞춘다. 한 번의 쓰기가 36h가 말하는 빈 클러스터(8MB)보다 많이 늘리면
  디스크가 찬 것으로 답한다. 3Dh의 접근 모드를 지켜 읽기 전용에 쓰거나
  쓰기 전용을 읽으면 오류 5다. 공유 모드는 검사만 하고 SHARE가 없어 막지
  않는다. AL 비트 7로 연 핸들은 EXEC 자식에게 넘기지 않는다), FCB(0Fh/10h/13h~17h/21h~24h/27h/28h, 확장 FCB —
  쓴 바이트는 닫을 때 파일이 된다, 29h 이름 나누기), findfirst/findnext,
  디렉터리(39h 만들기·3Ah 지우기·3Bh 옮기기·47h 현재 위치 — FCB 는 현재
  디렉터리의 파일을 가리킨다),
  메모리 할당(48h/49h/4Ah, 58h first·best·last fit),
  EXEC(4Bh 불러 실행, 4Dh 종료 코드, 31h/INT 27h 상주 종료 — 자식은
  같은 기계 위에서 부모 프레임을 갈아끼우며 돈다. 4B03h 오버레이 싣기.
  MZ 는 헤더의 페이지 수만큼 싣고 뒤에 붙은 데이터는 싣지 않는다. 자식은
  환경 블록을 따로 받고 그 끝에 프로그램 이름이 붙는다),
  날짜·시각, InDOS 주소(34h), 확장 에러(59h), 국가 정보(38h·63h·65h·66h,
  미국 437만),
  verify 플래그(2Eh/54h), 스위치 문자(37h), 확장 Ctrl-Break 깃발(33h —
  읽고 쓰기만 하고 Ctrl-C 검사는 하지 않는다), 부팅 드라이브 C:(3305h),
  진짜 버전 5.00(3306h), 벡터
  읽기·쓰기, IOCTL(4400h 장치 정보 — 부팅 때 여는 CON·AUX·PRN 포함,
  4406h·4407h 입출력 상태, 4408h 고정 매체), DOS 버전 5.0. PSP 에는 IVT 22h-24h 사본(0Ah),
  부모(16h, 루트는 자기), 이전 PSP FFFFFFFFh(38h), INT 21h·RETF(50h)가
  있다. 자식이 끝나면 IVT 22h-24h 를 그 자식 PSP 의 사본으로 되돌린다
- **XMS 3.0**: INT 2Fh AX=4300h·4310h 로 찾는 HIMEM 자리. 100000h 부터
  15MB 를 KB 단위 first fit 으로 나눠 주고, 핸들 32개, 잠금, 크기 바꾸기,
  이동(0Bh)을 받는다. 이동은 CPU 버스를 지나서 평면 그래픽 창도 게스트
  쓰기처럼 받는다. HMA·A20·UMB 는 없다
- **장치 포트**: PIT 타이머(분주비로 틱 속도가 바뀐다), 8259 PIC 두 개(20h·21h 와
  A0h·A1h. IRR·ISR·마스크, EOI 두 가지, OCW3 읽기 — IRQ0 이 IRQ1 보다 앞서고,
  두 번째 칩의 IRQ8-15 는 첫 칩의 IRQ2 자리에서 벡터 70h-77h 로 들어온다), 8042
  키보드 컨트롤러(0x60 데이터·0x64 상태, 바이트마다 IRQ1, 컨트롤러·키보드 명령), 8237 DMA
  (채널 0-3 의 주소·카운트·모드·마스크·TC 상태, 페이지 레지스터 81h-83h·87h
  — 장치가 한 바이트씩 당겨 간다), VGA DAC(0x3C8/0x3C9),
  CRTC(0x3D4/0x3D5, 0-7 번 쓰기 보호 포함), CGA 상태·모드, PC 스피커, 조이스틱,
  EMS 4.0(INT 67h — 프레임 0xD000, 페이지 실체는 겹칠 때 만든다.
  40h-48h, 4Bh-54h, 57h-5Ah. 운영체제 핸들 0 을 센다),
  OPL2(0x388/0x389) 타이머 둘과 상태 — 소리는 없다. 타이머는 레지스터
  값에서 256 까지 80µs·320µs 칸으로 세고, 마스크된 타이머는 플래그를
  세우지 않는다. 정지 상태는 06h, 타이머 1 이 넘친 뒤는 C6h 다(감지
  루틴은 E0h 로 걸러 C0h 를 본다),
  CMOS 시계(0x70/0x71) — INT 1Ah 와 같은 시각을 BCD 24시간 꼴로 준다.
  레지스터 B 로 이진·12시간 꼴을 고른다. 갱신 중 비트(UIP)는 초가 바뀌기
  244µs 전에 켜지고 1984µs 뒤에 꺼진다. 기본 메모리 640KB, 확장 메모리 15MB(XMS 와 같다),
  체크섬은 IBM AT 배치다. 레지스터 A 가 고른 주기(기본 1024Hz)가 끝나면
  레지스터 C 에 PF 가 서고(PIE 면 IRQF 도), C 를 읽으면 지워진다. IRQF 가
  서면 IRQ8 이 오르고, C 를 읽을 때까지 다시 오르지 않는다. BIOS 의 INT 70h 는
  C 를 읽고, INT 15h 83h·86h 의 남은 시간을 줄이고, 두 칩에 EOI 를 보낸다
  (71h-77h 는 EOI 만). 시각·RAM 쓰기는 버린다
- **MPU-401**(`dosboot --mpu401`, 기본은 뺌): 330h·331h 의 UART 전용
  카드(Sound Blaster 16 의 종류). 리셋(FFh)과 UART 모드(3Fh)에 FEh 로 답하고,
  MIDI 바이트는 버린다. intelligent 모드 명령에는 답하지 않는다
- **PIT 채널 2**: 모드 0~5, 카운터 래치, 1·2바이트 접근, BCD. 포트 0x61
  의 bit 0 이 GATE, bit 5 가 OUT 이다. 채널 2 를 모드 0 으로 걸고 bit 5
  를 기다리는 지연 루프가 끝난다. 읽기-되돌림 명령(0x43, bit 7-6 = 11)
  은 채널 0·2 의 카운트와 상태 바이트(OUT, NULL COUNT, 쓴 그대로의
  제어 워드)를 잡는다
- **Sound Blaster 2.0**(켤 때만, `--sound-blaster`): 220h 의 DSP 2.01 —
  리셋 뒤 AAh, E1h 버전 2.01, 시간 상수(40h)에 맞춰 DMA 채널 1 에서
  블록을 한 바이트씩 가져가고 블록 끝·F2h·80h 에 IRQ 7 을 올린다.
  8비트 PCM(14h·1Ch·48h·90h·91h)과 ADPCM(74h~77h·16h·17h·7Dh·7Fh·1Fh)
  블록을 받는다. ADPCM 한 바이트는 표본 2~4개 시간이 걸린다. 34h·35h 는
  MIDI UART 모드로 들어간다. 228h·229h 는 OPL2 다. 소리는 내지 않는다.
  DMA 입력(24h)과 E2h 는 없어서 그 명령을 쓰는 프로그램은 오지 않는 IRQ
  나 DMA 를 기다리다 멈춘다 — 그래서 기본은 카드가 없는 기계다
- **화면**: 아래 표의 모든 모드. RGB·PPM 으로 내보내고, 코드 페이지 437
  을 UTF-8 로 옮긴 텍스트도 준다

### 그래픽 모드

| 모드 | 크기 | 색 | 메모리 |
|---|---|---|---|
| 0,1 | 40x25 텍스트 | 16 | 0xB8000 글자+속성 |
| 2,3,7 | 80x25 텍스트 | 16 | 0xB8000 글자+속성 |
| 4,5 | 320x200 | 4 | 0xB8000, 짝·홀 줄이 8KB 떨어져 있다 |
| 6 | 640x200 | 2 | 0xB8000 |
| 0Dh | 320x200 | 16 | 0xA0000 평면 넷 |
| 0Eh | 640x200 | 16 | 0xA0000 평면 넷 |
| 0Fh,10h | 640x350 | 16 | 0xA0000 평면 넷 |
| 11h,12h | 640x480 | 16 | 0xA0000 평면 넷 |
| 13h | 320x200 | 256 | 0xA0000 평면 넷, chain-4 |
| 13h + chain-4 끔(Mode X) | CRTC 가 정한다(320x200, 320x240, 320x400, 360x…) | 256 | 0xA0000 평면 넷 |

EGA/VGA 16색 모드는 평면 넷을 같은 주소에 겹쳐 둔다. 한 바이트를 쓰면
그래픽 컨트롤러가 비트 마스크·래치·논리연산을 거쳐 최대 네 평면에 나눠
쓴다. 쓰기 모드 0-3, 읽기 모드 0-1, set/reset, 평면 마스크, 데이터 회전이
모두 실기 계약대로 돈다 — 읽기가 래치를 채우는 부수효과까지 포함해서다.
실기 코드의 관용구(한 번 읽어 래치를 채우고 같은 자리에 쓴다)가 그대로
먹는다.

256색도 같은 평면 넷에 산다. 13h 는 시퀀서 메모리 모드(4번)의 chain-4
비트가 서 있어 주소 a 가 평면 (a & 3) 의 (a & ~3) 자리다. 게임이 chain-4
를 끄면(Mode X) 16색 모드처럼 맵 마스크로 평면을 고르고, 래치 복사(쓰기
모드 1)도 그대로 먹는다.

16색과 256색 화면은 CRTC 가 그린다. 그래서 EGA 게임의 페이지 넘김과
하드웨어 스크롤, Mode X 의 320x240 이 된다:

- 시작 주소(0Ch/0Dh)로 페이지를 넘긴다.
- 행 간격은 오프셋(13h) × 2 이고, 주소 크기는 dword(14h bit6)·word·
  byte(17h bit6, 17h bit5 의 MA13/MA15 돌림)를 따른다.
- 크기는 표시 끝(01h, 12h, 07h 의 bit1·bit6)과 행 높이(09h 의 아래 다섯
  비트, bit7 두 배 주사)에서 나온다.
- 0x11 bit7 이 서 있으면 0-7 번 쓰기는 무시된다(07h bit4 만 예외). 모드
  표가 이 비트를 세우므로, Mode X 코드는 실기처럼 먼저 풀어야 한다.

그래픽 모드(0Dh-13h)의 CRTC 부팅값은 SeaBIOS `vgasrc/stdvgamodes.c` 의
`crtc_0d`·`crtc_0e`·`crtc_0f`·`crtc_11`·`crtc_13` 이다.

텍스트 화면(모드 0-3, 7)도 시작 주소·행 간격·주소 크기를 따른다. 시작
주소 800h 는 0xB9000(80x25 의 1 쪽)부터 보여 준다. 크기는 모드표대로
40·80 x 25 다. 텍스트 모드를 세우면(부팅 포함) `crtc_01`·`crtc_03`·
`crtc_07` 을 싣는다. 텍스트 메모리는 0xB8000 의 32KB 창 하나라 화면
자리가 그 안에서 돈다 — 실기 VGA 는 평면의 뒤쪽 32KB 도 보여 줄 수 있다.
CGA 그래픽(4-6)은 CRTC 를 거치지 않고 모드표대로 그린다. INT 10h AH=0Ch/0Dh 는
CRTC 와 상관없이 모드표의 줄 너비로 점을 찍고 읽는다.
chain-4 쓰기는 맵 마스크·그래픽 컨트롤러를 거치지 않고 바이트를 그대로
둔다 — 실기가 chain-4 에서 맵 마스크를 어떻게 쓰는지는 확인하지 못했다.
split screen(line compare)과 픽셀 패닝은 아직 없다.

텍스트는 쪽 여덟 장을 가진다(SeaBIOS `vgasrc/vgabios.c` 규칙).

- 모드를 세우면 쪽 크기(BDA 0x44C)를 다시 적는다. 80x25 는 4KB, 40x25 는
  2KB, CGA 는 16KB, 16색·256색은 화면 점 수 / 8 을 8KB 로 올린 값이다.
  0 쪽을 보이고 커서 여덟 칸(0x450 + 2 × 쪽)을 (0,0)으로 둔다. 텍스트
  모드는 32KB 창 전체를 0720h 로 지운다.
- INT 10h AH=05h 는 CRTC 시작 주소를 그 쪽 처음으로 옮기고 0x462·0x44E
  를 적는다. 0-7 밖이면 아무것도 하지 않는다. 그래픽 모드에서도 시작
  주소를 옮긴다(16색 4, 256색 1, CGA 2·4 를 곱하고 4 로 나눈다).
- AH=02h·03h·08h·09h·0Ah·13h 는 BH 쪽을 쓴다. 쪽마다 커서가 따로 있고,
  보이는 쪽의 커서만 CRTC 에 간다. AH=0Eh·06h·07h 와 INT 21h 출력은 보이는
  쪽을 쓴다.
- AH=06h·07h 의 창은 아래·오른쪽 끝을 화면 안으로 자른다. 자르지 않으면
  다음 쪽을 덮는다.
- 쪽 오프셋은 B800 세그먼트 안에서 16비트로 돈다. 게스트가 CRTC 커서를
  직접 쓰면 보이는 쪽의 커서 칸이 바뀐다.
- INT 10h AH=0Ch/0Dh 는 BH 를 보지 않고 0 쪽에 점을 찍는다.

CGA 4색 모드의 네 색은 포트 0x3D9 가 고르고, EGA/VGA 16색과 텍스트
모드의 색 번호는 속성 팔레트를 지나 DAC 자리로 간다. 속성 팔레트의
기본값은 항등이다 — 색 번호가 곧 DAC 자리다. 텍스트 바탕은 속성
컨트롤러 10h 의 깜박임 비트가 서 있으면 여덟 색, 꺼지면(INT 10h
AX=1003h BL=0) 열여섯 색이다. 깜박이는 글자는 늘 켜진 쪽으로 그린다.

INT 10h AH=00h 로 모드를 세우면 속성 팔레트와 DAC 256 자리가 기본값으로
돌아가고 PEL 마스크(0x3C6)는 0xFF 가 된다. AL bit7(화면 보존)을 줘도
마찬가지다. DAC 을 그대로 두는 길은 BDA 0x489 bit3(기본 팔레트 싣기
금지) 하나다. INT 10h AX=1201h BL=31h 가 이 비트를 켜고 AX=1200h BL=31h
가 끈다. 앞 프로그램이 DAC 을 검게 페이드하고 끝나도, 다음 프로그램이
모드만 세우고 팔레트를 안 쓰면 기본색이 보인다.

부팅 때 BDA 0x487·0x488·0x489 는 60h·F9h·51h 다(SeaBIOS·DOSBox-staging 과
같다). 모드를 세우면 0x487 은 60h 에 AL bit 7(화면 보존)을 bit 7 로 더한
값, 0x488 은 F9h 가 되고, 0x489 는 bit 7 만 지워진다. 그래서 AH=12h
BL=34h(커서 흉내 끔)는 다음 모드 세우기까지만 간다. AH=0Fh 는 AL 에
0x487 bit 7 을 더해 돌려준다. AH=12h BL=10h 는 0x463 이 3B4h 면 BH=1,
CL·CH 에 0x488 의 아래·위 니블을 준다.

시퀀서 1 번 bit 5(INT 10h AX=1201h BL=36h, 또는 포트 3C4h/3C5h)가 서
있으면 화면 그림은 DAC 0 번 색 한 가지다. 모드를 세우면 다시 켜진다.

회색 합산은 DAC 값을 (77R + 151G + 28B + 80h) >> 8 하나로 바꿔 세 칸에
같이 넣는다. AX=1200h BL=33h 가 BDA 0x489 bit 1 을 켜고 AX=1201h BL=33h
가 끈다. bit 1 이나 bit 2(흑백 모니터)가 서 있으면 AX=1010h·1012h 가
회색으로 싣는다. bit 1 이 서 있으면 모드 세우기도 기본 팔레트를 실은 뒤
256 자리를 합산한다. AX=101Bh 는 깃발과 상관없이 BX 부터 CX 자리를
합산한다. 포트 3C9h 로 쓴 값은 BIOS 를 거치지 않아 그대로다.

INT 10h AH=1Bh BX=0 은 ES:DI 에 64 바이트 상태 정보를 쓰고 AL=1Bh 로
돌아간다. 앞 네 바이트는 C000:0100 의 정적 기능 표(16 바이트)를 가리킨다.
그 뒤는 BDA 0x449-0x466·0x484-0x486 사본, DCC 08h, 지금 모드의 색·쪽·주사선,
BDA 0x487·0x489 에서 뽑은 깃발, 256KB 다. 정적 표는 이 기계가 아는 모드
(0-7, 0Dh-13h)와 실제로 하는 기능(회색 합산, 팔레트 싣기 켜고 끄기, 커서
흉내 비트, EGA 팔레트, DAC, DCC)만 켠다. BX 가 0 이 아니면 AL=0 이다.

### 계약

- **결정론**: 같은 이미지에 같은 키를 같은 순서로 넣으면 같은 화면이
  나온다. 시각도 난수도 호스트에서 오지 않는다. 시계는 셋이다.
  - RTC(INT 1Ah 02h·04h)는 `set_clock` 이 정한 기준시각에 CPU 사이클로
    환산한 경과를 더한다.
  - BIOS 틱(0x40:0x6C)은 부팅 때 기준시각으로 채우고 IRQ0 마다 오른다.
    하루치(0x1800B0)에 닿으면 0 으로 돌리며 0x40:0x70 을 올린다(SeaBIOS 와
    같다). 타이머 틱과 재주사 비트도 사이클에서 나온다.
  - DOS 의 시각(INT 21h 2Ch)은 BIOS 틱에서 계산해 1/100 초까지 준다. 날짜
    (2Ah)는 부팅 때 RTC 날짜로 정하고, 0x40:0x70 을 보고 하루씩 넘긴다.
    둘 다 MS-DOS 4.0 의 계산이다.

  라이브러리는 환경 변수를 읽지 않는다.
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
ZZT_DIR=/path/to/zzt ZZT_EXE=/path/to/ZZT.EXE dune exec --root . test/zzt_run_test/zzt_run_test.exe
SAMGUK3_DIR=/path/to/samguk3 dune exec --root . test/samguk3_scenario_test/samguk3_scenario_test.exe
```

프리웨어·쉐어웨어 게임 열다섯 개(Snarf, Caves of Thor, Xargon, Kiloblaster, Jetpack,
Arctic Adventure, Bio Menace, Monuments of Mars, Dark Ages, Halloween Harry,
Ken's Labyrinth, Supernova, Beyond the Titanic, Arcy 2, Charlie the Duck)가 시작해
키에 답하는지는 연기 시험 하나로 본다. 받는 스크립트가 게임을
`DEST/<이름>/` 에 푼다(curl 과 unzip 필요). 장면의 해시는 보지 않고, 화면에
글자나 비디오 모드와 색 수가 나올 때까지만 기다린다. 이미 받은 뒤에는 M3 Max 에서
10초 안팎 걸린다.

```sh
scripts/fetch-freeware-games.sh /path/to/games
DOS_GAMES_DIR=/path/to/games dune exec --root . test/games_smoke_test/games_smoke_test.exe
```

삼국지3 시나리오는 스텝 수를 적지 않는다. 단계마다 "화면의 이 영역이 이
digest 가 될 때까지" 기다린 뒤 키를 하나씩 넣고, 게임이 다시 키를 묻고
화면이 멈출 때까지 기다린다(`Dos_scenario`). 코어의 타이밍이 바뀌어도
같은 화면을 거쳐 간다. 어느 단계에서 다른 화면이 나오면 그 단계 이름과
마지막 프레임 경로를 남기고 실패한다.

## 게임 돌리기

```sh
dune exec --root . bin/dosboot.exe -- \
  --exe ZZT.EXE --mount ZZT.DAT=ZZT.DAT --mount TOWN.ZZT=TOWN.ZZT \
  --steps 6000000 --utf8 \
  --keys 256b,2e63,1c0d,1970,1c0d,3920,1c0d,1970,1c0d,1c0d,4d00@5000000
```

`--mount` 의 이름은 DOS 경로다. `--mount DATA/LEVEL1.DAT=...` 는 `DATA`
디렉터리 안에 파일을 두고, 디렉터리는 함께 생긴다. 게스트는
`C:\DATA\LEVEL1.DAT`, `DATA\LEVEL1.DAT`, `CD DATA` 뒤의 `LEVEL1.DAT`
어느 쪽으로도 연다. 드라이브는 C: 하나다. 이름은 DOS 가 읽듯 대문자로
바꾸고 8.3 으로 자른다. 이름 사이에 구분자가 여럿 있으면 하나로 읽는다
(`SAVE\\GAME.SAV` 는 `SAVE\GAME.SAV`). 앞의 두 구분자(`\\SERVER`)는
네트워크 이름이라 경로 없음이다.

`--keys` 의 한 항목은 `(스캔 코드 lsl 8) lor ASCII` 를 16진수로 쓴 것이고,
`@STEP` 은 그 스텝 전에는 넣지 않는다는 예약이다. 키를 미리 다 밀어
넣으면 앞선 메뉴의 "아무 키나" 루프가 전부 먹어 치우기 때문에, 게임이
특정 상태가 된 다음에 들어가야 하는 키는 `@` 로 묶는다. 글자만 칠
때는 `--type`.

스텝 수 대신 화면을 보고 키를 넣으려면 `--wait` 와 `--press` 를 쓴다.
적은 순서대로 돌고, 코어의 타이밍이 바뀌어도 같은 화면을 거쳐 간다.

```sh
dune exec --root . bin/dosboot.exe -- --com KOEI.COM --mount ... --steps 0 \
  --wait 220,130,200,120=418c2ef28ff2c7fb299ad234fe7bf09b --press enter \
  --wait 104,18,64,20=3e0a63288c39ede1bda328640a6579da --press 1,0,1,8,3,enter \
  --digest 200,165,240,75
```

| 인자 | 하는 일 |
|---|---|
| `--wait X,Y,W,H=MD5` | 프레임의 그 영역이 그 digest 가 될 때까지 돌린다 |
| `--press KEY,..` | 키 이름을 하나씩 넣고, 게임이 다시 키를 묻고 화면이 멈출 때까지 기다린다 |
| `--digest X,Y,W,H` | 끝난 뒤 그 영역의 digest 를 `--wait` 인자 모양으로 찍는다. 다음에 기다릴 화면을 정할 때 쓴다 |

이 단계들이 먼저 돌고, `--steps` 는 그 뒤에 `--keys`·`--feed` 와 함께
돈다. 한 단계가 5천만 스텝 안에 끝나지 않거나 프로그램이 먼저 끝나면
그 단계를 찍고, 멈춘 자리의 화면을 쓴 뒤 종료 코드 1 로 끝난다.

명령줄 인자는 `--args TEXT` 로 준다. `GAME TEXT` 를 친 것처럼 PSP:80h 꼬리가
` TEXT` 가 된다(COMMAND.COM 처럼 이름 뒤 공백부터). 환경 블록 끝의 프로그램
이름은 `C:\` 에 파일 이름을 붙인 것이다.

진단은 `--trace N`(N 스텝마다 CS:IP), `--int-trace`(INT 명령),
`--dump ADDR,LEN`(메모리), `--save NAME=PATH`(게스트가 쓴 파일 꺼내기).
`--sound-blaster` 는 카드를 꽂는다 — 게임 설정에서 Sound Blaster
(220h, IRQ 7, DMA 1)를 고를 때 쓴다.

`--audio-out FILE` 은 PC 스피커와 Sound Blaster 8비트 DAC 소리를
WAV(49716Hz, 16비트 모노)로 쓴다(RFC 0006). 표본 하나는 그 구간 CPU
사이클의 평균값이라 같은 입력이면 같은 파일이 나온다. 소리를 켜도 기계가
가는 길은 같다. 끝에 표본 수와 16비트를 넘어 자른 수를 찍는다. OPL2 와 SB
ADPCM 은 아직 소리가 없다.

```
audio /tmp/tone.wav: 20491 samples, clipped 0
```

`--unserved` 는 이 기계가 받지 못한 INT 호출을 끝에 stderr 로 센다.
`no-function` 은 호스트 서비스(21h·10h·15h·16h·1Ah·2Fh·33h)에 그 기능이 없을 때,
`iret-stub` 은 프로그램이 부른 INT 가 ROM 의 IRET 스텁으로 갔을 때다.
새 게임이 무엇을 기대하는지 여기서 먼저 본다.

```
UNSERVED no-function INT 33h AH=00h x2 AL=07,08
```

CPU 속도는 `--cpu-mhz MHZ` 로 바꾼다. 기본은 IBM PC 의 4.77MHz 다. 값은
PIT(1.193182MHz)의 정수배로 맞춘다. 12 를 주면 PIT 10 배인 11.93MHz 다.
IRQ0 간격, PIT 카운터, 리프레시 비트, 재주사 비트, OPL 타이머, 시각이
같은 비율로 따라간다. 사이클은 8086 명령표 값이라 이 MHz 는 8086 기준이다.
분기 명령은 표보다 작게 센다: Jcc 는 가면 8·안 가면 4(표 16/4), JMP 는 7(표 15),
LOOP·JCXZ 는 가든 안 가든 5(표 17/5·18/6). 이유와 남은 일은 #181 에 있다.
같은 MHz 의 286 은 한 사이클에 일을 더 한다. dosboot 는 실제 시간에
맞춰 돌지 않는다. 속도를 올리면 같은 게임 시간에 스텝이 더 들므로
`--steps` 도 그만큼 늘린다.

수직 귀선(포트 3DAh bit 3)은 VGA 처럼 프레임마다 두 줄 동안 켜진다.
400·350 줄 모드는 70.09Hz, 480 줄 모드는 59.94Hz 다.

그래픽 모드를 눈으로 보려면 내장 데모가 있다. 모드 0Dh 로 색 띠 열여섯을
그리고 PPM 으로 내놓는다.

```sh
dune exec --root . bin/dosboot.exe -- --demo ega --steps 2000000 --out /tmp/ega
```

게임 이미지는 저장소에 넣지 않는다.

## 터미널에서 하기

`dosplay` 는 사람이 직접 하는 실행기다. 화면은 Kitty 그래픽 프로토콜로
그리고, 입력은 Kitty 키보드 프로토콜과 SGR 픽셀 마우스로 받는다. Ghostty
와 kitty 처럼 두 프로토콜을 아는 터미널에서 돈다. F12 로 끝낸다.

```sh
dune build --root . --profile release
./_build/default/bin/dosplay.exe --dir /path/to/game --exe /path/to/game/GAME.EXE
```

- `--dir DIR` 은 폴더의 파일을 모두 대문자 이름으로 마운트한다. 하위
  폴더는 건너뛴다.
- 게스트는 벽시계에 맞춰 4.77MHz 로 돈다. 화면은 바뀐 프레임만 초당
  `--fps`(기본 30) 번까지 보낸다. DAC 만 바꾸는 페이드도 바뀐 프레임이다.
- 터미널이 앞 그림을 다 받기 전에는 새 그림을 만들지 않는다. 터미널이
  늦으면 프레임을 건너뛸 뿐 게스트 시간은 밀리지 않는다.
  - 12h 한 장은 base64 로 1.2MB 다. 이 저장소를 만든 Mac 에서 `cat` 이
    pty 에 쓰고 Python 이 읽으면 9.8MB/s(한 번에 1KB)라 한 장에 0.12초쯤
    걸렸다. 실제 터미널로는 재지 않았다.
- 같은 기계의 터미널이면 그림을 `/tmp` 의 임시 파일로 넘긴다(Kitty
  `t=t`). pty 에는 경로가 담긴 이스케이프 한 줄만 간다.
  - 시작할 때 1x1 시험 파일로 묻는다. 터미널이 OK 로 답하고 시험 파일을
    지워야 이 길을 쓴다. 답이 없거나 에러면 base64 로 보낸다(ssh 너머가
    이 경우다).
  - 터미널이 앞 프레임 파일을 지우기 전에는 새 프레임을 만들지 않는다.
  - 프레임 파일을 못 읽었다는 답이 오면 남은 프레임은 base64 로 보낸다.
  - 끝낼 때 DA1(`CSI c`)을 묻고 답을 기다린다. 그 사이 온 답은 버리고,
    남은 파일과 폴더를 지운다. DA1 에 답하지 않는 터미널이면 1초만
    기다린다.
  - 터미널 창을 닫거나(SIGHUP) SIGTERM 을 받아도 같은 정리를 거친다.
- `--audio-cmd CMD` 는 소리(49716Hz, 16비트 모노 raw)를 `CMD` 의 표준
  입력으로 흘린다. `CMD` 는 공백으로 나누고 셸을 거치지 않는다.
  재생기의 stdout·stderr 는 버린다.

  ```sh
  --audio-cmd "ffplay -nodisp -autoexit -f s16le -ar 49716 -ch_layout mono -"
  --audio-cmd "play -q -t raw -r 49716 -e signed -b 16 -c 1 -"
  ```

  - 쓰기는 막히지 않는다. 재생기가 늦으면 0.25초 몫까지 들고 있고,
    넘치면 오래된 표본부터 버린다. 표본 짝(2바이트)은 어긋나지 않는다.
  - 재생기가 끝나면 소리만 멈추고 게임은 계속 돈다.
  - 끝날 때 버린 표본 수와 재생기가 닫은 까닭을 stderr 에 찍는다.
  - Shift+F11 로 읽은 기계도 소리를 낸다. 스냅숏은 표본을 담지 않는다.
- 끝날 때 stderr 에 게스트 시간, 벽시계 시간, 보낸 프레임 수(그중 임시
  파일로 보낸 수)를 찍는다. 임시 파일 길을 버렸으면 까닭도 찍는다.
  게스트 시간이 많이 짧으면 이 호스트가 에뮬레이션을 못 따라간 것이다.
  dev 빌드는 느리니 release 로 빌드한다.
- 키는 누르는 순간 BIOS 워드 하나가 된다(누름과 뗌을 함께 넣는다).
  - 받는 키: 글자·숫자, Ctrl·Alt+글자, F1~F10(Shift·Ctrl·Alt 포함),
    방향키·Home·End·PgUp·PgDn·Ins·Del, 키패드.
  - BIOS 워드가 없는 키는 버린다: US 자판 표에 없는 구두점, F11,
    수식이 붙은 방향키. 끝날 때 몇 개를 버렸는지 알린다.
- 마우스는 그림 위의 픽셀을 드라이버 가상 화면으로 옮긴다. 크기는
  DOSBox-X 와 같다: 13h·0Dh 는 640x200, 12h 는 640x480, 텍스트는
  열×8 x 200.
  - 게임이 INT 33h 07h·08h 로 정한 범위는 아직 따르지 않는다.
- 화면은 터미널 왼쪽 위의 4:3 상자에 그린다.
- `--state FILE` 을 주면 F11 이 기계 상태를 그 파일에 저장하고,
  Shift+F11 이 그 상태로 돌아간다. `--com`·`--exe` 없이 `--state FILE`
  만 주면 거기서 이어 한다.
  - 상태에는 CPU·메모리·화면·마운트한 파일이 다 들어간다.
  - 게임이 쓴 파일(게임 안 저장)도 상태에 들어간다.
  - 옆 파일(`FILE.tmp`)에 쓴 뒤 이름을 바꾼다. 쓰다가 멈춰도 앞 상태는
    그대로다.
  - 상태 형식은 `Dos_snapshot.format_version` 하나만 읽는다. 형식이
    바뀐 빌드는 옛 상태를 읽지 못한다.
- 게임이 쓴 파일은 기계 메모리에만 있어서 끝내면 사라진다.
  `--save-dir DIR` 을 주면 끝날 때 시작과 내용이 달라진 파일만 DIR 에
  쓰고, 다음에 시작할 때 DIR 의 파일을 `--dir` 위에 싣는다.
  - 게임 폴더(`--dir`)의 원본은 건드리지 않는다. 바뀐 것만 DIR 에 모은다.
  - 에뮬레이터가 예외로 멈춰도 쓴다.
  - 게임이 지운 파일은 DIR 에서 지운다. 게임 폴더의 원본은 남기고
    알린다 — 다음에 시작하면 다시 보인다.
  - `/`·`\`·`:`·`..` 가 든 이름은 DIR 밖으로 나갈 수 있어 쓰지 않고
    알린다.
  - 끝날 때 열려 있던 파일의 마지막 쓰기는 들어가지 않는다. 핸들을
    닫을 때 기계의 파일 표에 들어가기 때문이다.
  - `--state` 만으로 이어 할 때는 상태 파일이 기준이라 DIR 을 싣지 않는다.

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

`run_until`은 `machine_steps`, `instructions`, `elapsed_cycles`, `stop_reason`을
담은 결과를 돌려준다. `max_steps`와 `run_with_keys`의 키 주입 시점은 입력 대기와
HLT idle을 포함한 machine-step clock이다. 실제 guest 명령 수는 `instructions`로
따로 센다. IRQ handler가 실행한 명령은 이 명령 수에 포함된다.

`push_key`는 키 하나를 눌렀다 뗀다. make 바이트는 바로, break 바이트는
100ms 뒤에 포트 0x60 으로 오고, 바이트 사이는 1ms 이상 벌어진다. 바이트마다
IRQ1 이 INT 9 로 가고, BIOS INT 9 가 make 바이트의 워드를 링에 넣는다. INT
9 를 가로챈 게임은 스캔 코드를 직접 읽고, 넘기지 않으면 링에는 아무것도
들어가지 않는다. 그래서 키는 넣은 순간이 아니라 IF 가 켜지고 IRQ1 이 풀린
뒤에 링에 닿는다. `key_unread`는 링이나 컨트롤러에 프로그램이 아직 안 가져간
키가 있는지 답한다.

워드가 어느 키와 어느 수식키에서 나왔는지는 AT BIOS 의 키 표(SeaBIOS
`src/kbd.c` 의 `scan_to_keycode`)로 거꾸로 찾는다. 수식키가 있으면 그 키의
make·break 가 앞뒤를 감싼다: `!`(0221h)는 `2A 02 82 AA`, alt-X(2D00h)는
`38 2D AD B8`, alt-1(7800h)은 1 키라 `38 02 82 B8` 이다. 한 키가 여러
수식키에서 같은 워드를 내면(스페이스, Enter) 수식키 없이 누른 것으로 본다.
표에 없는 워드는 상위 바이트를 make 로 보낸다. BIOS INT 9 는 왼쪽 shift·
ctrl·alt 를 0040:0017 과 0040:0018 에 둔다.

프로그램은 0x64 에 컨트롤러 명령을, 0x60 에 키보드 명령을 쓸 수 있다. 답은
QEMU(`hw/input/pckbd.c`·`ps2.c`)를 따른다.
- 컨트롤러: 20h/60h 명령 바이트(POST 뒤 61h), AAh 자체 시험(55h), ABh·A9h
  인터페이스 시험(00h), ADh/AEh 키보드 끔·켬, A7h/A8h, C0h, D0h/D1h 출력
  포트, D2h 는 쓴 바이트를 0x60 에 돌려준다.
- 키보드: EDh LED, EEh 에코, F0h 세트, F2h ID(변환 중이면 `FA AB 41`),
  F3h, F4h/F5h/F6h, FFh 리셋(`FA AA`). 모르는 바이트에는 FEh 로 답한다.
- 컨트롤러의 답이 키보드의 답보다, 키보드의 답이 키 바이트보다 먼저 온다.
  새 키보드 명령은 아직 안 읽은 이전 답을 버리고, 리셋은 보내지 않은 키를
  버린다.
- 명령 바이트 비트 0 을 끄면 IRQ1 이 오지 않는다. 바이트는 그대로 0x60 에
  올라와 폴링으로 읽는다.
- 하지 않는 것: A20 은 늘 꺼져 있어 D1h·DFh 로 켜도 1MB 에서 감는다. FEh 등의
  CPU 리셋은 하지 않는다. D3h·D4h 뒤에 aux 장치(PS/2 마우스)가 없다. F0h 로
  세트를 바꿔도 키는 BIOS 세트로 온다.

BIOS INT 16h AH=00/10은 키가 없으면 호출자를 멈추며, IRQ handler는 실행될 수
있다. `kbd_waiting`은 마지막 읽기나 비차단 poll이 빈 링을 관측했다는 뜻이다.
이 관측만으로 게임의 화면 전환 완료를 보장하지는 않는다.

DOS INT 21h AH=01/07/08, AH=0A, AH=0C의 입력 하위 기능과 console AH=3F도
미완료 입력을 성공으로 돌려주지 않는다. 부분 줄과 echo, 확장 키의 두 번째
바이트, AH=3F가 아직 돌려주지 않은 줄 데이터는 snapshot에 보존된다. AH=0C는
진입 시 한 번만 버퍼를 비운다. BIOS AH=01/11과 DOS AH=06/0B는 비차단이다.

## 지금 못 하는 것

- INT 21h 은 위 **DOS** 항목의 기능과 51h(62h 와 같다)만 한다. 그 밖의
  기능을 부르면 `Cpu86.Unsupported` 로 멈추고 IP 는 그 INT 에 남는다.
  레지스터를 그대로 둔 채 돌아가면 호출자가 성공으로 읽기 때문이다.
- 나눗셈이 트랩할 때 인터럽트 프레임에 밀리는 플래그 4,827 건이 실칩과
  다르다. 실칩의 나눗셈 미세동작(자리 옮김-빼기 루프)을 그대로 옮겨야
  좁혀진다.
- 그래픽 컨트롤러의 Misc 레지스터에 있는 메모리 창 선택 비트를 읽지
  않는다. 0xA0000 창의 주인은 모드가 정한다 — BIOS 로 모드를 세운
  프로그램은 이 둘이 늘 일치한다.
- 8087 코프로세서가 없다.
- 소리는 PC 스피커와 Sound Blaster 8비트 PCM(직접 DAC 10h, DMA)만 낸다
  (`dosboot --audio-out`, `dosplay --audio-cmd`). SB ADPCM 은 해독하지
  않고, OPL2 는 타이머만 있다. DAC 값은 스냅숏에 들지 않아서
  되살린 DSP 는 80h(쉼)에서 시작한다.

## 구조

| 모듈 | 맡은 것 |
|---|---|
| `Cpu86` | 8086/186 코어. 메모리와 포트는 콜백으로 바깥에 있다 |
| `Dos_state` | 기계 상태와 기본기 — 화면·커서·키 링·인터럽트 전달·시계, 게스트 메모리의 DOS 데이터 영역(0070h: ErrorMode·InDOS·WPErr·EXTERR·USER_IN_AX, 그 앞의 VERFLG·chSwitch·AllocMethod) |
| `Dos_error` | INT 21h 에러 코드의 class·action·locus 와 기능마다 돌려줄 수 있는 코드 (MS-DOS 4.0 `MS_TABLE.ASM`) |
| `Dos_ports` | 하드웨어 포트 — PIT, DAC, 스피커. PIC·DMA·키보드 포트는 아래 셋에 넘긴다 |
| `Dos_pic` | 8259 인터럽트 컨트롤러 두 개(AT 캐스케이드) — 어느 IRQ 를 언제 넣는가 |
| `Dos_dma` | 8237 DMA 컨트롤러 — 장치가 메모리의 어느 바이트를 언제 가져가는가 |
| `Dos_sb` | Sound Blaster 2.0 DSP — 포트 답, PCM·ADPCM 블록 시간, MIDI UART, IRQ, 8비트 DAC 값(ADPCM 해독 없음) |
| `Dos_audio` | 소리 표본(RFC 0006) — 사이클 구간의 평균값을 16비트로, PC 스피커·SB DAC 원천, WAV 머리. 포트 42h·43h·61h 접근과 DSP 출력 변화 앞에서 그때까지를 표본으로 만든다 |
| `Dos_opl` | OPL2 음색 레지스터(01h·08h·20h-F5h, RFC 0006 단계 4) — 기계 상태라 스냅숏에 든다. 아직 소리는 없다 |
| `Dos_kbc` | 8042 키보드 컨트롤러 — make·break 바이트를 언제 0x60 에 올리는가, 컨트롤러·키보드 명령의 답 |
| `Dos_keymap` | BIOS 키 워드 ↔ 키·수식키 표 |
| `Dos_video` | 비디오 어댑터 — 모드, 평면 메모리, 그래픽 컨트롤러, 시퀀서, CRTC |
| `Dos_bios` | ROM·IVT·BDA 와 INT 09h/10h/11h/12h/16h/1Ah/33h |
| `Dos_mouse_space` | 마우스 가상 화면 — 비디오 모드마다 좌표 넓이와 한 칸의 크기(CuteMouse 표) |
| `Dos_mouse_cursor` | 마우스 커서를 화면 메모리에 그리고 밑을 되돌린다(DOSBox-X 화살표·AND/XOR) |
| `Dos_dos` | INT 21h/20h — 파일, 디렉터리, 콘솔, 메모리, 날짜 |
| `Dos_clock` | MS-DOS 4.0 의 시각·날짜 계산 — 틱 ↔ 시각, 날 수 ↔ 날짜 |
| `Dos_path` | 게스트가 넘긴 이름을 DOS 5 처럼 읽어 경로로 — 드라이브, `.`·`..`, 8.3 자르기 |
| `Dos_xms` | XMS 3.0 드라이버 — 확장 메모리 블록, 잠금, 이동, INT 2Fh 답 |
| `Dos_fcb_parse` | INT 21h AH=29h — 글자열을 FCB 의 드라이브·이름·확장자로 나눈다(MS-DOS 4.0 `MakeFcb`) |
| `Dos_country` | 미국(국가 1) 437 의 대문자·정렬·파일 이름 글자·DBCS 표와 ROM 에 둘 바이트(MS-DOS 4.0 `DOSMES.ASM`) |
| `Dos_nls` | INT 21h AH=38h·63h·65h·66h — 나라 정보, 표 포인터, 대문자로 바꾸기, 코드 페이지(MS-DOS 4.0 `GETSET.ASM`) |
| `Dos_render` | 화면을 텍스트와 RGB 로 |
| `Dos_machine` | 배선·로더와 바깥에 보이는 얼굴 |
| `Dos_pit` | 8254 카운터 하나 — 지금은 채널 2. 마지막 사건 뒤 흐른 틱에서 카운트와 OUT 을 계산한다 |
| `Dos_snapshot` | 기계 전체를 바이트로 저장하고 새 기계로 되살린다. 형식 번호가 다르면 거절하고, 코어 digest 는 보여 주기만 한다 |
| `Dos_snap_codec` | 스냅숏의 정수·불·바이트 인코딩. 쓸 때와 읽을 때 같은 범위를 검사한다 — 되살릴 수 없는 스냅숏은 저장 단계에서 거절된다 |
| `Dosterm` (`term/`) | 터미널 바이트를 키·마우스 사건으로, 프레임을 Kitty 그래픽 이스케이프로. `dosplay` 가 쓴다 |
| `Dos_core_identity` | 이 빌드가 어떤 코어인지 — `lib/` 소스의 digest 를 빌드가 계산해 넣는다 (`ocaml-dos.core-identity`) |
| `Dos_scenario` | 코어 밖 하네스(`harness/`, `ocaml-dos.harness`). 화면 영역 digest 를 기다리고 키를 하나씩 넣는 시나리오를 돌린다 |

어떤 커밋의 digest 인지 알고 싶으면 그 커밋을 받아 `dune build` 하고
`_build/default/lib/identity/dos_core_identity.ml` 을 보면 된다. 소스가
같으면 커밋이 달라도 digest 는 같다.
