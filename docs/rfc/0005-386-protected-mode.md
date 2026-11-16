# RFC 0005: 80386 보호 모드와 DOS 익스텐더

- 상태: 제안
- 날짜: 2026-11-14
- 범위: `lib/cpu86*.ml`, `lib/dos_machine.ml` 의 메모리와 A20, 새 시험 러너
  `test/cpu86_test386.ml`
- 앞선 RFC: 0001(#63, 386 실모드)

## 왜

지금 기계는 실모드 CPU 로만 돈다. 1MB 위 메모리도 보호 모드도 없다. DOS
익스텐더로 보호 모드에 들어가는 게임은 시작하지 못한다.

원문으로 확인한 두 LucasArts 게임이 DOS/4GW 를 쓴다(2026-11-15,
[DOSDays](https://www.dosdays.co.uk/)).

- [Full Throttle](https://www.dosdays.co.uk/topics/Games/game_ft.php): 설치
  폴더 표에 `DOS4GW.EXE` 가 있고 "DOS Extender to use XMS memory" 라고 적혀
  있다. 최소 사양은 "Intel 486DX-33 or higher" 다.
- [The Dig](https://www.dosdays.co.uk/topics/Games/game_dig.php): "LucasArts
  provided 4 versions of the DOS4GW DOS extender on the CD-ROM." 최소 사양은
  "Intel 486DX2-66 CPU minimum for DOS use" 다.

Rebel Assault I·II, Dark Forces, X-COM 두 편, Panzer General, Heroes of Might
and Magic I·II, Master of Orion II 도 보호 모드 게임이라는 조사가 있다. 원문은
확인하지 않았다(확인 필요). 그 조사에서 이름이 나온 익스텐더는 모두 DOS/4GW
계열이었다.

삼국지3 같은 실모드 게임은 RFC 0001 과 지금 열린 PR 들이 다룬다. 이 RFC 는
그 바깥의 게임을 위한 것이다.

## 하네스 먼저

보호 모드는 실칩 데이터로 잴 수 없다. [SingleStepTests/80386](https://github.com/SingleStepTests/80386)
README 는 이렇게 쓴다: "The v1_ex_real_mode directory contains real mode
opcode tests derived from the 386EX." 보호 모드 시험은 "will (eventually)"
이고, FAQ 에 "I cannot say when protected mode tests will appear." 라고
적혀 있다. 실모드 데이터에도 0F00·0F01·0F20-23 파일은 없다(80386.csv 에만
있다).

그래서 [test386.asm](https://github.com/barotto/test386.asm) 을 하네스로
쓴다(2026-11-14 확인).

- BIOS 대신 도는 ROM 이다: "It runs as a BIOS replacement and does not
  depend on any OS or external library."
- 실모드, 보호 모드, ring 3 전환, V86, TSS 작업 전환, 페이징을 시험한다.
  POST 0x00-0x06 이 실모드이고, 0x08 에서 보호 모드로 들어간다. 작업 전환
  (0x22)은 128KB ROM 판에만 있다.
- 진행은 포트 0x190 의 POST 코드로 알린다(`configuration.asm` 의
  `POST_PORT equ 0x190`). 실패하면 HLT 로 멈추고, 마지막 POST 코드가 실패한
  시험이다. 끝까지 가면 0xFF 다.
- POST 0xEE 는 플래그를 바꾸는 연산의 결과를 글자로 찍는다. 이것을
  `test386-EE-reference.txt` 와 비교한다. 출력 포트(`LPT_PORT`·`COM_PORT`·
  `OUT_PORT`)는 기본이 0 이라 빌드할 때 하나를 켠다.
- 64KB 판은 물리 주소 0xF0000 에 두고 0xFFFF0000 에 겹쳐 보인다. 메모리는
  "All memory accesses will remain within the first 1MB." 다.
- 스스로 완성되지 않았다고 쓴다: "test386.asm is still **incomplete** and is
  not able to test every functionality of a CPU." 다 통과해도 게임이 돈다는
  뜻은 아니다.
- 라이선스가 GPL-3.0 이다. 이 저장소에 소스도 빌드한 ROM 도 넣지 않는다.
  SingleStepTests 처럼 밖에서 받아 NASM 으로 빌드하고, 러너는 경로를 환경
  변수로 받는다. 없으면 건너뛴다.
- IBMulator, [PCjs](https://www.pcjs.org/software/pcx86/test/trace),
  [pce](https://github.com/stenzek/pce/blob/master/src/pce-tests/cpu_x86/test386.cpp),
  [Tsugaru](https://github.com/captainys/TOWNSEMU/blob/master/src/test386vm/main.cpp)
  가 같은 방식으로 쓴다.

## DOS/4GW 가 바라는 것

[Open Watcom 문서](https://github.com/open-watcom/open-watcom-v2/tree/master/docs/doc/rsi)
(`config.gml`, `dos4gwqa.gml`, `errors.gml`)와 `dos4gw.doc` 기준이다.

- CPU: "DOS/4G requires an 80386 (or later) CPU."
- 보호 모드에 들어가는 길을 스스로 고른다. 전환 방식 표에 셋이 있다.
  - DPMI: "Set automatically if DPMI is active"
  - VCPI: "Set automatically if VCPI detected". "By default, DOS/4GW
    searches for a VCPI server and, if one is present, forces it on."
  - raw: "Set automatically for 386 or 486"
- DPMI 호스트가 없으면 INT 31h 를 DOS/4GW 가 직접 준다: "These are the
  services provided by DOS/4GW and DOS/4GW Professional in the absence of a
  DPMI host."
- A20: "The A20 line must be enabled for the CPU to run in protected mode".
  게이트를 8042 로 여는지 포트 92h 로 여는지는 확인 필요다.
- 확장 메모리 크기는 BIOS 에 묻는다: "Interrupt 15H function 88H, get
  extended memory size".
- 실모드 인터럽트로 넘기는 범위가 있다: "auto-passup range (8-2Eh, excluding
  21h)". 보호 모드에서 실모드로 자주 오간다.
- 페이징: 문서의 예시 덤프가 `CR0: PG:1 ... PE:1 CR3: 9067` 이고, FAQ 는
  DOS/4GW 를 두고 "Linear addresses at or above 1 MB do not map directly to
  physical memory" 라고 쓴다. 페이징을 쓴다는 뜻으로 읽힌다. 다만 둘 다 어느
  전환 방식에서의 이야기인지 밝히지 않아서, raw 경로가 페이징을 켜는지는
  확인 필요다.

이 기계는 DPMI 호스트도 VCPI 서버(EMM386)도 두지 않는다. 그러면 DOS/4GW 는
raw 경로를 탄다. 이 RFC 는 raw 경로에 필요한 CPU 와 기계를 만든다. VCPI
경로는 게스트가 EMM386 을 띄울 때만 쓰이고, V86·페이징·LTR 이 필요하다
([VCPI](https://www.edm2.com/index.php/VCPI_Standard): TR 은 "must be loaded
with an LTR instruction, not a task switch").

## 지금 코드

- 물리 주소는 `seg lsl 4 + off` 로 바로 센다. `Cpu86_state.physical` 과
  `Cpu86_decode` 의 `at`·`read16`·`write16`·`read32`·`write32`, 명령 가져오기,
  스택 한 곳이다.
- 결함은 벡터 번호만 싣는다(`exception Fault of int`). 오류 코드를 담을 자리가
  없다.
- 인터럽트는 `int_hook` 으로 호스트(기계의 BIOS·DOS)가 받는다.
- 기계 메모리는 1MB 다(`Bytes.make (1024 * 1024)`, 주소를 `land 0xfffff`).
  A20 은 늘 닫힌 것과 같다.
- XMS 모델(#115)은 15MB 를 CPU 가 닿지 않는 별도 저장소에 둔다. HMA 가 없고
  A20 은 꺼진 채다.

## 설계

단계마다 PR 하나 이상이다. 각 단계는 앞 단계의 확인을 모두 지킨다: 8086·186·
286·386 실모드 스위트 결과, 삼국지3 화면·시나리오, 속도 ±3%.

### 0 단계: test386 러너

- `test/cpu86_test386.ml`. 맨 CPU(`Cpu86.create ~model:I80386`)에 1MB 와
  ROM 별칭을 붙이고, 0x190 에 쓴 값과 출력 포트의 글자를 모은다.
- 지금 CPU 로 POST 0x06 끝까지 가는지 잰다. 0x08 에서 멈추는 것이 이 단계의
  기대값이다.
- 확인: 러너가 같은 ROM 에서 늘 같은 POST 열을 낸다. 실모드 POST 에서 실패가
  나오면 그 명령을 고치는 PR 을 따로 낸다.
- 미리 잰 값(2026-11-16, #166 머리 `8516ca3a`, test386.asm `cfd052d`):
  - POST 열은 `00 01 02 03 04 05 06 08` 이다. 793,772 스텝 뒤 f000:2662 의
    `66 0F 01`(32 비트 LGDT·LIDT)에서 `Unsupported` 로 멈춘다.
  - `INT`·결함은 러너가 실칩처럼 IVT 로 보낸다. FLAGS·CS·IP 를 넣고 IF·TF 를
    끈다.
  - 러너가 실패를 잡는지 CPU 를 세 군데 망가뜨려 봤다. MUL r/m32 가 EDX 를 버리면
    POST 02 에서, 문자열이 DF 를 무시하면 04 에서, LSS·LFS 가 세그먼트를 오프셋
    자리에서 읽으면 06 에서 멈춘다. 세 경우 모두 `error` 자리(f000:fe8a)의
    HLT 다.
  - NASM 3.02 는 이 소스를 빌드하지 못한다. 메모리 대상에 크기를 즉시값 쪽에만
    붙인 줄(`mov [eax*4], word error`)에서 "size wasn't specified" 로 멈춘다.
    이런 줄이 110 개다. 사본에서 `mov word [eax*4], error` 꼴로 옮겨 빌드했다.
    NASM 2 가 같은 줄을 읽던 뜻(피연산자 크기는 그 크기, 즉시값은 줄일 수 있음)과
    같은 꼴이다.

### 1 단계: 디스크립터 캐시

- 세그먼트 레지스터마다 셀렉터, base, limit, 속성을 둔다. 모든 메모리 접근이
  `seg lsl 4` 대신 base 를 쓴다.
- 실모드의 세그먼트 적재는 base 를 셀렉터×16 으로 바꾼다. limit·속성을 실모드
  적재가 어떻게 다루는지는 Intel 설명서와 test386 으로 정한다.
- 67h 의 FFFFh 한계 검사(RFC 0001)가 캐시의 limit 을 읽게 바꾼다.
- 확인: 모든 실모드 스위트가 같다. 메모리 접근마다 걸리는 자리라 명령 수를
  번갈아 세 번 잰다.
- 미리 잰 값(2026-11-16, #166 머리 위 시제품, base 만):
  - 세그먼트 쓰기는 `set_seg` 한 곳과 결함 되돌리기(`Cpu86_fault.atomic`)의
    배열 복사 하나로 모인다. 시제품은 `bases` 배열을 `set_seg` 에서 같이 쓰고,
    `atomic` 이 같이 떠 둔다. 주소를 세는 곳 여섯(`at`·`read16`·`write16`·
    `read32`·`write32`, 명령 가져오기)이 `seg lsl 4` 대신 `bases` 를 읽는다.
  - 결과는 #166 과 같다. SST 386 실모드 941 파일 1,758,700 건, SST 286 `00`,
    test386 POST 열과 스텝 수, 삼국지3 3천만 스텝 화면(286·386)의 출력이 바이트까지
    같다.
  - 호스트 명령 수는 +0.24% 다(삼국지3 3천만 스텝, 번갈아 세 번, 평균 24.346G →
    24.403G). 한 빌드 안의 흔들림은 0.07% 안이다. 부하 평균이 265 인 기계에서
    쟀다.
  - 이어서 limit 칸을 두고(실모드에서는 늘 FFFFh), 한계 검사 넷(`overruns`·
    `check_dword`·`within`, 문자열의 `crosses`)이 상수 대신 그 칸을 읽게 했다.
    결과는 위와 같이 모두 같다. 셋을 번갈아 세 번씩 잰 값은 #166 24.335G,
    base 24.386G(+0.21%), base·limit 24.443G(+0.44%)다. 부하 평균 530 에서
    쟀고 흔들림은 0.14% 안이다.
  - 속성(형식·DPL·존재) 검사는 보호 모드에서만 돈다. 실모드 비용에는 들지 않는다.

### 2 단계: 1MB 위 메모리와 A20

- 기계 메모리를 386 의 주소선 24 개(16MB)까지 늘린다. 286·8086 기계는 지금
  크기를 지킨다.
- A20 게이트를 둔다. 닫히면 1MB 에서 감는다. 여는 길(8042 출력 포트, 포트
  92h)은 DOS/4GW 가 쓰는 쪽을 확인하고 정한다.
- XMS 저장소(#115)를 이 메모리의 1MB 위 영역으로 합친다. INT 15h AH=88h(#90)는
  같은 크기를 알린다.
- 확인: XMS 시험이 같다. A20 이 닫히면 FFFF:0010 이 0:0000 과 같은 바이트다.

### 3 단계: 시스템 레지스터와 보호 모드 진입

- CR0·CR2·CR3, GDTR·IDTR·LDTR·TR. LGDT·LIDT·SGDT·SIDT·LLDT·SLDT·LTR·STR·
  LMSW·SMSW, MOV CRn·DRn·TRn, LAR·LSL·VERR·VERW·ARPL.
- CR0.PE 를 켜면 이후 세그먼트 적재가 디스크립터 표를 읽는다. 끄면 실모드로
  돌아간다.
- `Fault` 가 오류 코드를 싣는다. 보호 모드의 INT·결함은 IDT 의 게이트로 간다.
  `int_hook` 은 실모드(와 V86)에서만 호스트로 간다.
- 확인: test386 POST 0x08 을 넘는다.

### 4 단계: 보호 검사와 특권 전환

- 세그먼트 적재의 형식·DPL·존재 검사, #GP·#SS·#NP·#TS. ring 0↔3 전환의 TSS
  스택 바꾸기, 바깥 링으로 가는 IRET·RETF.
- 확인: test386 POST 0x09·0x20·0x0B-0x10·0x12-0x1C.

### 5 단계: 페이징

- CR3 의 두 단계 표, #PF 와 CR2, PTE 의 accessed·dirty.
- 확인: test386 POST 0x11.

### 6 단계: V86 과 작업 전환

- VCPI 경로와 test386 128KB 판(0x21·0x22)에 필요하다. raw 경로의 DOS/4GW 에
  필요한지는 3-5 단계 뒤 게임으로 확인한다.
- 확인: test386 128KB 판 POST 0xFF, 0xEE 출력이 기준 파일과 같다.

### 7 단계: DOS/4GW 프로그램

- raw 경로로 DOS/4GW 프로그램 하나가 `main` 까지 간다.
- 로컬에 DOS/4GW 게임이 없다. 무엇으로 확인할지는 「정할 것」에 둔다.

## 범위 밖

- 286 보호 모드. 시험은 [test286.asm](https://github.com/dbalsom/test286.asm) 이
  있다.
- DPMI 호스트와 VCPI 서버를 기계가 주는 일. DOS/4GW 는 호스트가 없으면 INT 31h
  를 스스로 준다.
- 486 명령, FPU, 캐시·TLB 시간, 사이클 정확도.

## 위험

- 1 단계가 메모리 접근마다 걸린다. RFC 0001 처럼 속도 ±3% 를 넘으면 그
  단계에서 멈추고 원인을 잰다.
- test386 이 통과해도 게임이 돈다는 보장은 없다(스스로 미완성이라고 쓴다).
- raw 경로의 페이징과 A20 방식이 확인되지 않았다. 5 단계와 2 단계의 범위가
  바뀔 수 있다.
- 확인한 두 게임의 최소 사양이 486 이다. 486 에서 새로 생긴 명령(BSWAP·XADD·
  CMPXCHG 등)을 게임이나 DOS/4GW 가 쓰는지는 확인 필요다. 쓰면 486 명령이
  범위 안으로 들어온다.
- 386 실모드(#154-#165)는 main 에 있다. 기계에서 386 을 고르는 #166 은 아직
  열려 있다. 0-6 단계는 맨 CPU 로 시험하니 #166 없이 갈 수 있고, 7 단계는 #166 이
  필요하다.

## 정할 것

1. CI 에서 test386 을 받아 NASM 으로 빌드해 돌릴지. CI 를 바꾸는 일이다.
   NASM 3 에서는 소스 110 줄을 고쳐야 하니 NASM 2.16 을 고정할지도 같이 정한다.
2. XMS 저장소를 CPU 메모리로 합칠지, 아니면 둘을 따로 두고 1MB 위 메모리를
   새로 둘지.
3. 7 단계를 무엇으로 확인할지(로컬 DOS/4GW 프로그램).

## 확인 방법 요약

| 단계 | 확인 |
|---|---|
| 0 | test386 러너가 같은 POST 열을 낸다. 실모드 POST 0x00-0x06(미리 잰 값: 통과, 0x08 의 `66 0F 01` 에서 멈춤) |
| 1 | 실모드 스위트·삼국지3 같음, 명령 수 ±3% |
| 2 | XMS 시험 같음, A20 감기 |
| 3 | test386 POST 0x08 통과 |
| 4 | test386 POST 0x09·0x20·0x0B-0x10·0x12-0x1C |
| 5 | test386 POST 0x11 |
| 6 | test386 128KB 판 POST 0xFF, 0xEE 기준 파일과 같음 |
| 7 | DOS/4GW 프로그램이 `main` 까지 |
