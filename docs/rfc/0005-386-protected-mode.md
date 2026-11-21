# RFC 0005: 80386 보호 모드와 DOS 익스텐더

- 상태: 채택(2026-11-21 사용자 결정), 3 단계 진행(3a·3b 구현)
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

로컬 서베이에서도 같은 벽이 나온다(2026-11-21). classicdosgames.com 에서 받은
게임을 386 모델로 3천만 스텝씩 돌린 결과 파일을 합치면, 32 비트 LGDT(`0F 01
/2`)에서 `Unsupported` 로 멈춘 게임이 44개다. 그중 39개가 같은 주소
(1248:031f)에서 멈춘다. 같은 DOS/4GW 빌드의 raw 전환 자리로 읽힌다(그 주소의
바이트를 원문과 맞추지 않았다: 확인 필요). Hexen, Strife, Tomb Raider, Mortal
Kombat II·3, SimCity 2000, Star Wars: Dark Forces, The Need for Speed, MDK,
Worms 가 여기에 든다. 나머지 5개는 다른 익스텐더 주소에서 멈춘다(Alien Breed,
Astro3D, Axia, ShadowCaster, Super Street Fighter II Turbo).

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
  게이트를 8042 로 여는지 포트 92h 로 여는지는 실행해 봐야 안다. 2 단계는 둘 다
  받는다. 로컬 조사 폴더의 DOS4GW.EXE 10 종(MD5 가 다른 빌드)을
  실행하지 않고 바이트로만 훑었다. 10 종 모두 "8042 timeout" 문자열이 있고,
  `B0 D1`(mov al,D1h)이 한 번씩 있다. `B0 DF`·`B0 DD` 는 5 종에 있다. 포트 92h 는
  `E4 92` 가 7 번, `E6 92` 가 8 번 나온다. 250KB 파일에서 2 바이트 패턴이 우연히 나올
  기대값(약 4 번)의 두 배 안쪽이라 코드인지 우연인지 가르지 못한다. 3a 에서 386
  기계로 실행해 한 빌드를 확인했다(3 단계 「구현한 값」).
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

- 물리 주소는 세그먼트 레지스터의 base 에 오프셋을 더해 센다. 읽는 곳은
  `Cpu86_decode` 의 `at`·`read16`·`write16`·`read32`·`write32`·`fetch8` 과
  `Cpu86_state.physical`(8086·186 의 PUSH SP 와 test386 러너가 부른다)이다.
  base 는 실모드라 셀렉터×16 이고 `set_seg` 가 쓴다. 기계의 호스트 서비스
  (`dos_*.ml`)는 게스트 버퍼 주소를 아직 셀렉터×16 으로 센다.
- 결함은 벡터 번호만 싣는다(`exception Fault of int`). 오류 코드를 담을 자리가
  없다.
- 인터럽트는 `int_hook` 으로 호스트(기계의 BIOS·DOS)가 받는다.
- 기계 메모리는 386 이 16MB, 나머지가 1MB 다. 버스가 `Dos_a20` 의 마스크를 모든
  물리 주소에 씌운다. 기계의 호스트 서비스(`dos_*.ml`)와 `Dos_machine.mem_read`·
  `mem_write` 는 아직 1MB 에서 감는다.
- XMS 블록은 386 기계에서 그 RAM 의 1MB 위 영역이고, 나머지 기계에서는 CPU 가 닿지
  않는 자기 바이트다. HMA 가 없고 XMS 의 A20 기능(03h-07h)은 게이트를 건드리지
  않는다.

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

- 세그먼트 레지스터마다 셀렉터, base, limit 을 둔다. 모든 메모리 접근이
  `seg lsl 4` 대신 base 를 쓴다. 속성(형식·DPL·존재)은 읽는 쪽이 3·4 단계라 그때
  둔다.
- 실모드의 세그먼트 적재는 base 를 셀렉터×16 으로 바꾼다. limit 은 적재가 쓰지
  않는다. 만들 때 FFFFh 이고 이 단계에서 아무도 바꾸지 않는다. 실모드 적재가
  limit 을 FFFFh 로 되돌리는지는 limit 을 쓰는 3 단계에서 Intel 설명서와
  test386 으로 정한다.
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
- 구현한 값(2026-11-21, main `5aec339b` 위):
  - `Cpu86_state` 에 `bases`·`limits` 배열을 두었다. 칸 수는 `segs` 와 같고,
    `set_seg` 가 셀렉터와 base 를 같이 쓴다. 세그먼트 레지스터를 쓰는 곳은 `set_seg` 와
    `Cpu86_fault.atomic` 의 되돌림 둘이다. `atomic` 은 셀렉터를 뜨는 앞 네 칸(ES CS
    SS DS)의 base 도 같이 뜨고 되돌린다.
  - base 를 읽는 곳은 `at`·`read16`·`write16`·`read32`·`write32`·`fetch8`·`physical`
    이다. `physical` 은 셀렉터 값 대신 세그먼트 레지스터 번호를 받는다. limit 을
    읽는 곳은 `overruns`·`check_dword`·`within` 이고, 문자열의 `crosses` 는
    `overruns` 로 간다. 상수 `segment_limit`·`dword_last_offset` 은 없앴다.
  - `fetch8` 은 `physical` 을 부르지 않고 합을 식으로 쓴다. 호출을 남기면 아래 표의
    +0.8% 이고, 풀어 쓰면 main 보다 줄어든다(표의 최종).
  - 명령 가져오기의 끝(`segment_end`)은 아직 CS 의 limit 이 아니라 상수다. CS 에
    limit 이 들어오는 3 단계에서 읽게 한다.
  - 결과는 main 과 같다.
    - SST 386 실모드 941 파일 1,758,700 건: fail 4(baseline), undef 127,247.
    - SST 286 326 파일 1,478,000 건: pass 1,438,263, undef 39,735, fail 2(baseline).
      받은 폴더의 `metadata.json` 은 시험이 아니다.
    - SST 8086 323 파일 646,000 건: pass 599,993, undef 41,180, fail 4,827(baseline).
    - `dune build @runtest` 의 출력.
    - test386: POST 열 `00 01 02 03 04 05 06 08`, 793,772 스텝 뒤 f000:2662 의
      `Unsupported`.
    - 삼국지3 3천만 스텝 화면(286·386)의 PPM 과 출력, 삼국지3 시나리오의 저장 파일
      MD5, ZZT, 게임 스모크 18 개.
  - 호스트 명령 수(`/usr/bin/time -l` 의 retired instructions): 삼국지3 `KOEI.COM` 에서
    3천만 스텝, 번갈아 세 번, dune 기본(dev) 빌드의 `dosboot`, 부하 평균 100-135 기계.
    한 빌드 안의 흔들림은 0.1% 안이다. 기준 커밋이 시제품과 달라 절대값은 시제품과
    비교하지 않는다.

    | 모델 | main | `physical` 호출을 남긴 판 | 최종 |
    |---|---|---|---|
    | 286 | 36.047G | 36.335G(+0.80%) | 35.196G(-2.36%) |
    | 386 | 39.351G | 39.656G(+0.78%) | 38.509G(-2.14%) |

  - 한계 검사가 limit 칸을 읽는지는 칸의 초기값을 FFFEh 로 바꿔 확인했다.
    `cpu86_286_test` 의 세 검사가 깨진다. limit 을 바꾸는 코드가 아직 없어서 공개
    API 로는 limit 이 FFFFh 가 아닌 경우를 시험할 수 없다. 3 단계에서 세그먼트 적재가
    limit 을 쓰면 그 시험이 생긴다.

### 2 단계: 1MB 위 메모리와 A20

- 기계 메모리를 386 의 주소선 24 개(16MB)까지 늘린다. 286·8086 기계는 지금
  크기를 지킨다.
- A20 게이트를 둔다. 닫히면 1MB 에서 감는다. 여는 길(8042 출력 포트, 포트
  92h)은 DOS/4GW 가 쓰는 쪽을 확인하고 정한다.
- XMS 저장소(#115)를 이 메모리의 1MB 위 영역으로 합친다. INT 15h AH=88h(#90)는
  같은 크기를 알린다.
- 확인: XMS 시험이 같다. A20 이 닫히면 FFFF:0010 이 0:0000 과 같은 바이트다.
- 구현한 값(2026-11-21, main `c183babc` 위):
  - `lib/dos_a20.ml` 이 보드 둘을 가른다. `One_megabyte`(8086·186·286)는 RAM 1MB,
    마스크 FFFFFh, 게이트 없음이다. `Sixteen_megabytes`(386)는 RAM 16MB, 24 비트다.
    게이트가 닫히면 마스크가 EFFFFFh(20번 비트를 0 으로)이고 열리면 FFFFFFh 다.
    닫힌 채 시작한다. 마스크 칸은 `set` 만 쓰고 버스가 접근마다 읽는다.
  - 386 의 주소는 24 비트로 자른다(386SX 보드). 1000000h 는 0 과 같다. 386DX 에 RAM 16MB 만
    꽂은 기계라면 그 위는 비어 있겠지만, RFC 의 "주소선 24 개" 를 따랐다.
  - 8042 의 D1h(출력 포트 bit 1)·DDh·DFh 와 포트 92h(bit 1)가 한 선을 움직인다. 나중에 쓴
    쪽이 이긴다. D0h 는 선의 상태를 bit 1 에 읽어 준다. 포트 92h 의 bit 0(빠른 리셋)은
    8042 의 리셋처럼 하지 않는다. 1MB 보드는 포트 92h 를 받지 않아 FFh 를 읽고,
    DDh·DFh·D1h 는 선을 올리지 못해 D0h 의 bit 1 이 0 이다(지금까지와 같다).
  - XMS 합치기(「정할 것」 2): 386 기계의 `Dos_xms` 는 `~memory` 로 그 RAM 을 받아 블록을
    RAM 의 보기로 쓴다. 잠금 주소가 곧 CPU 가 보는 주소다. 새 블록과 크기를 키운 꼬리는
    0 으로 채운다(기존 시험의 "새 메모리는 0"). 286·8086 기계는 1MB 를 지키려고 따로 둔다.
  - INT 15h AH=88h 는 RAM 의 1MB 위 크기를 준다. 386 은 15360KB(XMS 용량과 같다),
    나머지는 0 이다. CMOS 17h·18h 는 지금까지처럼 모든 모델에서 15360KB 이고, 286
    기계에서 88h 와 어긋나는 것도 지금과 같다.
  - 스냅숏 형식 42 → 43. 386 의 본문이 16MB RAM, 키보드 컨트롤러 뒤의 게이트 한 칸이고,
    XMS 블록은 바이트를 쓰지 않는다. 286·8086 의 본문은 바이트까지 같다(같은 기계를 42·43
    빌드로 저장해 본문 MD5 대조). RFC 0002 는 아직 제안이라 번호를 손으로 올렸고, 같은 번호를
    고른 열린 PR 은 없었다.
  - 결과는 main 과 같다. SST 386 941 파일 1,758,700 건(fail 4, undef 127,247), SST 286
    326 파일 1,478,000 건(pass 1,438,263, fail 2, undef 39,735), SST 8086 323 파일
    646,000 건(pass 599,993, fail 4,827, undef 41,180). test386 은 POST 열 `00 01 02 03 04
    05 06 08`, 793,772 스텝 뒤 f000:2662. 삼국지3 저장 파일 MD5, ZZT, 게임 스모크 18 개,
    삼국지3 3천만 스텝 화면(286·386)의 PPM 과 출력이 같다.
  - 호스트 명령 수(1 단계와 같은 방법: 삼국지3 `KOEI.COM` 3천만 스텝, 번갈아 세 번, dev
    빌드 `dosboot`, 부하 평균 40-75 기계). 한 빌드 안의 흔들림은 0.03% 안이다.

    | 모델 | `238d2c9b` | 2 단계 |
    |---|---|---|
    | 286 | 35.175G | 35.341G(+0.47%) |
    | 386 | 38.482G | 38.658G(+0.46%) |

    버스 클로저가 접근마다 마스크 칸을 한 번 더 읽는 값이다.
  - 386 스냅숏은 커진다. 같은 기계(XMS 블록 하나, COM 한 줄 실행)의 저장 크기는 1,325,577
    바이트에서 17,050,114 바이트, 저장 2.1ms 에서 27.6ms, 복원 2.1ms 에서 25.2ms 다(5 회
    평균, 부하 평균 40-75 기계). 286 은 1,325,489 바이트로 같다.
  - 변이 30 개를 따로 돌렸다. 29 개는 시험이 잡았다. 잡지 못한 하나는 형식 번호를 올리지
    않는 변이다. 시험은 번호를 값이 아니라 `Dos_snapshot.format_version` 과 비교한다.
  - 하지 않은 것: HMA 와 XMS 의 A20 기능(03h-07h), INT 15h AH=24h·87h·89h, 호스트
    서비스의 1MB 위 주소. DMA 는 `Bytes.length mem` 으로 감아서 386 기계에서는 페이지 레지스터
    값 10h 이상이 1MB 에서 감기지 않고 RAM 을 닿는다.

### 3 단계: 시스템 레지스터와 보호 모드 진입

- CR0·CR2·CR3, GDTR·IDTR·LDTR·TR. LGDT·LIDT·SGDT·SIDT·LLDT·SLDT·LTR·STR·
  LMSW·SMSW, MOV CRn·DRn·TRn, LAR·LSL·VERR·VERW·ARPL.
- CR0.PE 를 켜면 이후 세그먼트 적재가 디스크립터 표를 읽는다. 끄면 실모드로
  돌아간다.
- `Fault` 가 오류 코드를 싣는다. 보호 모드의 INT·결함은 IDT 의 게이트로 간다.
  `int_hook` 은 실모드(와 V86)에서만 호스트로 간다.
- 확인: test386 POST 0x08 을 넘는다.
- 3 단계는 PR 셋으로 나눈다. 메모리 접근 경로를 건드리는 3b 가 가장 크다.
  - 3a: 시스템 레지스터의 상태와 실모드에서 도는 명령. CR0.PE 를 켜는 쓰기는
    `Unsupported` 로 멈춘다.
  - 3b: CR0.PE 진입, GDT·LDT 에서 읽는 세그먼트 적재, 32 비트 기본 크기, LLDT·LTR·
    LAR·LSL·VERR·VERW·ARPL. test386 POST 0x08 을 넘는지는 여기서 확인한다.
  - 3c: 오류 코드가 실린 `Fault` 와 IDT 게이트 전달. PR 둘로 나눈다. 3c-1 은 `Fault` 를
    오류 코드가 구성자에 박힌 합타입으로 바꾼다(동작은 같다). 3c-2 가 IDT 전달을 넣는다.
- 구현한 값(3a, 2026-11-21, main `0821ce03` 위):
  - 386 상태에 CR0·CR2·CR3, GDTR·IDTR(기준 32 비트, 한계 16 비트), DR0-DR7, TR6·TR7 을
    더했다. 다른 모델은 리셋 값 그대로다. 286 의 SGDT·SIDT 도 같은 칸을 읽는다.
  - 명령은 `lib/cpu86_system.ml` 에 있다. LGDT·LIDT 는 66h 가 없으면 기준 24 비트를,
    있으면 32 비트를 싣는다. SGDT·SIDT 는 기준의 위 8비트를 쓴다. SMSW 는 32 비트
    레지스터에 CR0 전체를 쓰고(test386 의 `smsw eax` 와 `mov ebx, cr0` 가 같아야
    한다), 메모리와 16 비트 레지스터에는 아래 워드만 쓴다. LMSW 는 PE·MP·EM·TS 만 바꾼다.
    CLTS 는 TS 를 지운다. MOV CRn·DRn·TRn 은 modrm 의 mod 를 무시한다(변위를 가져오지
    않는다). CR1·CR4 이상과 TR6·TR7 이 아닌 시험 레지스터는 INT 6 이다.
  - 아직 없는 기능을 켜는 쓰기는 아무것도 쓰기 전에 `Unsupported` 로 멈추고 IP 는 그
    명령 앞이다: CR0.PE(3b), CR0.PG(5 단계), 중단점을 켜는 DR7(L0-G3 비트), 예약인 DR4·
    DR5. `load_state` 는 CR0.PE 가 켜진 상태를 거절한다.
  - CR0.EM·TS 는 ESC 를, MP 와 TS 는 WAIT 를 INT 7 로 보낸다. CR0 에는 정의된 비트
    (PE·MP·EM·TS·ET·PG)만 남는다.
  - 확인 필요(원문·실칩을 못 봤다): CR0 예약 비트(5-30)가 읽히는 값, 386 의 SGDT·SIDT 가
    16 비트 피연산자에서 위 8비트에 쓰는 값, CR1·DR4·DR5·TR3-5 의 실칩 동작, 명령마다
    사이클 수(PRM 표에서 기억한 값).
  - 스냅숏 형식 43 → 44. 386 의 CPU 구역이 GS 뒤에 CR0·CR2·CR3, GDTR·IDTR, DR0-DR7,
    TR6·TR7 을 싣는다(정수 17 개와 배열 길이 둘, 8 바이트씩 19 칸, 152 바이트).
    8086·186·286 의 본문은 바이트까지 같다(같은 기계를 43·44 빌드로 저장해 본문 MD5 대조: 8086
    `6fa7e344…`, 186 `71bfbda5…`, 286 `c384bb41…`). 386 은 17,050,114 바이트에서
    17,050,266 바이트이고 본문 MD5 가 `5e1e3dfb…` 에서 `405ef0e6…` 로 바뀐다. 저장
    시간은 흔들림 안이다(5 회 평균, 저장·복원: 286 2.5·2.4ms, 386 28.7·26.2ms, 기준선
    386 31.4·35.2ms). RFC 0002 는 아직 제안이라 번호를 손으로 올렸다.
  - test386 은 POST 열 `00 01 02 03 04 05 06 08` 로 같고, 멈추는 자리가 793,772 스텝 뒤
    f000:2662 의 32 비트 LIDT 에서 793,777 스텝 뒤 f000:267c 의 `mov cr0, eax`(PE·PG 를
    켠다)로 옮겨 갔다. POST 8 이 쓰는 LIDT·LGDT·MOV CR3·MOV EAX,CR0 를 지났다.
  - 결과는 main 과 같다. SST 386 941 파일 1,758,700 건(pass 1,631,449, fail 4, undef
    127,247), SST 286 326 파일 1,478,000 건(pass 1,438,263, fail 2, undef 39,735), SST 8086
    323 파일 646,000 건(pass 599,993, fail 4,827, undef 41,180). 삼국지3 저장 파일 MD5
    `a70dff95…`, ZZT, 게임 스모크 18 개(화면 모드·색 수·unserved 수), 삼국지3 3천만 스텝
    화면(286·386)의 PPM 과 출력이 같다.
  - 호스트 명령 수(1 단계와 같은 방법: 삼국지3 `KOEI.COM` 3천만 스텝, 번갈아 세 번, dev
    빌드 `dosboot`). 한 빌드 안의 흔들림은 0.06% 안이다.

    | 모델 | `0821ce03` | 3a |
    |---|---|---|
    | 286 | 35.352G | 35.351G(-0.002%) |
    | 386 | 38.674G | 38.672G(-0.004%) |

  - 변이 35 개를 따로 돌렸다. 34 개는 시험이 잡았다. 잡지 못한 하나는 형식 번호를 올리지
    않는 변이다(시험이 번호를 `Dos_snapshot.format_version` 과 비교한다).
  - DOS/4GW 의 A20(확인, 2026-11-21, 386 기계, 3a 코어): 게임 열세 개를 `dosboot --cpu 386`
    으로 돌려 A20 접근을 기록했다(기록용 출력은 저장소 밖 임시 작업 폴더에서만 넣었다).
    둘(SkyNET, MDK 데모. 같은 `DOS4GW.EXE`, MD5 `ee403358…`, "DOS/4G Copyright (C)
    Rational Systems, Inc. 1987 - 1993")이 1248:032a 의 CR0.PE 쓰기까지 가고, 그 앞에서 8042 명령 없이 포트 92h 만 쓴다: 읽기, 02h 쓰기, 읽기, 00h 쓰기, 읽기,
    읽기, 02h 쓰기. 한 비트가 붙는지 확인하고 연다. 포트 92h 가 FFh 를 읽고 쓰기를
    받지 않는 변형 기계(저장소에 넣지 않았다)에서는 92h 를 한 번 읽고 8042 로 D1h 와
    FFh 를 보낸다. 이 빌드는 포트 92h 가 있으면 그것으로, 없으면 8042 로 연다. 실기에서
    포트 92h 가 무엇을 읽는지, 다른 DOS/4GW 빌드가 어떤지는 확인 필요다. 나머지 열한
    게임은 A20 까지 가지 못했다: 여덟은 스텁이 `C:\dos4gw.exe` 를 EXEC 하지 못한다는 글을
    찍고 종료 코드 1 로 끝나고, 둘(TerraFire, AstroFire)은 INT 21h AH=52h AL=0Ah 에서
    `Unsupported` 로 멈추고, 하나(RAVEN.EXE)는 DOS 용이 아닌 실행 파일이다. 앞의 두 원인은
    조사하지 않았다: 7 단계 전에 볼 것. 확인 필요: 여덟 스텁의 EXEC 실패는 CPU 가 아니라
    호스트 쪽 EXEC(`C:\dos4gw.exe` 경로)의 문제로 보인다. TerraFire·AstroFire 의 AH=52h 는
    RFC 0004 2 단계 항목이다.
  - 하지 않은 것: LLDT·SLDT·LTR·STR·LAR·LSL·VERR·VERW·ARPL(보호 모드의 명령, 3b), 보호 모드
    진입, 오류 코드, IDT 게이트(3c), 디버그 중단점, 페이징.

- 구현한 값(3b, 2026-11-21, main `9ba0ab47` 위):
  - 기술자를 읽고 적재하는 코드는 `lib/cpu86_segment.ml`, LLDT·SLDT·LTR·STR·VERR·VERW·LAR·
    LSL·ARPL 은 `lib/cpu86_protected.ml` 에 있다. 세그먼트 레지스터를 싣는 모든 명령(MOV·POP·
    LDS 류·far JMP/CALL/RET·IRET)이 `Cpu86_segment.set_seg` 를 지난다.
  - CR0.PE 가 켜지면 적재가 GDT·LDT 에서 base(32 비트), limit(G 비트면 4KB 단위), D·B 비트를
    읽어 레지스터의 캐시에 쓴다. 쓰기는 모든 검사를 지난 마지막이라 멈춘 적재는 캐시를 반쯤
    쓰지 않는다. 널 셀렉터(인덱스 0, RPL 은 무시)는 DS·ES·FS·GS 에 들어가면 limit -1 이 되어
    어떤 접근도 범위 밖이고, CS·SS 에는 들어가지 못한다. CR0.PE 를 끄면 실모드로 돌아간다.
    캐시의 limit·D·B 는 그대로고 이후 적재는 base 만 쓴다(unreal 모드). LMSW 는 PE 를 켜기만 한다.
  - CS.D 가 기본 피연산자·주소 크기를 정하고 66h·67h 가 뒤집는다. SS.B 가 스택 포인터를
    SP 와 ESP 로 가른다(POP SS 는 적재 전 폭으로 SP 를 올린다. test386 POST 9). 명령 fetch 는
    CS 의 limit 에서 멈추고, far 점프·호출·복귀의 목표 오프셋은 새 CS 의 limit 으로 따진다.
    16 비트 주소의 둘째 접근(far 포인터의 셀렉터, BOUND 의 상한)은 64KB 에서 감기고 32 비트
    주소는 감기지 않는다(SingleStepTests/80386 `0F B4 07` BX=FFFEh).
  - 특권은 링 0 하나만 안다. CS·SS 는 RPL 과 DPL 이 0 이어야 하고 데이터 세그먼트는 RPL 이 DPL
    이하여야 한다. 결함이어야 하는 적재(표 밖 셀렉터, 없는 기술자, 시스템 기술자, 쓸 수 없는
    SS, 실행 전용 데이터 레지스터, 확장 하향 세그먼트, 링을 넘는 적재)와 보호 모드의 INT·결함·
    단일 단계 트랩은 `Unsupported` 로 멈춘다. 메시지가 무엇이 틀렸는지 적는다. 결함을 IDT 로
    보내는 것은 3c, 검사와 링 전환은 4 단계다.
  - 새 명령은 실모드에서 INT 6 이다. LTR 은 GDT 의 TSS 기술자를 사용 중(busy)으로 쓴다.
    VERR·VERW·LAR·LSL 은 결함 없이 ZF 로 답하고, 닿지 않거나 형식이 틀리면 목적지를 그대로
    둔다.
  - CR0.PG(「정할 것」 4): 켜는 쓰기와 PG 가 켜진 때의 CR3 쓰기는 CR3 의 표를 읽어 첫 1MB(PDE
    하나, PTE 256 개)가 항등 매핑인지 본다. 아니면 무엇이 달랐는지 적고 `Unsupported` 로
    멈춘다. 맞으면 선형 주소를 그대로 물리 주소로 쓴다. 이후 게스트가 표 메모리를 고쳐도
    반영하지 않는다. 코드에 `WORKAROUND` 주석이 있고 5 단계가 이것을 대신한다. 플래그
    비트(R/W, U/S)와 PDE 0 밖은 보지 않는다.
  - PG 가 켜져 있는 동안 선형 주소가 FFFFFh 를 넘는 접근(데이터, 명령 fetch, 기술자 표 읽기)은
    `Unsupported` 로 멈춘다. 기계의 버스가 위쪽 주소 비트를 버려서(16MB 보드는 24 비트)
    E0000000h 같은 별칭이 낮은 주소와 우연히 같아지는 거짓 통과를 막는다. CPU 의 읽기·쓰기
    클로저를 PG 가 켜질 때만 감싸서(`Cpu86_state.set_paging_guard`) PG 가 꺼진 기계의 메모리
    접근 경로는 그대로다. 호스트 서비스(XMS·EMS 복사)와 표 읽기는 물리 주소라 감싸지 않은
    버스를 쓴다. 감싼 상태는 스냅숏에 싣지 않고 CR0 에서 다시 정한다.
  - 스냅숏 형식 44 → 45. 386 의 CPU 구역이 세그먼트 base 6·limit 6, CS.D·SS.B, LDTR·TR 의
    셀렉터·base·limit 을 싣고 EIP 범위가 33 비트로 넓어진다(+162 바이트). 8086·186·286 의
    본문은 바이트까지 같다(같은 기계를 44·45 빌드로 저장해 본문 MD5 대조: 8086 `6fa7e344…`,
    186 `71bfbda5…`, 286 `c384bb41…`). 386 은 17,050,266 바이트에서 17,050,428 바이트이고
    본문 MD5 가 `405ef0e6…` 에서 `6669a544…` 로 바뀐다. PG 가 켜지고 PE 가 꺼진 CR0 를 싣는
    스냅숏은 읽을 때 거절한다.
  - test386 은 POST 열이 `00 01 02 03 04 05 06 08 09 20` 이다. 3a 는 793,777 스텝 뒤 f000:267c 의
    `mov cr0, eax` 에서 멈췄고, 3b 는 795,480 스텝 뒤 00d0:2b79 에서 멈춘다. POST 20 의 링 3 으로
    가는 CS 적재(셀렉터 00ab)다. POST 8(표를 만들고 PE·PG 로 들어간다)과 POST 9(보호 모드의
    스택)를 지난다.
  - 결과는 main 과 같다. SST 386 941 파일 1,758,700 건(pass 1,631,449, fail 4, undef 127,247), SST 286 326 파일 1,478,000 건(pass 1,438,263, fail 2, undef 39,735), SST 8086 323 파일 646,000 건(pass 599,993, fail 4,827, undef 41,180). 삼국지3 저장 파일 MD5 `a70dff95…`, ZZT, 게임 스모크 18 개(화면
    모드·색 수·unserved 수)가 같다. 삼국지3 3천만 스텝 화면(286·386)의 PPM 과 출력이 같다.
  - 호스트 명령 수(3a 와 같은 방법: 삼국지3 `KOEI.COM` 3천만 스텝, 번갈아 세 번, dev 빌드
    `dosboot`). 한 빌드 안의 흔들림은 286 0.13% 안, 386 0.39% 안이다.

    | 모델 | `9ba0ab47` | 3b |
    |---|---|---|
    | 286 | 35.367G | 35.591G(+0.63%) |
    | 386 | 38.737G | 36.241G(-6.44%) |

    처음 짠 3b 는 286 +2.9%, 386 +3.6% 였다. dune 개발 빌드는 모듈 사이 호출을 인라인하지
    않는다(`Implementations imported` 의 CRC 가 비어 있다). 스택 헬퍼 셋을 거치던 push·pop 과
    `set_seg` 의 둘째 호출이 286 에서 늘었고, 386 은 `step` 의 fetch 한계 계산이 정수에
    다형 `min` 을 불러 3b 전부터 호출 하나가 더 있었다. push·pop 의 16 비트 스택 길과 `set_seg`
    의 실모드 쓰기를 호출 없이 쓰고 `min` 을 정수 비교로 바꿨다. 386 이 줄어든 것은 3b 가 아니라
    이 `min` 때문이다.
  - 변이 70 개를 따로 돌렸다. 67 개는 시험이 잡았다. 잡지 못한 셋은 형식 번호를 올리지 않는
    변이(시험이 번호를 `Dos_snapshot.format_version` 과 비교한다), LSL 의 16 비트
    `land 0xffff` 를 지우는 변이(`set_reg16` 가 같은 마스크를 해서 동작이 같다), fetch 의 EIP
    마스크를 32 비트로 줄이는 변이(4GB 세그먼트 끝을 넘는 fetch 를 시험하지 않았다)다. 처음
    돌렸을 때 살아남은 일곱 중 넷은 PUSHAD·POPAD·ENTER·RET imm16 과 스냅숏 limit 시험을 더해
    잡았다.
  - 확인 필요(원문·실칩을 못 봤다): LAR 가 32 비트 피연산자에 얹는 플래그 니블의 마스크, 없는
    기술자에 대한 VERR·VERW, 32 비트 SLDT·STR 의 위 16 비트, 4GB 코드 세그먼트의 끝을 넘는
    fetch(EIP 를 33 비트로 두어 감기지 않고 멈춘다), 명령마다 사이클 수(PRM 표에서 기억한 값).
  - 하지 않은 것: 오류 코드와 IDT 게이트(3c), 링 전환과 보호 검사의 결함 전달(4 단계), 작업
    전환(6 단계), 페이징(5 단계), 보호 모드에서 호스트가 IRQ 를 IVT 로 넣는 길(7 단계).

### 4 단계: 보호 검사와 특권 전환

- 세그먼트 적재의 형식·DPL·존재 검사, #GP·#SS·#NP·#TS. ring 0↔3 전환의 TSS
  스택 바꾸기, 바깥 링으로 가는 IRET·RETF.
- 확인: test386 POST 0x20·0x0B-0x10·0x12-0x1C. POST 0x09 는 3b 에서 이미 지난다.

### 5 단계: 페이징

- 3b 의 CR0.PG 처리(첫 1MB 가 항등 매핑인지 보고 선형을 물리로 쓴다)는 이 단계를 대신하지 않는다.
- CR3 의 두 단계 표, #PF 와 CR2, PTE 의 accessed·dirty.
- 확인: test386 POST 0x11.

### 6 단계: V86 과 작업 전환

- VCPI 경로와 test386 128KB 판(0x21·0x22)에 필요하다. raw 경로의 DOS/4GW 에
  필요한지는 3-5 단계 뒤 게임으로 확인한다.
- 확인: test386 128KB 판 POST 0xFF, 0xEE 출력이 기준 파일과 같다.

### 7 단계: DOS/4GW 프로그램

- raw 경로로 DOS/4GW 프로그램 하나가 `main` 까지 간다.
- 확인할 게임은 「정할 것」 3 에 둔다.

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
- raw 경로의 페이징이 확인되지 않았다. 5 단계의 범위가 바뀔 수 있다. DOS/4GW 의
  A20 은 한 빌드를 확인했다(3 단계 「구현한 값」).
- 확인한 두 게임의 최소 사양이 486 이다. 486 에서 새로 생긴 명령(BSWAP·XADD·
  CMPXCHG 등)을 게임이나 DOS/4GW 가 쓰는지는 확인 필요다. 쓰면 486 명령이
  범위 안으로 들어온다.
- 386 실모드(#154-#165)는 main 에 있다. 기계에서 386 을 고르는 #166 은 아직
  열려 있다. 0-6 단계는 맨 CPU 로 시험하니 #166 없이 갈 수 있고, 7 단계는 #166 이
  필요하다.

## 정할 것

1. CI 에서 test386 을 받아 NASM 으로 빌드해 돌릴지. CI 를 바꾸는 일이다.
   NASM 3 에서는 소스 110 줄을 고쳐야 하니 NASM 2.16 을 고정할지도 같이 정한다.
2. (정함) XMS 저장소를 386 기계에서는 CPU 메모리로 합치고, 286·8086 기계는 1MB 를
   지켜 따로 둔다.
3. 7 단계를 무엇으로 확인할지. 로컬 조사 폴더(저장소 밖)에 DOS/4GW 를 싣는
   게임이 있다(「왜」 참고). 게임 파일은 저장소에 두지 않고, 폴더가 있을 때만
   도는 시험으로 한다(SST 와 같은 방식). 어느 게임으로 할지는 7 단계 PR 에서
   정한다.
4. (정함) 3b 에서 CR0.PG 를 어떻게 받을지. test386 의 POST 8 은 PE 와 PG 를 한 번에 켜고 첫
   1MB 를 선형=물리로 매핑해 두었다. 페이징은 5 단계라, 3b 에서 PG 를 받으면 선형
   주소를 그대로 물리 주소로 쓰고 한계를 적어 두거나, PG 를 `Unsupported` 로 두고
   test386 에서 POST 8 을 못 넘는 채로 5 단계까지 간다. 앞쪽을 골랐다: 첫 1MB 가 항등
   매핑이면 받고 아니면 멈춘다(3 단계 「구현한 값(3b)」).

## 확인 방법 요약

| 단계 | 확인 |
|---|---|
| 0 | test386 러너가 같은 POST 열을 낸다. 실모드 POST 0x00-0x06(미리 잰 값: 통과, 0x08 의 `66 0F 01` 에서 멈춤) |
| 1 | 실모드 스위트·삼국지3 같음, 명령 수 ±3% |
| 2 | XMS 시험 같음, A20 감기 |
| 3 | 3a: 실모드 스위트·삼국지3 같음, 명령 수 ±3%, test386 이 `mov cr0` 까지. 3b: test386 POST 0x08·0x09 통과, 명령 수 ±3% |
| 4 | test386 POST 0x20·0x0B-0x10·0x12-0x1C |
| 5 | test386 POST 0x11 |
| 6 | test386 128KB 판 POST 0xFF, 0xEE 기준 파일과 같음 |
| 7 | DOS/4GW 프로그램이 `main` 까지 |
