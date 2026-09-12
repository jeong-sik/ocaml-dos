# ocaml-dos Handoff — 2026-09-12

> 다음 세션용 인수인계. ZZT 게임 런 + 플레이 조작 달성 시점에서 끊는다.
> 상세 관문 기록: `memory/project-ocaml-dos-lane-20260912.md` (Second Brain)

## 한 줄 상태

ZZT 3.2가 TOWN.ZZT를 로드해 보드+사이드바를 그리고 게임 루프에 진입,
방향키 한 번에 언파즈+이동(right→1셀, down→1셀)까지 실측.
PR #8 (`zzt-ring`) 전 커밋 CI 녹색(macos/ubuntu), Draft.

## 기존에 한 일

| 단계 | 내용 | 산출 |
|------|------|------|
| M0 | 8086 코어 기본 — regs/ALU/플래그/seg:off/스택/jcc | PR #1 |
| M1 | 명령집합 완성 — call/ret, 그룹 80-83·FE·FF, string+rep, mul/div, shift, xchg, flag ops | PR #2 (96b27e9) |
| M2a | Dos_machine — 1MB RAM, 텍스트 VRAM 0xB8000, CGA 16색 640x400 렌더, INT 10h/16h/21h 표면, COM 로더 | PR #3 |
| M2b | MZ EXE 로더(재배치), INT 21h 파일 표면(3D/3F/3E/42), VGA Mode 13h + DAC | PR #4 |
| 검증 | SingleStepTests 실칩 스위트 **286,000 케이스 전부 통과** — 잡은 버그: FLAGS 고정비트 0xF002, 세그먼트 override SS↔DS 반전, adc/sbb OF, PUSH SP | PR #5, #6 |
| ZZT 리허설 | LZEXE 0.91 언패커(python), 186 명령세트(push imm/popa/shift imm/enter/leave), BCD 4종, load_exe 실기 배치(memtop 공식, DS/ES=PSP) | PR #7 |
| **ZZT 런** | 관문 14+ 돌파(아래 표) | PR #8 (7d74e20) |
| **플레이 조작** | `key@step` 스케줄로 방향키 이동 실측 | PR #8 (eb626be) |

### ZZT 런 관문 (전부 실측으로 지정)

1. ROM INT 8 루틴(F000:0000) — 0x46C tick 범프 → INT 1Ch → EOI → IRET
2. 전 IVT 벡터 IRET 스텁 — TP의 옛 벡터 체인(AH=35h)이 0:0 으로 떨어지지 않게
3. 호스트 서빙 벡터 {10,16,20,21}h 도 IVT 실주소 — TP Intr 썽크가 CD 없이 IVT 직독 retf
4. BDA 초기화(0x413/0x449/0x44A/0x463=0x3D4 등) — CRTC 상태포트 계산
5. 포트 0x3DA 읽을 때마다 bit0 토글 — CRT 상승 에지 대기
6. INT 21h AH=44 IOCTL + 표준핸들 0-4 — 없으면 TP "Runtime error 006"
7. BIOS 키 링(0x40:0x1E) 단일 SSOT + 포인터 클램프 [0x1E,0x3C]
8. Cpu86.wake — HLT idle을 IRQ0로 해제
9. **string_op stride** — movs/cmps=SI·DI, stos/scas=DI만, lods=SI만 (lodsb+stosw의 DI+3 오염)
10. load_com PSP 세그 0x1000 — 세그 0이면 PSP:0x80이 IVT[20h] 소거
11. load_exe 배치 — img_paras 헤더 제외, 0x9FF0 공식(VRAM 미침범)
12. INT 10 02/03/06/07/08/11·30 — 커서·스크롤·폰트 포인터
13. INT 21 02/06/3C/40 — TP 에러메시지·LPT1 create·핸들 출력
14. keybot 굶주림 주입 + `key@STEP` — 사전 메뉴가 키를 먹는 문제 해결

### 이번 사이클의 판정 교훈

- 폰트 오독("Pausing"→"Izuzing")은 비전 모델 해상도 한계였다 — ASCII 아트 렌더로 글리프 무죄 증명, 4배 확대로 정독 확인. **관측 도구의 해상도가 대상보다 낮으면 그 부정 보고는 증거가 아니다.**
- 굶주림 keybot의 결함: "게임이 키를 원한다" ≠ "게임이 *이* 키를 원한다". 게임 상태 전이 후 주입은 `key@STEP`으로만 보장된다.

## 다음 할 일 (우선순위 순)

1. **PR #8 Ready → 머지** — 미검증 항목 없음(로컬 테스트 6종 + CI 2플랫폼 + 실측). 머지 후 worktree 정리(`git worktree remove .worktrees/zzt-ring`).
2. **README 마일스톤 갱신** — ZZT 런 달성, `--keys`의 `key@step` 문법 반영.
3. **게임플레이 심화** — ZZT 메뉴 키(T=transport, S=save, I=info, ESC) 검증. keeper 멀티플레이 입력 맵의 기초. ZZT 키 디코드: TP ReadKey 2회 → `Chr(scan|$80)` = Keys 유닛 상수.
4. **M3: 삼국지 III** — EXE 확보 필요(사용자가 구함). 예상 신규 계약: PIT/IRQ 타이머 정밀화, PC 스피커/AdLib 사운드, INT 33h 마우스, 그래픽 모드(0x0D/0x10/0x12). Unsupported 예외 목록이 다음 계약을 알려준다.
5. **디버그 계층 정리 여부 결정** — WRLOG/VRAMLOG/DOSDBG/MEM_DUMP는 커밋에 포함(하네스 진단용 명시). 남길지 축소할지 머지 전 결정.

## 미래 Lane 계획 (3층)

```
③ Lane addon (관측 레이어)      — 기존 Lane 프레임, docs/design/lane-addon-v0.md
        ↑ 관측
② masc 서버 내장 (RFC-0439 패턴) — 코어를 서버가 링크, masc_dos_* 도구 노출
   예: masc_dos_boot / step / push_key / screen_text / frame_rgb
        ↑ 링크
① ocaml-dos 코어 (현재 층)       — 게임 런·조작 증명 완료
```

- **② 서버 내장이 다음 큰 덩어리.** ocaml-msx의 Koei 카트 선례 참조: keeper가
  `press`로 커서·return·숫자 입력 → 게임 멀티플레이 가능했음(`--tap-key`, msx PR #14).
  "도구 한계를 대상 한계로 오독" 금지.
- **③ addon은 관측만 가능** — 새 기계를 못 만든다(조사 결론, lane-addon-v0.md).
  기계 창조는 ②에서만.
- 삼국지 III가 MSX에 없어 DOS 경로로 갔다 — 동기 원문.

## 재현 명령 (ZZT)

```bash
cd ~/me/workspace/yousleepwhen/ocaml-dos/.worktrees/zzt-ring
dune build --root . @runtest --force        # 6종 + vtest 2000/2000
./_build/default/bin/dosboot.exe --exe /tmp/zztc/ZZT.EXE \
  --mount ZZT.DAT=/tmp/zzt/zzt/ZZT.DAT --mount ZZT.CFG=/tmp/zzt/zzt/ZZT.CFG \
  --mount TOWN.ZZT=/tmp/zzt/zzt/TOWN.ZZT --steps 60000000 \
  --keys 256b,2e63,1c0d,1970,1c0d,3920,1c0d,1970,1c0d,1c0d,4d00@5000000
# 판정: "Pausing..." 소실 + 플레이어(스마일리, VRAM 코드 2)가 (26,18)→(27,18)
```

- 게임 파일: ZZT는 Epic 프리웨어(archive.org `msdos_ZZT_1991`). `/tmp/zztc/ZZT.EXE`는
  LZEXE 언패킹본(96,080B). **게임 이미지는 repo 커밋 금지** — 원칙 그대로.
- 관측 도구: DOSDBG=1(INT 트레이스), MEM_DUMP=addr,len, WRLOG=a,l, VRAMLOG, --trace N

## 회수 지점

- repo: `github.com/jeong-sik/ocaml-dos`, worktree `.worktrees/zzt-ring`, PR #8 Draft
- 메모리: `memory/project-ocaml-dos-lane-20260912.md` (틱별 상세)
- 빌드 주의: 워크트리 안에서 `dune build`가 외부 루트로 올라갈 때가 있다 — `--root .` 명시
- /tmp 파일(zztc, zzt, unlz91.py, sst8086)은 재부팅 시 소실 — ZZT는 archive.org에서 재수급
