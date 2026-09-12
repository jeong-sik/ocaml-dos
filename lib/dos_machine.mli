(** DOS 머신 (M2a) — 8086 + 1MB RAM + 텍스트 비디오 + INT 표면 + COM 로더.

    Cpu86 을 감싸고 DOS 관점의 최소 기계를 제공한다: INT 10h 텔레타입,
    INT 21h 종료·출력, INT 16h 키보드(하네스가 큐에 넣는 논블로킹 입력),
    0xB8000 텍스트 VRAM(80x25, 글자+속성), COM 이미지 적재(PSP 포함).

    결정론: 같은 이미지 + 같은 키 입력 순서 = 같은 실행. 그래픽(VGA
    13h), MZ EXE, 파일 표면(INT 21h 핸들)은 M2b. *)

type t

val create : unit -> t

val load_com : t -> string -> unit
(** COM 이미지를 0x100 에 적재하고 PSP 를 심는다: 0x00 에 INT 20h(CD 20),
    0x80 에 커맨드라인 길이 0. CS=DS=ES=SS=0, IP=0x100, SP=0xFFFE,
    스택 top 에 0x0000 — 프로그램이 RET 으로 돌아오면 PSP 의 INT 20h
    로 종료되는 실기 관례를 그대로 둔다. *)

val step : t -> int
(** 한 명령. 종료 후(int 21h AH=4Ch)에도 무해하게 2 를 돌려준다. *)

val run : t -> max_steps:int -> unit
(** 종료·HALT·[max_steps] 까지 실행. Cpu86.Unsupported 는 그대로
    올려보낸다 — 하네스가 다음 구현 우선순위를 읽는다. *)

val exited : t -> bool
(** INT 21h AH=4Ch (또는 PSP 의 INT 20h) 를 만났다. *)

val exit_code : t -> int

val halted : t -> bool
(** CPU HLT — 인터럽트 대기 중. 종료와 다르다. *)

val screen_text : t -> string
(** 텍스트 VRAM 80x25 를 개행 포함 문자 그리드로 — 판정용. *)

val frame_rgb : t -> string
(** 텍스트 화면 640x400x3 RGB — 8x8 글리프 스케일업, CGA 16색 속성. *)

val push_key : t -> int -> unit
(** BIOS 스캔 코드 큐에 넣는다(하네스 입력). INT 16h 가 소비한다. *)

val mem_read : t -> int -> int
(** 물리 주소 1바이트 — 디버깅용. *)
