(** DOS 기계 — 8086/186 코어 + 1MB RAM + 장치 포트 + BIOS/DOS 인터럽트
    표면 + COM/MZ-EXE 로더.

    이 모듈은 바깥에 보이는 얼굴이다. 상태는 [Dos_state], 포트는
    [Dos_ports], BIOS 는 [Dos_bios], DOS 는 [Dos_dos] 가 맡는다.

    결정론: 같은 이미지에 같은 키를 같은 순서로 넣으면 같은 화면이
    나온다. 시각도 난수도 호스트에서 오지 않는다 — 날짜·시각은
    [set_clock] 이 정한 기준시각에 CPU 사이클로 환산한 경과를 더해
    만들고, 타이머 틱도 사이클에서 나온다.

    파일 계약: 하네스가 마운트한 것만 보인다. 게스트가 쓴 내용은 파일을
    닫을 때 마운트 표로 돌아가므로 같은 세션 안에서 저장하고 다시 열 수
    있다. 호스트 디스크에는 나가지 않는다. *)

type t = Dos_state.t

val create : unit -> t

(** {1 적재} *)

val load_com : t -> string -> unit
(** COM 이미지를 PSP 세그먼트 0x1000 의 0x100 에 싣는다. CS=DS=ES=SS=PSP,
    IP=0x100, SP=0xFFFE, 스택 맨 위는 0 — RET 으로 돌아오면 PSP 의
    INT 20h 로 끝나는 실기 관례 그대로다. *)

val load_exe : t -> string -> unit
(** MZ EXE 를 싣는다. 재배치 워드에 로드 세그먼트를 더하고 CS:IP/SS:SP
    를 헤더값 + 로드 세그먼트로 잡는다. DS/ES 는 PSP 세그먼트다.
    MZ 서명이 아니면 [Invalid_argument]. *)

val mount_file : t -> string -> string -> unit
(** 게스트가 INT 21h 로 열 수 있는 파일. 이름은 대소문자를 가리지 않는다. *)

val read_mounted : t -> string -> string option
(** 마운트 표의 현재 내용 — 게스트가 저장한 결과를 하네스가 꺼낸다. *)

val mounted_names : t -> string list

(** {1 실행} *)

val step : t -> int
(** 한 명령. 타이머 틱이 찼고 인터럽트가 열려 있으면 그 전에 IRQ0 을
    넣는다. 종료 후에는 무해하게 2 를 돌려준다. *)

val run : t -> max_steps:int -> unit
(** 종료·HLT·[max_steps] 중 먼저 오는 것까지. HLT 에서 멈추므로
    타이머로 깨어나는 대기 루프를 계속 돌리려면 [run_until] 을 쓴다. *)

val run_until : t -> max_steps:int -> stop:(t -> bool) -> int
(** 종료·[stop]·[max_steps] 까지 돌리고 실행한 명령 수를 돌려준다.
    HLT 는 멈춤 조건이 아니다 — 타이머 인터럽트가 깨운다. *)

type key_plan = { word : int; not_before : int }
(** [word] 는 (스캔 코드 lsl 8) lor ASCII. [not_before] 이전 스텝에는
    넣지 않는다. *)

val run_with_keys :
  ?on_step:(t -> int -> unit) ->
  ?on_key:(int -> int -> unit) ->
  t -> max_steps:int -> keys:key_plan list -> int
(** 키를 미리 다 밀어 넣지 않고, 게스트가 입력을 기다리다 굶는 순간
    하나씩 넣는다. 미리 넣으면 앞선 메뉴의 "아무 키나" 루프가 전부 먹어
    치운다. 어떤 상태가 된 다음에 넣어야 하는 키는 [not_before] 로
    묶는다. 실행한 명령 수를 돌려준다.

    [on_step] 은 명령마다, [on_key] 는 키를 넣을 때마다 불린다 — 추적
    출력을 붙이는 자리다. *)

val exited : t -> bool
val exit_code : t -> int

val halted : t -> bool
(** CPU 가 HLT 로 인터럽트를 기다린다. 종료와 다르다. *)

(** {1 입력} *)

val push_key : t -> int -> unit
(** BIOS 키 링에 워드 하나. INT 16h 와 INT 21h 입력이 같은 링을 본다. *)

val push_ascii : t -> char -> unit
(** ASCII 한 글자 — US 자판 스캔 코드를 붙인다. 방향키처럼 글자가 아닌
    키는 [push_key] 로 워드를 직접 넣는다. *)

val type_string : t -> string -> unit

val kbd_waiting : t -> bool
(** 직전 입력 요청이 빈 링으로 돌아갔다 — 실기라면 지금 블록 중이다.
    하네스가 이걸 보고 키를 넣는다. *)

val attach_mouse : t -> unit
(** INT 33h 에 마우스가 있다고 답하게 한다. 기본은 미장착 — 없는 장치를
    있다고 하면 게임이 오지 않을 커서를 기다린다. *)

val set_mouse : t -> x:int -> y:int -> buttons:int -> unit

(** {1 화면} *)

val frame_dims : t -> int * int
(** 텍스트 640x400, VGA 13h 320x200. *)

val screen_text : t -> string
(** 80x25 를 개행 포함 ASCII 로 — 사람이 눈으로 훑기 좋다. *)

val screen_text_utf8 : t -> string
(** 같은 화면을 코드 페이지 437 그대로 UTF-8 로 — 테두리와 기호가
    살아 있어 기계 판정에 쓴다. *)

val frame_rgb : t -> string
val frame_ppm : t -> string
val video_mode : t -> int
(** 지금 세워진 BIOS 비디오 모드 번호. *)

val pixel : t -> x:int -> y:int -> int
(** 그래픽 모드의 점 하나 — CGA 는 0-3, EGA/VGA 16색은 0-15, 13h 는
    0-255. 텍스트 모드에서는 늘 0 이다. *)

(** {1 관측} *)

val cpu_of : t -> Cpu86.t
val psp_seg_of : t -> int

val mem_read : t -> int -> int
(** 물리 주소 한 바이트. *)

val tick_count : t -> int
(** BIOS 타이머 틱(0x40:0x6C). 18.2Hz 가 기본이고 게스트가 PIT 분주비를
    바꾸면 그만큼 빨라진다. *)

val speaker_on : t -> bool
(** 스피커의 게이트와 데이터가 둘 다 열렸다. 소리는 내지 않는다. *)

val free_paras : t -> int
(** INT 21h AH=48h 이 지금 줄 수 있는 가장 큰 덩어리. *)

val set_clock :
  t -> year:int -> month:int -> day:int -> hour:int -> minute:int ->
  second:int -> unit
(** 게스트가 보는 기준시각. 기본은 1990-01-01 08:00:00. *)
