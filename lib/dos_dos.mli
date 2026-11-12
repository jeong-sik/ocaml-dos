(** DOS 표면 — INT 21h 와 INT 20h.

    파일은 하네스가 마운트한 것만 보인다. 쓰기는 메모리 사본에 쌓였다가
    닫을 때 마운트 표로 돌아간다 — 같은 세션 안에서 저장하고 다시 여는
    것이 그래서 된다. 호스트 디스크의 원본은 바뀌지 않는다. *)

val conv_mem_top : int
(** 할당 가능한 마지막 세그먼트 — 비디오 메모리 바로 앞. *)

val service : Dos_state.t -> return_to:Dos_state.service_return -> Dos_state.service_result
(** Execute INT 21h. An EXEC preserves its owned return frame until the
    child returns; terminating or starting a process transfers control
    instead of completing the current interrupt frame. *)

val resume_input : Dos_state.t -> Dos_state.dos_input_request -> Dos_state.service_result
(** Resume the captured request without rereading the function selector or
    repeating its initial flush. Partial line contents and prior echo are
    retained in the request. *)

val terminate : Dos_state.t -> unit
(** INT 20h — RET 로 돌아온 프로그램의 종료. *)

val init_memory : Dos_state.t -> psp_seg:int -> unit
(** 적재 직후의 메모리 소유 상태. 실기처럼 프로그램이 남은 전부를
    가진다 — AH=4Ah 로 줄여야 AH=48h 이 성공한다. *)

val load_com : ?name:string -> Dos_state.t -> string -> unit
(** COM 이미지를 루트 프로그램으로 PSP 세그 0x1000 에 싣는다. EXEC 자식은
    AH=4Bh 가 싣는다.

    [name] 은 C: 루트 디렉터리의 파일 이름(예: [GAME.COM])이다. 주면 환경
    블록 꼬리에 워드 1 과 [C:\GAME.COM] 을 쓴다 — C 런타임의 argv[0] 이
    이것이다. 없으면 꼬리를 쓰지 않는다. 0x500 의 환경 자리(256 바이트)를
    넘는 이름은 [Invalid_argument]. *)

val load_exe : ?name:string -> Dos_state.t -> string -> unit
(** MZ EXE 이미지를 memtop 아래 루트 자리에 싣는다. 적재 모듈은 헤더의
    페이지 수가 정한다(MS-DOS 4.0 EXEC.ASM) — 파일 뒤에 붙은 데이터는
    싣지 않는다. 헤더가 파일 안의 모듈을 말하지 않으면 [Invalid_argument].
    [name] 은 [load_com] 과 같다. EXEC 자식은 AH=4Bh 가 같은 규칙으로
    싣고, 환경을 따로 받는다. *)

val int27 : Dos_state.t -> unit
(** INT 27h — 옛 방식 상주 종료. DX 오프셋까지 자기 블록을 남긴다. *)

val largest_free : Dos_state.t -> int
(** 지금 한 번에 줄 수 있는 가장 큰 덩어리(파라그래프). *)

val matches_pattern : pattern:string -> name:string -> bool
(** DOS 8.3 와일드카드. '*' 는 그 칸의 남은 자리를 '?' 로 채운다. *)
