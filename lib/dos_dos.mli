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

val divide_overflow : Dos_state.t -> unit
(** INT 0 — DOS 의 기본 처리. 콘솔에 "Divide overflow" 를 쓰고 프로그램을
    종료 코드 0 으로 끝낸다(MS-DOS 2.0 CTRLC.ASM 의 DIVOV). *)

val init_memory : Dos_state.t -> psp_seg:int -> block_end:int -> unit
(** 적재 직후의 메모리 소유 상태. 프로그램이 [psp_seg] 부터 [block_end] 직전까지를
    가진다. COM 과 EXE 는 실기처럼 남은 전부를 받으니 AH=4Ah 로 줄여야 AH=48h 이
    성공한다. 헤더의 maxalloc 이 작은 EXE 는 그 위가 처음부터 비어 있다. *)

val load_com : ?name:string -> ?tail:string -> Dos_state.t -> string -> unit
(** COM 이미지를 루트 프로그램으로 PSP 세그 0x1000 에 싣는다. EXEC 자식은
    AH=4Bh 가 싣는다.

    [name] 은 C: 루트 디렉터리의 파일 이름(예: [GAME.COM])이다. 주면 환경
    블록 꼬리에 워드 1 과 [C:\GAME.COM] 을 쓴다 — C 런타임의 argv[0] 이
    이것이다. 없으면 꼬리를 쓰지 않는다. 0x500 의 환경 자리(256 바이트)를
    넘는 이름은 [Invalid_argument].

    [tail] 은 명령 이름 바로 뒤 글자들이다("GAME a" 면 [" a"]). PSP:80h 에
    길이, 81h 부터 글자와 0Dh 를 쓴다. 없으면 빈 꼬리다. 126 바이트를
    넘거나 CR 이 들어 있으면 아무것도 싣지 않고 [Invalid_argument]. *)

val load_exe : ?name:string -> ?tail:string -> Dos_state.t -> string -> unit
(** MZ EXE 이미지를 memtop 아래 루트 자리에 싣는다. 적재 모듈은 헤더의
    페이지 수가 정한다(MS-DOS 4.0 EXEC.ASM) — 파일 뒤에 붙은 데이터는
    싣지 않는다. 헤더가 파일 안의 모듈을 말하지 않으면 [Invalid_argument].
    [name]·[tail] 은 [load_com] 과 같다. EXEC 자식은 AH=4Bh 가 같은 규칙으로
    싣고, 환경을 따로 받는다. *)

val exe_payload_bytes : string -> (int, string) result
(** [load_exe] 가 이 이미지를 받는지, 싣기 전에 묻는다. 받으면 [Ok n] 이고
    [n] 은 메모리에 들어가는 바이트 수다 — 헤더의 페이지 수가 끝을 정하고
    파일이 먼저 끝나면 파일 끝까지, 거기서 헤더를 빼고 0 아래로는 내려가지
    않는다. 아니면 [Error 글]
    이고 글은 [load_exe] 가 [Invalid_argument] 로 내는 것과 같다. 아무것도
    바꾸지 않는다. *)

val int27 : Dos_state.t -> unit
(** INT 27h — 옛 방식 상주 종료. DX 오프셋까지 자기 블록을 남긴다. *)

val largest_free : Dos_state.t -> int
(** 지금 한 번에 줄 수 있는 가장 큰 덩어리(파라그래프). *)

val matches_pattern : pattern:string -> name:string -> bool
(** DOS 8.3 와일드카드. '*' 는 그 칸의 남은 자리를 '?' 로 채운다. *)

val set_date_from_clock : Dos_state.t -> unit
(** DOS 날짜(1980-01-01 부터 센 날 수)를 지금 RTC 날짜로 정한다. 부팅 때
    MS-DOS 가 하는 일이다(READCLOC.INC [read_real_date]). 자정 넘김은
    {!Dos_bios.set_ticks_from_clock} 을 먼저 불러 지운다. 날짜가
    1980-01-01..2099-12-31 밖이면 [Invalid_argument]. *)
