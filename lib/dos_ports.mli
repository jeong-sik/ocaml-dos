(** 게스트가 보는 하드웨어 포트 — PIT 타이머, PIC({!Dos_pic}), DMA({!Dos_dma}), 키보드
    컨트롤러({!Dos_kbc}), VGA DAC, CRTC, PC 스피커, CGA 상태 레지스터, CMOS 시계({!Dos_cmos}).

    이 포트들이 무엇을 돌려주는지가 게임의 진행 조건이 된다. 고정값을
    돌려주면 상태 변화를 기다리는 루프가 영원히 안 풀린다 — CGA 재주사
    비트와 스피커 리프레시 비트가 그렇다(ZZT 실측).

    시간에 의존하는 포트(PIT 카운터, 재주사 비트)는 벽시계가 아니라
    CPU 누적 사이클로 답한다. 같은 이미지 + 같은 입력 = 같은 실행이라는
    결정론 계약을 포트도 지킨다. *)

type t

(** {1 CPU 속도}

    속도는 PIT 한 틱(1/1193182초)에 CPU 사이클이 몇 개 도는가다. 게스트가
    시간을 재는 길 — IRQ0 간격, PIT 카운터, 리프레시 비트, 재주사 비트,
    OPL 타이머, 시각 — 은 이 값으로 사이클을 실제 시간에 맞춘다. 사이클은
    8086 명령표의 값이라서 속도는 "8086 으로 치면 몇 MHz" 다. *)

type speed

val pit_hz : int
(** PIT 입력 클록(1.193182MHz). 한 펄스가 PIT 한 틱이다. *)

val pc_speed : speed
(** IBM PC 의 4.77MHz(PIT 한 틱에 4 사이클). *)

val speed_of_mhz : float -> (speed, string) result
(** 가장 가까운 PIT 정수배로 맞춘다. 12 는 PIT 10 배인 11.93MHz 가 된다.
    1.19MHz(1 배)보다 느리거나 1193MHz(1000 배)보다 빠르면, 또는 NaN 이나
    무한대면 [Error]. *)

val speed_hz : speed -> int
(** 그 속도의 CPU 클록(Hz). *)

val cga_palette : (int * int * int) array
(** CGA 16색 — 속성 바이트의 하위 니블이 글자색, 상위 니블이 바탕색. *)

val create :
  speed:speed -> mpu401:bool -> mem:Bytes.t -> video:Dos_video.t -> sound_blaster:bool ->
  cmos:Dos_cmos.t -> t
(** [speed] 는 CPU 속도다. [mpu401] 이면 330h·331h 를 MPU-401 에 넘긴다(MIDI
    바이트는 버린다). 포트 70h·71h 는 [cmos] 로 넘긴다. [mem] 은 1MB 게스트 메모리 — CRTC 커서 레지스터를 BDA(0x450/0x451)
    와 같은 값으로 유지하는 데 쓴다. 커서의 진실 원천은 하나여야 한다.
    비디오 어댑터의 포트(시퀀서·그래픽 컨트롤러·속성·CRTC)는 [video] 로
    넘긴다. CRTC 커서 레지스터(0x0A/0x0B/0x0E/0x0F)를 쓰면 어댑터가 받은
    뒤 BDA 를 맞춘다. [sound_blaster] 이면 220h 에 Sound Blaster 2.0({!Dos_sb})을
    꽂는다 — DSP 포트를 받고, 228h·229h 를 388h·389h 의 OPL2 로 보낸다. *)

val bda_page_size : int
val bda_page_start : int
val bda_cursor_pos : int
val bda_active_page : int
val page_count : int
(** BIOS 데이터 영역에서 쪽 크기(0x44C), 보이는 쪽의 시작 오프셋(0x44E),
    쪽 여덟 장의 커서(0x450 + 2 × 쪽: 열, 행), 보이는 쪽 번호(0x462).
    게스트가 CRTC 커서 레지스터를 쓰면 이 모듈이 보이는 쪽의 커서 칸을
    맞춘다. BIOS 도 같은 자리를 쓴다. *)

val set_now : t -> int -> unit
(** 현재 CPU 누적 사이클. 기계가 매 스텝 알려준다. *)

val cpu_hz : t -> int
(** 이 기계의 CPU 클록(Hz). 사이클을 초로 바꿀 때 쓴다. *)

val port_in : t -> int -> int
val port_out : t -> int -> int -> unit

val irq0_due : t -> bool
(** 지난 물음 뒤로 PIT 채널 0 의 OUT 이 낮음에서 높음으로 바뀌었다 — IRQ0
    요청이다. 참이면 [set_now] 의 시각 뒤로 다음 상승을 잡는다. 그 사이 여러
    번 올랐어도 한 번이다(8259 IRR 은 한 비트다). 기계가 매 스텝 [set_now]
    뒤에 부른다.

    채널 0 은 BIOS POST 뒤의 상태(모드 3, 카운트 65536 = 18.2Hz)로 켜진다.
    게임이 음악·부드러운 이동을 위해 모드와 카운트를 바꾼다. 상승 간격과
    시점은 {!Dos_pit} 가 그 카운트를 실은 때부터 센다. *)

val pic : t -> Dos_pic.t
(** 포트 0x20·0x21 의 인터럽트 컨트롤러. 기계가 IRQ 를 올리고 고른다. *)

val dma : t -> Dos_dma.t
(** 포트 00h-0Fh 와 81h·82h·83h·87h 의 DMA 컨트롤러. 장치가 바이트를 당겨 간다. *)

val sound_blaster : t -> Dos_sb.t option
(** 꽂힌 Sound Blaster. 기계가 때가 되면 {!Dos_sb.advance} 를 부르고 IRQ 7 을
    올린다. 22Eh 읽기·리셋·새 전송으로 DSP 가 선을 내리면, CPU 가 아직 안
    받은 PIC 요청은 이 모듈이 거둔다. *)

val kbc : t -> Dos_kbc.t
(** 포트 0x60·0x64 의 키보드 컨트롤러. *)

val palette : t -> (int * int * int) array
(** 256색 DAC — 채널당 6비트. 포트 0x3C8/0x3C9 와 INT 10h AH=10h 이
    같은 배열을 쓴다. *)

val reset_dac : t -> unit
(** DAC 256 자리를 기본값으로, 쓰기·읽기 순번과 PEL 마스크(0x3C6)를
    처음 상태로. INT 10h AH=00h 가 모드를 세울 때 부른다 — 실기 BIOS 가
    그 순간 팔레트를 다시 싣는다. *)

val speaker_on : t -> bool
(** 포트 0x61 의 bit0·bit1 이 둘 다 켜졌다 — 게이트와 데이터가 열렸다.
    소리를 내지는 않고, 하네스가 관측만 한다. *)

(** {1 Sound (RFC 0006)} *)

val start_audio : t -> rate:int -> unit
(** Samples the PC speaker and the Sound Blaster's DAC from now on,
    [rate] samples a second. A port access that changes the speaker, and
    a DSP change at its own cycle, first turn the time before it into
    samples. *)

val take_audio : t -> Bytes.t option
(** The samples not taken yet, signed 16-bit little endian mono, up to
    now — or up to the DSP's next event when that is earlier, since the
    next step runs it at its own cycle. [None] when {!start_audio} was
    not called. *)

val audio_clipped : t -> int option
(** How many samples were clipped to the 16-bit range. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
(** Every field but the RAM and the video adapter, which the machine writes.
    For {!Dos_snapshot}. *)

val read_state : Dos_snap_codec.reader -> t -> unit
(** Overwrites [t] with what {!write_state} wrote. Raises
    [Dos_snap_codec.Invalid] on a value out of range. *)
