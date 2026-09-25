(** 비디오 어댑터 — 모드, 평면 메모리, 그래픽 컨트롤러, 시퀀서, 속성
    컨트롤러.

    EGA/VGA 의 16색 모드는 평면 넷을 같은 주소(0xA0000)에 겹쳐 둔다.
    CPU 가 한 바이트를 쓰면 그래픽 컨트롤러가 그것을 비트 마스크·래치·
    논리연산을 거쳐 최대 네 평면에 나눠 쓴다. 그래서 이 메모리는 1MB
    평면 배열 안에 있을 수 없고 여기가 소유한다.

    래치는 읽기의 부수효과다. 읽기-쓰기 관용구(`mov al,[di]` 다음
    `mov [di],al`)가 그 래치에 기대므로, 읽기에서 네 평면을 모두 걷어
    두지 않으면 쓰기가 엉뚱한 값을 남긴다. *)

type kind =
  | Text                (** 0,1,2,3,7 — 0xB8000 의 글자+속성 *)
  | Cga4                (** 4,5 — 0xB8000, 픽셀당 2비트, 짝·홀 줄 분리 *)
  | Cga2                (** 6 — 0xB8000, 픽셀당 1비트 *)
  | Planar              (** 0Dh,0Eh,0Fh,10h,11h,12h — 0xA0000 평면 넷 *)
  | Linear256           (** 13h — 0xA0000 선형 1바이트/픽셀 *)

type t

val create : mem:Bytes.t -> t

val mode : t -> int
val kind : t -> kind
val dims : t -> int * int
(** 그릴 프레임의 크기. 텍스트는 글자 격자를 8x16 으로 편 크기다. *)

val text_cols : t -> int
(** 이 모드의 BIOS 열 수(BDA 0x44A) — 40 또는 80. 그래픽 모드도 값을
    가진다: 모드 0Dh 는 40, 12h 는 80 이다. *)

val set_mode : t -> int -> clear:bool -> unit
(** INT 10h AH=00h. 모드에 맞는 화면 메모리를 지우고(요청하면) 레지스터를
    그 모드의 부팅값으로 되돌린다. 모르는 번호는 텍스트로 본다. *)

val owns_address : t -> int -> bool
(** 이 물리 주소가 평면 메모리인가 — 그렇다면 읽기·쓰기가 그래픽
    컨트롤러를 지나야 한다. *)

val mem_read : t -> int -> int
val mem_write : t -> int -> int -> unit
(** 물리 주소를 받는다. 읽기는 네 평면의 래치를 채우는 부수효과가 있다. *)

val port_in : t -> int -> int option
val port_out : t -> int -> int -> bool
(** 이 어댑터의 포트가 아니면 [None] / [false]. 부르는 쪽이 다음 장치로
    넘긴다. *)

val reset_attr_flip : t -> unit
(** 속성 컨트롤러의 index/data 번갈이를 index 로 되돌린다. 실기에서는
    입력 상태 레지스터(0x3DA)를 읽으면 이렇게 된다. *)

val planes : t -> Bytes.t array
val attr_palette : t -> int array
(** 색 번호 0-15 를 DAC 자리로 옮기는 표. BIOS 기본값은 항등이다 —
    속성 레지스터 값을 DAC 번호로 그대로 쓴다. *)

val set_attr_palette : t -> int -> int -> unit

val screen_digest : t -> int
(** 지금 모드가 그리는 메모리의 지문. 화면이 멈췄는지 보는 용도다 —
    픽셀을 다 만들어 비교하는 것보다 싸다. 충돌하면 "안 변했다" 고 잘못
    읽을 뿐이라 암호학적 강도가 필요하지 않다. *)

val put_pixel : t -> x:int -> y:int -> color:int -> unit
val get_pixel : t -> x:int -> y:int -> int
(** INT 10h AH=0Ch/0Dh. 텍스트 모드에서는 아무 일도 하지 않는다. *)

val cga_color_select : t -> int
val set_cga_color_select : t -> int -> unit
(** CGA 4색 모드의 팔레트·바탕색을 고르는 레지스터(포트 0x3D9). *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
(** Every field but the RAM it shares with the machine, which the machine
    writes. For {!Dos_snapshot}. *)

val read_state : Dos_snap_codec.reader -> t -> unit
(** Overwrites [t] with what {!write_state} wrote. Raises
    [Dos_snap_codec.Invalid] on a value out of range. *)
