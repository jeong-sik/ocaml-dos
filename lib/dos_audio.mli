(** Sound as PCM samples (RFC 0006). A sample is the average of the
    machine's sound level over the CPU cycles it covers: sample [i] covers
    cycles [b i] to [b (i + 1)), where [b i = start + i * cpu_hz / rate]. A level
    that holds over part of a sample counts by the cycles it holds, so the
    same sequence of levels always gives the same samples, at any rate.

    The module only listens. A caller asks it to {!render} up to a cycle
    before anything that changes the level, and hands it the level as a
    function of the cycle. Nothing here reads a clock or changes the
    machine. *)

type t

val opl2_rate : int
(** 49716 Hz: the YM3812 at 3.579545 MHz (14.31818 MHz / 4) makes one
    sample every 72 clocks (ModdingWiki, "OPL chip"). *)

val create : cpu_hz:int -> rate:int -> start:int -> t
(** Samples from cycle [start] on: a machine whose sound is turned on, or
    restored from a snapshot, at that cycle. Raises [Invalid_argument]
    unless [0 < rate <= cpu_hz]: every sample is then at least one cycle
    long. *)

val render : t -> until:int -> (int -> int * int) -> unit
(** [render t ~until source] covers cycles {!position} to [until]. [source
    c] is the level at cycle [c] and the first cycle after [c] at which it
    may change: the level holds over every cycle in between. Each sample
    whose last cycle is now covered is finished. Nothing happens when
    [until] is not after {!position}. Raises [Invalid_argument] when the
    source gives a next cycle that is not after [c]. *)

val position : t -> int
(** The first cycle not yet covered. *)

val take : t -> Bytes.t
(** The samples finished since the last [take], signed 16-bit little
    endian, mono. A sample past the 16-bit range is clipped. *)

val clipped : t -> int
(** How many samples so far were clipped. *)

val wav_header : rate:int -> data_bytes:int -> Bytes.t
(** The 44 bytes in front of {!take}'s samples in a WAV file: RIFF WAVE,
    PCM, mono, 16 bits, [rate] samples a second, [data_bytes] of
    samples. *)

(** {1 Sources} *)

val speaker_high : int
(** The PC speaker's level when its cone is out. Out and in are the two
    levels; in is 0. A quarter of the 16-bit range leaves room for the
    other sources of RFC 0006. *)

val speaker : Dos_pit.t -> data:bool -> speed:int -> int -> int * int
(** The PC speaker as a source. Its cone follows the AND of port 61h bit 1
    ([data]) and the OUT of PIT channel 2 ([pit]) (DOSBox-staging
    intel8255.cpp). [speed] is the CPU cycles per PIT tick. Between two
    writes to ports 42h, 43h (channel 2) and 61h the level only changes
    where OUT does, so the source walks {!Dos_pit.next_change}. The PIT
    must not have been called past the cycles asked for. *)

val sb_step : int
(** The level of one step of the Sound Blaster's 8-bit DAC. A full swing
    (-128..127) takes half the 16-bit range, so the speaker and the FM
    chip still fit beside it. *)

val sb_level : int -> int
(** The level of a DAC output of -128..127 ({!Dos_sb.output}). Between two
    changes the DAC holds, so the source is constant: the DSP tells the
    caller before each change ({!Dos_sb.on_output}). *)
