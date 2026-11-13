(** A Sound Blaster 2.0 DSP (version 2.01) at port 220h: its ports and its
    timing, without sound. The card plays through DMA channel 1 and asks on
    IRQ 7. The module stands alone: a caller hands it the DMA controller
    and memory, and turns a [true] from {!advance} into an IRQ.

    {b Ports.} 226h resets: write 1, then 0, and 20µs later the read data
    holds AAh. 22Ch takes commands and their parameter bytes; reading it
    tells whether the DSP takes a byte now (bit 7 clear). 22Ah gives the
    DSP's answers in order, and repeats the last one when none is left.
    22Eh bit 7 says an answer is waiting, and reading 22Eh acknowledges
    the IRQ.

    {b Digitized sound.} 40h sets the time constant TC: one byte every
    256 - TC µs. 14h plays one block of length + 1 bytes. 48h sets the
    auto-init block length and 1Ch plays such blocks until DAh or a reset.
    91h and 90h are the same with the 48h length. The DSP takes each byte
    from the DMA channel when its time comes and drops it (no sound). On a
    masked channel it waits. At the end of each block it raises the IRQ.
    D0h pauses and D4h continues. A 14h during an auto-init block plays
    after that block, which ends the auto-init run.

    {b IRQ.} Block ends, F2h (10µs later) and 80h (silence of length + 1
    bytes) raise it. A raise while the last one is not yet acknowledged
    through 22Eh is lost, as on the card's line into an edge-triggered PIC.

    {b Sources.} DOSBox-staging [src/hardware/audio/soundblaster.cpp]
    (command lengths, reset delay, 0E0h/0E1h/0D8h/0F8h answers, the
    auto-init to single-cycle handoff, the lost IRQ) and 86Box
    [src/sound/snd_sb_dsp.c] (the 2Ah/AAh status bytes of a DSP 2.01-3.xx,
    the 8000h-byte auto-init length after reset). Where they differ: 86Box
    puts AAh in the read data at once, DOSBox 20µs later (this model);
    DOSBox resets the auto-init length to 0, 86Box to 8000h bytes (this
    model).

    {b Outside the model.} Sound output, ADPCM, DMA input (24h), MIDI,
    0E2h, the DREQ bits of the DMA status, and a busy bit that toggles
    while DMA runs. Those commands still swallow their parameter bytes so
    the next command parses. *)

type t

val create : unit -> t
(** Power-on: as after a reset whose AAh was already read (DOSBox's init
    leaves the DSP running with AAh as the last answer). *)

val port_in : t -> int -> int option
(** A read of 226h, 22Ah, 22Ch or 22Eh; [None] for any other port. *)

val port_out : t -> now:int -> int -> int -> bool
(** A write to 226h or 22Ch at CPU cycle [now]; [false] for any other
    port. *)

val due_at : t -> int
(** The CPU cycle of the DSP's next event, [max_int] when none is
    scheduled. *)

val advance : t -> now:int -> dma:Dos_dma.t -> mem:Bytes.t -> bool
(** Runs every event due by [now]: the end of a reset, timer IRQs, and the
    bytes of the playing block, taken from DMA channel 1. [true] when the
    DSP raises its IRQ: the machine asks the PIC. *)

(** {1 Snapshot} *)

val write_state : Dos_snap_codec.writer -> t -> unit
val read_state : Dos_snap_codec.reader -> t -> unit
