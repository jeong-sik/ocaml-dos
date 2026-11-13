(** INT 21h AH=44h, IOCTL: device questions about a handle or a drive.

    The answers follow the MS-DOS 4.0 source (github.com/microsoft/MS-DOS,
    v4.0/src): [$IOCTL] in IOCTL.ASM, the [sf_flags] a device open builds
    (IOCHK in DIR2.ASM), the device headers in BIOS/MSBDATA.INC, and the
    standard handles BIOS/SYSINIT1.ASM opens at boot: CON as 0, 1 and 2,
    AUX as 3, PRN as 4.

    - 4400h: device information. For a device, DX is the high byte of its
      attribute word over the low byte of its [sf_flags]: CON 80D3h, AUX
      80C0h, PRN A0C0h. For a file, DX is the drive number (0002h). AX
      returns the same value. A handle that is not open is error 6.
    - 4406h: input status, AL = FFh ready or 00h not. A file is ready
      until its position reaches its end. The console is ready when a key
      waits, as for AH=0Bh.
    - 4407h: output status: FFh for every open handle.
    - 4408h: whether drive BL (0 = default, 1 = A:) has fixed media. This
      machine has one drive, C:, and it is fixed: AX = 1. Any other drive
      is error 0Fh.

    Every other subfunction answers error 1. *)

val service : Dos_state.t -> unit
