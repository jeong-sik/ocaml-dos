(** The mouse driver's cursor on the screen (issue #77). Like a real driver
    it changes video memory where the cursor stands and keeps what was
    under it, to put back before the cursor hides or moves.

    The way it draws and the default arrow follow DOSBox-X
    src/ints/mouse.cpp (github.com/joncampbell123/dosbox-x, master as of
    2026-11-16): [DrawCursor], [DrawCursorText], [ClipCursorArea],
    [defaultScreenMask], [defaultCursorMask].

    - Graphics modes: a 16x16 block. Where a screen mask bit is 1 the point
      under the cursor stays, where it is 0 the point becomes 0; where a
      cursor mask bit is 1 the result is XORed with 0Fh. The block's corner
      is the virtual x divided by the mode's cell width, and the virtual y,
      less the hot spot. Points off the screen are cut off.
    - Text modes: the character and attribute word of the cell under the
      cursor is ANDed with the screen mask and XORed with the cursor mask,
      on the page shown.
    - A hardware text cursor (INT 33h AX=0Ah BX<>0) is not drawn. DOSBox-X
      moves the CRTC cursor to the mouse's cell; this machine's CRTC cursor
      writes also move the BIOS cursor, so that needs a path of its own. *)

val default_screen_mask : int array
val default_cursor_mask : int array
(** The arrow after a reset: sixteen rows, the leftmost point in bit 15. *)

val restore : Dos_state.t -> unit
(** Puts back what the drawn cursor covered, if it is drawn. *)

val draw : Dos_state.t -> unit
(** When the driver is present and the cursor shown (flag 0): puts back
    the old cursor's background, then draws the cursor where the mouse is
    now. Otherwise nothing. *)
