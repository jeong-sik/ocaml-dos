(** Drive a game the way a person does: wait until a known picture is on
    the screen, then press keys one at a time and let the game answer each.

    A schedule written in machine steps breaks as soon as the core's timing
    moves. A game that seeds its random choices from the clock then asks a
    different question at the same step (삼국지3's copy protection did after
    the input-wait cycles changed). A picture does not move with timing: the
    same prompt draws the same pixels.

    This module only uses {!Dos_machine}'s public face. It is not part of
    the core and is not in the core's identity digest. *)

type region = { x : int; y : int; w : int; h : int }
(** A rectangle of the current frame, in frame pixels. *)

val region_digest : Dos_machine.t -> region -> string option
(** The MD5, as 32 hex characters, of the region's RGB bytes in the frame
    the screen shows now. [None] when the region does not fit in the frame
    of the current video mode. *)

type step =
  | Wait_for of { what : string; region : region; digest : string; budget : int }
      (** Run until [region] shows [digest]. The screen is looked at every
          {!chunk_steps} machine steps, so [budget] is a bound, not an
          exact count. *)
  | Press of { what : string; keys : string list; budget : int }
      (** Each key, by {!Dos_machine.key_of_string} name, goes in only when
          the ring is empty, and the next waits until the game settled:
          within one chunk it asked for a key with an empty ring, and the
          screen digest did not change since the previous chunk. A key the
          program takes and then exits on is answered too. [budget] bounds
          each key. *)

val chunk_steps : int

type failure = {
  index : int;  (** the failing step's position in the list, from 0 *)
  what : string;
  reason : string;
  machine_steps : int;  (** spent by the whole run up to the failure *)
}

val failure_to_string : failure -> string

val run : Dos_machine.t -> step list -> (int, failure) result
(** Runs the steps in order. [Ok n] is the machine steps spent. A step
    fails when its budget runs out, when the program exits first, or when a
    key name does not parse; the machine is left where it failed, so the
    caller can dump its frame. *)
