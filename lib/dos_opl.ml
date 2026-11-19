(* The OPL2's voice registers. See dos_opl.mli. *)

let register_count = 0xF6

type t = { regs : Bytes.t }

let create () = { regs = Bytes.make register_count '\000' }

let write t reg v = if reg >= 0 && reg < register_count then Bytes.set_uint8 t.regs reg (v land 0xFF)

let register t reg = if reg >= 0 && reg < register_count then Bytes.get_uint8 t.regs reg else 0

module C = Dos_snap_codec

let write_state w t =
  for reg = 0 to register_count - 1 do
    C.put w ~what:"OPL2 register" C.byte (Bytes.get_uint8 t.regs reg)
  done

let read_state r t =
  for reg = 0 to register_count - 1 do
    Bytes.set_uint8 t.regs reg (C.get r ~what:"OPL2 register" C.byte)
  done
