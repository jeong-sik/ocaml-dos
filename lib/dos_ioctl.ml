(* INT 21h AH=44h IOCTL. See the .mli for the answers and their sources. *)

open Dos_state

let invalid_function = 1
let invalid_handle = 6
let invalid_drive = 0x0F

(* Device header attribute words (MSBDATA.INC). *)
let attribute = function
  | Con -> 0x8013    (* character device, INT 29h, standard output, standard input *)
  | Aux -> 0x8000    (* character device *)
  | Prn -> 0xA040    (* character device, output until busy, generic IOCTL *)

(* Opening a device sets the low byte of [sf_flags] to the attribute's low
   byte with C0h (device, not at end of file) added (IOCHK). IOCHK also
   clears 20h (raw), which none of the three attributes has. 4400h puts the
   attribute's high byte above it. *)
let device_bit = 0x80
let not_eof_bit = 0x40

let device_info dev =
  let a = attribute dev in
  (a land 0xFF00) lor ((a lor device_bit lor not_eof_bit) land 0xFF)

(* A file: high byte 0, low byte the drive. Bit 6 (not written since it was
   opened) needs a record the handle does not keep, so it reads as written. *)
let file_info = default_drive

let fixed_media = 1

(* AUX and PRN keep the answer they had before this module: ready. What a
   machine without serial and parallel ports answers depends on the BIOS
   INT 14h/17h, which this machine does not model yet. *)
let input_ready t hd =
  match hd.target with
  | File { data; _ } -> hd.pos < Bytes.length !data
  | Device Con -> console_ready t
  | Device (Aux | Prn) -> true

let status_byte ready = if ready then 0xFF else 0x00

let with_handle t f =
  match Hashtbl.find_opt t.handles (Cpu86.reg16 t.cpu 3) with
  | None -> fail t invalid_handle
  | Some hd -> f hd; ok t

let service t =
  let cpu = t.cpu in
  match Cpu86.reg8 cpu 0 with
  | 0x00 ->
    with_handle t (fun hd ->
        let info = match hd.target with File _ -> file_info | Device d -> device_info d in
        Cpu86.set_reg16 cpu 2 info;
        Cpu86.set_reg16 cpu 0 info)
  | 0x06 -> with_handle t (fun hd -> Cpu86.set_reg8 cpu 0 (status_byte (input_ready t hd)))
  | 0x07 -> with_handle t (fun _ -> Cpu86.set_reg8 cpu 0 (status_byte true))
  | 0x08 ->
    let drive = Cpu86.reg8 cpu 3 in
    if drive = 0 || drive = default_drive + 1 then begin
      Cpu86.set_reg16 cpu 0 fixed_media;
      ok t
    end
    else fail t invalid_drive
  | _ -> fail t invalid_function
