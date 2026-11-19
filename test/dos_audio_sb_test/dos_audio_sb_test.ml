(* RFC 0006 stage 3: the Sound Blaster's 8-bit DAC as sound. COM programs
   drive the DSP through ports 22Ch and DMA channel 1; the samples are
   checked for the DAC's values in order, for the speaker switch, and for
   being the same however often the host takes them. *)

let failed = ref 0

let check name ok =
  if not ok then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let w16 n = Printf.sprintf "%c%c" (Char.chr (n land 0xff)) (Char.chr ((n lsr 8) land 0xff))
let byte n = String.make 1 (Char.chr n)

let rate = Dos_audio.opl2_rate
let budget = 2_000_000
let psp = 0x10000

let out_al port v = "\xb0" ^ byte v ^ "\xe6" ^ byte port        (* mov al,v; out port,al *)
let dsp v = "\xb0" ^ byte v ^ "\xee"                              (* mov al,v; out dx,al *)
let to_dsp = "\xba" ^ w16 0x22C                                    (* mov dx,22Ch *)
let wait n = "\xb9" ^ w16 n ^ "\xe2\xfe"                           (* mov cx,n; loop $ *)
let quit = "\xcd\x20"

(* 10h: FFh, then 00h, then the speaker off, then C0h while off, which
   sounds once D1h turns the speaker on again, then 40h, then a reset,
   which turns the speaker off. *)
let direct =
  to_dsp ^ dsp 0xD1
  ^ dsp 0x10 ^ dsp 0xFF ^ wait 500
  ^ dsp 0x10 ^ dsp 0x00 ^ wait 500
  ^ dsp 0xD3 ^ wait 500
  ^ dsp 0x10 ^ dsp 0xC0 ^ wait 500
  ^ dsp 0xD1 ^ wait 500
  ^ dsp 0x10 ^ dsp 0x40 ^ wait 500
  ^ "\xba" ^ w16 0x226 ^ dsp 0x01 ^ dsp 0x00 ^ wait 500
  ^ quit

(* The block: a ramp, so a sample that mixes two neighbours never equals
   a third. None is 80h, the DAC at rest. *)
let ramp = List.init 15 (fun i -> 0x08 + (0x10 * i))
let last = List.nth ramp (List.length ramp - 1)
let buffer_at = 0x200
let time_constant = 256 - 100   (* 100 us a byte *)

(* DMA channel 1: single, memory to device, the ramp at 1000:0200. Then
   14h with the block length. *)
let dma ?(command = 0x14) ~speaker () =
  let len = List.length ramp - 1 in
  let code =
    out_al 0x0A 0x05                              (* mask channel 1 *)
    ^ out_al 0x0C 0x00                            (* flip-flop *)
    ^ out_al 0x0B 0x49                            (* single, increment, read, channel 1 *)
    ^ out_al 0x02 (buffer_at land 0xff) ^ out_al 0x02 (buffer_at lsr 8)
    ^ out_al 0x83 (psp lsr 16)                    (* page *)
    ^ out_al 0x03 (len land 0xff) ^ out_al 0x03 (len lsr 8)
    ^ out_al 0x0A 0x01                            (* unmask *)
    ^ to_dsp
    ^ (if speaker then dsp 0xD1 else "")
    ^ dsp 0x40 ^ dsp time_constant
    ^ dsp command ^ dsp (len land 0xff) ^ dsp (len lsr 8)
    ^ wait 2000 ^ quit
  in
  code ^ String.make (buffer_at - 0x100 - String.length code) '\x90'
  ^ String.concat "" (List.map byte ramp)

let play ?(every = 0) code =
  let m = Dos_machine.create ~sound_blaster:true () in
  Dos_machine.load_com m code;
  Dos_machine.start_audio m ~rate;
  let buf = Buffer.create 65536 in
  let take () = Option.iter (Buffer.add_bytes buf) (Dos_machine.take_audio m) in
  let n = ref 0 in
  while (not (Dos_machine.exited m)) && !n < budget do
    ignore (Dos_machine.step m);
    incr n;
    if every > 0 && !n mod every = 0 then take ()
  done;
  take ();
  (m, Array.init (Buffer.length buf / 2) (fun i -> Bytes.get_int16_le (Buffer.to_bytes buf) (2 * i)))

(* The levels that hold over whole samples, runs merged, in order. *)
let plateaus s ~levels =
  Array.fold_left
    (fun acc v ->
      if List.mem v levels then match acc with x :: _ when x = v -> acc | _ -> v :: acc
      else acc)
    [] s
  |> List.rev

let level b = Dos_audio.sb_level (b - 0x80)

let test_direct () =
  let m, s = play direct in
  check "direct: ends" (Dos_machine.exited m);
  (* the program reaches 10h inside the first sample: no silence before *)
  let levels = [ level 0xFF; level 0x00; 0; level 0xC0; level 0x40 ] in
  let got = plateaus s ~levels in
  check
    (Printf.sprintf "direct: FFh, 00h, off, C0h on again, 40h, reset (%s)"
       (String.concat " " (List.map string_of_int got)))
    (got = [ level 0xFF; level 0x00; 0; level 0xC0; level 0x40; 0 ]);
  checki "direct: ends silent" s.(Array.length s - 1) 0

let test_dma () =
  let m, s = play (dma ~speaker:true ()) in
  check "dma: ends" (Dos_machine.exited m);
  let levels = List.map level ramp in
  let got = plateaus s ~levels in
  check
    (Printf.sprintf "dma: every byte in order (%d of %d)" (List.length got) (List.length ramp))
    (got = levels);
  (* 100 us a byte at 49716 Hz is 4.97 samples: 3 to 5 whole ones. The
     last byte holds on after the block. *)
  List.iter
    (fun b ->
      let n = Array.fold_left (fun k v -> if v = level b then k + 1 else k) 0 s in
      check (Printf.sprintf "dma: byte %02Xh holds %d samples" b n) (n >= 3 && n <= 5))
    (List.filter (( <> ) last) ramp);
  checki "dma: the last byte holds after the block" s.(Array.length s - 1) (level last);
  let _, s = play (dma ~speaker:false ()) in
  check "dma, speaker off: silence" (Array.for_all (( = ) 0) s);
  let _, s = play (dma ~command:0x74 ~speaker:true ()) in
  check "ADPCM is not decoded: the DAC stays at rest" (Array.for_all (( = ) 0) s)

(* Each byte takes over at its own cycle. In the sample where level a
   gives way to b at cycle x, the value is (a(x - s0) + b(s1 - x)) / L:
   x comes back to within a few hundredths of a cycle, and the bytes are
   exactly 100 us (477 cycles) apart. *)
let test_byte_times () =
  let _, s = play (dma ~speaker:true ()) in
  let cpu_hz = Dos_ports.speed_hz Dos_ports.pc_speed in
  let byte_cycles = 100 * cpu_hz / 1_000_000 in
  let bound i = i * cpu_hz / rate in
  let levels = List.map level ramp in
  let times =
    List.filteri (fun i _ -> i > 0) levels
    |> List.mapi (fun k b ->
           let a = List.nth levels k in
           (* the first sample not at level a, after the plateau of a *)
           let rec find i seen_a =
             if i >= Array.length s then None
             else if s.(i) = a then find (i + 1) true
             else if seen_a then Some i
             else find (i + 1) false
           in
           match find 0 false with
           | None -> nan
           | Some i ->
             let s0 = bound i and s1 = bound (i + 1) in
             let v = float_of_int s.(i) and l = float_of_int (s1 - s0) in
             let a = float_of_int a and b = float_of_int b in
             ((v *. l) +. (a *. float_of_int s0) -. (b *. float_of_int s1)) /. (a -. b))
  in
  let rec gaps = function x :: (y :: _ as rest) -> (y -. x) :: gaps rest | _ -> [] in
  List.iter
    (fun g ->
      check (Printf.sprintf "a byte every %d cycles (%.2f)" byte_cycles g)
        (Float.abs (g -. float_of_int byte_cycles) < 0.5))
    (gaps times)

let test_split () =
  List.iter
    (fun (name, code) ->
      let _, once = play code in
      let _, eager = play ~every:1 code in
      let _, some = play ~every:333 code in
      check (name ^ ": once = every step") (once = eager);
      check (name ^ ": once = every 333 steps") (once = some))
    [ ("direct", direct); ("dma", dma ~speaker:true ()) ]

let () =
  test_direct ();
  test_dma ();
  test_byte_times ();
  test_split ();
  if !failed > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failed;
    exit 1
  end
  else print_endline "dos audio SB: all passed"
