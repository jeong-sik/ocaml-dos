(* 모드를 세우면 시퀀서 1 번(clocking mode)이 VGA BIOS 값으로 시작한다.
   화면을 끄는 게임은 이 값을 읽어 두었다가 켤 때 되돌리고, 읽은 값이 0
   이면 되돌리기를 건너뛴다(Trugg 의 21d2:0060·0073). 0 으로 시작하면
   그 게임의 화면이 꺼진 채 남는다. *)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %02x want %02x\n%!" name got want
  end

let checkb name got =
  if not got then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let seq_index = 0x3C4
let seq_data = 0x3C5

let video () = Dos_video.create ~mem:(Bytes.make 0x100000 '\000')

let read_seq v index =
  ignore (Dos_video.port_out v seq_index index);
  match Dos_video.port_in v seq_data with Some x -> x | None -> -1

let write_seq v index value =
  ignore (Dos_video.port_out v seq_index index);
  ignore (Dos_video.port_out v seq_data value)

(* 모드 번호 -> 시퀀서 1 번 (DOSBox vga_modes) *)
let table =
  [ (0x00, 0x08); (0x01, 0x08); (0x02, 0x00); (0x03, 0x00); (0x07, 0x00);
    (0x04, 0x09); (0x05, 0x09); (0x06, 0x01); (0x0D, 0x09); (0x0E, 0x01);
    (0x0F, 0x01); (0x10, 0x01); (0x11, 0x01); (0x12, 0x01); (0x13, 0x01) ]

(* Trugg 의 화면 끄기·켜기: 끌 때 읽은 값을 담아 두고 bit 5 를 세운다.
   켤 때 담은 값이 0 이 아니면 그 값을 되돌린다. *)
let screen_off v =
  let saved = read_seq v 1 in
  write_seq v 1 (saved lor 0x20);
  saved

let screen_on v saved = if saved <> 0 then write_seq v 1 saved

let () =
  List.iter
    (fun (mode, want) ->
       let v = video () in
       Dos_video.set_mode v mode ~clear:true;
       checki (Printf.sprintf "모드 %02Xh 의 시퀀서 1 번" mode) (read_seq v 1) want)
    table;
  (* 13h: 끄고 켜면 화면이 돌아온다 *)
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  let saved = screen_off v in
  checkb "끄면 화면이 꺼진다" (Dos_video.screen_blanked v);
  screen_on v saved;
  checkb "켜면 화면이 돌아온다" (not (Dos_video.screen_blanked v));
  (* 모드를 다시 세우면 bit 5 가 꺼지고 모드 값으로 돌아간다 *)
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  ignore (screen_off v);
  Dos_video.set_mode v 0x0D ~clear:true;
  checkb "모드 세우기가 화면을 켠다" (not (Dos_video.screen_blanked v));
  checki "모드 세우기 뒤 시퀀서 1 번" (read_seq v 1) 0x09;
  (* 스냅숏을 돌려도 같다 *)
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  ignore (screen_off v);
  let w = Dos_snap_codec.writer () in
  Dos_video.write_state w v;
  let v2 = video () in
  Dos_video.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) v2;
  checki "복원한 시퀀서 1 번" (read_seq v2 1) 0x21;
  if !failed > 0 then exit 1;
  print_endline "dos vga clocking: all passed"
