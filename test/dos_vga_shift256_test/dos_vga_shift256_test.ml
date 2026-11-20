(* GC05 bit 6(256색 시프트)가 그래픽 모드의 그림 방식을 정한다. 게임은 BIOS
   모드를 바꾸지 않고 이 비트를 직접 써서 256색 Mode X 와 16색 평면 모드를
   오간다. 텍스트와 CGA 는 이 비트를 보지 않는다. *)

let failed = ref 0

let checki name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %d want %d\n%!" name got want
  end

let gc_index = 0x3CE
let gc_data = 0x3CF
let gc_mode = 5
let shift256 = 0x40

let video () = Dos_video.create ~mem:(Bytes.make 0x100000 '\000')

let write_gc v index value =
  ignore (Dos_video.port_out v gc_index index);
  ignore (Dos_video.port_out v gc_data value)

let read_gc v index =
  ignore (Dos_video.port_out v gc_index index);
  match Dos_video.port_in v gc_data with Some x -> x | None -> -1

let kind_name = function
  | Dos_video.Text -> "Text" | Cga4 -> "Cga4" | Cga2 -> "Cga2" | Planar -> "Planar" | Linear256 -> "Linear256"

let check_kind name v want =
  if Dos_video.kind v <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s: got %s want %s\n%!" name (kind_name (Dos_video.kind v)) (kind_name want)
  end

let () =
  (* 13h 는 GC05 = 40h 로 시작한다(실기 BIOS 와 같다). 그 값을 읽고 쓰는 코드가 비트를 지키게 한다 *)
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  checki "13h boots GC05" (read_gc v gc_mode) shift256;
  check_kind "13h boot kind" v Dos_video.Linear256;
  checki "13h dims" (fst (Dos_video.dims v)) 320;
  (* 쓰기 모드만 바꾸고 bit 6 을 지키면 그대로 256색이다(Mode X 의 래치 복사) *)
  write_gc v gc_mode (shift256 lor 1);
  check_kind "write mode 1 keeps 256 colours" v Dos_video.Linear256;
  (* bit 6 을 끄면 16색 평면이 되고, 그림 크기는 글자 하나 8 점이다 *)
  write_gc v gc_mode 0;
  check_kind "bit 6 off is Planar" v Dos_video.Planar;
  checki "Planar dims use 8 dots per char" (fst (Dos_video.dims v)) 640;
  (* 다시 켜면 256색으로 돌아온다 *)
  write_gc v gc_mode shift256;
  check_kind "bit 6 on again is Linear256" v Dos_video.Linear256;
  checki "Linear256 dims use 4 dots per char" (fst (Dos_video.dims v)) 320;
  (* GC05 이 아닌 레지스터를 써도 그림 방식은 그대로다 *)
  write_gc v 8 0x0F;
  check_kind "other GC register leaves kind" v Dos_video.Linear256;
  (* 16색 모드(12h)도 bit 6 을 켜면 256색 시프트가 된다 *)
  Dos_video.set_mode v 0x12 ~clear:true;
  checki "12h boots GC05" (read_gc v gc_mode) 0;
  check_kind "12h boot kind" v Dos_video.Planar;
  write_gc v gc_mode shift256;
  check_kind "12h with bit 6 on is Linear256" v Dos_video.Linear256;
  (* 모드를 다시 세우면 그 모드의 부팅값으로 돌아간다 *)
  Dos_video.set_mode v 0x13 ~clear:true;
  write_gc v gc_mode 0;
  Dos_video.set_mode v 0x13 ~clear:true;
  check_kind "set_mode restores 13h kind" v Dos_video.Linear256;
  checki "set_mode restores GC05" (read_gc v gc_mode) shift256;
  (* 텍스트와 CGA 는 bit 6 을 보지 않는다 *)
  List.iter
    (fun (mode, want) ->
       Dos_video.set_mode v mode ~clear:true;
       write_gc v gc_mode shift256;
       check_kind (Printf.sprintf "mode %02Xh ignores GC05 bit 6" mode) v want;
       write_gc v gc_mode 0;
       check_kind (Printf.sprintf "mode %02Xh ignores GC05 clear" mode) v want)
    [ (0x03, Dos_video.Text); (0x04, Cga4); (0x06, Cga2) ]

(* 스냅숏은 GC05 를 싣고, 되살린 그림 방식은 모드가 아니라 그 값을 따른다 *)
let () =
  let round_trip v =
    let w = Dos_snap_codec.writer () in
    Dos_video.write_state w v;
    let q = video () in
    Dos_video.read_state (Dos_snap_codec.reader (Dos_snap_codec.contents w)) q;
    q
  in
  let v = video () in
  Dos_video.set_mode v 0x13 ~clear:true;
  write_gc v gc_mode 0;
  let q = round_trip v in
  check_kind "restored 13h with GC05 clear is Planar" q Dos_video.Planar;
  checki "restored dims follow kind" (fst (Dos_video.dims q)) 640;
  Dos_video.set_mode v 0x13 ~clear:true;
  check_kind "restored 13h with GC05 40h is Linear256" (round_trip v) Dos_video.Linear256;
  Dos_video.set_mode v 0x03 ~clear:true;
  check_kind "restored text stays Text" (round_trip v) Dos_video.Text

let () =
  if !failed = 0 then print_endline "dos vga shift256: all passed"
  else begin
    Printf.eprintf "dos vga shift256: %d failures\n%!" !failed;
    exit 1
  end
