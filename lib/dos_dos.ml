(* DOS 표면 — INT 21h 와 INT 20h.

   파일 계약: 하네스가 마운트한 것만 보인다. 호스트 경로로 나가는 길은
   없다. 한 이름을 여는 모든 핸들은 메모리 위 버퍼 하나를 같이 쓴다 —
   한 핸들의 쓰기가 다른 핸들의 다음 읽기에 그대로 보인다. 닫을 때마다
   그 버퍼를 마운트 표에 되돌린다 — 같은 세션 안에서 저장하고 다시
   불러오는 게 그래서 된다(게임 세이브). 호스트 디스크의 원본은 바뀌지
   않는다.

   메모리 계약: 프로그램 뒤부터 0x9FFF 까지가 할당 가능한 자리다.
   첫 맞는 자리(first fit)로 준다 — MCB 사슬을 게스트에게 보여주지는
   않는다. 사슬을 직접 훑는 프로그램은 이 모델 밖이고, 그때는 조용히
   틀리는 대신 할당 실패로 답한다. *)

open Dos_state

let dos_version_major = 5
let dos_version_minor = 0
let default_drive = 2                  (* C: *)
let conv_mem_top = 0x9FFF              (* 비디오 메모리 바로 앞 *)

(* ---------- 이름과 와일드카드 ---------- *)

(* "NAME.EXT" 를 8+3 칸으로. 비교는 여기서만 한다 — 대소문자 무시. *)
let to_8_3 name =
  let name = String.uppercase_ascii name in
  let base, ext =
    match String.rindex_opt name '.' with
    | Some i ->
      (String.sub name 0 i, String.sub name (i + 1) (String.length name - i - 1))
    | None -> (name, "")
  in
  let pad s n =
    if String.length s >= n then String.sub s 0 n
    else s ^ String.make (n - String.length s) ' '
  in
  (pad base 8, pad ext 3)

(* DOS 의 '*' 는 그 칸의 남은 자리를 전부 '?' 로 바꾸는 것과 같다. *)
let expand_stars field =
  let b = Bytes.make (String.length field) ' ' in
  let star = ref false in
  String.iteri
    (fun i c ->
      if !star then Bytes.set b i '?'
      else if c = '*' then begin star := true; Bytes.set b i '?' end
      else Bytes.set b i c)
    field;
  Bytes.to_string b

let matches_pattern ~pattern ~name =
  let pb, pe = to_8_3 pattern and nb, ne = to_8_3 name in
  let pb = expand_stars pb and pe = expand_stars pe in
  let cmp p n =
    let okay = ref true in
    String.iteri (fun i c -> if c <> '?' && c <> n.[i] then okay := false) p;
    !okay
  in
  cmp pb nb && cmp pe ne

(* ---------- 열린 파일 ---------- *)

let is_console h = h >= 0 && h <= 2

(* 가장 낮은 빈 번호 — 실기 DOS 가 JFT 에서 첫 빈 칸을 고르는 것과 같다.
   닫은 번호는 다시 쓰이고, 표가 차면 None (DOS 오류 4). 규칙은
   Dos_state.max_handles 주석. *)
let free_handle t =
  let rec go h =
    if h >= max_handles then None
    else if Hashtbl.mem t.handles h then go (h + 1)
    else Some h
  in
  go first_file_handle

let too_many_open_files = 4
let invalid_handle = 6

(* [name]을 아직 물고 있는 핸들이 있는지 — 살아있는 [t.handles] 뿐 아니라
   EXEC 로 잠든 부모 핸들(자식이 실행되는 동안은 [t.handles] 밖에 있다)도
   센다. 자식이 상속받은 핸들을 닫았다 다시 그 이름을 여는 사이, 부모의
   원래 자리는 여전히 그 이름을 물고 있다고 봐야 한다 — 아니면 그 사이의
   open 이 이미 지워진 공유 버퍼를 못 찾고 새로 하나 만들어, 부모가
   복귀했을 때 둘이 다시 갈라진다. *)
let name_still_claimed t name =
  Hashtbl.fold (fun _ hd acc -> acc || hd.hname = name) t.handles false
  || List.exists
       (fun (f : exec_frame) -> List.exists (fun (_, hd) -> hd.hname = name) f.parent_handles)
       t.exec_frames

(* [name]이 지금 열려 있으면 그 공유 버퍼를, 아니면 [seed]로 새로 만든
   버퍼를 돌려준다. 같은 이름을 여는 모든 핸들이 이 버퍼 하나를
   가리켜서, 한 핸들의 쓰기가 다른 핸들의 다음 읽기에 그대로 보인다. *)
let shared_buffer t name seed =
  match Hashtbl.find_opt t.open_files name with
  | Some buf -> buf
  | None ->
    let buf = ref seed in
    Hashtbl.replace t.open_files name buf;
    buf

let open_handle t name data =
  match free_handle t with
  | None -> None
  | Some h ->
    Hashtbl.replace t.handles h { hname = name; data = shared_buffer t name data; pos = 0 };
    Some h

(* 닫을 때 마운트 표로 되돌린다 — 저장한 파일을 같은 세션에서 다시
   열 수 있어야 게임의 세이브/로드가 성립한다. 공유 버퍼는 같은 이름의
   다른 핸들이 남아 있으면(부모의 잠든 자리 포함) 그대로 두고, 아무도
   안 남았을 때만 지운다 — 다음 open 이 다시 [host_files] 에서 시작하게. *)
let close_handle t h =
  match Hashtbl.find_opt t.handles h with
  | None -> false
  | Some hd ->
    Hashtbl.remove t.handles h;
    if hd.hname <> "" then begin
      Hashtbl.replace t.host_files hd.hname !(hd.data);
      if not (name_still_claimed t hd.hname) then Hashtbl.remove t.open_files hd.hname
    end;
    true

(* ---------- 메모리 할당 ---------- *)

let block_end (seg, paras) = seg + paras

let largest_free t =
  let sorted = List.sort compare t.blocks in
  let best = ref 0 and cursor = ref t.free_base in
  List.iter
    (fun b ->
      let seg, _ = b in
      if seg - !cursor > !best then best := seg - !cursor;
      if block_end b > !cursor then cursor := block_end b)
    sorted;
  if t.free_top - !cursor > !best then best := t.free_top - !cursor;
  max 0 !best

(* 자리를 찾기만 한다 — 블록 표를 고치지 않는다. EXEC 는 배치를 정한
   뒤에야 부모 프레임(블록 표 포함)을 찍는다. [allocate] 를 그대로 쓰면
   자식 몫이 프레임에 새어 들어가, 자식이 죽어도 블록이 남는다. *)
let find_free t paras =
  let sorted = List.sort compare t.blocks in
  let cursor = ref t.free_base and placed = ref None in
  List.iter
    (fun b ->
      let seg, _ = b in
      if !placed = None && seg - !cursor >= paras then placed := Some !cursor;
      if block_end b > !cursor then cursor := block_end b)
    sorted;
  if !placed = None && t.free_top - !cursor >= paras then placed := Some !cursor;
  !placed

let allocate t paras =
  match find_free t paras with
  | Some seg -> t.blocks <- (seg, paras) :: t.blocks; Some seg
  | None -> None

let free_block t seg =
  if List.mem_assoc seg t.blocks then begin
    t.blocks <- List.remove_assoc seg t.blocks;
    true
  end
  else false

let resize_block t seg paras =
  match List.assoc_opt seg t.blocks with
  | None -> Error 9                        (* 잘못된 블록 주소 *)
  | Some cur ->
    if paras <= cur then begin
      t.blocks <- (seg, paras) :: List.remove_assoc seg t.blocks;
      Ok ()
    end
    else begin
      (* 뒤로 늘릴 자리가 있는가 *)
      let limit =
        List.fold_left
          (fun acc (s, _) -> if s > seg && s < acc then s else acc)
          t.free_top
          (List.remove_assoc seg t.blocks)
      in
      if seg + paras <= limit then begin
        t.blocks <- (seg, paras) :: List.remove_assoc seg t.blocks;
        Ok ()
      end
      else Error (limit - seg)             (* 최대 가능 크기를 알린다 *)
    end

(* 실기 DOS 는 .EXE 에 남은 메모리를 통째로 준다. 힙이 필요한 프로그램은
   AH=4Ah 로 제 블록을 줄여 뒤를 내놓고, 그 다음에야 AH=48h 이 성공한다
   — Turbo Pascal 런타임이 정확히 그렇게 한다. 처음부터 남는 자리를
   비워 두면 4Ah 없이도 할당이 되어 실기와 다르게 움직인다.
   [~psp_seg] 를 주는 EXEC 자식 경로는 이 함수를 부르지 않는다 — 블록
   표에 이미 부모·상주 프로그램이 있어 통째로 초기화하면 지워진다. *)
let init_memory t ~psp_seg =
  t.free_base <- psp_seg;
  t.free_top <- conv_mem_top;
  t.blocks <- [ (psp_seg, conv_mem_top - psp_seg) ]

(* ---------- 로더 ----------

   Dos_machine 이 루트 프로그램을 싣는 데 쓰던 코드가 EXEC 자식도
   싣게 되면서 이리로 옮겨왔다. 배치만 매개변수로 갈린다 — 루트는
   정해진 자리, 자식은 블록 표 위에 얹는다. *)

(* 환경 블록 — PSP+2Ch 가 가리킨다. "이름=값" ASCIIZ 문자열들이 빈
   문자열(NUL 하나)로 끝나고, 그 뒤에 워드 1 과 프로그램 이름(ASCIIZ)이
   온다(MS-DOS 4.0 EXEC.ASM exec_save_environ). C 런타임은 이 이름으로
   argv[0] 을 채우고, 오버레이 관리자는 이 이름으로 자기 EXE 를 다시 연다.

   루트의 환경은 0x500:0 에 둔다(IVT·BDA 위의 스크래치, 블록 표 밖).
   세그먼트가 0 이면 KOEI 로더처럼 AH=49h 로 환경을 지우고 AH=4Ah 로
   줄이는 프로그램이 연쇄로 실패해 EXEC 자리가 안 생긴다(실측). 문자열은
   비어 있다. 이름을 모르면 꼬리를 쓰지 않는다. EXEC 자식은
   [child_environment] 로 블록을 따로 받는다. *)
let root_env_seg = 0x500
let root_env_room = 0x100                (* 0x500..0x5FF *)
let env_program_count = 1
let env_scan_limit = 0x7FFF              (* EXEC.ASM: at most 32k of environment *)
let error_bad_environment = 10
let error_not_enough_memory = 8

(* 문자열 목록 [list] 뒤에 꼬리(워드 1, [name], NUL)를 붙인 바이트 *)
let environment_bytes ~list ~name =
  let b = Buffer.create (String.length list + String.length name + 3) in
  Buffer.add_string b list;
  Buffer.add_char b (Char.chr env_program_count);
  Buffer.add_char b '\x00';
  Buffer.add_string b name;
  Buffer.add_char b '\x00';
  Buffer.contents b

let empty_env_list = "\x00\x00"

(* 루트 환경을 0x500 에 쓴다. [name] 은 C: 루트 디렉터리의 파일 이름이다
   — 이 기계의 루트 프로그램은 거기 있다. 꼬리에는 드라이브와 역슬래시를
   붙인 전체 경로를 쓴다. *)
let write_root_env t ~name =
  let bytes =
    match name with
    | None -> empty_env_list
    | Some file ->
      let drive = Char.chr (Char.code 'A' + default_drive) in
      environment_bytes ~list:empty_env_list
        ~name:(Printf.sprintf "%c:\\%s" drive (String.uppercase_ascii file))
  in
  if String.length bytes > root_env_room then
    invalid_arg "program name does not fit the root environment";
  String.iteri (fun i c -> wr8 t ((root_env_seg * 16) + i) (Char.code c)) bytes

(* 환경 문자열 목록의 길이 — 끝의 NUL 두 개까지 센다. EXEC.ASM
   exec_get_environ_len 처럼 0x7FFF 바이트 안에서 찾지 못하면 None. *)
let env_list_length t seg =
  let base = seg lsl 4 in
  let rec scan i =
    if i + 2 > env_scan_limit then None
    else if rd8 t (base + i) = 0 && rd8 t (base + i + 1) = 0 then Some (i + 2)
    else scan (i + 1)
  in
  scan 0

(* EXEC 자식의 환경. [source] 가 0 이면 자식도 환경이 없다
   (exec_check_environ). 아니면 목록을 새 블록에 옮기고 꼬리를 붙인다.
   [name] 은 호출자가 DS:DX 에 준 문자열 그대로다 — EXEC.ASM 은 이름을
   바꾸지 않고 DStrLen 길이(NUL 포함)만큼 옮긴다. 블록은 블록 표에
   들어간다. 실패하면 블록 표를 건드리지 않는다. *)
type child_env = No_environment | Environment of int

let child_environment t ~source ~name =
  if source = 0 then Ok No_environment
  else
    match env_list_length t source with
    | None -> Error error_bad_environment
    | Some len ->
      let list = String.init len (fun i -> Char.chr (rd8 t ((source lsl 4) + i))) in
      let bytes = environment_bytes ~list ~name in
      match allocate t ((String.length bytes + 15) / 16) with
      | None -> Error error_not_enough_memory
      | Some seg ->
        String.iteri (fun i c -> wr8 t ((seg lsl 4) + i) (Char.code c)) bytes;
        Ok (Environment seg)

let env_word = function No_environment -> 0 | Environment seg -> seg

let write_psp ?(init = true) ~env t psp_seg =
  let psp = psp_seg * 16 in
  wr8 t psp 0xCD;                      (* int 20h *)
  wr8 t (psp + 1) 0x20;
  wr16 t (psp + 2) conv_mem_top;       (* 가진 메모리의 끝 *)
  wr16 t (psp + 0x0A) 0;               (* 종료 주소 *)
  wr16 t (psp + 0x0C) 0;
  wr16 t (psp + 0x2C) env;             (* 환경 세그먼트 *)
  wr8 t (psp + 0x80) 0;                (* 커맨드라인 길이 0 *)
  wr8 t (psp + 0x81) 0x0D;
  t.psp_seg <- psp_seg;
  t.dta <- physical psp_seg 0x80;      (* 기본 DTA = PSP:0x80 *)
  if init then init_memory t ~psp_seg

(* COM: 실기 DOS 는 PSP 를 독립 세그먼트에 준다. 세그 0 에 두면 PSP 가
   IVT·BDA 와 겹쳐 부팅 스텁을 지운다(실측: PSP:0x80 쓰기가 IVT[20h]
   오프셋을 0 으로 만들었다). 세그 0x1000 — DOS 의 오랜 배치.
   자식 COM 은 64KB 블록 한 장을 할당받은 자리에 실린다. *)
let load_com_at ~psp_seg ~child ~env t image =
  let base = (psp_seg * 16) + 0x100 in
  Bytes.blit (Bytes.of_string image) 0 t.mem base (String.length image);
  write_psp ~init:(not child) ~env t psp_seg;
  if child then begin
    t.blocks <- t.blocks @ [ (psp_seg, 0x1000) ];
    t.free_base <- psp_seg
  end;
  Cpu86.set_seg t.cpu 1 psp_seg;
  Cpu86.set_seg t.cpu 3 psp_seg;
  Cpu86.set_seg t.cpu 0 psp_seg;
  Cpu86.set_seg t.cpu 2 psp_seg;
  Cpu86.set_ip t.cpu 0x100;
  Cpu86.set_reg16 t.cpu 4 0xFFFE;
  (* RET 로 돌아오면 PSP 의 INT 20h 로 끝나는 실기 관례 — 스택 맨 위에
     0x0000 을 둔다. *)
  wr16 t ((psp_seg * 16) + 0xFFFE) 0x0000;
  t.exited <- false;
  t.exit_code <- 0

(* ---------- MZ 적재 모듈 ----------

   MS-DOS 4.0 src/DOS/EXEC.ASM 대로 헤더를 읽는다.
   - "MZ" 와 "ZM" 둘 다 EXE 서명이다(exe_valid_signature,
     exe_valid_old_signature).
   - 적재 모듈은 파일의 [헤더, 페이지 수 × 512) 다. 마지막 페이지
     바이트(02h)는 보지 않는다(exec_save_start). 메모리도 이 길이로 센다.
   - 파일이 그보다 짧으면 있는 만큼 싣고, 512 바이트 이상 모자라면 형식
     오류다(exec_read_ok).
   이 기계가 EXEC.ASM 과 다르게 거절하는 것: 고정 헤더(0x1C)보다 짧은
   파일(EXEC.ASM 은 COM 으로 싣는다), 페이지 수가 헤더보다 짧은 것, 파일
   밖으로 나간 재배치 표(EXEC.ASM 은 버퍼에 남은 값을 읽는다). *)
let mz_fixed_header = 0x1C
let mz_page = 512
let mz_reloc_entry = 4

let has_mz_signature image =
  String.length image >= 2
  && ((image.[0] = 'M' && image.[1] = 'Z') || (image.[0] = 'Z' && image.[1] = 'M'))

type mz_module = {
  header_bytes : int;
  load_end : int;     (* 파일에서 실을 끝 — 페이지 끝과 파일 끝 가운데 앞쪽 *)
  load_paras : int;   (* 적재 모듈이 차지하는 메모리 — 페이지 × 32 − 헤더 문단 *)
  reloc_table : int;
  reloc_count : int;
}

(* 헤더가 파일 안의 적재 모듈을 말하지 않는 까닭 *)
type mz_error =
  | Header_too_short          (* 고정 헤더(0x1C)보다 짧다 *)
  | Pages_end_in_header       (* 페이지 수 × 512 가 헤더 크기보다 작다 *)
  | File_short_by_a_page      (* 파일이 페이지 끝보다 512 바이트 이상 짧다 *)
  | Relocations_past_file     (* 재배치 표가 파일 밖으로 나간다 *)

let mz_error_message = function
  | Header_too_short -> "MZ header too short"
  | Pages_end_in_header -> "MZ pages end inside the header"
  | File_short_by_a_page -> "MZ file is a page or more shorter than its header says"
  | Relocations_past_file -> "MZ relocation table runs past the file"

let header_u16 image i = Char.code image.[i] lor (Char.code image.[i + 1] lsl 8)

let mz_module image =
  let len = String.length image in
  if len < mz_fixed_header then Error Header_too_short
  else
    let u16 = header_u16 image in
    let module_end = u16 0x04 * mz_page and header_bytes = u16 0x08 * 16 in
    let reloc_count = u16 0x06 and reloc_table = u16 0x18 in
    if module_end < header_bytes then Error Pages_end_in_header
    else if module_end - len >= mz_page then Error File_short_by_a_page
    else if reloc_table + (reloc_count * mz_reloc_entry) > len then Error Relocations_past_file
    else
      Ok { header_bytes; load_end = min module_end len;
           load_paras = (module_end - header_bytes) / 16; reloc_table; reloc_count }

(* 모듈을 [base] 에 옮기고 재배치 항목마다 그 워드에 [factor] 를 더한다
   (16비트에서 돈다 — wr16 이 하위 16비트만 쓴다). 주소는 20비트에서
   돈다(wr8). *)
let place_mz_module t image m ~base ~factor =
  for i = m.header_bytes to m.load_end - 1 do
    wr8 t (base + i - m.header_bytes) (Char.code image.[i])
  done;
  for i = 0 to m.reloc_count - 1 do
    let e = m.reloc_table + (i * mz_reloc_entry) in
    let addr = base + (header_u16 image (e + 2) lsl 4) + header_u16 image e in
    wr16 t addr (rd16 t addr + factor)
  done

type loadable = Com_image | Exe_image of mz_module

let loadable image =
  if not (has_mz_signature image) then Ok Com_image
  else Result.map (fun m -> Exe_image m) (mz_module image)

(* MZ EXE — 실기 배치. 프로그램은 conventional RAM 위쪽에 실린다:
   LZEXE 같은 자기 압축 해제 스텁이 이 위치를 기준으로 원본 진입점을
   계산하기 때문에, 낮은 고정 세그먼트에 두면 엉뚱한 주소로 뛴다
   (ZZT 실측). 상한은 이미지 끝이 0xA000(비디오 메모리 시작)을 넘지
   않는 자리다 — 넘으면 비디오 모드 세팅이 코드를 지운다.
   [psp_seg] 를 주면 그 자리에(배치는 호출자 계약), 없으면 루트 공식
   대로 memtop 아래에 블록 하나로 받는다. 자식 EXE 도 마찬가지로
   남은 메모리를 통째로 블록으로 받는다 — 실기와 같아서, 자식이 다시
   EXEC 하려면 먼저 AH=4Ah 로 줄여야 한다. *)
let load_exe_module ?psp_seg ~child ~env t image m =
  let u16 = header_u16 image in
  let exe_ip = u16 0x14 and exe_cs = u16 0x16 in
  let exe_sp = u16 0x10 and exe_ss = u16 0x0E in
  let minalloc = u16 0x0A in
  let psp_seg =
    match psp_seg with
    | Some p -> p
    | None -> max 0x1000 (0x9FF0 - m.load_paras - minalloc)
  in
  let image_seg = psp_seg + 0x10 in
  place_mz_module t image m ~base:(image_seg * 16) ~factor:image_seg;
  write_psp ~init:(not child) ~env t psp_seg;
  if child then begin
    t.blocks <- t.blocks @ [ (psp_seg, t.free_top - psp_seg) ];
    t.free_base <- psp_seg
  end;
  Cpu86.set_seg t.cpu 1 (image_seg + exe_cs);
  Cpu86.set_ip t.cpu exe_ip;
  Cpu86.set_seg t.cpu 2 (image_seg + exe_ss);
  (* MS-DOS 진입 스택 계약: 스택 꼭대기에 [0, PSP] 쌍을 심는다 —
     프로그램 최상위의 far ret 가 PSP:0000 의 INT 20h(write_psp 가
     심음)로 떨어져 깨끗이 종료하는 CP/M 관례의 계승이다. 심지 않으면
     retf 가 게임 실행 중 남은 찌꺼기(0 과 near-call 반환 주소)를 팝해
     빈 메모리로 점프한다 — 삼국지3 MAIN 의 retf 가 7119:0000(제로
     메모리)로 떨어진 실측. *)
  let sp0 = (exe_sp - 4) land 0xffff in
  let sbase = (image_seg + exe_ss) lsl 4 in
  wr16 t (sbase + ((sp0 + 2) land 0xffff)) psp_seg;
  wr16 t (sbase + sp0) 0;
  Cpu86.set_reg16 t.cpu 4 sp0;
  (* DOS 는 DS/ES 를 PSP 세그먼트로 넘긴다 — 진입점이 곧바로
     mov cx,[PSP+0x0C] 로 읽는다(실측). *)
  Cpu86.set_seg t.cpu 3 psp_seg;
  Cpu86.set_seg t.cpu 0 psp_seg;
  t.exited <- false;
  t.exit_code <- 0

(* 루트 프로그램의 명령줄 꼬리. PSP:80h 가 길이, 81h 부터 글자, 그 뒤에
   0Dh 다(RBIL INT 21/26 PSP 표). COMMAND.COM 은 명령 이름 바로 뒤 글자부터
   CR 까지를 옮긴다(MS-DOS 4.0 TCODE.ASM COMTAIL). "GAME a" 면 꼬리는 앞
   공백을 포함한 " a" 이고 길이는 2 다. 81h..FFh 에 CR 까지 들어가야 하므로
   126 바이트가 끝이다. 꼬리 안에 CR 은 올 수 없다 — COMMAND.COM 은 첫 CR
   에서 멈춘다. *)
let command_tail_room = 126
let carriage_return = '\r'

let check_command_tail tail =
  if String.length tail > command_tail_room then
    invalid_arg "command tail longer than 126 bytes";
  if String.contains tail carriage_return then
    invalid_arg "command tail contains a carriage return"

let write_command_tail t tail =
  let psp = t.psp_seg * 16 in
  wr8 t (psp + 0x80) (String.length tail);
  String.iteri (fun i c -> wr8 t (psp + 0x81 + i) (Char.code c)) tail;
  wr8 t (psp + 0x81 + String.length tail) (Char.code carriage_return)

let load_com ?name ?(tail = "") t image =
  check_command_tail tail;
  write_root_env t ~name;
  load_com_at ~psp_seg:0x1000 ~child:false ~env:root_env_seg t image;
  write_command_tail t tail

let load_exe ?name ?(tail = "") t image =
  check_command_tail tail;
  match loadable image with
  | Ok (Exe_image m) ->
    write_root_env t ~name;
    load_exe_module ~child:false ~env:root_env_seg t image m;
    write_command_tail t tail
  | Ok Com_image -> invalid_arg "not an MZ image"
  | Error e -> invalid_arg (mz_error_message e)

(* ---------- EXEC (AH=4Bh) · 자식 종료 ----------

   자식은 같은 기계 위에서 이어서 돈다 — CPU 레지스터와 프로세스
   상태(PSP·DTA·블록 표·핸들)만 갈아끼운다. IVT·BDA·시계는 하드웨어
   그대로 공유한다(TSR 드라이버가 건 벡터가 다음 자식에게 남는 것도
   그래서 자연스럽다). 부모의 재개점은 프레임에 찍어둔 IP 다 — 훅은
   INT 명령의 fetch 가 IP 를 넘긴 뒤에 돌기 때문에 그 값이 곧 'INT
   다음 명령'이다. *)

let snapshot_cpu t =
  let cpu = t.cpu in
  {
    snap_regs = Array.init 8 (Cpu86.reg16 cpu);
    snap_segs = Array.init 4 (Cpu86.seg cpu);
    snap_ip = Cpu86.dump_ip cpu;
    snap_flags = Cpu86.flags cpu;
  }

let restore_cpu t s =
  let cpu = t.cpu in
  Array.iteri (Cpu86.set_reg16 cpu) s.snap_regs;
  Array.iteri (Cpu86.set_seg cpu) s.snap_segs;
  Cpu86.set_ip cpu s.snap_ip;
  Cpu86.set_flags cpu s.snap_flags

(* MZ 자식의 배치 — 실기 DOS 의 EXEC 처럼 남은 자리 맨 아래(first fit)
   에 PSP+이미지+minalloc 을 얹는다. 위로 남는 메모리가 곧 자식 몫이
   되어(MSC 런타임이 PSP+0x02 와 이미지 끝 사이로 환경·argv 공간을
   재니 넉넉해야 한다 — R6009 실측). 루트 적재의 memtop 아래 공식은
   LZEXE 자기 해제 스텁용이고 자식 계약이 아니다. LZEXE 로 묶인 자식은
   이 배치를 기준 삼지 않으므로 아직 계약 밖이다. *)
let exe_child_psp t ~img_paras ~minalloc =
  find_free t (0x10 + img_paras + minalloc)

(* 자식 종료. 프레임이 없으면(루트) 기계가 멈춘다. 있으면 부모를
   되살린다 — [keep] 은 TSR(AH=31h·INT 27h) 이 자기 블록을 남기는
   크기다. 자식이 연 핸들은 닫는다(쓴 데이터는 마운트 표로 돌아간다),
   자식의 블록은 사라지고 TSR 몫만 남는다. *)
let child_exit t ~code ~keep =
  match t.exec_frames with
  | [] ->
    t.exited <- true;
    t.exit_code <- code
  | f :: rest ->
    t.exec_frames <- rest;
    t.last_child_code <- code;
    (* A handle is the parent's only if the same number still names the
       same record: numbers are reused (lowest free), so a child that closed
       an inherited handle and opened another under that number opened a
       file of its own.
       Close in handle order: two handles on one file each write their bytes
       back, so the order picks which write survives. The table's bucket
       order depends on its insertion history, which differs between a
       machine and one rebuilt from its snapshot. *)
    let inherited h hd =
      match List.assoc_opt h f.parent_handles with
      | Some phd -> phd == hd
      | None -> false
    in
    let opened_by_child =
      List.sort compare
        (Hashtbl.fold (fun h hd acc -> if inherited h hd then acc else h :: acc)
           t.handles [])
    in
    List.iter (fun h -> ignore (close_handle t h)) opened_by_child;
    (* The parent's table comes back as it was: real DOS gives the child a
       copy of the JFT, so a handle the child closed stays open for the
       parent. *)
    List.iter (fun (h, hd) -> Hashtbl.replace t.handles h hd) f.parent_handles;
    (* 상주 종료(31h·INT 27h)는 PSP 블록만 [keep] 으로 줄이고 자식의 다른
       블록(환경, 48h 로 받은 것)은 남긴다(RBIL INT 21/31: "the value in DX
       only affects the memory block containing the PSP"). 그냥 끝나면 자식
       블록이 모두 사라진다. *)
    (match keep with
     | Some paras ->
       t.blocks <- (t.psp_seg, paras) :: List.remove_assoc t.psp_seg t.blocks
     | None -> t.blocks <- f.parent_blocks);
    t.free_base <- f.parent_free_base;
    t.free_top <- f.parent_free_top;
    t.psp_seg <- f.parent_psp;
    t.dta <- f.parent_dta;
    restore_cpu t f.parent;
    (* EXEC 에서 돌아온 직후의 값 — AH=4Dh 로도 같은 코드를 읽는다 *)
    Cpu86.set_reg16 t.cpu 0 code;
    set_cf t false;
    finish_service_return t f.parent_service_return

(* INT 21h AX=4B03h — 오버레이 싣기. RBIL INT 21/4B(Table 01591)와 MS-DOS 4.0
   src/DOS/EXEC.ASM 을 따른다. ES:BX 의 파라미터 블록은 [싣는 세그먼트;
   재배치 인수] 두 워드다. 호출자가 마련한 메모리에 싣기만 하고 실행하지
   않는다. PSP·블록 표·핸들은 그대로다. MZ 는 위 [mz_module] 대로 싣고,
   재배치 항목에 재배치 인수를 더한다. MZ 가 아니면 파일 전체를 재배치 없이
   싣는다. EXEC.ASM 은 0FFFFh 바이트를 읽으라 하고, 그만큼 다 읽히면 메모리
   부족(8)으로 답한다(exec_com_file). *)
let com_overlay_read = 0xFFFF

let load_overlay t image =
  let cpu = t.cpu in
  let block = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 3 in
  let base = rd16 t block lsl 4 in
  let factor = rd16 t (block + 2) in
  match loadable image with
  | Ok Com_image ->
    let len = String.length image in
    if len >= com_overlay_read then fail t 8
    else begin
      String.iteri (fun i c -> wr8 t (base + i) (Char.code c)) image;
      set_cf t false
    end
  | Ok (Exe_image m) -> place_mz_module t image m ~base ~factor; set_cf t false
  | Error (Header_too_short | Pages_end_in_header | File_short_by_a_page
          | Relocations_past_file) -> fail t 11

let exec_load_and_run = 0x00
let exec_load_overlay = 0x03

(* INT 21h AH=4Bh — AL=00 불러 실행하기, AL=03 오버레이 싣기. AL=01(불러만)
   은 아직 계약 밖이다: 실패로 답해 조용히 넘어가는 일이 없게 한다. *)
let exec_program t ~return_to =
  let cpu = t.cpu in
  let raw_name = asciiz_at t (seg_off t 3 2) in
  let name = String.uppercase_ascii raw_name in
  let al = Cpu86.reg8 cpu 0 in
  if al <> exec_load_and_run && al <> exec_load_overlay then begin fail t 1; Completed end
  else
    match Hashtbl.find_opt t.host_files name with
    | None -> fail t 2; Completed              (* 파일이 없다 *)
    | Some img_bytes when al = exec_load_overlay ->
      load_overlay t (Bytes.to_string img_bytes);
      Completed
    | Some img_bytes ->
      let image = Bytes.to_string img_bytes in
      (* EPB(ES:BX) 는 자식 적재 전에 읽는다 — 적재가 ES 를 바꾼다 *)
      let epb = (Cpu86.seg cpu 0 lsl 4) + Cpu86.reg16 cpu 3 in
      let epb_env = rd16 t epb in
      let tail_seg = rd16 t (epb + 4) and tail_off = rd16 t (epb + 2) in
      let fcb1_seg = rd16 t (epb + 8) and fcb1_off = rd16 t (epb + 6) in
      let fcb2_seg = rd16 t (epb + 12) and fcb2_off = rd16 t (epb + 10) in
      (* EXEC.ASM 순서: 환경(10·8) → 헤더(11) → 프로그램 자리(8). 뒤에서
         실패하면 환경 블록을 놓는다(exec_bomb → exec_dealloc). EPB 의
         환경이 0 이면 부모의 환경을 옮긴다. *)
      let parent_blocks = t.blocks in
      let source =
        if epb_env <> 0 then epb_env else rd16 t ((t.psp_seg * 16) + 0x2C)
      in
      let give_up code = t.blocks <- parent_blocks; fail t code; Completed in
      match child_environment t ~source ~name:raw_name with
      | Error code -> fail t code; Completed
      | Ok env ->
      (* 헤더가 파일 안의 적재 모듈을 말하지 않으면(짧은 헤더, 모자란 파일,
         파일 밖 재배치 표) DOS 오류 11(잘못된 형식)로 답한다 — 이 호출만
         실패하고 부모는 그대로 산다. *)
      match loadable image with
      | Error (Header_too_short | Pages_end_in_header | File_short_by_a_page
              | Relocations_past_file) -> give_up 11
      | Ok kind ->
      let placement =
        match kind with
        | Exe_image m ->
          exe_child_psp t ~img_paras:m.load_paras ~minalloc:(header_u16 image 0x0A)
        | Com_image -> find_free t 0x1000
      in
      match placement with
      | None ->
        t.blocks <- parent_blocks;
        Cpu86.set_reg16 cpu 3 (largest_free t);
        fail t error_not_enough_memory; Completed
      | Some psp ->
         t.exec_frames <-
           {
             parent = snapshot_cpu t;
             parent_service_return = return_to;
             parent_psp = t.psp_seg;
             parent_dta = t.dta;
             parent_free_base = t.free_base;
             parent_free_top = t.free_top;
             parent_blocks;
             parent_handles =
               List.sort (fun (a, _) (b, _) -> compare a b)
                 (Hashtbl.fold (fun h v acc -> (h, v) :: acc) t.handles []);
           }
           :: t.exec_frames;
         let env = env_word env in
         (match kind with
          | Exe_image m -> load_exe_module ~psp_seg:psp ~child:true ~env t image m
          | Com_image -> load_com_at ~psp_seg:psp ~child:true ~env t image);
         (* 커맨드라인·FCB 를 자식 PSP 에 옮긴다 *)
         let cpsp = t.psp_seg * 16 in
         let copy_dword_into dst seg off n =
           if seg <> 0 || off <> 0 then
             for i = 0 to n - 1 do
               wr8 t (dst + i) (rd8 t ((seg lsl 4) + off + i))
             done
         in
         if tail_seg <> 0 || tail_off <> 0 then begin
           let src = (tail_seg lsl 4) + tail_off in
           let n = rd8 t src in
           wr8 t (cpsp + 0x80) n;
           copy_dword_into (cpsp + 0x81) tail_seg (tail_off + 1) n;
           wr8 t (cpsp + 0x81 + n) 0x0D
         end;
         copy_dword_into (cpsp + 0x5C) fcb1_seg fcb1_off 12;
         copy_dword_into (cpsp + 0x6C) fcb2_seg fcb2_off 12;
         Control_transferred

(* INT 27h — 옛 방식의 상주 종료. DX 는 '마지막 상주 바이트 다음
   오프셋' 이고 블록은 16바이트로 올림한 만큼(마지막 한 칸은 여유)을
   남긴다. *)
let int27 t =
  let dx = Cpu86.reg16 t.cpu 2 in
  child_exit t ~code:0 ~keep:(Some ((dx + 15) / 16 + 1))

(* ---------- findfirst / findnext ---------- *)

let fill_find_block t name =
  let data =
    match Hashtbl.find_opt t.host_files name with
    | Some d -> d
    | None -> Bytes.empty
  in
  let size = Bytes.length data in
  wr8 t (t.dta + 0x15) 0x20;                 (* 속성: 보관 *)
  wr16 t (t.dta + 0x16) 0;                   (* 시각 *)
  (* DOS 날짜 워드: 상위 7비트 연도-1980, 다음 4비트 월, 하위 5비트 일.
     0x2821 은 2000-01-01 이다. 마운트 표에는 시각이 없어 고정값을 준다. *)
  wr16 t (t.dta + 0x18) 0x2821;
  wr16 t (t.dta + 0x1A) (size land 0xffff);
  wr16 t (t.dta + 0x1C) ((size lsr 16) land 0xffff);
  String.iteri (fun i c -> wr8 t (t.dta + 0x1E + i) (Char.code c)) name;
  wr8 t (t.dta + 0x1E + String.length name) 0

(* ---------- FCB ---------- *)

let fcb_name t fcb =
  let take n off =
    let b = Buffer.create 12 in
    for i = 0 to n - 1 do
      let c = rd8 t (fcb + off + i) in
      if c <> 0x20 then Buffer.add_char b (Char.chr c)
    done;
    Buffer.contents b
  in
  let base = take 8 1 and ext = take 3 9 in
  if ext = "" then base else base ^ "." ^ ext

let fcb_service t ah =
  let cpu = t.cpu in
  let fcb = seg_off t 3 2 in
  match ah with
  | 0x0F ->
    let name = String.uppercase_ascii (fcb_name t fcb) in
    (match Hashtbl.find_opt t.host_files name with
     | Some data ->
       Hashtbl.replace t.fcbs fcb (data, 0);
       wr16 t (fcb + 0x0E) 128;
       wr16 t (fcb + 0x10) (Bytes.length data land 0xffff);
       wr16 t (fcb + 0x12) ((Bytes.length data lsr 16) land 0xffff);
       for i = 0 to 3 do wr8 t (fcb + 0x20 + i) 0 done;
       Cpu86.set_reg8 cpu 0 0
     | None -> Cpu86.set_reg8 cpu 0 0xFF)
  | 0x10 -> Hashtbl.remove t.fcbs fcb; Cpu86.set_reg8 cpu 0 0
  | 0x14 ->
    let recsize = max 1 (rd16 t (fcb + 0x0E)) in
    (match Hashtbl.find_opt t.fcbs fcb with
     | Some (data, pos) ->
       let avail = Bytes.length data - pos in
       if avail <= 0 then Cpu86.set_reg8 cpu 0 1
       else begin
         let take = min recsize avail in
         blit_to_mem t data pos t.dta take;
         Hashtbl.replace t.fcbs fcb (data, pos + take);
         let recno = rd8 t (fcb + 0x20) + 1 in
         if recno >= 128 then begin
           wr16 t (fcb + 0x0C) (rd16 t (fcb + 0x0C) + 1);
           wr8 t (fcb + 0x20) 0
         end
         else wr8 t (fcb + 0x20) recno;
         Cpu86.set_reg8 cpu 0 (if take < recsize then 1 else 0)
       end
     | None -> Cpu86.set_reg8 cpu 0 0xFF)
  | 0x21 ->
    let recsize = max 1 (rd16 t (fcb + 0x0E)) in
    let recno =
      rd8 t (fcb + 0x21) lor (rd8 t (fcb + 0x22) lsl 8)
      lor (rd8 t (fcb + 0x23) lsl 16)
    in
    (match Hashtbl.find_opt t.fcbs fcb with
     | Some (data, _) ->
       let off = recno * recsize in
       if off >= Bytes.length data then Cpu86.set_reg8 cpu 0 1
       else begin
         let take = min recsize (Bytes.length data - off) in
         blit_to_mem t data off t.dta take;
         Hashtbl.replace t.fcbs fcb (data, off + take);
         Cpu86.set_reg8 cpu 0 (if take < recsize then 1 else 0)
       end
     | None -> Cpu86.set_reg8 cpu 0 0xFF)
  | _ -> Cpu86.set_reg8 cpu 0 0

(* ---------- 콘솔 입력 ---------- *)

(* An empty ring is absence, never a completed zero-valued input. *)
let console_read t =
  if key_pending t then begin
    t.kbd_wait <- false;
    Some (pop_key t)
  end
  else begin
    starve t;
    None
  end

let ascii_of_key w =
  let a = w land 0xff in
  if a = 0 then 0 else a

(* 실기 DOS 콘솔 입력은 바이트 스트림이다 — 확장키(방향키 등, ascii=0)는
   첫 읽기에 0x00, 다음 읽기에 스캔코드를 반환한다(MS-DOS AH=01/07/08
   계약). 링은 단어 단위라 스캔을 잠시 여기에 걸어 두었다가 다음 읽기로
   내준다. 게스트 왕복 테스트가 이 계약을 지킨다. *)
let ext_read t =
  if t.ext_scan_pending <> 0 then begin
    let sc = t.ext_scan_pending in
    t.ext_scan_pending <- 0;
    t.kbd_wait <- false;
    Some sc
  end
  else
    match console_read t with
    | Some w ->
      let a = w land 0xff and sc = w lsr 8 in
      if a = 0 && sc <> 0 then begin
        t.ext_scan_pending <- sc;
        Some 0
      end
      else Some a
    | None -> None

type line_result = Line_complete of string | Line_waiting of line_input

let echo_line_character t c =
  if c = 0x09 then begin
    let col, _ = get_cursor t in
    let width = 8 - (col mod 8) in
    for _ = 1 to width do put_char t 0x20 0x07 done;
    width
  end
  else if c < 0x20 then begin
    put_char t 0x5e 0x07;
    put_char t (c + 0x40) 0x07;
    2
  end
  else begin put_char t c 0x07; 1 end

let erase_line_character t width =
  for _ = 1 to width do
    let col, row = get_cursor t in
    let col, row = if col > 0 then col - 1, row
      else if row > 0 then cols t - 1, row - 1 else 0, 0 in
    write_cell t row col 0x0720;
    set_cursor t col row
  done

let rec read_line t ~capacity line =
  match console_read t with
  | None -> Line_waiting line
  | Some w ->
    let c = ascii_of_key w in
    match c, line.echo_widths with
    | 0x0d, _ -> put_char t 0x0d 0x07; Line_complete line.text
    | (0x08 | 0x7f), width :: widths ->
      erase_line_character t width;
      read_line t ~capacity
        { text = String.sub line.text 0 (String.length line.text - 1); echo_widths = widths }
    | (0x00 | 0x08 | 0x7f), _ -> read_line t ~capacity line
    | 0x0a, _ ->
      if line.text <> "" then begin put_char t 0x0d 0x07; put_char t 0x0a 0x07 end;
      read_line t ~capacity line
    | _, _ when String.length line.text >= capacity - 1 ->
      put_char t 0x07 0x07; (* Full buffers still wait for CR or an editing key. *)
      read_line t ~capacity line
    | _, _ ->
      let width = echo_line_character t c in
      read_line t ~capacity
        { text = line.text ^ String.make 1 (Char.chr c); echo_widths = width :: line.echo_widths }

let return_console_bytes t ~destination ~length =
  let count = min length (String.length t.console_pending) in
  String.iteri (fun i c -> wr8 t (destination + i) (Char.code c))
    (String.sub t.console_pending 0 count);
  t.console_pending <- String.sub t.console_pending count (String.length t.console_pending - count);
  Cpu86.set_reg16 t.cpu 0 count;
  ok t;
  Completed

let resume_input t request =
  match request with
  | Read_character { echo } ->
    (match ext_read t with
     | None -> Awaiting_input (Dos_input request)
     | Some c ->
       Cpu86.set_reg8 t.cpu 0 c;
       if echo && c <> 0 then put_char t c 0x07;
       Completed)
  | Read_buffered_line ({ base; capacity; line } as input) ->
    (match read_line t ~capacity line with
     | Line_waiting line -> Awaiting_input (Dos_input (Read_buffered_line { input with line }))
     | Line_complete text ->
       wr8 t (base + 1) (String.length text);
       String.iteri (fun i c -> wr8 t (base + 2 + i) (Char.code c)) (text ^ "\r");
       Completed)
  | Read_console_line ({ destination; length; line } as input) ->
    (match read_line t ~capacity:console_line_capacity line with
     | Line_waiting line -> Awaiting_input (Dos_input (Read_console_line { input with line }))
     | Line_complete text ->
       put_char t 0x0a 0x07;
       t.console_pending <-
         if String.length text > 0 && text.[0] = '\026' then "" else text ^ "\r\n";
       return_console_bytes t ~destination ~length)

(* ---------- INT 21h ---------- *)

let service_immediate t =
  let cpu = t.cpu in
  let ah = Cpu86.reg8 cpu 4 in
  match ah with
  | 0x4D ->
    (* 마지막 자식의 종료 코드 — AH 는 종료 사유, 정상 종료는 0 *)
    Cpu86.set_reg8 cpu 0 t.last_child_code;
    Cpu86.set_reg8 cpu 4 0;
    ok t
  | 0x02 -> put_char t (Cpu86.reg8 cpu 2) 0x07
  | 0x06 ->
    (* 직접 콘솔 입출력. DL=FF 면 읽기(ZF 로 있고없음), 아니면 쓰기.
       Turbo Pascal 런타임의 에러 메시지가 이 길로 나온다 — 없으면
       화면에 아무 흔적 없이 죽는다(실측: "Runtime error 006"). *)
    let dl = Cpu86.reg8 cpu 2 in
    if dl = 0xFF then
      (match ext_read t with
       | Some c ->
         Cpu86.set_reg8 cpu 0 c;
         Cpu86.set_flags cpu (Cpu86.flags cpu land lnot Cpu86.f_zero)
       | None ->
        Cpu86.set_reg8 cpu 0 0;
        Cpu86.set_flags cpu (Cpu86.flags cpu lor Cpu86.f_zero)
      )
    else put_char t dl 0x07
  | 0x09 ->
    (* '$' 로 끝나는 문자열. 길이를 안 주는 대신 끝 표시를 찾는다. *)
    let base = seg_off t 3 2 in
    let i = ref 0 and stop = ref false in
    while (not !stop) && !i < 65536 do
      let c = rd8 t (base + !i) in
      if c = Char.code '$' then stop := true
      else begin put_char t c 0x07; incr i end
    done
  | 0x0B ->
    let available = t.ext_scan_pending <> 0 || key_pending t in
    Cpu86.set_reg8 cpu 0 (if available then 0xFF else 0x00);
    if available then t.kbd_wait <- false else starve t
  | 0x0D -> ()                              (* 디스크 리셋 *)
  | 0x0E -> Cpu86.set_reg8 cpu 0 (default_drive + 1)
  | 0x19 -> Cpu86.set_reg8 cpu 0 default_drive
  | 0x1A -> t.dta <- seg_off t 3 2
  | 0x2F ->
    Cpu86.set_seg cpu 0 ((t.dta lsr 4) land 0xffff);
    Cpu86.set_reg16 cpu 3 (t.dta land 0x0f)
  | 0x25 ->
    let v = Cpu86.reg8 cpu 0 in
    wr16 t (v * 4) (Cpu86.reg16 cpu 2);
    wr16 t ((v * 4) + 2) (Cpu86.seg cpu 3)
  | 0x35 ->
    let v = Cpu86.reg8 cpu 0 in
    Cpu86.set_reg16 cpu 3 (rd16 t (v * 4));
    Cpu86.set_seg cpu 0 (rd16 t ((v * 4) + 2))
  | 0x2A ->
    let y, m, d, _, _, _ = now_fields t in
    Cpu86.set_reg16 cpu 1 y;
    Cpu86.set_reg8 cpu 6 m;
    Cpu86.set_reg8 cpu 2 d;
    Cpu86.set_reg8 cpu 0 (day_of_week (y, m, d))
  | 0x2B ->
    t.epoch_year <- Cpu86.reg16 cpu 1;
    t.epoch_month <- Cpu86.reg8 cpu 6;
    t.epoch_day <- Cpu86.reg8 cpu 2;
    Cpu86.set_reg8 cpu 0 0
  | 0x2C ->
    let _, _, _, h, mi, s = now_fields t in
    Cpu86.set_reg8 cpu 5 h;
    Cpu86.set_reg8 cpu 1 mi;
    Cpu86.set_reg8 cpu 6 s;
    Cpu86.set_reg8 cpu 2 0
  | 0x2D ->
    t.epoch_hour <- Cpu86.reg8 cpu 5;
    t.epoch_min <- Cpu86.reg8 cpu 1;
    t.epoch_sec <- Cpu86.reg8 cpu 6;
    Cpu86.set_reg8 cpu 0 0
  | 0x30 ->
    Cpu86.set_reg8 cpu 0 dos_version_major;
    Cpu86.set_reg8 cpu 4 dos_version_minor;
    Cpu86.set_reg8 cpu 3 0xFF;              (* OEM: 일반 *)
    Cpu86.set_reg16 cpu 1 0
  | 0x33 -> Cpu86.set_reg8 cpu 2 0          (* Ctrl-Break 검사 꺼짐 *)
  | 0x36 ->
    (* 남은 디스크 공간: 섹터/클러스터, 가용 클러스터, 섹터 크기,
       전체 클러스터. 마운트 표는 메모리 위라 넉넉한 값을 답한다. *)
    Cpu86.set_reg16 cpu 0 4;
    Cpu86.set_reg16 cpu 3 0x1000;
    Cpu86.set_reg16 cpu 1 512;
    Cpu86.set_reg16 cpu 2 0x2000
  | 0x39 | 0x3A | 0x3B ->
    (* 디렉터리가 없는 모델이다. 조용한 성공 대신 '경로 없음' 으로
       답한다 — 성공으로 속이면 게스트가 그 뒤를 헛돈다. *)
    fail t 3
  | 0x3C | 0x5B ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    if ah = 0x5B && Hashtbl.mem t.host_files name then fail t 0x50
    else begin
      match open_handle t name Bytes.empty with
      | None -> fail t too_many_open_files
      | Some h ->
        Hashtbl.replace t.host_files name Bytes.empty;
        (* [open_handle] joins a buffer already shared by another live
           handle on [name] instead of seeding a fresh empty one -- create
           truncates regardless, so force it empty here too, visible to
           whoever else already has [name] open. *)
        (match Hashtbl.find_opt t.handles h with
         | Some hd -> hd.data := Bytes.empty
         | None -> ());
        Cpu86.set_reg16 cpu 0 h;
        ok t
    end
  | 0x3D ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    (match Hashtbl.find_opt t.host_files name with
     | Some data ->
       (match open_handle t name data with
        | Some h -> Cpu86.set_reg16 cpu 0 h; ok t
        | None -> fail t too_many_open_files)
     | None -> fail t 2)
  | 0x3E -> if close_handle t (Cpu86.reg16 cpu 3) then ok t else fail t 6
  (* dup/dup2 는 같은 열린 파일을 가리킨다 — 위치와 내용을 나눠 쓴다.
     사본을 주면 한쪽에 쓴 것이 다른 쪽에 안 보인다. *)
  | 0x45 ->
    (match Hashtbl.find_opt t.handles (Cpu86.reg16 cpu 3) with
     | Some hd ->
       (match free_handle t with
        | Some h ->
          Hashtbl.replace t.handles h hd;
          Cpu86.set_reg16 cpu 0 h;
          ok t
        | None -> fail t too_many_open_files)
     | None -> fail t invalid_handle)
  | 0x46 ->
    (match Hashtbl.find_opt t.handles (Cpu86.reg16 cpu 3) with
     | Some hd ->
       let target = Cpu86.reg16 cpu 1 in
       if target >= max_handles then fail t invalid_handle
       else begin Hashtbl.replace t.handles target hd; ok t end
     | None -> fail t invalid_handle)
  | 0x3F ->
    let h = Cpu86.reg16 cpu 3 in
    let want = Cpu86.reg16 cpu 1 in
    let dst = seg_off t 3 2 in
    (match Hashtbl.find_opt t.handles h with
       | None -> fail t 6
       | Some hd ->
         let take = min want (max 0 (Bytes.length !(hd.data) - hd.pos)) in
         blit_to_mem t !(hd.data) hd.pos dst take;
         hd.pos <- hd.pos + take;
         Cpu86.set_reg16 cpu 0 take;
         ok t)
  | 0x40 ->
    let h = Cpu86.reg16 cpu 3 in
    let n = Cpu86.reg16 cpu 1 in
    let src = seg_off t 3 2 in
    if is_console h then begin
      (* 실기 DOS 는 부팅 때 stdin/stdout/stderr 를 열어 준다 — 없으면
         런타임이 잘못된 핸들(6)로 즉사한다(실측). *)
      for i = 0 to n - 1 do put_char t (rd8 t (src + i)) 0x07 done;
      Cpu86.set_reg16 cpu 0 n;
      ok t
    end
    else
      (match Hashtbl.find_opt t.handles h with
       | None -> fail t 6
       | Some hd ->
         let chunk = Bytes.create n in
         for i = 0 to n - 1 do
           Bytes.set chunk i (Char.chr (rd8 t (src + i)))
         done;
         let len = Bytes.length !(hd.data) in
         let grown =
           if hd.pos + n <= len then begin
             let copy = Bytes.copy !(hd.data) in
             Bytes.blit chunk 0 copy hd.pos n;
             copy
           end
           else begin
             let head = Bytes.sub !(hd.data) 0 (min hd.pos len) in
             let gap =
               if hd.pos > len then Bytes.make (hd.pos - len) '\000'
               else Bytes.empty
             in
             Bytes.concat Bytes.empty [ head; gap; chunk ]
           end
         in
         hd.data := grown;
         hd.pos <- hd.pos + n;
         Cpu86.set_reg16 cpu 0 n;
         ok t)
  | 0x41 ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    if Hashtbl.mem t.host_files name then begin
      Hashtbl.remove t.host_files name;
      ok t
    end
    else fail t 2
  | 0x42 ->
    let h = Cpu86.reg16 cpu 3 in
    let off32 = (Cpu86.reg16 cpu 1 lsl 16) lor Cpu86.reg16 cpu 2 in
    (match Hashtbl.find_opt t.handles h with
     | None -> fail t 6
     | Some hd ->
       let signed =
         if off32 >= 0x80000000 then off32 - 0x100000000 else off32 in
       let size = Bytes.length !(hd.data) in
       let newpos =
         match Cpu86.reg8 cpu 0 with
         | 0 -> signed
         | 1 -> hd.pos + signed
         | _ -> size + signed
       in
       if newpos < 0 then fail t 6
       else begin
         hd.pos <- newpos;
         Cpu86.set_reg16 cpu 0 (newpos land 0xffff);
         Cpu86.set_reg16 cpu 2 ((newpos lsr 16) land 0xffff);
         ok t
       end)
  | 0x43 ->
    let name = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    if Hashtbl.mem t.host_files name then begin
      if Cpu86.reg8 cpu 0 = 0 then Cpu86.set_reg16 cpu 1 0x20;
      ok t
    end
    else fail t 2
  | 0x44 ->
    (* IOCTL. 전부 실패시키면 런타임이 핸들을 무효로 보고 죽는다(실측).
       콘솔은 문자 장치 비트, 파일은 읽기·쓰기 가능. *)
    (match Cpu86.reg8 cpu 0 with
     | 0x00 ->
       let h = Cpu86.reg16 cpu 3 in
       if is_console h then begin Cpu86.set_reg16 cpu 2 0x80D3; ok t end
       else if Hashtbl.mem t.handles h then begin
         Cpu86.set_reg16 cpu 2 0x0002; ok t
       end
       else fail t 6
     | 0x06 | 0x07 -> Cpu86.set_reg8 cpu 0 0xFF; ok t
     | _ -> fail t 1)
  | 0x47 ->
    (* 현재 디렉터리는 늘 루트다 — 빈 문자열이 루트를 뜻한다. *)
    wr8 t (seg_off t 3 6) 0;
    Cpu86.set_reg16 cpu 0 0x0100;
    ok t
  | 0x48 ->
    (match allocate t (Cpu86.reg16 cpu 3) with
     | Some seg -> Cpu86.set_reg16 cpu 0 seg; ok t
     | None ->
       Cpu86.set_reg16 cpu 3 (largest_free t);
       fail t 8)
  | 0x49 -> if free_block t (Cpu86.seg cpu 0) then ok t else fail t 9
  | 0x4A ->
    (match resize_block t (Cpu86.seg cpu 0) (Cpu86.reg16 cpu 3) with
     | Ok () -> ok t
     | Error 9 -> fail t 9
     | Error avail -> Cpu86.set_reg16 cpu 3 (max 0 avail); fail t 8)
  | 0x4E ->
    let pattern = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    let all = Hashtbl.fold (fun k _ acc -> k :: acc) t.host_files [] in
    let hits =
      List.sort compare
        (List.filter (fun n -> matches_pattern ~pattern ~name:n) all)
    in
    (match hits with
     | [] -> t.find_queue <- []; fail t 2
     | first :: rest -> t.find_queue <- rest; fill_find_block t first; ok t)
  | 0x4F ->
    (match t.find_queue with
     | [] -> fail t 18                      (* 더 이상 없음 *)
     | next :: rest -> t.find_queue <- rest; fill_find_block t next; ok t)
  | 0x56 ->
    let src = String.uppercase_ascii (asciiz_at t (seg_off t 3 2)) in
    let dst = String.uppercase_ascii (asciiz_at t ((Cpu86.seg cpu 0 lsl 4)
                                                   + Cpu86.reg16 cpu 7)) in
    (match Hashtbl.find_opt t.host_files src with
     | Some d ->
       Hashtbl.remove t.host_files src;
       Hashtbl.replace t.host_files dst d;
       ok t
     | None -> fail t 2)
  | 0x57 ->
    if Hashtbl.mem t.handles (Cpu86.reg16 cpu 3) then begin
      if Cpu86.reg8 cpu 0 = 0 then begin
        Cpu86.set_reg16 cpu 1 0;
        Cpu86.set_reg16 cpu 2 0x2821
      end;
      ok t
    end
    else fail t 6
  | 0x62 -> Cpu86.set_reg16 cpu 3 t.psp_seg
  | 0x68 -> ok t                            (* 파일 반영 — 메모리라 즉시 *)
  | 0x0F | 0x10 | 0x14 | 0x21 | 0x24 -> fcb_service t ah
  | _ -> ()

let terminate t = child_exit t ~code:0 ~keep:None

let rec service t ~return_to =
  match Cpu86.reg8 t.cpu 4 with
  | (0x01 | 0x07 | 0x08) as ah -> resume_input t (Read_character { echo = ah = 0x01 })
  | 0x0A ->
    let base = seg_off t 3 2 in
    let capacity = rd8 t base in
    if capacity = 0 then Completed
    else resume_input t (Read_buffered_line { base; capacity; line = { text = ""; echo_widths = [] } })
  | 0x0C ->
    let sub_fn = Cpu86.reg8 t.cpu 0 in
    while key_pending t do ignore (pop_key t) done;
    t.ext_scan_pending <- 0;
    (match sub_fn with
     | 0x01 | 0x06 | 0x07 | 0x08 | 0x0A ->
       Cpu86.set_reg8 t.cpu 4 sub_fn;
       service t ~return_to
     | _ -> Cpu86.set_reg8 t.cpu 0 0; Completed)
  | 0x3F when is_console (Cpu86.reg16 t.cpu 3) ->
    let destination = seg_off t 3 2 and length = Cpu86.reg16 t.cpu 1 in
    if length = 0 || t.console_pending <> "" then return_console_bytes t ~destination ~length
    else resume_input t (Read_console_line { destination; length; line = { text = ""; echo_widths = [] } })
  | 0x4B -> exec_program t ~return_to
  | 0x00 -> terminate t; Control_transferred
  | 0x4C -> child_exit t ~code:(Cpu86.reg8 t.cpu 0) ~keep:None; Control_transferred
  | 0x31 ->
    child_exit t ~code:(Cpu86.reg8 t.cpu 0) ~keep:(Some (Cpu86.reg16 t.cpu 2));
    Control_transferred
  | _ -> service_immediate t; Completed
