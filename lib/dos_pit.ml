(* 8254 카운터 하나. 출처는 Intel 82C54 데이터시트(231244)의 Mode
   Definitions, Read Operations, Write Operations 절이다.

   카운터를 틱마다 줄이지 않는다. 마지막 사건 뒤 흐른 틱에서 지금 카운트와
   OUT 을 계산한다. 사건 사이에 수백만 틱이 흘러도 읽기 비용이 같다. *)

type mode =
  | Interrupt_on_terminal_count  (* 0 *)
  | Hardware_one_shot            (* 1 *)
  | Rate_generator               (* 2 *)
  | Square_wave                  (* 3 *)
  | Software_strobe              (* 4 *)
  | Hardware_strobe              (* 5 *)

type access = Lsb_only | Msb_only | Lsb_then_msb

type encoding = Binary | Bcd

(* 세는 중인 구간. [n] 은 이 구간의 초기 카운트(자연수: 바이너리 1..65536,
   BCD 1..10000 — 레지스터 0 이 최댓값이다). 구간 시작 뒤 지나간 펄스는
   [base] + ([since] 부터 지금까지). GATE 가 셈을 막은 동안은 [since] 가
   [None] 이다. [since] 는 카운트를 실은 펄스 다음 틱이다 — 싣는 펄스는
   카운트를 줄이지 않는다. *)
type segment = {
  n : int;
  low_first : bool;  (* 모드 3: 이 구간은 낮은 반주기부터 시작한다 *)
  base : int;
  since : int option;
}

(* 모드 2·3 에서 세는 동안 쓴 새 카운트는 지금 주기(모드 3 은 반주기)가
   끝날 때 실린다. [at] 은 그때의 지나간 펄스 수다. *)
type reload = { at : int; next_n : int; next_low_first : bool }

type run =
  | Stopped of int
    (* 제어 워드 뒤 아직 셈을 시작하지 않았다. 값은 멈춘 카운트 레지스터. *)
  | Counting of { seg : segment; reload : reload option }

type t = {
  mutable mode : mode;
  mutable access : access;
  mutable encoding : encoding;
  mutable lsb_written : int option;  (* 2바이트 쓰기의 첫 바이트 *)
  mutable msb_read_next : bool;      (* 2바이트 읽기의 순번 *)
  mutable latched : int option;      (* 래치 명령이 잡은 카운트 *)
  mutable count_register : int option;
  (* 제어 워드 뒤 쓴 초기 카운트(자연수). 모드 1·5 의 트리거와 모드 2·3 의
     GATE 상승이 이 값을 싣는다. *)
  mutable run : run;
}

let create () = {
  mode = Interrupt_on_terminal_count;
  access = Lsb_then_msb;
  encoding = Binary;
  lsb_written = None;
  msb_read_next = false;
  latched = None;
  count_register = None;
  run = Stopped 0;
}

let binary_modulus = 0x10000
let bcd_modulus = 10000

let modulus = function Binary -> binary_modulus | Bcd -> bcd_modulus

(* 레지스터 값 → 자연수 카운트. BCD 자리가 9 를 넘는 값은 데이터시트가 정하지
   않는다. 이 기계는 자리값을 그대로 10 의 거듭제곱에 곱해 더하고 10000 으로
   나눈 나머지를 쓴다. *)
let natural encoding register =
  let v = match encoding with
    | Binary -> register
    | Bcd ->
      let digit i = (register lsr (4 * i)) land 0xf in
      ((digit 3 * 1000) + (digit 2 * 100) + (digit 1 * 10) + digit 0) mod bcd_modulus
  in
  if v = 0 then modulus encoding else v

(* 자연수 → 레지스터 값. 0 아래로 내려간 카운트는 한 바퀴 돈다(바이너리
   FFFFh, BCD 9999). *)
let register encoding v =
  let m = modulus encoding in
  let v = ((v mod m) + m) mod m in
  match encoding with
  | Binary -> v
  | Bcd ->
    ((v / 1000) lsl 12) lor ((v / 100 mod 10) lsl 8) lor ((v / 10 mod 10) lsl 4)
    lor (v mod 10)

let pulses seg tick =
  seg.base + (match seg.since with Some s -> max 0 (tick - s) | None -> 0)

(* 모드 3 의 한 주기는 높은 반 (n+1)/2 펄스, 낮은 반 n/2 펄스다. 홀수 n 은
   높은 쪽이 한 펄스 길다("high for (N+1)/2 counts and low for (N-1)/2
   counts"). 돌려주는 값은 (OUT, 반주기 안에서 지나간 펄스, 반주기 길이).
   지나간 펄스는 늘 반주기 길이보다 작다. 데이터시트는 모드 2·3 에서
   카운트 1 을 금지한다(Figure 22). 1 을 받으면 모드 2 는 OUT 이 늘 낮고
   모드 3 은 늘 높다 — 실칩과 견주지 않았다. *)
let square seg k =
  let high = (seg.n + 1) / 2 and low = seg.n / 2 in
  let p = k mod seg.n in
  if seg.low_first then
    if p < low then (false, p, low) else (true, p - low, high)
  else if p < high then (true, p, high)
  else (false, p - high, low)

(* 미뤄 둔 새 카운트가 실릴 때가 지났으면 구간을 바꾼다. 바뀐 구간은
   그 펄스부터 센다. *)
let settle t ~tick =
  match t.run with
  | Counting { seg = { since = Some s; _ } as seg; reload = Some r }
    when pulses seg tick >= r.at ->
    let since = Some (s + (r.at - seg.base)) in
    t.run <-
      Counting
        { seg = { n = r.next_n; low_first = r.next_low_first; base = 0; since };
          reload = None }
  | Counting _ | Stopped _ -> ()

let current_count t ~tick =
  settle t ~tick;
  match t.run with
  | Stopped ce -> ce
  | Counting { seg; _ } ->
    let k = pulses seg tick in
    let v = match t.mode with
      | Interrupt_on_terminal_count | Hardware_one_shot | Software_strobe
      | Hardware_strobe -> seg.n - k
      | Rate_generator -> seg.n - (k mod seg.n)
      | Square_wave ->
        (* 반주기마다 짝수로 내린 n 에서 2 씩 준다 *)
        let _, j, _ = square seg k in
        (seg.n land lnot 1) - (2 * j)
    in
    register t.encoding v

let out t ~tick =
  settle t ~tick;
  match t.run, t.mode with
  | Stopped _, Interrupt_on_terminal_count -> false
  | Stopped _,
    (Hardware_one_shot | Rate_generator | Square_wave | Software_strobe
    | Hardware_strobe) -> true
  | Counting { seg; _ }, mode ->
    let k = pulses seg tick in
    (match mode with
     | Interrupt_on_terminal_count ->
       (* 2바이트 카운트를 다시 쓰는 중이면 첫 바이트에서 OUT 이 낮아진다 *)
       t.lsb_written = None && k >= seg.n
     | Hardware_one_shot ->
       (* 트리거 다음 펄스에 낮아진다 *)
       let triggered = match seg.since with Some s -> tick >= s | None -> true in
       (not triggered) || k >= seg.n
     | Rate_generator ->
       (* GATE 가 낮으면 OUT 은 곧바로 높다. 카운트가 1 일 때 한 펄스 낮다. *)
       seg.since = None || k mod seg.n <> seg.n - 1
     | Square_wave ->
       let high, _, _ = square seg k in
       seg.since = None || high
     | Software_strobe | Hardware_strobe -> k <> seg.n)

let fresh_segment ~tick ~gate n =
  { n; low_first = false; base = 0; since = (if gate then Some (tick + 1) else None) }

let load t ~tick ~gate value =
  settle t ~tick;
  let n = natural t.encoding value in
  t.count_register <- Some n;
  match t.mode, t.run with
  | (Interrupt_on_terminal_count | Software_strobe), (Stopped _ | Counting _) ->
    (* 다음 펄스에 실리고 셈이 새 카운트에서 이어진다 *)
    t.run <- Counting { seg = fresh_segment ~tick ~gate n; reload = None }
  | (Hardware_one_shot | Hardware_strobe), (Stopped _ | Counting _) ->
    (* 무장만 한다. 지금 도는 원숏은 다음 트리거까지 그대로다. *)
    ()
  | (Rate_generator | Square_wave), Stopped _ ->
    t.run <- Counting { seg = fresh_segment ~tick ~gate n; reload = None }
  | (Rate_generator | Square_wave), Counting { seg = { since = None; _ }; _ } ->
    (* GATE 가 낮다 — 다음 상승이 이 카운트를 싣는다 *)
    ()
  | Rate_generator, Counting { seg = { since = Some _; _ } as seg; _ } ->
    let k = pulses seg tick in
    let at = k - (k mod seg.n) + seg.n in
    t.run <- Counting { seg; reload = Some { at; next_n = n; next_low_first = false } }
  | Square_wave, Counting { seg = { since = Some _; _ } as seg; _ } ->
    let k = pulses seg tick in
    let high, j, len = square seg k in
    (* 높은 반이 끝나면 낮은 반부터, 낮은 반이 끝나면 높은 반부터 *)
    t.run <-
      Counting { seg; reload = Some { at = k - j + len; next_n = n; next_low_first = high } }

let write t ~tick ~gate v =
  let v = v land 0xff in
  match t.access with
  | Lsb_only -> load t ~tick ~gate v
  | Msb_only -> load t ~tick ~gate (v lsl 8)
  | Lsb_then_msb ->
    (match t.lsb_written with
     | None -> t.lsb_written <- Some v
     | Some lsb ->
       t.lsb_written <- None;
       load t ~tick ~gate (lsb lor (v lsl 8)))

let read t ~tick =
  let v = match t.latched with Some v -> v | None -> current_count t ~tick in
  match t.access with
  | Lsb_only -> t.latched <- None; v land 0xff
  | Msb_only -> t.latched <- None; (v lsr 8) land 0xff
  | Lsb_then_msb ->
    if t.msb_read_next then begin
      t.msb_read_next <- false;
      t.latched <- None;
      (v lsr 8) land 0xff
    end else begin
      t.msb_read_next <- true;
      v land 0xff
    end

(* 제어 워드 bit 3-1. X10 은 모드 2, X11 은 모드 3 이다. 받는 값은 3비트라
   마지막 갈래는 5 뿐이다. *)
let mode_of_bits = function
  | 0 -> Interrupt_on_terminal_count
  | 1 -> Hardware_one_shot
  | 2 | 6 -> Rate_generator
  | 3 | 7 -> Square_wave
  | 4 -> Software_strobe
  | _ -> Hardware_strobe

let mode_code = function
  | Interrupt_on_terminal_count -> 0
  | Hardware_one_shot -> 1
  | Rate_generator -> 2
  | Square_wave -> 3
  | Software_strobe -> 4
  | Hardware_strobe -> 5

(* 제어 워드 bit 5-4 가운데 0(래치)이 아닌 것. 마지막 갈래는 3 뿐이다. *)
let access_of_bits = function
  | 1 -> Lsb_only
  | 2 -> Msb_only
  | _ -> Lsb_then_msb

let access_code = function Lsb_only -> 1 | Msb_only -> 2 | Lsb_then_msb -> 3

let control t ~tick v =
  match (v lsr 4) land 3 with
  | 0 ->
    (* 읽기 전에 다시 래치하면 무시한다 — 처음 래치한 값을 읽는다 *)
    (match t.latched with
     | Some _ -> ()
     | None -> t.latched <- Some (current_count t ~tick))
  | rw ->
    let ce = current_count t ~tick in
    t.access <- access_of_bits rw;
    t.mode <- mode_of_bits ((v lsr 1) land 7);
    t.encoding <- (if v land 1 = 0 then Binary else Bcd);
    t.lsb_written <- None;
    t.msb_read_next <- false;
    t.latched <- None;
    t.count_register <- None;
    t.run <- Stopped ce

let trigger t ~tick =
  match t.count_register with
  | None -> ()
  | Some n -> t.run <- Counting { seg = fresh_segment ~tick ~gate:true n; reload = None }

let gate_edge t ~tick ~high =
  settle t ~tick;
  match t.mode with
  | Interrupt_on_terminal_count | Software_strobe ->
    (match t.run with
     | Stopped _ -> ()
     | Counting { seg; reload } ->
       (* GATE 는 셈만 막고 OUT 은 건드리지 않는다 *)
       let seg =
         if high then
           match seg.since with Some _ -> seg | None -> { seg with since = Some tick }
         else { seg with base = pulses seg tick; since = None }
       in
       t.run <- Counting { seg; reload })
  | Hardware_one_shot | Hardware_strobe ->
    (* 상승이 트리거다. 하강은 아무것도 안 한다. *)
    if high then trigger t ~tick
  | Rate_generator | Square_wave ->
    (* 상승은 다음 펄스에 초기 카운트를 다시 싣는다 *)
    if high then trigger t ~tick
    else (
      match t.run with
      | Stopped _ -> ()
      | Counting { seg; _ } ->
        (* 셈을 멈춘다. OUT 은 [out] 이 높게 낸다. *)
        t.run <-
          Counting { seg = { seg with base = pulses seg tick; since = None }; reload = None })

(* ---------- snapshot ---------- *)

module C = Dos_snap_codec

let mode_r = C.range 0 5
let access_r = C.range 1 3
let natural_r = function
  | Binary -> C.range 1 binary_modulus
  | Bcd -> C.range 1 bcd_modulus

let put_option w f = function
  | None -> C.put_bool w false
  | Some v -> C.put_bool w true; f v

let get_option r f = if C.get_bool r then Some (f ()) else None

(* The full pattern fails the build when a field is added, until the
   snapshot carries it. Keep [write_state] and [read_state] in one order. *)
let write_state w t =
  let { mode; access; encoding; lsb_written; msb_read_next; latched;
        count_register; run } = t
  in
  let put what r v = C.put w ~what r v in
  let nat = natural_r encoding in
  put "pit mode" mode_r (mode_code mode);
  put "pit access" access_r (access_code access);
  C.put_bool w (match encoding with Binary -> false | Bcd -> true);
  put_option w (put "pit lsb" C.byte) lsb_written;
  C.put_bool w msb_read_next;
  put_option w (put "pit latch" C.word) latched;
  put_option w (put "pit count register" nat) count_register;
  match run with
  | Stopped ce -> C.put_bool w false; put "pit stopped count" C.word ce
  | Counting { seg = { n; low_first; base; since }; reload } ->
    C.put_bool w true;
    put "pit n" nat n;
    C.put_bool w low_first;
    put "pit base" C.count base;
    put_option w (put "pit since" C.count) since;
    put_option w
      (fun { at; next_n; next_low_first } ->
        put "pit reload at" C.count at;
        put "pit reload n" nat next_n;
        C.put_bool w next_low_first)
      reload

let read_state r t =
  let get what rng = C.get r ~what rng in
  t.mode <- mode_of_bits (get "pit mode" mode_r);
  t.access <- access_of_bits (get "pit access" access_r);
  t.encoding <- (if C.get_bool r then Bcd else Binary);
  let nat = natural_r t.encoding in
  t.lsb_written <- get_option r (fun () -> get "pit lsb" C.byte);
  t.msb_read_next <- C.get_bool r;
  t.latched <- get_option r (fun () -> get "pit latch" C.word);
  t.count_register <- get_option r (fun () -> get "pit count register" nat);
  t.run <-
    (if C.get_bool r then begin
       let n = get "pit n" nat in
       let low_first = C.get_bool r in
       let base = get "pit base" C.count in
       let since = get_option r (fun () -> get "pit since" C.count) in
       let reload =
         get_option r (fun () ->
             let at = get "pit reload at" C.count in
             let next_n = get "pit reload n" nat in
             let next_low_first = C.get_bool r in
             { at; next_n; next_low_first })
       in
       Counting { seg = { n; low_first; base; since }; reload }
     end
     else Stopped (get "pit stopped count" C.word))
