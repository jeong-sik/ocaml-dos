(* BCD 조정 여섯: DAA·DAS·AAA·AAS·AAM·AAD. 돌려주는 값은 사이클 수다. *)

open Cpu86_state
open Cpu86_fault
open Cpu86_decode

(* DAA/DAS 의 두 번째 보정 경계. 8086 은 실칩 관측(아래 [daa] 주석), 286 은
   Intel 문서 그대로 99H 다. *)
let bcd_threshold t ~old_af ~nib =
  match t.model with
  | I8086 | I80186 -> if old_af && nib then 0x9F else 0x99
  | I80286 | I80386 -> 0x99

(* BCD 조정 4종(ZZT/TP 런타임 실측 요구: AAA). 두 번째 보정의 경계값은
   8086 에서 Intel 문서의 99H 가 아니라 실칩 관측에서 뽑았다: AF 가 이미
   서 있고 낮은 니블도 9 를 넘은 경우에만 9FH 이고, 나머지는 99H 다. 이
   갈래를 빼면 AL0 가 9A..9F 인 구간에서 실칩과 어긋난다
   (SingleStepTests/8086 27/2F 각 2000 케이스로 확인). 286 은 문서대로 늘
   99H 다(SingleStepTests/80286 27/2F). *)
let daa t =
  let al0 = reg8 t 0 in
  let old_cf = t.cf and old_af = t.af in
  let nib = al0 land 0x0F > 9 in
  if nib || old_af then begin
    set_reg8 t 0 (al0 + 6);
    t.af <- true
  end else t.af <- false;
  let threshold = bcd_threshold t ~old_af ~nib in
  if old_cf || al0 > threshold then begin
    set_reg8 t 0 (reg8 t 0 + 0x60);
    t.cf <- true
  end else t.cf <- false;
  let r = reg8 t 0 in
  t.zf <- r = 0; t.sf <- r land 0x80 <> 0; t.pf <- parity_even r;
  4

(* 286 의 DAS 는 AL-6 의 빌림도 CF 에 남긴다. Intel 의 의사코드
   "CF ← old_CF OR (Borrow from AL ← AL − 6)" 의 앞 절이다. 이
   빌림은 AL 에서 60H 를 더 빼게 하지는 않는다
   (SingleStepTests/80286 2F: AL=00, AF=1 → AL=FA, CF=1). *)
let das t =
  let al0 = reg8 t 0 in
  let old_cf = t.cf and old_af = t.af in
  let nib = al0 land 0x0F > 9 in
  let borrow =
    if nib || old_af then begin
      set_reg8 t 0 (al0 - 6);
      t.af <- true;
      al0 < 6
    end else begin
      t.af <- false;
      false
    end
  in
  let threshold = bcd_threshold t ~old_af ~nib in
  if old_cf || al0 > threshold then begin
    set_reg8 t 0 (reg8 t 0 - 0x60);
    t.cf <- true
  end else
    t.cf <- (match t.model with I8086 | I80186 -> false | I80286 | I80386 -> borrow);
  let r = reg8 t 0 in
  t.zf <- r = 0; t.sf <- r land 0x80 <> 0; t.pf <- parity_even r;
  4

(* AAA/AAS 는 8086 에서 AL 과 AH 를 따로 고친다. 286 은 AX 에 0x106 을
   한 번에 더하고 빼서 AL±6 의 자리올림과 빌림이 AH 로 샌다
   (SingleStepTests/80286 37: DFFC → E102, 3F: 1D04 → 1B0E). *)
let aaa t =
  if reg8 t 0 land 0x0F > 9 || t.af then begin
    (match t.model with
     | I8086 | I80186 ->
       set_reg8 t 0 (reg8 t 0 + 6);
       set_reg8 t 4 (reg8 t 4 + 1)
     | I80286 | I80386 -> set_reg16 t 0 (reg16 t 0 + 0x106));
    t.af <- true; t.cf <- true
  end else begin
    t.af <- false; t.cf <- false
  end;
  set_reg8 t 0 (reg8 t 0 land 0x0F);
  8

let aas t =
  if reg8 t 0 land 0x0F > 9 || t.af then begin
    (match t.model with
     | I8086 | I80186 ->
       set_reg8 t 0 (reg8 t 0 - 6);
       set_reg8 t 4 (reg8 t 4 - 1)
     | I80286 | I80386 -> set_reg16 t 0 (reg16 t 0 - 0x106));
    t.af <- true; t.cf <- true
  end else begin
    t.af <- false; t.cf <- false
  end;
  set_reg8 t 0 (reg8 t 0 land 0x0F);
  8

(* aam base: AH=몫 AL=나머지. 플래그는 AL 기준이다. 십진 변환에 자주
   쓰인다. *)
let aam t ctx =
  let base = fetch8 t in
  let al = reg8 t 0 in
  if base = 0 then begin
    (* 0 으로 나누면 divide error 다. 8086 은 트랩 전에 결과 0 으로
       SF·ZF·PF 를 세워 두고, 그 플래그가 인터럽트 프레임에 밀린다
       — 인터럽트 핸들러가 보는 값이라 관측 가능한 차이다. 286 은
       AL 을 한 비트 오른쪽으로 민 값으로 세운다. SingleStepTests/
       80286 D4 의 aam 0 열한 건이 모두 이 규칙을 따른다(FFh → PF=0,
       97h → PF=1). 표본이 적어 마이크로코드의 이유는 모른다. *)
    let r = match t.model with I8086 | I80186 -> 0 | I80286 | I80386 -> al lsr 1 in
    t.zf <- r = 0; t.sf <- r land 0x80 <> 0; t.pf <- parity_even r;
    t.cf <- false; t.of_ <- false; t.af <- false;
    divide_error t ctx
  end
  else begin
    set_reg8 t 4 (al / base);
    set_reg8 t 0 (al mod base);
    let r = al mod base in
    t.zf <- r = 0;
    t.sf <- r land 0x80 <> 0;
    t.pf <- parity_even r
  end;
  15

(* aad base. 밑은 피연산자 바이트다 — 10 으로 굳히면 그 바이트를 안 먹어
   IP 가 한 칸 어긋난다. *)
let aad t =
  let base = fetch8 t in
  let v = ((reg8 t 4 * base) + reg8 t 0) land 0xff in
  set_reg8 t 0 v;
  set_reg8 t 4 0;
  t.sf <- v land 0x80 <> 0;
  t.zf <- v = 0;
  t.pf <- parity_even v;
  10
