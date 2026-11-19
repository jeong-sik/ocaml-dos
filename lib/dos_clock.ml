(* MS-DOS 4.0 의 시각과 날짜 계산. 시각은 BIOS 틱에서, 날짜는
   1980-01-01 부터 센 날 수(DAYCNT)에서 나온다.
   src/BIOS/MSCLOCK.ASM TIM$READ·TIME_TO_TICKS, src/DOS/TIME.ASM. *)

type time = { hour : int; minute : int; second : int; hundredths : int }
type date = { year : int; month : int; day : int }

let hundredths_per_second = 100
let seconds_per_minute = 60
let minutes_per_hour = 60
let hours_per_day = 24

(* 틱 하나는 65536 / 1193180 초다. MSCLOCK.ASM 은 이것을
   5 × 65536 / 59659 (1/100 초)로 줄여 쓴다. *)
let tick_numerator = 5 * 65536
let tick_denominator = 59659

(* TIM$READ: 1/100 초 = ⌊틱 × 5 × 65536 / 59659⌋ 을 시·분·초·1/100 로 *)
let time_of_ticks ticks =
  let total = ticks * tick_numerator / tick_denominator in
  let seconds = total / hundredths_per_second in
  let minutes = seconds / seconds_per_minute in
  { hour = minutes / minutes_per_hour;
    minute = minutes mod minutes_per_hour;
    second = seconds mod seconds_per_minute;
    hundredths = total mod hundredths_per_second }

(* TIME_TO_TICKS: ⌊⌊1/100 초 × 59659 / 65536⌋ / 5⌋ — 곱의 아래 16 비트를
   버린 뒤 5 로 나눈다. 두 번 내림한 나눗셈은 곱한 수로 한 번 나눈 것과
   같다. *)
let ticks_of_time { hour; minute; second; hundredths } =
  let total =
    ((((hour * minutes_per_hour) + minute) * seconds_per_minute) + second)
    * hundredths_per_second + hundredths
  in
  total * tick_denominator / tick_numerator

(* $SET_TIME 의 검사: CH<24, CL<60, DH<60, DL<100 *)
let time_of_fields ~hour ~minute ~second ~hundredths =
  let within n limit = n >= 0 && n < limit in
  if within hour hours_per_day && within minute minutes_per_hour
     && within second seconds_per_minute && within hundredths hundredths_per_second
  then Some { hour; minute; second; hundredths }
  else None

(* ---------- 날짜 ---------- *)

let first_year = 1980
let last_year = 2099                       (* $SET_DATE: CX-1980 > 119 이면 거절 *)
let first_weekday = 2                      (* 1980-01-01 은 화요일 *)
let days_per_week = 7
let month_days = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]   (* MONTAB *)
let leap_year_days = 366
let common_year_days = 365

(* SETYEAR: (연도 − 1980) 의 아래 두 비트가 0 이면 윤년. 2100 년을 따로
   보지 않는다 — 2099 년까지만 받는다. *)
let is_leap year = (year - first_year) land 3 = 0

let year_days year = if is_leap year then leap_year_days else common_year_days

let days_in_month ~year month =
  if month = 2 && is_leap year then month_days.(month - 1) + 1
  else month_days.(month - 1)

(* $SET_DATE·DODATE 의 검사 *)
let date_of_fields ~year ~month ~day =
  if year >= first_year && year <= last_year && month >= 1
     && month <= Array.length month_days && day >= 1
     && day <= days_in_month ~year month
  then Some { year; month; day }
  else None

let sum_from first last f =
  let rec go acc i = if i > last then acc else go (acc + f i) (i + 1) in
  go 0 first

(* DODATE: 1980-01-01 부터 센 날 수 *)
let days_of_date { year; month; day } =
  sum_from first_year (year - 1) year_days
  + sum_from 1 (month - 1) (days_in_month ~year)
  + (day - 1)

(* READTIME 의 DSLIDE: 해와 달을 차례로 빼 나간다 *)
let date_of_days days =
  let rec year_of y rest =
    if rest < year_days y then (y, rest) else year_of (y + 1) (rest - year_days y)
  in
  let year, rest = year_of first_year days in
  let rec month_of m rest =
    let n = days_in_month ~year m in
    if rest < n then (m, rest) else month_of (m + 1) (rest - n)
  in
  let month, rest = month_of 1 rest in
  { year; month; day = rest + 1 }

(* WKDAY: (DAYCNT + 2) mod 7, 0 이 일요일 *)
let weekday days = (days + first_weekday) mod days_per_week

(* READTIME 은 날 수가 120 년(FOURYEARS*30) 이상이면 날짜를 바꾸지 않는다.
   그래서 2099-12-31 다음 자정에도 날짜는 2099-12-31 에 머문다. *)
let four_years = (3 * common_year_days) + leap_year_days
let day_limit = four_years * 30

let next_day days = if days + 1 < day_limit then days + 1 else days
