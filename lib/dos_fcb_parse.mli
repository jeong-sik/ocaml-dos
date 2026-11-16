(** INT 21h AH=29h — 글자열을 FCB 의 드라이브·이름·확장자로 나눈다.

    MS-DOS 4.0 [src/DOS/FCB.ASM] 의 [MakeFcb] 를 옮겼다. 고친 버그도 그대로
    둔다: 이름이 8 글자(확장자는 3 글자)를 넘으면 넘는 글자를 버리면서
    끝 문자까지 계속 읽는다("UGH!!! Horrible bug here"). 2 바이트 문자(DBCS)
    갈래는 없다 — 미국판 빌드다. *)

type options = {
  skip_separator : bool;  (** AL bit 0 — 앞 공백 뒤 구분자 하나를 건너뛴다 *)
  keep_drive : bool;      (** AL bit 1 — 드라이브가 없으면 FCB 를 그대로 둔다 *)
  keep_name : bool;       (** AL bit 2 — 이름이 없으면 그대로 둔다 *)
  keep_extension : bool;  (** AL bit 3 — 확장자가 없으면 그대로 둔다 *)
}

val options_of_al : int -> options

type outcome = {
  al : int;       (** 00h 와일드카드 없음, 01h 있음, FFh 드라이브가 틀림 *)
  next : int;     (** 이름 다음 글자의 오프셋(16 비트) — 호출자의 SI *)
}

val parse :
  options ->
  drive_exists:(int -> bool) ->
  read:(int -> int) ->
  write:(int -> int -> unit) ->
  int ->
  outcome
(** [parse opts ~drive_exists ~read ~write si] 는 오프셋 [si] 부터
    [read] 로 글자를 읽고 FCB 의 [0..15] 바이트를 [write] 로 쓴다.
    [drive_exists n] 은 드라이브 번호 [n](A=1)이 있는지 답한다. FCB 는
    읽지 않는다 — 남기라고 한 칸은 쓰지 않을 뿐이다. 읽기와 쓰기는 DOS 와
    같은 순서로 일어난다. *)

