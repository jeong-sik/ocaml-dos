# RFC 0002: 스냅숏 형식을 번호 대신 스키마 지문으로

- 상태: 제안
- 날짜: 2026-11-13
- 범위: `lib/dos_snap_codec.ml`, `lib/dos_snapshot.ml`, `test/dos_snapshot_test.ml`
- 관련: #68

## 왜

스냅숏을 바꾸는 PR은 `format_version`을 손으로 하나 올린다.
동시에 열린 PR이 같은 번호를 고른다.

2026-11-13에 열린 PR 38개의 `lib/dos_snapshot.ml`을 읽어 셌다.

| format | PR |
|---|---|
| 6 (main) | 나머지 29개 |
| 7 | #54 #57 #58 #69 #70 #72 #81 |
| 8 | #55 #60 |

7을 고른 PR 가운데 #54·#57·#69·#70·#81은 서로 위에 쌓이지 않았다.
다섯 PR이 서로 다른 필드를 더하고 같은 번호를 쓴다.
8을 고른 #55와 #60도 둘 다 #54에서 갈라져, 서로 다른 내용에 같은 번호를 쓴다.

복원은 번호만 본다(`restore`의 `format <> format_version`).
그래서 번호가 같고 내용이 다른 스냅숏을 받으면 필드를 엉뚱하게 읽는다.
범위 검사에 걸리면 `Corrupt`가 되고, 우연히 통과하면 틀린 기계가 된다.

번호를 피하려고 상태를 게스트 메모리에 두는 PR도 생겼다(#85~#88의 SDA).
그 자리는 실기와도 맞아서 그대로 두어도 된다.
하지만 앞으로 호스트에만 둘 수 있는 상태(XMS 블록, 기다리는 INT 15h 86h)는 번호를 피할 수 없다.

## 지금 구조

- `Dos_snap_codec`은 정수·불·문자열·바이트를 쓰고 읽는다. 정수마다 범위를 양쪽에서 같은 값으로 검사한다.
- `Dos_snapshot`은 `write_payload`와 `read_payload`를 거울처럼 손으로 쓴다.
  - 리스트는 `put_list`/`get_list`가 개수와 원소를 쓴다.
  - variant는 태그 정수를 쓰고 읽는다. 태그 범위(`C.range 0 3`)가 양쪽에 따로 있고, 읽기 쪽은 `| _ -> C.fail`로 끝난다.
  - 핸들 공유는 `handle_pool`이 `==`를 인덱스로 바꿔 쓴다.
- 헤더: magic, format 번호(8바이트), 코어 digest(보여 주기만), 본문 MD5, 본문.

번호와 필드 목록 사이에 기계적인 연결이 없다.
쓰기와 읽기 사이에도 없다. 둘이 어긋나면 왕복 테스트가 그 경로를 지날 때만 잡힌다.

## 선택지

### A. 스키마를 값으로 (추천)

코덱을 `'a t` 값으로 만든다.
그 값 하나에서 쓰기, 읽기, 모양 문자열을 모두 뽑는다.
헤더에는 번호 대신 모양 문자열의 digest를 넣는다.

- 장점:
  - 번호를 고를 일이 없다. 필드를 더하거나 순서·이름·범위를 바꾸면 지문이 저절로 바뀐다.
  - 쓰기와 읽기가 어긋날 수 없다. 같은 값에서 나온다.
  - 리스트·option·variant는 모양이 값과 무관하다. 빈 리스트도, None도 지문에 들어간다. #68이 걱정한 "상태에 따라 안 쓰이는 필드" 문제가 없다.
  - 주석이나 코드 정리는 지문을 바꾸지 않는다.
- 단점:
  - `dos_snapshot.ml`(541줄)을 거의 다시 쓴다.
  - 뜻만 바뀌고 모양은 같은 변경은 잡지 못한다. 아래 `note`로 손으로 표시한다. 이 부분만 지금과 같은 수작업이 남는다.

### B. 소스 digest

`dos_snapshot.ml`과 코덱 소스의 digest를 형식으로 쓴다. `Dos_core_identity`가 이미 `lib/` 소스 digest를 빌드 때 만든다.

- 장점: 거의 공짜다. dune 규칙 하나다.
- 단점:
  - 주석 한 줄만 고쳐도 모든 스냅숏이 거절된다.
  - 쓰기·읽기가 어긋나는 문제는 그대로다.
  - `dos_state.ml`에서 필드 뜻을 바꾸면 스냅숏 소스가 그대로여서 지문도 그대로다.

### C. 지금처럼 두고 병합 순서대로 번호를 다시 매긴다

- 장점: 할 일이 없다.
- 단점: 위 표의 충돌이 계속 난다. 충돌을 푸는 사람이 번호를 잘못 고르면 같은 번호에 두 형식이 생긴다.

## 설계 (A)

### 1. 코덱 값

```ocaml
(* lib/dos_snap_codec.mli 에 더한다 *)
type 'a t

val int : what:string -> range -> int t
val bool : what:string -> bool t
val string : what:string -> string t
val bytes : what:string -> Bytes.t t
val int_array : what:string -> len:int -> range -> int array t
val list : what:string -> 'a t -> 'a list t
val option : what:string -> 'a t -> 'a option t
val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t
val tup2 : 'a t -> 'b t -> ('a * 'b) t     (* tup3 .. 필요한 만큼 *)
val note : string -> 'a t -> 'a t           (* 모양에만 들어가는 설명 *)

type ('a, 'b) case
val case : tag:int -> what:string -> 'b t -> ('b -> 'a) -> ('a, 'b) case
type 'a any_case = Any : ('a, 'b) case -> 'a any_case
type 'a selected
val select : ('a, 'b) case -> 'b -> 'a selected
val union : what:string -> 'a any_case list -> ('a -> 'a selected) -> 'a t

val write : 'a t -> writer -> 'a -> unit
val read : 'a t -> reader -> 'a
val shape : 'a t -> string
```

- variant는 `union`으로 쓴다.
  - 쓰기는 `'a -> 'a selected` 하나가 모든 생성자를 `match`로 고른다. `_ ->`가 없어서, 생성자를 더하면 컴파일러가 빠진 곳을 잡는다.
  - 읽기는 case 목록에서 태그를 찾는다.
  - 태그가 겹치면 모듈을 만들 때 실패한다. 테스트 하나만 돌아도 드러난다.
- `shape`은 이름, 범위, 구조를 정해진 순서로 적은 문자열이다. 지문은 `Digest.string (shape machine)`이다.

### 2. 기계와 바이트 사이에 평범한 데이터를 둔다

```
Dos_state.t --capture--> Snapshot_data.t --codec--> bytes
bytes --codec--> Snapshot_data.t --rebuild--> Dos_state.t
```

- `Snapshot_data.t`는 불변 레코드다. 클로저, `Hashtbl`, 물리적 공유가 없다.
- `capture`는 지금의 `write_payload` 앞부분과 `handle_pool`이 하는 일이다.
  - `Hashtbl`은 정렬된 쌍 리스트로, `==` 공유는 인덱스로 바꾼다.
  - 지금처럼 `Dos_state.t`의 모든 필드를 패턴에 적는다. 필드를 더하면 이 함수에서 빌드가 실패한다.
- `rebuild`는 `Dos_machine.create ()`에 데이터를 부어 넣는다. 인덱스로 공유를 되살리고 `check_consistency`를 한다.
- 코덱은 `Snapshot_data.t` 하나만 안다. 구조만 다루므로 A의 지문이 온전하다.

### 3. 헤더

```
magic "OCAML-DOS-SNAPSHOT\001"   (끝 바이트를 \000 에서 바꾼다)
schema  16 바이트                 Digest.string (shape codec)
core    32 바이트                 지금처럼 보여 주기만
sum     16 바이트                 본문 MD5
payload
```

- magic이 다르면 `Old_format`으로 거절한다. "번호를 쓰던 옛 스냅숏"이라고 알려 준다.
- 지문이 다르면 `Wrong_schema { saved; expected }`로 거절한다. 둘 다 hex로 보여 준다.

### 4. 뜻이 바뀌는 변경

모양이 같아도 읽는 법이 바뀌면 그 필드에 `note`를 단다.

```ocaml
C.note "tick counts from the 0x46C origin of #80" (C.int ~what:"last tick" C.any)
```

- 두 PR이 서로 다른 `note`를 달면 지문도 다르다. 번호처럼 우연히 같아지지 않는다.
- `note`를 잊으면 지금과 같은 위험이 남는다. 리뷰 체크리스트에 "필드 뜻을 바꿨으면 note"를 넣는다.

## 단계

각 단계는 따로 PR이다. 단계 1~2 동안 바이트 형식은 그대로다.

1. **코덱 값 추가**: `'a t`와 조합자, `write`·`read`·`shape`를 더한다. 아직 아무도 쓰지 않는다.
   - 확인: 조합자마다 왕복 테스트. 필드 순서·이름·범위·태그를 바꾸면 `shape`이 바뀌는 테스트.
2. **`Snapshot_data` 도입**: `capture`·`rebuild`로 나누고 본문을 새 코덱으로 쓴다. 헤더는 아직 번호 6이다.
   - 확인: 같은 기계를 옛 코드와 새 코드로 저장한 본문 바이트가 같다. 기존 `dos_snapshot_test`, 삼국지3 세이브·로드(#49)가 그대로 통과한다.
3. **헤더를 지문으로**: magic과 헤더를 바꾸고 `format_version`을 지운다.
   - 확인: 형식 6 파일은 `Old_format`, 필드를 하나 바꾼 빌드의 파일은 `Wrong_schema`로 거절된다.
4. **열린 PR 옮기기**: 형식 7·8 PR(9개)은 번호 줄을 지우고 자기 필드를 코덱에 더한다.

## 위험

- 단계 2에서 바이트가 하나라도 달라지면 기존 세이브가 깨진다. "옛 코드와 같은 바이트" 테스트로 막는다.
- 성능: 저장·복원은 게임 루프에 없다. 1MB 메모리 블릿이 대부분이라 조합자 비용은 묻힌다. 단계 2에서 삼국지3 세이브 시간을 잰다.
- 단계 3이 지나면 형식 6 스냅숏은 모두 거절된다(hard cut, 변환기 없음). 번호를 올릴 때마다 이미 그랬으므로 새로운 손실은 아니다.

## 범위 밖

- 옛 형식을 읽는 변환기
- 스냅숏 압축, 부분 스냅숏
- 기계 밖 상태(dosboot 진단, `on_unserved` 관찰자)

## 정할 것

1. A·B·C 가운데 무엇으로 할지. 이 RFC는 A를 권한다.
2. 단계 3의 hard cut. MASC dos 레인에 오래 둔 형식 6 스냅숏이 있으면, 단계 3 전에 다시 저장할지 버릴지.
3. 순서. 단계 1~3을 형식 7·8 PR보다 먼저 넣을지, 그 PR들을 먼저 번호를 다시 매겨 넣고 뒤에 할지.

## 확인 방법 요약

| 단계 | 확인 |
|---|---|
| 1 | 조합자 왕복, 모양 변화 테스트 |
| 2 | 옛·새 본문 바이트 동일, 기존 스냅숏 테스트, 삼국지3 세이브·로드 |
| 3 | 형식 6 → `Old_format`, 바뀐 스키마 → `Wrong_schema`, 같은 빌드 왕복 |
| 4 | 각 PR의 기존 테스트, 서로 다른 필드를 더한 두 PR의 지문이 다름 |
