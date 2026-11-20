#!/bin/sh
# 연기 시험(test/games_smoke_test)이 쓰는 프리웨어 게임을 받아 DEST/<이름>/ 에 푼다.
# 쓰기: scripts/fetch-freeware-games.sh DEST
#       DOS_GAMES_DIR=DEST dune exec --root . test/games_smoke_test/games_smoke_test.exe
#
# 게임 파일은 저장소에 넣지 않는다. 받는 곳은 classicdosgames.com 이고, 각 게임의
# 배포 조건은 그 사이트의 licenses.html 에 적혀 있다. curl 과 unzip 이 필요하다.
set -eu

dest=${1:?usage: fetch-freeware-games.sh DEST}
base=https://classicdosgames.com/files/games

fetch() {
  name=$1
  url=$2
  skip=${3:-}
  mkdir -p "$dest/$name"
  tmp=$(mktemp "${TMPDIR:-/tmp}/game.XXXXXX")
  curl -fsSL "$url" -o "$tmp"
  # -j: 압축 안의 폴더를 펴서 게임 폴더 바로 아래에 둔다
  # skip: 풀지 않을 압축 안의 경로 패턴(소스 폴더가 게임 파일과 이름이 겹친다)
  if [ -n "$skip" ]; then
    unzip -o -q -j "$tmp" -x "$skip" -d "$dest/$name"
  else
    unzip -o -q -j "$tmp" -d "$dest/$name"
  fi
  rm -f "$tmp"
  echo "fetched $name"
}

# 설치본: 압축 안의 NAME.1 이 한 번 더 압축된 게임이다(DEICE 설치기가 풀던 것).
# 이 압축을 한 번 더 푼다.
fetch_installer() {
  name=$1
  url=$2
  inner=$3
  fetch "$name" "$url"
  unzip -o -q -j "$dest/$name/$inner" -d "$dest/$name"
}

fetch snarf   "$base/everettkaser/snarf26.zip"
fetch thor    "$base/apogee/cavethor.zip"
fetch xargon  "$base/epic/%24xargon.zip"
fetch kilo    "$base/epic/kilo.zip"
fetch jetpack "$base/adampedersen/jetpak15.zip"
fetch arctic  "$base/microfx/aa_freew.zip"
fetch bio     "$base/apogee/bmfreew.zip"
fetch mars    "$base/scenario/marsfree.zip"
fetch dark    "$base/scenario/da_freew.zip"
fetch_installer harry "$base/subzero/1hh12.zip" HARRY1.1
fetch ken     "$base/kensilverman/%24ken.zip"
fetch supernova "$base/apogee/snova_fw.zip" 'source/*'
fetch titanic   "$base/apogee/tit_free.zip" 'source/*'
fetch arcy2     "$base/caps/1arcy.zip"
fetch charlie   "$base/wiering/charlie.zip"
fetch epicpinball "$base/epic/epicpn21.zip"
