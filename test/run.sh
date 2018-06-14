#! /bin/bash

#set -x
#set -e

# cd self directory
cd "${0%/*}"

cmd () { if ! "$@" ; then printf "FAILED: %s\n" "$*" >&2; exit 2; fi }

function run() {
  dir=$(dirname $1)
  echo -n "$dir ... "
  (
  set -e
  cmd ../esgg.native output $dir/mapping.json $dir/query.json > $dir/output.atd
  cmd ../esgg.native vars $dir/mapping.json $dir/query.json > $dir/input.atd
  ) && echo "ok"
}

if [ $# -eq 0 ]; then
  for p in */mapping.json ; do
    run "$p"
  done
else
  for i in "$@"; do
    run "$i"
  done
fi

cmd git diff --no-ext-diff --quiet --exit-code */*
