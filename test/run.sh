#! /bin/bash

#set -x
#set -e

# cd self directory
cd "${0%/*}"

cmd () { if ! "$@" ; then printf "FAILED: %s\n" "$*" >&2; exit 2; fi }

function run() {
  dir=$(dirname $1)
  echo -n "$dir ... "
  if [ -e $dir/fail ]; then
    if out=$(../_build/default/esgg.exe output $dir/mapping.json $dir/query.json 2>&1); then
      printf "FAILED: %s: expected esgg to reject this query\n" "$dir" >&2; exit 2
    fi
    if ! grep -qF "$(cat $dir/fail)" <<< "$out"; then
      printf "FAILED: %s: error message mismatch, got: %s\n" "$dir" "$out" >&2; exit 2
    fi
    echo "ok (rejected)"
    return
  fi
  (
  set -e
  cmd ../_build/default/esgg.exe output $dir/mapping.json $dir/query.json > $dir/output.atd
  cmd ../_build/default/esgg.exe vars $dir/mapping.json $dir/query.json > $dir/input.atd
  cmd ../_build/default/esgg.exe input_j $dir/mapping.json $dir/query.json > $dir/query.ml
  ) && echo "ok"
}

cmd dune build ../esgg.exe

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
