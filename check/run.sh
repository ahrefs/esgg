#! /bin/bash

#set -x
#set -e

# cd self directory
cd "${0%/*}"

cmd () { if ! "$@" ; then printf "FAILED: %s\n" "$*" >&2; exit 2; fi }

function check {
  name=${1%.query.json}
  dir=$(dirname $name)
  echo $name
  (
  set -e
  cmd ../_build/default/esgg.exe output -name $(basename $dir) $dir/mapping.json $name.query.json > check.atd
  cmd cp check.atd $name.atd
  cmd atdgen -t -open Mapping check.atd
  cmd atdgen -j -open Mapping check.atd
  cmd dune exec --root . ./run.bc -- < $name.result.json
  )
}

cmd dune build ../esgg.exe

if [ $# -eq 0 ]; then
  printf "include Common\n" > mapping.ml
  for p in **/mapping.json ; do
    name=$(basename $(dirname $p))
    printf "\n" >> mapping.ml
    cmd ../_build/default/esgg.exe reflect $name $p >> mapping.ml
  done
  for p in **/*.query.json ; do
    check $p
  done
else
  for i in $@; do
    check $i
  done
  # keep generated check.atd
fi
