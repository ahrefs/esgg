#! /bin/bash

#set -x
#set -e

function check {
  name=$1
  dir=$(dirname $name)
  echo $name
  ./esgg.native derive $dir/mapping.json $name > check.atd
  atdgen -t check.atd
  atdgen -j check.atd
  ocamlfind ocamlc -package atdgen,extlib -linkpkg check_t.mli check_t.ml check_j.mli check_j.ml run_check.ml -o run_check.byte
  ./run_check.byte < $(echo $name | sed s%/q%/r%)
}

if [ $# -eq 0 ]; then
  for dir in check/*; do
    for name in $dir/q*.json ; do
      check $name
    done
  done
  rm check.atd
else
  for i in $@; do
    check $i
  done
  # keep generated check.atd
fi

rm -f run_check.cm* run_check.byte
rm -f check_j.* check_t.*
