#! /bin/bash

#set -x
#set -e

# cd self directory
cd "${0%/*}"

function check {
  name=${1%.query.json}
  echo $name
  (
  set -e
  ../esgg.native derive $(dirname $name)/mapping.json $name.query.json > check.atd
  cp check.atd $name.atd
  atdgen -t check.atd
  atdgen -j check.atd
  ocamlfind ocamlc -package atdgen,extlib -linkpkg check_t.mli check_t.ml check_j.mli check_j.ml run.ml -o run.byte
  ./run.byte < $name.result.json
  )
}

if [ $# -eq 0 ]; then
  for p in **/*.query.json ; do
    check $p
  done
  rm check.atd
else
  for i in $@; do
    check $i
  done
  # keep generated check.atd
fi

rm -f run.cm* run.byte
rm -f check_j.* check_t.*
