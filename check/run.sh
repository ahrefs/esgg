#! /bin/bash

#set -x
#set -e

# cd self directory
cd "${0%/*}"

function check {
  name=${1%.query.json}
  dir=$(dirname $name)
  echo $name
  (
  set -e
  ../esgg.native derive $(basename $dir) $dir/mapping.json $name.query.json > check.atd
  cp check.atd $name.atd
  atdgen -t check.atd
  atdgen -j check.atd
  ocamlbuild -use-ocamlfind -pkg atdgen,extlib run.byte
  ./run.byte < $name.result.json
  )
}

if [ $# -eq 0 ]; then
  for p in **/*.query.json ; do
    check $p
  done
else
  for i in $@; do
    check $i
  done
  # keep generated check.atd
fi
