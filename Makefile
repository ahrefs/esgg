.SUFFIXES:
.PHONY: build clean test check

build:
	ocamlbuild -use-ocamlfind -package yojson,extlib,devkit,atd,easy-format,jsonm,ppx_deriving.std esgg.native

test:
	./test/run.sh

check:
	./check/run.sh

clean:
	ocamlbuild -clean
