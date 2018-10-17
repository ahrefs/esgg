.SUFFIXES:
.PHONY: build clean test check

build:
	ocamlbuild -use-ocamlfind -package yojson,extlib,devkit,atd,easy-format,jsonm,ppx_deriving.std esgg.native

test: build
	./test/run.sh

check: build
	./check/run.sh

clean:
	ocamlbuild -clean
	cd check && ocamlbuild -clean
