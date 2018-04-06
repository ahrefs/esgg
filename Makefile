.SUFFIXES:
.PHONY: build clean test

build:
	ocamlbuild -use-ocamlfind -package yojson,extlib,devkit,atd,easy-format,jsonm esgg.native

test:
	./check/run.sh

clean:
	ocamlbuild -clean
