.SUFFIXES:
.PHONY: build clean

build:
	ocamlbuild -use-ocamlfind -package yojson,extlib,devkit,atd,easy-format esgg.native

clean:
	ocamlbuild -clean
