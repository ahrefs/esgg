.SUFFIXES:
.PHONY: build clean

build:
	ocamlbuild -use-ocamlfind -package yojson,extlib,devkit esgg.native

clean:
	ocamlbuild -clean
