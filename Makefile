.SUFFIXES:
.PHONY: build clean

build:
	ocamlbuild -use-ocamlfind -package yojson,extlib,devkit,atd,easy-format,jsonm esgg.native

clean:
	ocamlbuild -clean
