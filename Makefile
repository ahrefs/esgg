.SUFFIXES:
.PHONY: build clean test check

build:
	dune build esgg.exe

test: build
	./test/run.sh

check: build
	./check/run.sh

clean:
	dune clean
