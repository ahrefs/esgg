#! /usr/bin/env bash
set -e -u

. /shared/ci-utils/opam-setup.sh

echo "--- install opam deps"
cmd opam install . --deps-only

echo "+++ build"
cmd make clean build

echo "+++ test"
cmd make test check
