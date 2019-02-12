#! /usr/bin/env bash
set -e -u

. /shared/ci-utils/opam-setup.sh

echo "--- install opam deps"
# remove self to remove extra constraints
cmd opam remove esgg
cmd opam install . --deps-only

echo "+++ build"
cmd make clean build

echo "+++ test"
cmd make test check
