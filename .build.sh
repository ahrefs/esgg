#! /usr/bin/env bash
set -e -u

. ./ci-utils/prelude.sh
. ./ci-utils/opam-setup.sh

echo "-- install opam deps"
cmd opam install . --deps-only

echo "+++ build"
cmd make clean build

echo "++ test"
cmd make test check
