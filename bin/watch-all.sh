#!/usr/bin/env bash
set -e

command -v entr >/dev/null 2>&1 || { echo >&2 "This script requires entr, seems you don't have that installed. Install it from http://entrproject.org. Aborting."; exit 1; }

pushd `dirname $0`/.. > /dev/null

find . -name \*.hs \
 -or -name \*.tpl \
 -or -name \*.cabal \
 | entr -c -r bin/backend-build-run.sh &

npm start

popd > /dev/null

