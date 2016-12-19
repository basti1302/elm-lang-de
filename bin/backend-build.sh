#!/usr/bin/env bash
set -e
pushd `dirname $0`/.. > /dev/null
stack build
popd > /dev/null

