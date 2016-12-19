#!/usr/bin/env bash
set -e
pushd `dirname $0`/.. > /dev/null
stack exec moo-postgresql new $1
popd > /dev/null
