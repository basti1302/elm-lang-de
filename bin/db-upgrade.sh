#!/usr/bin/env bash
set -e
pushd `dirname $0`/.. > /dev/null
moo-postgresql upgrade
popd > /dev/null
