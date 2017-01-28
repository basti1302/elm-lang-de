#!/usr/bin/env bash
set -e

pushd `dirname $0`/../docker > /dev/null

docker-compose up -d

popd > /dev/null

