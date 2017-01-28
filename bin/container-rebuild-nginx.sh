#!/usr/bin/env bash
set -e

pushd `dirname $0`/../docker > /dev/null

docker-compose stop elmlangde-nginx
docker-compose rm -f elmlangde-nginx
docker-compose build elmlangde-nginx
popd > /dev/null

