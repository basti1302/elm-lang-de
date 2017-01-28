#!/usr/bin/env bash
set -e

pushd `dirname $0`/../docker > /dev/null
docker-compose stop elmlangde-app
docker-compose rm -f elmlangde-app
cd ../frontend
npm run clean-make
cd ..
stack image container
popd > /dev/null

