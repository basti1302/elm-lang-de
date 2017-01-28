#!/usr/bin/env bash

# Add the following line to /etc/rc.local
# /opt/elm-lang-de/bin/remote/start.sh
# to start all docker containers after boot.
set -e
pushd `dirname $0`/../../docker > /dev/null
docker-compose --file prod.yml up -d
popd > /dev/null

