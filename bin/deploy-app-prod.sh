#!/usr/bin/env bash
set -xe

pushd `dirname $0`/.. > /dev/null

bin/container-rebuild-app.sh
docker push basti1302/elmlangde-app

ssh elm-lang.de /opt/elm-lang-de/bin/remote/deploy.sh
