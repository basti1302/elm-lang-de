#!/usr/bin/env bash
set -xe

cd /opt/elm-lang-de
git pull
docker pull basti1302/elmlangde-app

pushd docker > /dev/null
docker-compose --file prod.yml up -d
docker exec elmlangde-nginx cron || true
docker ps
popd > /dev/null

