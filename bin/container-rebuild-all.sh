#!/usr/bin/env bash
set -e

pushd `dirname $0`/../docker > /dev/null

source ../bin/create-secrets
sed                                                               \
  s/$\{elmLangDeDatabasePassword\}/$POSTGRES_ELMLANGDE_PASSWORD/g \
  ../backend/sql/init_db.sql.template                 \
  > ../backend/sql/init_db.sql
docker-compose stop
docker-compose rm -f
docker-compose build
rm ../backend/sql/init_db.sql
../bin/container-rebuild-app.sh
popd > /dev/null

