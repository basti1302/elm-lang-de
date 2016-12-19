#!/usr/bin/env bash
set -e

pushd `dirname $0`/.. > /dev/null

# postgres
psql -U postgres -q < backend/sql/delete_db.sql
sed                                            \
  s/$\{elmLangDeDatabasePassword\}/elmlangde/g \
  backend/sql/init_db.sql.template             \
  | psql -U postgres -q
bin/db-upgrade.sh

popd > /dev/null
