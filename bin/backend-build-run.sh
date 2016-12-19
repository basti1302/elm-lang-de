#!/usr/bin/env bash
set -e
pushd `dirname $0`/.. > /dev/null
stack build
MAIL_DISABLED=true SECURE_COOKIES_DISABLED=true stack exec elm-lang-de
popd > /dev/null

