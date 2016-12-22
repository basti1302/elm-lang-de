#!/usr/bin/env bash
set -e

# This file is intended to be used during development, not for production.

pushd `dirname $0`/.. > /dev/null

stack build

if [[ -f .github-secret ]]; then
  source .github-secret
else
  echo "Warning: The file .github-secret does not exist. Sign in via GitHub will not work. Take a look at the instructions in .github-secret.template."
fi

# Binding to host 127.0.0.1 prevents the macOS firewall from asking if it is
# okay to accept incoming connections every time the app is recompiled and
# restarted. On production, we use the default bind host.
HOST=127.0.0.1 SECURE_COOKIES_DISABLED=true stack exec elm-lang-de

popd > /dev/null

