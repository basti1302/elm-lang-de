#!/usr/bin/env bash
set -e

# Starts the the app just like bin/backend-build-run.sh, just without
# DEVELOPMENT_MODE=true, so front end assets are served from dist. The intention
# is to test the production webpack build.

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
HOST=127.0.0.1 \
  SECURE_COOKIES_DISABLED=true \
  GITHUB_REDIRECT_URL=http://localhost:7000/oauth/github \
  stack exec elm-lang-de

popd > /dev/null

