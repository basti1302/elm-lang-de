#!/usr/bin/env bash
set -e

pushd `dirname $0`/../docker > /dev/null

# For production we need to build the executable in an ephemeral Docker image,
# otherwise it will have the executable file format of the local architecture,
# that is, if you build on a Mac, the executable will have Mach-O format which
# can not be used in the production Docker container (which is Linux based, so
# we need an ELF executable).
#
# Furthermore, we do not only build the executable, but a complete Docker
# image that runs it: basti1302/elmlangde-app. Note that this latter Docker
# image (that will be used to run the app in production) has nothing to do with
# the first Docker container (the ephemeral one) which we use to build the Linux
# executable.

STACK_BINARY_RELATIVE_PATH=stack-binary/stack-1.3.2-linux-x86_64-static/stack
STACK_BINARY_ABSOLUTE_PATH=$(pwd)/$STACK_BINARY_RELATIVE_PATH
echo "I'll be using stack binary $STACK_BINARY_ABSOLUTE_PATH to build the app executable inside a Docker container to make sure the app executable has the ELF (Linux) executable file format and can be run in production."

docker-compose stop elmlangde-app
docker-compose rm -f elmlangde-app
cd ../frontend
npm run clean-make
cd ..
# build app executable and app Docker container with stack
stack \
  --system-ghc \
  --docker \
  --docker-auto-pull \
  --docker-stack-exe $STACK_BINARY_ABSOLUTE_PATH \
  image container

popd > /dev/null

