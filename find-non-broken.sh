#!/usr/bin/env bash

trap "echo Exited! 1>&2; exit;" SIGINT SIGTERM

FILE="./nix/non-broken-haskell-packages.nix"
ATTR="non-broken"
TIMEOUT_SECONDS=3600

# TODO: build $numCpu drvs at a time?

STORE_PATHS=$(nix-instantiate -A $ATTR $FILE)

# For testing: pick `n` random derivations to build
if [ ! -z $1 ]; then
  STORE_PATHS=$(echo "$STORE_PATHS"|shuf -n $1)
fi

for f in $STORE_PATHS; do
  echo "##### Building $f ..."  1>&2
  nix-build --no-out-link --timeout $TIMEOUT_SECONDS $f  1>&2
  if [ $? -eq 0 ]; then
    echo $f
  fi
done
