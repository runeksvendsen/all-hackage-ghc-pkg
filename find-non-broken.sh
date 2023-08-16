#!/usr/bin/env bash

trap "echo Exited! 1>&2; exit;" SIGINT SIGTERM

FILE="./nix/non-broken-haskell-packages.nix"
ATTR="non-broken"
TIMEOUT_SECONDS=3600

for f in $(nix-instantiate -A $ATTR $FILE); do
  echo "##### Building $f ..."  1>&2
  nix-build -j8 --timeout $TIMEOUT_SECONDS $f  1>&2
  if [ $? -eq 0 ]; then
    echo $f
  fi
done
