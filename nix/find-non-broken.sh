#!/usr/bin/env bash

trap "echo Exited!; exit;" SIGINT SIGTERM

FILE="non-broken-haskell-packages.nix"
ATTR="non-broken"
TIMEOUT_SECONDS=3600

cat good.txt >> good.txt.bak
cat bad.txt >> bad.txt.bak
rm good.txt
rm bad.txt

for f in $(nix-instantiate -A $ATTR $FILE); do
  echo "##### Building $f ..."
  nix-build -j8 --timeout $TIMEOUT_SECONDS $f
  if [ $? -eq 0 ]; then
    echo $f >> good.txt
  else
    echo $f >> bad.txt
  fi
done

