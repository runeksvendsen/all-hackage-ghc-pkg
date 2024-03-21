#!/usr/bin/env bash

set -euo pipefail

DRV_LIST_FILE="drv-list-test.txt"
NUM_DRVS=10

mv "$DRV_LIST_FILE" "$DRV_LIST_FILE.bak"

SHELL_CMD=$("$(nix-build --no-out-link --arg ciPickRandomCount $NUM_DRVS --arg drvListFile \"$DRV_LIST_FILE\" build.nix)/bin/build.sh")

if ! grep --silent -F "/nix/store/" "$DRV_LIST_FILE" ; then
  echo "Derivation list file does not contain any derivations. Perhaps increase NUM_DRVS?"
  exit 1
fi

PACKAGE_NAMES=$(sed -rn 's|/nix/store/[0-9a-z]+-([a-z0-9\.\-]+)\.drv|\1|p' "$DRV_LIST_FILE")
SHELL_PACKAGES=$(eval "$SHELL_CMD --run 'ghc-pkg list --simple-output'")

for pkg in $PACKAGE_NAMES; do
  if ! grep --silent -F "$pkg" <(echo "$SHELL_PACKAGES") ; then
    echo "FAIL: Missing package $pkg in shell package list: $SHELL_PACKAGES"
    exit 1
  fi
done

