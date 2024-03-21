#!/usr/bin/env bash

set -euo pipefail

DRV_LIST_FILE="drv-list-test.txt"
NUM_DRVS=10

mv "$DRV_LIST_FILE" "$DRV_LIST_FILE.bak"

### Step 1:
# Attempt to build $NUM_DRVS randomly picked Hackage libraries; write successfully built derivations to $DRV_LIST_FILE.
# Returns nix-shell command to enter a shell where the built libraries are installed into the package DB.
SHELL_CMD=$("$(nix-build --no-out-link --arg ciPickRandomCount $NUM_DRVS --arg drvListFile \"$DRV_LIST_FILE\" build.nix)/bin/build.sh")

# Fail if all of the randomly picked libraries failed to build
if ! grep --silent -F "/nix/store/" "$DRV_LIST_FILE" ; then
  echo "Derivation list file does not contain any derivations. Perhaps increase NUM_DRVS?"
  exit 1
fi

# Extract library names, including versio, from Nix store paths
PACKAGE_NAMES=$(sed -rn 's|/nix/store/[0-9a-z]+-([a-z0-9\.\-]+)\.drv|\1|p' "$DRV_LIST_FILE")
# List libraries installed into the package DB inside the shell produced by Step 1
SHELL_PACKAGES=$(eval "$SHELL_CMD --run 'ghc-pkg list --simple-output'")

# For each library successfully built in Step 1: assert that it's installed into the package DB inside the shell produced by Step 1
for pkg in $PACKAGE_NAMES; do
  if ! grep --silent -F "$pkg" <(echo "$SHELL_PACKAGES") ; then
    echo "FAIL: Missing package $pkg in shell package list: $SHELL_PACKAGES"
    exit 1
  fi
done
