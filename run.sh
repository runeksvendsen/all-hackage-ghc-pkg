#!/usr/bin/env bash

nix-shell --run "\
  cabal build all && $(cabal list-bin exe:all-hackage-ghc-pkg) \"$1\" > shell-tmp.nix && \
  nix-shell --arg nixpkgs 'import ./nix/pkgs.nix {}' --argstr compiler ghc962 shell-tmp.nix --run 'ghc-pkg list' \
"
