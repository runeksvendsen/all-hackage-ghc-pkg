#!/usr/bin/env bash

nix-shell --run '\
  cabal build all && ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/all-hackage-ghc-pkg-0.1.0.0/x/all-hackage-ghc-pkg/build/all-hackage-ghc-pkg/all-hackage-ghc-pkg "./data/good-custom.txt" > shell-tmp.nix; \
  nix-shell --arg nixpkgs "import ./nix/pkgs.nix {}" --argstr compiler "ghc962" shell-tmp.nix --run "ghc-pkg list" \
'
