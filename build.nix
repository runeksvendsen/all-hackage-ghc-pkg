let
  pkgs = import ./nix/pkgs.nix {};
  shellNixFile = ./shell.nix;
in
pkgs.writeScriptBin "build.sh" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i bash ${shellNixFile}

    set -e

    cabal build all && $(cabal list-bin exe:all-hackage-ghc-pkg) "$1" > shell-tmp.nix
    nix-shell --arg nixpkgs 'import ./nix/pkgs.nix {}' --argstr compiler ghc962 shell-tmp.nix --run 'ghc-pkg list'
''