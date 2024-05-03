let
  pkgs = import ./nix/pkgs.nix {};
in pkgs.haskell.compiler.ghc965