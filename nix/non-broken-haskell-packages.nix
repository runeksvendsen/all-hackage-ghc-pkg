{ pkgs ? import ./pkgs.nix { system = "x86_64-linux"; }
, get-hs-pkgs ? pkgs: pkgs.haskell.packages.ghc965
}:
let
  hs-pkgs = get-hs-pkgs pkgs;
  isNonBroken = name: x: !(x.meta.broken or false);
  hs-pkgs-filtered = pkgs.lib.attrsets.filterAttrs isNonBroken hs-pkgs;
  isNonBrokenDerivation = name: x:
    (x.type or "?") == "derivation" &&
      (builtins.tryEval (toString x)).success;
  isStandardVersion = name: x:
    !(pkgs.lib.hasInfix "_" name);
  non-broken =
    pkgs.lib.attrsets.filterAttrs
    (name: x: isStandardVersion name x && isNonBrokenDerivation name x)
    hs-pkgs-filtered;
in
{ inherit non-broken pkgs hs-pkgs; }
