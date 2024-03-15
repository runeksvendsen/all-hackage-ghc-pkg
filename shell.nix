let
  release-23-05 =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/9a333eaa80901efe01df07eade2c16d183761fa3.tar.gz";
      sha256 = "0xhqjli4m9wkzv7xhs6fr1iajdjbv7xnj0bwvwldq9s6arlwkhj3";
    }) {};
  release-21-05 =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-21.05.tar.gz";
      sha256 = "12q00nbd7fb812zchbcnmdg3pw45qhxm74hgpjmshc2dfmgkjh4n";
    }) {};
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/4ecab3273592f27479a583fb6d975d4aba3486fe.tar.gz";
      sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
    }) {};
in
  release-23-05.mkShell {
    nativeBuildInputs = [
      unstable.haskell.compiler.ghc96
      unstable.cabal-install
      release-21-05.nix
      release-23-05.cabal2nix
    ];
}
