let
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/ca6912ef4e004e09637dcbeed71dd352f11aaa76.tar.gz";
      sha256 = "06s217ci10z75ab9w88qpl9wjbsjk28pz3dgl21hxfa0az71q71v";
    });
in
  unstable
