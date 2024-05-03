let
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/1dc7345389a2baced186589cab9824cec3dc0281.tar.gz";
      sha256 = "0rbrbnscib7q8kgqilikk55wwsdgg0xwkwmfd7akz1di0cwh1xl1";
    });
in
  unstable
