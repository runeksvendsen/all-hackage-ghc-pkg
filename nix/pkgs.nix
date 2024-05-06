let
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/runeksvendsen/nixpkgs/archive/d1b139547d1cfacef42bfd4761461691811547fe.tar.gz";
      sha256 = "092zmbgsy3vgn4ly7g41f43npcrji54q00d622pivzhvgsy27gpz";
    });
in
  unstable
