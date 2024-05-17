let
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/640cf550ee5fac82f3701af64fa57af3b271117a.tar.gz";
      sha256 = "0161zqckqagfyq054x4vi2pxb6fr4akykwwikd1h3xx7c964007c";
    });
in
  unstable
