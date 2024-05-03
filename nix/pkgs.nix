let
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/runeksvendsen/nixpkgs/archive/c3a1749f84f968949551f825777b56171fe7a95f.tar.gz";
      sha256 = "1lr64nkkw08402x49q5l08sl85irg84ahlv39mvx7yqn9is00vj4";
    });
in
  unstable
