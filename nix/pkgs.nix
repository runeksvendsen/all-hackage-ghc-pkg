let
  unstable =
    import (builtins.fetchTarball {
      url = "https://github.com/runeksvendsen/nixpkgs/archive/b574a891f9d1172e455234b282774d588cd5c150.tar.gz";
      sha256 = "1j81mwzf7w7wyw9pkjbhhbr2hpvffnw062jlvgric2sk976dxfzr";
    });
in
  unstable
