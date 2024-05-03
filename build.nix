# Produce a shell.nix file with all buildable Hackage packages installed into the GHC package database

{ ciPickRandomCount ? null # For testing/CI: if not null then this number of random derivations will be picked out from all avaiable
, drvListFile ? "drv-list.txt" # Write the list of successfully built Haackage package derivations to this file
, shellFile ? "shell-tmp.nix" # The name of the Nix shell file produced
}:
let
  pkgs = import ./nix/pkgs.nix {};
  shellNixFile = ./shell.nix;
  shellCmd = ''nix-shell --arg nixpkgs 'import ./nix/pkgs.nix {}' --argstr compiler ghc965 ${shellFile}'';
  doneMessage = "Done! Run the following command to enter the shell:";
  findNonBrokenArg = if ciPickRandomCount != null then toString ciPickRandomCount else "";
in
pkgs.writeScriptBin "build.sh" ''
  #! /usr/bin/env nix-shell
  #! nix-shell -i bash ${shellNixFile}

  set -euox pipefail

  if [ ! -f "${drvListFile}" ]; then
    # WORKAROUND: https://github.com/NixOS/cabal2nix/issues/607
    ./find-non-broken.sh ${findNonBrokenArg} | grep --line-buffered -v -e '-if-0.1.0.0.drv' > ${drvListFile}
  else
    echo "INFO: The file ${drvListFile} already exists. Skipping 'find non-broken'-step. Remove the file to run this step." >&2
  fi
  cabal update >&2
  cabal build all >&2
  $(cabal list-bin exe:all-hackage-ghc-pkg) ${drvListFile} > ${shellFile}
  # WORKAROUND: see README.md
  sed -e 's/hercules-ci-cnix-expr, //' -e 's/hercules-ci-cnix-expr//' -i shell-tmp.nix
  ${shellCmd} --run "echo -e \"${doneMessage}\" >&2; echo -n \"${shellCmd}\""
''
