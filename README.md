# All Hackage packages in GHC package DB

Obtain a shell in which all working Hackage packages are installed into the GHC package DB.

## TODO

- [ ] Implement workaround/fix for https://github.com/NixOS/cabal2nix/issues/607
  -  Current workaround: filter from input file passed to exe: `grep -v -e '-if-0.1.0.0.drv' good.txt > good-2.txt`
- [ ] Filter off packages that fail to build when running nix-shell for the .nix file produced by cabal2nix
  -  ```
      The following packages are broken, either because they have a problem
      listed above, or because they depend on a broken package.
      hercules-ci-cnix-expr-0.3.5.1
      ```
  - Current workaround: filter from input file passed to exe: `grep -v -e '-hercules-ci-cnix-expr-0.3.5.1.drv' good-2.txt > good-3.txt`

## Steps

Run the Bash script produced by building [build.nix](build.nix) like so:

```bash
$(nix-build build.nix)/bin/build.sh
```

