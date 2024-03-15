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

First run `find-non-broken.sh` to get a list of all working Hackage packages. Note that this takes quite a while.

```bash
./find-non-broken.sh > data/good-custom.txt
```

Next run the Haskell exe that produces the .nix file for the shell. This exe is run via the `run.sh` script that provides the needed runtime dependencies.

```bash
./run.sh data/good-custom.txt
```
