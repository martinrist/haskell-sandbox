[![Build Status](https://travis-ci.org/martinrist/haskell-sandbox.svg?branch=master)](https://travis-ci.org/martinrist/haskell-sandbox)

This repository contains various samples, experiments and notes relating to Haskell.

# Environment Setup Notes

```bash
# Install base stack package
sudo apt install haskell-stack

# On OS X, install Homebrew, then:
brew install stack

# Install exuberant ctags and hasktags for tagbar
brew install ctags
stack install hasktags

# Installing `fzf`
brew install fzf
/usr/local/opt/fzf/install

# Installing `haskell-ide-engine`
git clone https://github.com/haskell/haskell-ide-engine.git --recurse-submodules
cd haskell-ide-engine
stack ./install.hs help              # To force GHC installation and see options
stack ./install.hs hie-8.6.4         # Update based on GHC version
stack ./install.hs build-data        # Update based on GHC version
```


# FAQs

## Error building GHC 8.6.2 - dependency on libgmp

Need to `brew install gmp` first

## Compiler warnings with GHC 8.0.2 when first running `stack build`

Need to apply [this patch](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/ghc/ghc-8.0.2-no-cpp-warnings.patch) to the GHC 8.0.2 source.

The source files can be found in `$(stack path
--programs)/ghc-8.0.2/lib/ghc-8.0.2/include`

## Test workflow
```
stack test --fast --file-watch
```

## Running tests with `ghcid`
Firstly, install `ghcid`:
```
stack install ghcid
```

To run:
```
ghcid --command "stack ghci haskell-sandbox:lib haskell-sandbox:test:haskell-sandbox-test" \
      --test "Main.main"
```

or, add the following to [`.ghcid`](.ghcid), to set default options and allow running with just `ghcid`:
```
--command "stack ghci haskell-sandbox:lib haskell-sandbox:test:haskell-sandbox-test" \
--test "Main.main"
```

# TODO

- Get `hasktags` to regenerate tags files automatically on save (including extra
  metadata) - command to run is `hasktags -cx .` from project root.

- Investigate setting up more Vim shortcuts

- Work out how to install dev tools using `stack build --copy-compiler-tool ghc-mod ...` - how do we set up the `stack` environment (including `$PATH`) before using Vim?

- Other things suggested in [this page](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)
