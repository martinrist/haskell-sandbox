[![Build Status](https://travis-ci.com/martinrist/haskell-sandbox.svg?branch=master)](https://travis-ci.com/martinrist/haskell-sandbox)

This repository contains various samples, experiments and notes relating to Haskell.

# 'Thinking with Types' Notes

Some notes and exercises for the book ['Thinking with Types'](https://thinkingwithtypes.com):

- [Chapter 1 - The Algebra Behind Types](src/ThinkingWithTypes/Chapter1/README.md)

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

# Install `hspec-discover` for discovering spec files
cd haskell-sandbox
stack install hspec-discover

# Installing `haskell-ide-engine`
git clone https://github.com/haskell/haskell-ide-engine.git --recurse-submodules
cd haskell-ide-engine
stack ./install.hs help              # To force GHC installation and see options
stack ./install.hs hie-8.6.4         # Update based on GHC version
stack ./install.hs build-data        # To generate Hoogle DB
```



# Common Commands

## Running tests with `stack`
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

## Build `haddock` docs for dependencies
Need to do this for projects so that we get documentation for external dependencies in VS Code / `haskell-ide-engine`:
```
cd haskell-sandbox
stack haddock --keep-going
```


## Updating `haddock` docs for local code
Need to do this for VS Code / `haskell-ide-engine` to start showing up updated documentation on hover:
```
cd haskell-sandbox
stack haddock --haddock-internal --file-watch
```
Note: `--file-watch` allows docs to be regenerated when the source changes.



# Troubleshooting Notes

## Error building GHC 8.6.2 - dependency on libgmp

Need to `brew install gmp` first

## Compiler warnings with GHC 8.0.2 when first running `stack build`

Need to apply [this patch](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/ghc/ghc-8.0.2-no-cpp-warnings.patch) to the GHC 8.0.2 source.

The source files can be found in `$(stack path
--programs)/ghc-8.0.2/lib/ghc-8.0.2/include`