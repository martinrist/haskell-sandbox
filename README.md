[![Build Status](https://travis-ci.org/martinrist/haskell-sandbox.svg?branch=master)](https://travis-ci.org/martinrist/haskell-sandbox)

This repository contains various samples, experiments and notes relating to Haskell.

# ['Haskell Programming from First Principles'](http://haskellbook.com)

- ✅ [Chapter 5 - Types](src/ProgrammingHaskell/Chapter05/README.md)
- ✅ [Chapter 6 - Typeclasses](src/ProgrammingHaskell/Chapter06/README.md)
- ✅ [Chapter 7 - More functional patterns](src/ProgrammingHaskell/Chapter07/README.md)
- ✅ [Chapter 8 - Recursion](src/ProgrammingHaskell/Chapter08/README.md)
- ✅ [Chapter 9 - Lists](src/ProgrammingHaskell/Chapter09/README.md)
- ✅ [Chapter 10 - Folding Lists](src/ProgrammingHaskell/Chapter10/README.md)
- ✅ [Chapter 11 - Algebraic Datatypes](src/ProgrammingHaskell/Chapter11/README.md)
- ✅ [Chapter 12 - Signalling Adversity](src/ProgrammingHaskell/Chapter12/README.md)
- ✅ [Chapter 13 - Building Projects](src/ProgrammingHaskell/Chapter13/README.md)
- ✅ [Chapter 14 - Testing](src/ProgrammingHaskell/Chapter14/README.md)
- ✅ [Chapter 15 - Monoid, Semigroup](src/ProgrammingHaskell/Chapter15/README.md)
- ✅ [Chapter 16 - Functor](src/ProgrammingHaskell/Chapter16/README.md)
- [Chapter 17 - Applicative](src/ProgrammingHaskell/Chapter17/README.md)
- [Chapter 18 - Monad](src/ProgrammingHaskell/Chapter18/README.md)
- [Chapter 20 - Foldable](src/ProgrammingHaskell/Chapter20/README.md)
- [Chapter 21 - Traversable](src/ProgrammingHaskell/Chapter21/README.md)
- [Chapter 22 - Reader](src/ProgrammingHaskell/Chapter22/README.md)
- [Chapter 23 - State](src/ProgrammingHaskell/Chapter23/README.md)
- [Chapter 24 - Parser Combinators](src/ProgrammingHaskell/Chapter24/README.md)
- [Chapter 25 - Composing Types](src/ProgrammingHaskell/Chapter25/README.md)
- [Chapter 26 - Monad Transformers](src/ProgrammingHaskell/Chapter26/README.md)
- [Chapter 27 - Non-strictness](src/ProgrammingHaskell/Chapter27/README.md)
- [Chapter 28 - Basic Libraries](src/ProgrammingHaskell/Chapter28/README.md)
- [Chapter 29 - IO](src/ProgrammingHaskell/Chapter29/README.md)
- [Chapter 30 - When things go wrong](src/ProgrammingHaskell/Chapter30/README.md)


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
stack ./install.hs help.             # To force GHC installation and see options
stack ./install.hs hie-8.6.4         # Update based on GHC version
stack ./install.hs build-doc-8.6.4   # Update based on GHC version
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
```
ghcid -c="stack ghci test/Spec.hs" -T=main
```

# TODO

- Get `hasktags` to regenerate tags files automatically on save (including extra
  metadata) - command to run is `hasktags -cx .` from project root.

- Investigate setting up more Vim shortcuts

- Work out how to install dev tools using `stack build --copy-compiler-tool ghc-mod ...` - how do we set up the `stack` environment (including `$PATH`) before using Vim?

- Other things suggested in [this page](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)
