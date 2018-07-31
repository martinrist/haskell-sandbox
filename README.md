[![Build Status](https://travis-ci.org/martinrist/haskell-sandbox.svg?branch=master)](https://travis-ci.org/martinrist/haskell-sandbox)

This repository contains various samples, experiments and notes relating to Haskell.

# ['Haskell Programming from First Principles'](http://haskellbook.com)

- [Chapter 6 - Less Ad-hoc Polymorphism](src/programmingHaskell/chapter06/notes.md)
- [Chapter 7 - More functional patterns](src/programmingHaskell/chapter07/notes.md)
- [Chapter 8 - Recursion](src/programmingHaskell/chapter08/notes.md)
- [Chapter 9 - Lists](src/programmingHaskell/chapter09/notes.md)
- [Chapter 10 - Folding Lists](src/programmingHaskell/chapter10/notes.md)
- [Chapter 11 - Algebraic Datatypes](src/programmingHaskell/chapter11/notes.md)
- [Chapter 12 - Signalling Adversity](src/programmingHaskell/chapter12/notes.md)
- [Chapter 13 - Building Projects](src/programmingHaskell/chapter13/notes.md)
- [Chapter 14 - Testing](src/programmingHaskell/chapter14/notes.md)
- [Chapter 15 - Monoid, Semigroup](src/programmingHaskell/chapter15/notes.md)
- [Chapter 16 - Functor](src/programmingHaskell/chapter16/notes.md)
- [Chapter 17 - Applicative](src/programmingHaskell/chapter17/notes.md)
- [Chapter 18 - Monad](src/programmingHaskell/chapter18/notes.md)
- [Chapter 20 - Foldable](src/programmingHaskell/chapter20/notes.md)
- [Chapter 21 - Traversable](src/programmingHaskell/chapter21/notes.md)
- [Chapter 22 - Reader](src/programmingHaskell/chapter22/notes.md)
- [Chapter 23 - State](src/programmingHaskell/chapter23/notes.md)
- [Chapter 24 - Parser Combinators](src/programmingHaskell/chapter24/notes.md)
- [Chapter 25 - Composing Types](src/programmingHaskell/chapter25/notes.md)
- [Chapter 26 - Monad Transformers](src/programmingHaskell/chapter26/notes.md)
- [Chapter 27 - Non-strictness](src/programmingHaskell/chapter27/notes.md)
- [Chapter 28 - Basic Libraries](src/programmingHaskell/chapter28/notes.md)
- [Chapter 29 - IO](src/programmingHaskell/chapter29/notes.md)
- [Chapter 30 - When things go wrong](src/programmingHaskell/chapter30/notes.md)


# Environment Setup Notes

```bash
# Install base stack package
sudo apt install haskell-stack

# On OS X, install Homebrew, then:
brew install stack

# Install exuberant ctags for tagbar
brew install ctags

# Set up stack (outside project)
cd
stack setup

# The above command will start installing a later GHC
# so abort it and edit ~/.stack/global-project/stack.yaml
# to set resolver to lts-9.21
# This is to force the use of GHC 8.0.2 until ghc-mod supports
# later versions.  Run `stack setup` again to install GHC 8.0.2

# Install dev tools (from ~)
stack install hlint stylish-haskell hindent ghc-mod hdevtools hoogle hasktags hspec-discover

# Generate hoogle database
hoogle generate
```


# FAQs

## Compiler warnings with GHC 8.0.2 when first running `stack build`

Need to apply [this patch](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/ghc/ghc-8.0.2-no-cpp-warnings.patch) to the GHC 8.0.2 source.

The source files can be found in `$(stack path
--programs)/ghc-8.0.2/lib/ghc-8.0.2/include`



# TODO

- Get `hasktags` to regenerate tags files automatically on save (including extra
  metadata).

- Work out why `syntastic` and `vim-gitgutter` icons are going wrong when
  `stylish-haskell` is installed

- Do we need both `hdevtools` and `ghc-mod` in Vim?  Investigate just using
  GhcModCheck and GhcModLint in place of Syntastic + hdevtools + hlint, since
  the former gives a proper QuickFix list that's formatted better.  Perhaps
  switch Syntastic to package mode?  Note that you don't get handy gutter marks
  without Syntastic

- Investigate setting up more Vim shortcuts

- Work out how to install dev tools using `stack build --copy-compiler-tool ghc-mod ...` - how do we set up the `stack` environment (including `$PATH`) before using Vim?

- Other things suggested in [this page](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)

- Investigate why there are problems with the above setup when using versions of
  the stack resolver later than 9.21 / GHC later than 8.0.2

- Investigate using package.yaml in preference to .cabal files
    - Use https://github.com/yamadapc/hpack-convert
