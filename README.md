This repository contains various samples, experiments and notes relating to Haskell.

# ['Haskell Programming from First Principles'](http://haskellbook.com)

- [Chapter 6 - Less Ad-hoc Polymorphism](app/programmingHaskell/chapter06/notes.md)
- [Chapter 7 - More functional patterns](app/programmingHaskell/chapter07/notes.md)
- [Chapter 8 - Recursion](app/programmingHaskell/chapter08/notes.md)
- [Chapter 9 - Lists](app/programmingHaskell/chapter09/notes.md)
- [Chapter 10 - Folding Lists](app/programmingHaskell/chapter10/notes.md)
- [Chapter 11 - Algebraic Datatypes](app/programmingHaskell/chapter11/notes.md)
- [Chapter 12 - Signalling Adversity](app/programmingHaskell/chapter12/notes.md)
- [Chapter 13 - Building Projects](app/programmingHaskell/chapter13/notes.md)
- [Chapter 14 - Testing](app/programmingHaskell/chapter14/notes.md)
- [Chapter 15 - Monoid, Semigroup](app/programmingHaskell/chapter15/notes.md)
- [Chapter 16 - Functor](app/programmingHaskell/chapter16/notes.md)
- [Chapter 17 - Applicative](app/programmingHaskell/chapter17/notes.md)
- [Chapter 18 - Monad](app/programmingHaskell/chapter18/notes.md)
- [Chapter 20 - Foldable](app/programmingHaskell/chapter20/notes.md)
- [Chapter 21 - Traversable](app/programmingHaskell/chapter21/notes.md)
- [Chapter 22 - Reader](app/programmingHaskell/chapter22/notes.md)
- [Chapter 23 - State](app/programmingHaskell/chapter23/notes.md)
- [Chapter 24 - Parser Combinators](app/programmingHaskell/chapter24/notes.md)
- [Chapter 25 - Composing Types](app/programmingHaskell/chapter25/notes.md)
- [Chapter 26 - Monad Transformers](app/programmingHaskell/chapter26/notes.md)
- [Chapter 27 - Non-strictness](app/programmingHaskell/chapter27/notes.md)
- [Chapter 28 - Basic Libraries](app/programmingHaskell/chapter28/notes.md)
- [Chapter 29 - IO](app/programmingHaskell/chapter29/notes.md)


# Environment Setup Notes

```bash
# Install base stack package
sudo apt install haskell-stack

# On OS X, install Homebrew, then:
brew install haskell-stack

# Set up stack (outside project)
cd
stack setup

# Upgrade to latest version
# At time of writing - v1.3.2 - 181 packages updated)
stack upgrade

# Install dev tools (from ~)
stack install hlint stylish-haskell hindent ghc-mod hdevtools hoogle
```
