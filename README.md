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


# Environment Setup Notes

## Ubuntu 16.04
```
# Install base stack package
sudo apt install haskell-stack

# Set up stack (outside project)
cd
stack setup

# Upgrade to latest version
# At time of writing - v1.3.2 - 181 packages updated)
stack upgrade

# Install dev tools (from ~)
stack install hlint stylish-haskell hindent ghc-mod
```
