# Chapter 1 - The Algebra Behind Types - Exercises

## Exercise 1.2-i

Determine the cardinality of `Either Bool (Bool, Maybe Bool) -> Bool`:

```haskell
  |Either Bool (Bool, Maybe Bool) -> Bool|
= |Bool| ^ |Either Bool (Bool, Maybe Bool)|
= |Bool| ^ (|Bool| + |(Bool, Maybe Bool)|)
= |Bool| ^ (|Bool| + (|Bool| * |Maybe Bool|))
= |Bool| ^ (|Bool| + (|Bool| * ( 1 + |Bool|)))
= 2 ^ (2 + (2 * (1 + 2)))
= 2 ^ 8
= 256
```


## Exercise 1.4-i

_Use Curry–Howard to prove that `(a^b)^c = a^(b×c)`.  That is, provide a
function of type `(b -> c -> a) -> (b, c) -> a`, and one of
`((b,c) -> a) -> b -> c - >a`.  Make sure they satisfy the equalities
`to . from = id` and `from . to = id`.  Do these functions remind you of
anything from Prelude?_

See [Exercises.hs](Exercises.hs)


## Exercsie 1.4-ii

_Give a proof of the exponent law that `a^b x a^c = a ^ (b + c)`_

See [Exercises.hs](Exercises.hs)


## Exercise 1.4-iii

_Prove `(a x b) ^ c = a^c x b^c`_

See [Exercises.hs](Exercises.hs)