----------------------------
-- Chapter 27 - Exercises --
----------------------------

main :: IO ()
main = undefined


-- Exercises: Evaluate
----------------------

{-
    > const 1 undefined
    1

    > const undefined 1
    *** Exception: Prelude.undefined

    > flip const undefined 1
    1

    > flip const 1 undefined
    *** Exception: Prelude.undefined

    > const undefined undefined
    *** Exception: Prelude.undefined

    > foldr const 'z' ['a'..'e']
    'a'

    > foldr (flip const) 'z' ['a'..'e']
    'z'
-}


