module ProgrammingHaskell.Chapter17.Exercises.FixerUpper where


-- Question 1
answer1 :: Maybe String
answer1 = const <$> Just "Hello" <*> pure "World"


-- Question 2
answer2 :: Maybe (Int, Int, String, [Int])
answer2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]