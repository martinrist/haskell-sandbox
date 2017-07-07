import           Control.Applicative
import           Data.Ratio              ((%))
import           Text.Parser.Combinators
import           Text.Trifecta

----------------------------
-- Chapter 23 - Exercises --
----------------------------

main :: IO ()
main = undefined


-- Exercises: Parsing Practice
------------------------------
stop :: Parser a
stop = unexpected "stop"

-- Question 1 - eof
one :: Parser Char
one = char '1'

one' :: Parser ()
one' = one >> eof

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser ()
oneTwo' = oneTwo >> eof


-- Question 2 - string
oneTwoThree :: Parser Char
oneTwoThree = oneTwo >> char '3'

oneTwoThree' :: Parser String
oneTwoThree' = string "123" <|> string "12" <|> string "1"


-- Question 3 - `string` from `char`
string' :: String -> Parser String
string' = mapM char



-- Exercise: Unit of Success
----------------------------

parseInteger :: Parser Integer
parseInteger = integer <* eof


-- Exercise: Try Try

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
         0 -> fail "Denominator cannot be zero"
         _ -> return (numerator % denominator)

type FractionOrInteger = Either Rational Integer

parseFractionOrInteger :: Parser FractionOrInteger
parseFractionOrInteger = try (Left <$> parseFraction) <|> (Right <$> integer)