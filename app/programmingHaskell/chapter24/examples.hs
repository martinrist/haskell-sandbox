import           Control.Monad.Trans.State
import           Data.Ratio                ((%))
import           Text.Trifecta

-----------------------------------
-- Chapter 24 - State - Examples --
-----------------------------------
main :: IO ()
main = undefined



-- 24.3 - Understanding the parsing process
-------------------------------------------

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1' then die
one' :: Parser Char
one' = one >> stop

-- read a '1' then a '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- read a '1' then a '2' then die
oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

-- test a given parser against the string '123'
testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"


-- 24.4 - Parsing Fractions
---------------------------
slash :: Parser Char
slash = char '/'

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    slash
    denominator <- decimal
    return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    slash
    denominator <- decimal
    case denominator of
         0 -> fail "Denominator cannot be zero"
         _ -> return (numerator % denominator)

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"
