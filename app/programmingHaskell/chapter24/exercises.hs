import           Control.Applicative
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