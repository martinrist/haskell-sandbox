import           Control.Monad.Trans.State
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
