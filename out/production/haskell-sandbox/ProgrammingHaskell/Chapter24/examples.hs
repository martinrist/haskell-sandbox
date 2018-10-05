{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text (parseOnly)
import           Data.Ratio           ((%))
import           Data.String          (IsString)
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


-- 24.6 - Alternative
---------------------

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNoS :: Parser NumberOrString
parseNoS = (Left <$> integer) <|> (Right <$> some letter)



-- 24.9 - Polymorphic Parsers
-----------------------------

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"


-- Polymorphic version of parseFraction :: Parser Rational
polyParseFraction :: (Monad m, TokenParsing m) => m Rational
polyParseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
         0 -> fail "Denominator cannot be zero"
         _ -> return (numerator % denominator)


runAttoparsec :: IO ()
runAttoparsec = do
    -- parseOnly is from Attoparsec
    print $ parseOnly polyParseFraction badFraction
    print $ parseOnly polyParseFraction shouldWork
    print $ parseOnly polyParseFraction shouldAlsoWork
    print $ parseOnly polyParseFraction alsoBad

runTrifecta :: IO ()
runTrifecta = do
    -- parseString is from Trifecta
    print $ parseString polyParseFraction mempty badFraction
    print $ parseString polyParseFraction mempty shouldWork
    print $ parseString polyParseFraction mempty shouldAlsoWork
    print $ parseString polyParseFraction mempty alsoBad

