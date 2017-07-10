{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- Example of parsing configuration files of the form:
--
-- ; comment
-- [section]
-- host=wikipedia.org
-- alias=claw


module Data.Ini where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta


-- | Helper function to run a parser
testParser :: ByteString -> Parser a -> Result a
testParser s p = parseByteString p mempty s



---------------------
-- Section Headers --
---------------------

headerExample :: ByteString
headerExample = "[blah]"

-- | Type representing a section header - e.g. "[blah]"
newtype Header =
    Header String
    deriving (Eq, Ord, Show)


-- | Takes a `Parser` and parses it when it appears inside a
-- bracket pair.
--
-- e.g.
-- > testParser headerExample $ parseBracketPair (some letter)
-- Success "blah"
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'


-- | Parser that parses a header line, returning a `Header`.
--
-- e.g.
-- > testParser headerExample parseHeader
-- Success (Header "blah")
parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)



-----------------
-- Assignments --
-----------------

assignmentExample :: ByteString
assignmentExample = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value



-- | Parser that parses an 'assignment' line into a (name, value)
-- tuple.
--
-- e.g.
-- > testParser assignmentExample parseAssignment
-- Success ("woot", 1)
parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

-- | Parser that skips EOL characters and subsequent blank lines
skipEOL :: Parser()
skipEOL = skipMany (oneOf "\n")


--------------
-- Comments --
--------------

commentExample :: ByteString
commentExample = "; Last modified 1 April\
    \ 2001 by John Doe"

commentExample' :: ByteString
commentExample'= "; blah\n; woot\n   \n; hah"


-- | Parser that skips any comment lines and does not return input
--
-- e.g.:
-- > testParser commentExample skipComments
-- Success ()
skipComments :: Parser ()
skipComments = skipMany (do _ <- char ';' <|> char '#'
                            skipMany (noneOf "\n")
                            skipEOL)


--------------
-- Sections --
--------------

sectionExample :: ByteString
sectionExample = [r|
; ignore me
[states]
Chris=Texas
|]

sectionExample' :: ByteString
sectionExample' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]


data Section =
    Section Header Assignments
    deriving (Eq, Show)

newtype Config =
    Config (Map Header Assignments)
    deriving (Eq, Show)


-- | Parser to skip whitespace
skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')


-- | Parser to parse an entire configuration section
--
-- e.g.:
--
-- > testParser sectionExample parseSection
-- Success (Section (Header "states" (fromList [("Chris", "Texac")]))
--
-- > testParser sectionExample' parseSection
-- Success (Section (Header "section") (fromList [("alias","claw"),("host","wikipedia.org")]))
--
parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)