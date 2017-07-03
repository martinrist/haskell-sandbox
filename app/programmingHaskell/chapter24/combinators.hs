{-# LANGUAGE InstanceSigs #-}

module Combinators where

import           Control.Applicative (liftA2)
import           Data.Char

main :: IO ()
main = undefined

-- Based on Scott Wlaschin's talk 'Understanding Parser Combinators: A Deep Dive
-- https://skillsmatter.com/skillscasts/9731-understanding-parser-combinators-a-deep-dive

-- Part 2 - 4 versions of a simple parser
-----------------------------------------

-- Version 1 parses just the character 'A'

_pcharA :: String -> (Bool, String)
_pcharA []        = (False, "")
_pcharA input@(x:xs) = if x == 'A' then
                                  (True, xs)
                                  else
                                  (False, input)



-- Version 2 parses any character and returns either a Left (failure) with an error message
-- or a Right (success), containing a tuple of the matched character and the remaining string

data PResult a = Error String | Ok (a, String)
    deriving Show


_pchar :: Char -> String -> PResult Char
_pchar a []           = Error "No more input"
_pchar a input@(x:xs) = if x == a then
                                 Ok (a, xs)
                                 else
                                 Error $ "Expecting " ++ [a] ++ ". Got " ++ [x]


-- Version 3 - returns a function which takes a string and returns a PResult
-- I think you get this for free with currying...


-- Version 4 - Wraps the resulting function in a type
data Parser a = Parser (String -> PResult a)

pchar :: Char -> Parser Char
pchar = Parser <$> _pchar

run :: Parser a -> String -> PResult a
run (Parser f) = f


pcharA = pchar 'A'
pcharB = pchar 'B'


-- Part 3 - Combinators
-----------------------

-- andThen

andThen :: Parser a -> Parser a -> Parser (a, a)
andThen (Parser p1) (Parser p2) =
    Parser $ \input -> case p1 input of
                            Error e     -> Error e
                            Ok (v1, r1) -> case p2 r1 of
                                                Error e     -> Error e
                                                Ok (v2, r2) -> Ok ((v1, v2), r2)

-- orElse
orElse :: Parser a -> Parser a -> Parser a
orElse (Parser p1) (Parser p2) =
    Parser $ \input -> case p1 input of
                            Ok (v1, r1) -> Ok (v1, r1)
                            Error e     -> case p2 input of
                                                Error e     -> Error e
                                                Ok (v2, r2) -> Ok (v2, r2)

-- map
-- This is basically the functor instance for Parser
map :: Parser a -> (a -> b) -> Parser b
map (Parser p) f =
    Parser $ \input -> case p input of
                            Error e   -> Error e
                            Ok (v, r) -> Ok (f v, r)

-- Infix versions of operators
(.>>.) = andThen
(<|>) = orElse
(|>>) = Combinators.map


instance Functor Parser where
    fmap = flip (|>>)


-- andThen is Applicative for Parser with the combining function being (,)
instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \input -> Ok (a, input)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pfab) <*> (Parser pa) =
        Parser $ \input -> case pfab input of
                                Error e        -> Error e
                                Ok (fab, rem1) -> case pa rem1 of
                                                    Error e      -> Error e
                                                    Ok (a, rem2) -> Ok (fab a, rem2)

andThen' :: Parser a -> Parser a -> Parser (a, a)
andThen' = liftA2 (,)



-- Are there really two Monoid instances for Parser, one which is andThen-based, the other of which is `orElse` based?
instance Monoid a => Monoid (Parser a) where
    mempty :: Parser a
    mempty = Parser $ \input -> Ok (mempty, input)

    mappend :: Parser a -> Parser a -> Parser a
    mappend p1 p2 = snd <$> (p1 .>>. p2)







-- More complicated combinators using reduce
--------------------------------------------

choice :: [Parser a] -> Parser a
-- TODO: This is a partial function - what's the result of choice []?  It should be some sort of 'Identity' parser, which probably depends on the underlying Monoid...
choice [x]    = x
choice (x:xs) = foldr (<|>) x xs

anyOf :: [Char] -> Parser Char
anyOf cs = choice $ pchar <$> cs

parseLowercase :: Parser Char
parseLowercase = anyOf ['a'..'z']

parseDigit :: Parser Char
parseDigit = anyOf ['0'..'9']





sequence :: [Parser a] -> Parser [a]
sequence ps = let pls = reverse $ (\p -> (:[]) <$> p) <$> ps in
                  foldr concatResults (head pls) (tail pls)
    where concatResults p1 p2 = (p1 .>>. p2) |>> uncurry (++)


choiceM :: Monoid a => [Parser a] -> Parser a
choiceM = foldr (<|>) mempty

-- This doesn't work well, because there's no sensible 'mempty' for Char
-- ('' is a syntax error in Haskell)
instance Monoid Char where
    -- Eek!
    mempty = ' '
    mappend c1 c2 = c2