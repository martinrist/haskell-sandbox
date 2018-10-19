{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           ProgrammingHaskell.Chapter11.Exercises.Cipher

import           Options.Applicative.Common
import           Options.Applicative.Builder
import           Options.Applicative.Extra
import           Data.Semigroup                 ( (<>) )
import           Control.Applicative            ( (<|>) )

data Cipher = Caesar Int | Vignere String deriving Show

data Arguments = Arguments
    { cipher :: Cipher
    , text :: String }
    deriving (Show)

shiftOption :: Parser Int
shiftOption = option
    auto
    (long "shift" <> metavar "INT" <> help
        "Use a Caesar cipher and shift by the specified amount"
    )

caesarOption :: Parser Cipher
caesarOption = Caesar <$> shiftOption

keywordOption :: Parser String
keywordOption = strOption
    (long "keyword" <> metavar "STRING" <> help
        "Use a Vignere cipher with the specified keyword"
    )

vignereOption :: Parser Cipher
vignereOption = Vignere <$> keywordOption

cipherOption :: Parser Cipher
cipherOption = caesarOption <|> vignereOption

textOption :: Parser String
textOption =
    strOption (long "text" <> metavar "TEXT" <> help "Text to be encrypted")

argParser :: Parser Arguments
argParser = Arguments <$> cipherOption <*> textOption

opts :: ParserInfo Arguments
opts = info
    (helper <*> argParser)
    (fullDesc <> progDesc "Encrypts text using a cipher" <> header
        "cipher - text encryption"
    )

doEncrypt :: Cipher -> String -> String
doEncrypt (Caesar n) s = caesar n s
doEncrypt (Vignere ks) s = vignere ks s

encrypt :: Arguments -> IO ()
encrypt Arguments { cipher, text } =
    do
        putStrLn $ "Plain text: " ++ text
        putStrLn $ "Encrypted text: " ++ doEncrypt cipher text

main :: IO ()
main = execParser opts >>= encrypt
