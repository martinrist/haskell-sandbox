module Main where

import Options.Applicative.Common
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Monad

data Sample = Sample
    { hello :: String
    , repeat :: Int
    , quiet :: Bool }

helloOption :: Parser String
helloOption = strOption
                ( long "hello"
                <> metavar "TARGET"
                <> help "Target for the greeting" )

quietOption :: Parser Bool
quietOption = switch
               ( long "quiet"
               <> help "Whether to be quiet" )

repeatOption :: Parser Int
repeatOption = option auto
                ( long "repeat"
                <> help "Repeats for greeting"
                <> showDefault
                <> value 1
                <> metavar "INT" )

sample :: Parser Sample
sample = Sample <$> helloOption <*> repeatOption <*> quietOption
-- This also works:
-- sample = liftA3 Sample helloOption repeatOption quietOption

opts :: ParserInfo Sample
opts = info (helper <*> sample)
            ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h n False) = replicateM_ n . putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet

-- Alternative implementation using 'do'
--main :: IO ()
--main = do
--    o <- execParser opts
--    greet o

