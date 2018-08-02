module Chapter24.Exercises.SemVer where

import           Control.Applicative
import           Test.Hspec
import           Text.Trifecta

data NumberOrString
    = StringPart String
    | IntPart Integer
    deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
    SemVer Major
           Minor
           Patch
           Release
           Metadata
    deriving (Eq, Show)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (IntPart <$> integer) <|> (StringPart <$> some letter)

parseNOSSList :: Parser [NumberOrString]
parseNOSSList = parseNumberOrString `sepBy` char '.'

parseRelease :: Parser Release
parseRelease = skipSome (char '-') >> parseNOSSList

parseMetadata :: Parser Metadata
parseMetadata = skipSome (char '+') >> parseNOSSList

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- integer
    _ <- char '.'
    minor <- integer
    _ <- char '.'
    patch <- integer
    release <- option [] parseRelease
    metadata <- option [] parseMetadata
    return $ SemVer major minor patch release metadata

runParser :: Show a => Parser a -> String -> IO (Maybe a)
runParser p s = do
    let m = parseString p mempty s
    print m
    let r = case m of
                Success a -> Just a
                _         -> Nothing
    return r

main :: IO ()
main =
    hspec $ do

        describe "NumberOrString Parsing" $ do
            let runNOSParser = runParser parseNumberOrString
            it "can parse String to StringPart" $ do
                r <- runNOSParser "foo"
                r `shouldBe` Just (StringPart "foo")
            it "cannot parse empty String to StringPart" $ do
                r <-runNOSParser ""
                r `shouldBe` Nothing
            it "can parse single-digit integer to IntPart" $ do
                r <- runNOSParser "1"
                r `shouldBe` Just (IntPart 1)
            it "can parse multi-digit integer to IntPart" $ do
                r <- runNOSParser "123"
                r `shouldBe` Just (IntPart 123)

        describe "Release Parsing" $ do
            let runReleaseParser = runParser parseRelease
            it "cannot parse with missing parts" $ do
                r <- runReleaseParser "-1..2"
                r `shouldBe` Nothing
            it "can parse with all single digits" $ do
                r <- runReleaseParser "-1.2.3.4"
                r `shouldBe` Just [IntPart 1, IntPart 2, IntPart 3, IntPart 4]
            it "can parse with all multiple digits" $ do
                r <- runReleaseParser "-12.34.56.78"
                r `shouldBe` Just [IntPart 12, IntPart 34, IntPart 56, IntPart 78]
            it "can parse with single char strings" $ do
                r <- runReleaseParser "-x.y.z"
                r `shouldBe` Just [StringPart "x", StringPart "y", StringPart "z"]
            it "can parse with multi char strings" $ do
                r <- runReleaseParser "-abc.def.ghi"
                r `shouldBe` Just [StringPart "abc", StringPart "def", StringPart "ghi"]
            it "can parse with mixed Strings and Integers" $ do
                r <- runReleaseParser "-12.ab.34.cd"
                r `shouldBe` Just [IntPart 12, StringPart "ab", IntPart 34, StringPart "cd"]

        describe "SemVer Parsing" $ do
            let runSemVerParser = runParser parseSemVer

            it "can parse major / minor / patch without release & metadata" $ do
                r <- runSemVerParser "2.1.1"
                r `shouldBe` Just (SemVer 2 1 1 [] [])

            it "can parse major / minor / patch / release without metadata" $ do
                r <- runSemVerParser "1.0.0-x.7.z.92"
                r `shouldBe` Just (SemVer 1 0 0 [StringPart "x", IntPart 7, StringPart "z", IntPart 92] [])

            it "can parse all components" $ do
                r <- runSemVerParser "1.0.0-x.7.z.92+1.y.z.3"
                r `shouldBe` Just (SemVer 1 0 0 [StringPart "x", IntPart 7, StringPart "z", IntPart 92] [IntPart 1, StringPart "y", StringPart "z", IntPart 3])

            it "can parse metadata without intervening release data" $ do
                r <- runSemVerParser "1.0.0+1.y.z.3"
                r `shouldBe` Just (SemVer 1 0 0 [] [IntPart 1, StringPart "y", StringPart "z", IntPart 3])