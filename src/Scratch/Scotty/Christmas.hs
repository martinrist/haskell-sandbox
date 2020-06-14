{-# LANGUAGE OverloadedStrings #-}

module Scratch.Scotty.Christmas where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( object
                                                , (.=)
                                                )
import           Network.HTTP.Types.Status
import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger

chrstmsly :: ScottyM ()
chrstmsly = do
    get "/" showLandingPage
    post "/register" register
    post "/register" registrationFailure

showLandingPage :: ActionM ()
showLandingPage = do
    setHeader "Content-Type" "text/html"
    text "<h1>Landing Page!</h1>"

register :: ActionM ()
register = do
    emailAddress <- param "email" `rescue` const next
    registered   <- liftIO (registerInterest emailAddress)
    case registered of
        Just errorMessage -> do
            json $ object ["error" .= errorMessage]
            status internalServerError500

        Nothing -> json $ object ["ok" .= ("ok" :: String)]


registrationFailure :: ActionM ()
registrationFailure = do
    json $ object ["error" .= ("Invalid request" :: String)]
    status badRequest400

main :: IO ()
main = scotty 9176 $ do
    middleware logStdoutDev
    chrstmsly

registerInterest :: String -> IO (Maybe String)
registerInterest "a@b.com" = putStrLn "Registered!" >> return Nothing
registerInterest _         = return (Just "I broke :(")
