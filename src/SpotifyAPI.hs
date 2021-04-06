{-# LANGUAGE OverloadedStrings #-}
module SpotifyAPI where

import Control.Monad.Trans

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import qualified Network.HTTP.Req as R

import Data.Aeson
import qualified Data.ByteString.Lazy as Lbs
import System.Directory
import Data.Time.Clock.System

reqTest :: IO ()
reqTest = R.runReq R.defaultHttpConfig $ do
  response <- R.req
    R.GET
    (R.https "example.org")
    R.NoReqBody
    R.lbsResponse
    mempty
  liftIO $ print (R.responseBody response)

httpTest :: IO ()
httpTest = do
  manager <- newManager tlsManagerSettings
  let request = defaultRequest { host = "date.jsontest.com"
                               , port = 80
                               , secure = False
                               , method = "GET"
                               }
  response <- responseBody <$> httpLbs request manager
  let date = decode response :: Maybe Date
  print date

data Date = Date {
    time         :: String
  , date         :: String
  , msSinceEpoch :: Integer
  } deriving Show

instance FromJSON Date where
  parseJSON = withObject "Date" $ \v -> 
    Date <$> v .: "time" <*> v .: "date" <*> v .: "milliseconds_since_epoch"

data SpotifyClient = SpotifyClient {
    clientId      :: String
  , clientSecret  :: String
  } deriving Show

instance FromJSON SpotifyClient where
  parseJSON = withObject "SpotifyClient" $ \v ->
    SpotifyClient <$> v .: "client_id" <*> v .: "client_secret"

getClient :: IO ()
getClient = do
  configFile <- getXdgDirectory XdgConfig "visualikey/config.json"
  configRaw  <- Lbs.readFile configFile
  let client = decode configRaw :: Maybe SpotifyClient
  print client

data Token = Token {
    token  :: String
  , expiry :: Int
  }

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v ->
    Token <$> v .: "oauth_token" <*> v .: "expiry_time"

instance ToJSON Token where
  toJSON (Token token expiry) = 
    object ["oauth_token" .= token, "expiry_time" .= expiry]
  
  toEncoding (Token token expiry) =
    pairs ("oauth_token" .= token <> "expiry_time" .= expiry)
