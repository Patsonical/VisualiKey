{-# LANGUAGE OverloadedStrings #-}
module SpotifyAPI where

import Control.Monad.Trans

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import Data.Aeson
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Base64.Lazy as B64
import System.Directory
import Data.Time.Clock.System
import Data.Maybe
import Data.ByteString.Lazy.Char8

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

getClient :: IO (Maybe SpotifyClient)
getClient = getConfig >>= Lbs.readFile >>= pure . decode
  where getConfig = getXdgDirectory XdgConfig "visualikey/config.json"

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

getToken :: Manager -> IO ()
getToken manager = do
  client <- pack . (\c -> clientId c ++ ":" ++ clientSecret c) . fromJust <$> getClient
  let b64Client = Lbs.append "Basic " (B64.encode client)
      headers   = [ ("Authorization", Lbs.toStrict b64Client)
                  , ("Content-Type", "application/x-www-form-urlencoded")
                  ]
      body      = "grant_type=client_credentials"
      request   = defaultRequest { method = "POST"
                                 , port = 443
                                 , secure = True
                                 , host = "accounts.spotify.com"
                                 , path = "/api/token"
                                 , requestHeaders = headers
                                 , requestBody = body
                                 }
  print client
  print headers
  print request
  response    <- httpLbs request manager
  currentTime <- fromIntegral . systemSeconds <$> getSystemTime
  print response
