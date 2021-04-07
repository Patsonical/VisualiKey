-- vim:foldmethod=marker
{-# LANGUAGE OverloadedStrings #-}

module SpotifyAPI where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

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
import Data.Functor ((<&>))

import Types
import Instances

-- TESTING {{{
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
-- }}}

getClient :: IO (Maybe SpotifyClient)
getClient = getConfig >>= Lbs.readFile <&> decode
  where getConfig = getXdgDirectory XdgConfig "visualikey/config.json"

okResponse :: Response a -> Bool
okResponse res = (statusCode . responseStatus) res == 200

-- runExceptT . flip evalStateT st $ getToken
getToken :: VisualiKey Token
getToken = do
  client <- liftIO $ pack . show . fromJust <$> getClient
  let b64Client = Lbs.append "Basic " (B64.encode client)
      headers   = [ ("Authorization", Lbs.toStrict b64Client)
                  , ("Content-Type", "application/x-www-form-urlencoded")
                  ]
      body      = "grant_type=client_credentials"
      request   = defaultRequest { 
          method = "POST"
        , port = 443
        , secure = True
        , host = "accounts.spotify.com"
        , path = "/api/token"
        , requestHeaders = headers
        , requestBody = body
        }
  manager <- gets manager
  response    <- liftIO $ httpLbs request manager
  unless (okResponse response) $
    throwError ("Error getting access token:\n" ++ show response)
  currentTime <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
  let body = responseBody response
  case decode body of
    Nothing               -> throwError $ "Error parsing token:\n" ++ show body
    Just (Token tk ty ex) -> pure       $ Token tk ty (ex + currentTime)
