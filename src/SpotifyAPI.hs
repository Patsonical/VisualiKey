{-# LANGUAGE OverloadedStrings #-}
module SpotifyAPI where

import Control.Monad.Trans

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import qualified Network.HTTP.Req as R

import Data.Aeson

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
