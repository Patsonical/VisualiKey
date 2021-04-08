{-# LANGUAGE OverloadedStrings #-}

module SpotifyAPI where

import Control.Applicative ((<|>))
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
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Internal (packChars)
import Data.Functor ((<&>))

import Lib
import Types

runVK :: VKState -> VisualiKey a -> IO (Either VKError a)
runVK st = runExceptT . flip evalStateT st

testVK :: VisualiKey a -> IO (Either VKError a)
testVK vk = do
  manager <- newManager tlsManagerSettings
  let st = VKState manager (Token "" "" 0) []
  runVK st vk

getConfigFile :: String -> IO FilePath
getConfigFile file = getXdgDirectory XdgConfig ("visualikey/" ++ file)

getJSONConfig :: FromJSON a => String -> VisualiKey a
getJSONConfig file = do
  cfg <- catchIO (Lbs.readFile =<< getConfigFile file)
  case decode cfg of
    Nothing  -> throwError ("Error decoding file: " ++ file)
    Just cli -> pure cli

getClient :: VisualiKey SpotifyClient
getClient = getJSONConfig "client.json"

getTokenFromFile :: VisualiKey Token
getTokenFromFile = do
  token <- getJSONConfig "token.json"
  currentTime <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
  if expires_in token < currentTime
  then throwError "Access token expired"
  else pure token

saveToken :: Token -> VisualiKey ()
saveToken tk = catchIO (getConfigFile "token.json" >>= flip encodeFile tk)

okResponse :: Response a -> Bool
okResponse res = (statusCode . responseStatus) res == 200

getTokenFromAPI :: VisualiKey Token
getTokenFromAPI = do
  client <- pack . show <$> getClient
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
  manager  <- gets manager
  response <- liftIO $ httpLbs request manager
  unless (okResponse response) $
    throwError ("Error getting access token:\n" ++ show response)
  currentTime <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
  let body = responseBody response
      updateTime (Token tk ty ex) = Token tk ty (ex + currentTime)
  case updateTime <$> decode body of
    Nothing -> throwError $ "Error parsing token:\n" ++ show body
    Just tk -> saveToken tk >> pure tk

getToken :: VisualiKey ()
getToken = do
  token <- getTokenFromFile <|> getTokenFromAPI
  modify (\s -> s { oauth = token })

formatToken :: Token -> Lbs.ByteString
formatToken (Token tk ty _) = pack (ty ++ " " ++ tk)

searchSongSAPI :: String -> VisualiKey SearchResults
searchSongSAPI searchTerm = do
  token   <- gets oauth
  manager <- gets manager
  let headers   = [ ("Authorization", Lbs.toStrict (formatToken token))
                  , ("Accept", "application/json")
                  , ("Content-Type", "application/json")
                  ]
      query = packChars $ "type=track&limit=50&offset=0&q=" ++ searchTerm
      request   = defaultRequest { 
          method = "GET"
        , port = 443
        , secure = True
        , host = "api.spotify.com"
        , path = "/v1/search"
        , requestHeaders = headers
        , queryString = query
        }
  response <- liftIO $ httpLbs request manager
  unless (okResponse response) $
    throwError ("Error searching song:\n" ++ show response)
  let body = responseBody response
  case decode body of
    Nothing -> throwError $ "Error parsing tracks:\n" ++ show body
    Just ts -> pure ts
