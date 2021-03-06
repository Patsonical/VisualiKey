{-# LANGUAGE OverloadedStrings #-}

module SpotifyAPI where

import Control.Applicative ((<|>))
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Zip (mzip)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.URL (encString)

import Data.Aeson
import qualified Data.ByteString.Internal as Bs
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Base64.Lazy as B64
import System.Directory
import Data.Time.Clock.System
import Data.Maybe
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (intercalate)
import Data.Char (isAlphaNum)

import Lib
import Types
import Music

getConfigFile :: String -> IO FilePath
getConfigFile file = getXdgDirectory XdgConfig ("visualikey/" ++ file)

getJSONConfig :: FromJSON a => String -> VisualiKey a
getJSONConfig file = do
  cfg <- safeLiftIO (Lbs.readFile =<< getConfigFile file)
  case decode cfg of
    Nothing  -> throwError ("Error decoding file: " ++ file)
    Just cli -> pure cli

getClient :: VisualiKey SpotifyClient
getClient = getJSONConfig "client.json"

getTokenFromFile :: VisualiKey Token
getTokenFromFile = do
  token <- getJSONConfig "token.json"
  currentTime <- safeLiftIO $ fromIntegral . systemSeconds <$> getSystemTime
  if expires_in token < currentTime
  then throwError "Access token expired"
  else pure token

saveToken :: Token -> VisualiKey ()
saveToken tk = safeLiftIO (getConfigFile "token.json" >>= flip encodeFile tk)

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
  response <- safeLiftIO $ httpLbs request manager
  unless (okResponse response) $
    throwError ("Error getting access token:\n" ++ show response)
  currentTime <- safeLiftIO $ fromIntegral . systemSeconds <$> getSystemTime
  let body = responseBody response
      updateTime (Token tk ty ex) = Token tk ty (ex + currentTime)
  case updateTime <$> decode body of
    Nothing -> throwError $ "Error parsing token:\n" ++ show body
    Just tk -> saveToken tk >> pure tk

getToken :: VisualiKey ()
getToken = do
  token <- getTokenFromFile <|> getTokenFromAPI
  modify (\s -> s { oauth = token })

checkToken :: VisualiKey Bool
checkToken = do
  token       <- gets oauth
  currentTime <- safeLiftIO $ fromIntegral . systemSeconds <$> getSystemTime
  pure (expires_in token > currentTime)

formatToken :: Token -> Lbs.ByteString
formatToken (Token tk ty _) = pack (ty ++ " " ++ tk)

apiGetRequest :: Bs.ByteString -> Bs.ByteString -> VisualiKey (Response Lbs.ByteString)
apiGetRequest path query = do
  checkToken >>= flip unless getToken
  token   <- gets oauth
  manager <- gets manager
  let headers   = [ ("Authorization", Lbs.toStrict (formatToken token))
                  , ("Accept", "application/json")
                  , ("Content-Type", "application/json")
                  ]
      request   = defaultRequest {
          method = "GET"
        , port = 443
        , secure = True
        , host = "api.spotify.com"
        , path = path
        , requestHeaders = headers
        , queryString = query
        }
  safeLiftIO $ httpLbs request manager

-- ♫ > encString True (isAlphaNum) "hello there, general kenobi7''//@"
-- "hello+there%2c+general+kenobi7%27%27%2f%2f%40"
searchSong :: String -> VisualiKey [Track]
searchSong searchTerm = do
  let format = encString True isAlphaNum
      query  = Bs.packChars $ "type=track&limit=50&offset=0&q=" ++ format searchTerm
  response <- apiGetRequest "/v1/search" query
  unless (okResponse response) $
    throwError ("Error searching song:\n" ++ show response)
  let body = responseBody response
  case decode body of
    Nothing -> throwError $ "Error parsing tracks:\n" ++ show body
    Just ts -> (pure . items . tracks ) ts

finalise :: Track -> TuneableTrack -> VisualiKey FinalTrack
finalise t@(Track id1 artists name) af@(TuneableTrack id2 _ _) =
  if id1 /= id2 then
    throwError $ "Mismatching ids for " ++ show t ++ " and " ++ show af
  else case getKey af of
    Nothing  -> throwError $ "Error parsing key for " ++ show af
    Just key -> pure $ FinalTrack name (map artist_name artists) key

getAFFromResults :: [Track] -> VisualiKey [FinalTrack]
getAFFromResults searchTracks = do
  let getIds =  intercalate "%2C" . map Types.id
      query = Bs.packChars $ "ids=" ++ getIds searchTracks
  response <- apiGetRequest "/v1/audio-features" query
  unless (okResponse response) $
    throwError ("Error getting audio features:\n" ++ show response)
  let body = responseBody response
  audioFeatures <- case decode body of
    Nothing -> throwError $ "Error parsing audio features:\n" ++ show body
    Just af -> pure af
  let tracks = catMaybes $ zipWith mzip (map Just searchTracks)
                                        (audio_features audioFeatures)
  mapM (uncurry finalise) tracks

getAudioFeatures :: Track -> VisualiKey FinalTrack
getAudioFeatures t@(Track id artists name) = do
  response <- apiGetRequest "/v1/audio-features" (Bs.packChars id)
  unless (okResponse response) $
    throwError ("Error getting audio features:\n" ++ show response)
  let body = responseBody response
  tuneableTrack <- case decode body of
    Nothing -> throwError $ "Error parsing audio features:\n" ++ show body
    Just tt -> pure tt
  finalise t tuneableTrack
