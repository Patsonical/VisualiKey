{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Network.HTTP.Client
import Control.Monad.State
import Control.Monad.Except
import GHC.Generics
import Data.Aeson hiding (Result(..))

data Note = C | C'
          | D | D'
          | E -- E' == F
          | F | F'
          | G | G'
          | A | A'
          | B -- B' == C
          deriving (Show, Eq, Enum, Bounded)

data Mode = Major | Minor deriving (Show, Eq)

type Key = (Note, Mode)

type Result = (Int, FinalTrack)

data SpotifyClient = SpotifyClient {
    client_id     :: String
  , client_secret :: String
  } deriving Generic

instance Show SpotifyClient where
  show (SpotifyClient id secret) = id ++ ":" ++ secret
instance FromJSON SpotifyClient where

data Token = Token {
    access_token  :: String
  , token_type    :: String
  , expires_in    :: Int
  } deriving (Show, Generic)

instance FromJSON Token where
instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions

data VKState = VKState {
    manager :: Manager
  , oauth   :: Token
  , results :: [Result]
  }

type VKError = String

type VisualiKey = StateT VKState (ExceptT VKError IO)

-- Response Types {{{
newtype SearchResults = SearchResults {
    tracks      :: TrackList
  } deriving (Show, Generic)
newtype TrackList = TrackList {
    items       :: [Track]
  } deriving (Show, Generic)

data Track = Track {
    id          :: String
  , artists     :: [Artist]
  , name        :: String
  } deriving (Show, Generic)

instance Eq Track where
  (Track _ a1 n1) == (Track _ a2 n2) = a1 == a2 && n1 == n2

newtype Artist = Artist {
    artist_name :: String
  } deriving (Show, Generic, Eq)

newtype AudioFeaturesResults = AudioFeaturesResults {
    audio_features :: [Maybe TuneableTrack]
  } deriving (Show, Generic)
data TuneableTrack = TuneableTrack {
    ttId        :: String
  , ttKey       :: Int
  , ttMode      :: Int
  } deriving (Show, Generic)

instance FromJSON SearchResults where
instance FromJSON TrackList where
instance FromJSON Track where
instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v ->
    Artist <$> v .: "name"
instance FromJSON AudioFeaturesResults where
instance FromJSON TuneableTrack where
  parseJSON = withObject "TuneableTrack" $ \v ->
    TuneableTrack <$> v .: "id"
                  <*> v .: "key"
                  <*> v .: "mode"
-- }}}

data FinalTrack = FinalTrack {
    ftName      :: String
  , ftArtists   :: [String]
  , ftKey       :: Key
  } deriving (Show, Eq)
