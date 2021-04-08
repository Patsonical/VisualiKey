{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Network.HTTP.Client
import Control.Monad.State
import Control.Monad.Except
import GHC.Generics
import Data.Aeson hiding (Result(..))

data Note = A | A'
          | B -- B' == C
          | C | C'
          | D | D'
          | E -- E' == F
          | F | F'
          | G | G'
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

newtype Artist = Artist {
    artist_name :: String
  } deriving (Show, Generic)

data TuneableTrack = TuneableTrack {
    key         :: Int
  , mode        :: Int
  } deriving (Show, Generic)

instance FromJSON SearchResults where
instance FromJSON TrackList where
instance FromJSON Track where
instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v ->
    Artist <$> v .: "name"
instance FromJSON TuneableTrack where
-- }}}

data FinalTrack = FinalTrack {
    ftName      :: String
  , ftArtists   :: [String]
  , ftKey       :: Key
  } deriving (Show, Eq)
