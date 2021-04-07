module Types where

import Network.HTTP.Client
import Control.Monad.State
import Control.Monad.Except

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

data MetaData = MetaData {
    _name   :: String
  , _artist :: String
  , _key    :: Maybe Key
  } deriving (Show, Eq)

type Result = (Int, MetaData)

data SpotifyClient = SpotifyClient {
    clientId      :: String
  , clientSecret  :: String
  }

data Token = Token {
    access_token  :: String
  , token_type    :: String
  , expires_in    :: Int
  } deriving Show

data VKState = VKState {
    manager :: Manager
  , oauth   :: Token
  , results :: [Result]
  }

type VKError = String

type VisualiKey = StateT VKState (ExceptT VKError IO)
