{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Data.Aeson

import Types

instance Show SpotifyClient where
  show (SpotifyClient id secret) = id ++ ":" ++ secret

instance FromJSON SpotifyClient where
  parseJSON = withObject "SpotifyClient" $ \v ->
    SpotifyClient <$> v .: "client_id" <*> v .: "client_secret"

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v ->
    Token <$> v .: "access_token"
          <*> v .: "token_type"
          <*> v .: "expires_in"

instance ToJSON Token where
  toJSON     (Token tk ty ex) = 
    object [ "access_token" .= tk
           , "token_type"   .= ty
           , "expires_in"   .= ex
           ]
  toEncoding (Token tk ty ex) =
    pairs (  "access_token" .= tk
          <> "token_type"   .= ty
          <> "expires_in"   .= ex
          )

