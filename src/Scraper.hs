{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Text.HTML.Scalpel
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP

import Music

data MetaData = MetaData 
                { _name   :: String
                , _artist :: String
                , _key    :: String
                , _bpm    :: String
                } deriving Show

managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
    return $ req' {
      HTTP.requestHeaders = (HTTP.hUserAgent, "Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0")
                          : HTTP.requestHeaders req'
    }
}

findSong :: URL -> IO (Maybe [MetaData])
findSong url = do
  scrapeURL url scrapeSongs

findSong2 :: URL -> IO (Maybe [MetaData])
findSong2 url = do
  manager <- Just <$> HTTP.newManager managerSettings
  scrapeURLWithConfig (Config defaultDecoder manager) url scrapeSongs

scrapeSongs :: Scraper String [MetaData]
scrapeSongs = chroots ("div" @: [hasClass "searchResultNode"]) $ do
                artist <- text  $ "div" @: [hasClass "search-artist-name"]
                name   <- text  $ "div" @: [hasClass "search-track-name"]
                rest   <- texts $ "div" @: [hasClass "search-attribute-value"]
                return $ MetaData name artist (head rest) (last rest)
