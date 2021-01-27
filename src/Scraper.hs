{-# LANGUAGE OverloadedStrings #-}

module Scraper (MetaData(..), findSong) where

import Text.HTML.Scalpel
import Text.Read         (readMaybe)

import Lib
import Music

data MetaData = MetaData 
                { _name   :: String
                , _artist :: String
                , _key    :: Maybe Key
                , _keyFmt :: String
                , _bpm    :: Maybe Int
                } deriving Show

findSong :: URL -> IO (Maybe [MetaData])
findSong url = do
  scrapeURL url scrapeSongs

scrapeSongs :: Scraper String [MetaData]
scrapeSongs = chroots ("div" @: [hasClass "searchResultNode"]) $ do
                artist <- text  $ "div" @: [hasClass "search-artist-name"]
                name   <- text  $ "div" @: [hasClass "search-track-name"]
                rest   <- texts $ "div" @: [hasClass "search-attribute-value"]
                let hRest  = words . head $ rest
                    bpm    = readMaybe $ last rest
                    note   = readNote . head   $ hRest
                    mode   = readMode . (!! 1) $ hRest
                    key    = zipMaybe (note, mode)
                    keyFmt = (formatNote . head) hRest ++ " " ++ hRest !! 1
                return $ MetaData name artist key keyFmt bpm
