{-# LANGUAGE OverloadedStrings #-}

module Scraper (MetaData(..), findSong, findDebug) where

import Control.Monad.Zip (mzip)
import Text.HTML.Scalpel
import Text.Read         (readMaybe)

import Lib
import Music
import Types hiding (MetaData(..))

data MetaData = MetaData 
                { _name   :: String
                , _artist :: String
                , _key    :: Maybe Key
                , _keyFmt :: String
                , _bpm    :: Maybe Int
                } deriving (Show, Eq)

findSong :: URL -> IO (Maybe [MetaData])
findSong url = scrapeURL url scrapeSongs

findDebug url = scrapeURL url scrapeDebug

scrapeSongs :: Scraper String [MetaData]
scrapeSongs = chroots ("div" @: [hasClass "searchResultNode"]) $ do
                artist <- text  $ "div" @: [hasClass "search-artist-name"]
                name   <- text  $ "div" @: [hasClass "search-track-name"]
                rest   <- texts $ "div" @: [hasClass "search-attribute-value"]
                let hRest  = words . head $ rest
                    bpm    = readMaybe $ last rest
                    note   = readNote . head   $ hRest
                    mode   = readMode . (!! 1) $ hRest
                    key    = mzip note mode
                    keyFmt = (formatNote . head) hRest ++ " " ++ hRest !! 1
                pure (MetaData name artist key keyFmt bpm)

scrapeDebug :: Scraper String String
scrapeDebug = html "html"
