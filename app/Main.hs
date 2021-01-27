{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List          (intercalate)
import Rainbow
import System.Environment (getArgs)
import Text.Read          (readMaybe)

import Keyboard
import Lib
import Music
import Scraper

main :: IO ()
main = do
  searchRes <- searchSong . unwords =<< getArgs
  case searchRes of
    Nothing  -> putChunkLn $ fore red "Error retrieving results"
    Just res -> presentResults $ zip [1..] res

searchSong :: String -> IO (Maybe [MetaData])
searchSong name = findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)

showResult :: (Int, MetaData) -> IO ()
showResult (ix, md) = do
  putChunk   . bold . fore blue  . cp $ '[' : show ix ++ "] "
  putChunk          . fore blue  . cp $ _artist md ++ " - "
  putChunkLn        . fore green . cp $ _name md

expandResult :: MetaData -> IO ()
expandResult md = do
  putChunk   . bold . fore blue   . cp $ '\t' : _artist md ++ " - "
  putChunkLn . bold . fore green  . cp $ _name md
  putChunkLn . bold . fore yellow . cp $ replicate 22 ' ' ++ "Key: " ++ _keyFmt md
  case _key md of
    Nothing  -> putChunkLn $ fore red "Error parsing key"
    Just key -> drawKeyboard . uncurry resolveScale $ key
  putStrLn ""

presentResults :: [(Int, MetaData)] -> IO ()
presentResults mds = do
  putChunkLn . bold . fore yellow . cp $ "\tResults found: " ++ show (length mds)
  mapM_ showResult mds
  if null mds then pure () else selection mds

selection :: [(Int, MetaData)] -> IO ()
selection mds = do
  putChunk . bold . fore blue $ "> "
  select <- getLine
  let choice    = readMaybe select >>= flip lookup mds
      incorrect = putChunkLn . fore red . cp $ "Number out of bounds [1-" ++ show (length mds) ++ "]"
  if select == "q" || select == ":q"
  then pure ()
  else case choice of
    Nothing -> incorrect       >> selection mds
    Just md -> expandResult md >> selection mds
