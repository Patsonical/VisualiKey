{-# LANGUAGE OverloadedStrings #-}
module Main where

import Rainbow
import Data.List
import System.Environment (getArgs)
import Data.Text (pack)
import Text.Read (readMaybe)

import Music
import Scraper
import Keyboard

main :: IO ()
main = do
  searchRes <- searchSong . unwords =<< getArgs
  case searchRes of
    Nothing  -> putChunkLn $ fore red "Error retrieving results"
    Just res -> presentResults $ zip [1..] res

searchSong :: String -> IO (Maybe [MetaData])
searchSong name = findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)

cp :: String -> Chunk
cp = chunk . pack

testMd = MetaData "Colors" "Tobu" (Just (A,Major)) "A Major" (Just 128)

showResult :: (Int, MetaData) -> IO ()
showResult (ix, md) = do
  putChunk   . bold . fore blue  . cp $ '[' : show ix ++ "] "
  putChunk          . fore blue  . cp $ _artist md ++ " - "
  putChunkLn        . fore green . cp $ _name md

expandResult :: MetaData -> IO ()
expandResult md = do
  putChunk   . bold . fore blue   . cp $ '\t' : _artist md ++ " - "
  putChunkLn . bold . fore green  . cp $ _name md
  --putStrLn ""
  putChunkLn . bold . fore yellow . cp $ replicate 22 ' ' ++ "Key: " ++ _keyFmt md
  case _key md of
    Nothing  -> putChunkLn $ fore red "Error parsing key"
    Just key -> drawKeyboard . uncurry resolveScale $ key
  putStrLn ""

presentResults :: [(Int, MetaData)] -> IO ()
presentResults mds = do
  putChunkLn . bold . fore yellow . cp $ "\tResults found: " ++ show (length mds)
  --let (pres, rest) = splitAt 10 mds
  mapM_ showResult mds
  selection mds

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
