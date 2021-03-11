{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Monad.State
import Data.List          (intercalate, isPrefixOf, nub)
import Rainbow
import System.Environment (getArgs)
import Text.Read          (readMaybe)

import Keyboard
import Lib
import Music
import Scraper

type Result = (Int, MetaData)

main :: IO ()
main = getArgs >>= searchSong . unwords

controlLoop :: StateT [Result] IO ()
controlLoop = do
  lift . putChunk . bold . fore blue $ "> "
  input   <- lift getLine
  case input of
    (':' : cmd) -> command cmd
    nonCmd      -> select nonCmd

showError :: String -> IO ()
showError = putChunkLn . fore red . cp

strip :: String -> String
strip = dropWhile (==' ')

command :: String -> StateT [Result] IO ()
command (cmd:rest) =
  let cmds = [ ('q', pure ()) 
             , ('s', search >> controlLoop) 
             , ('k', key    >> controlLoop)
             ]
      search = (searchSongST   . strip) rest
      key    = (lift . showKey . strip) rest
  in case lookup cmd cmds of
    Just resolved -> resolved
    Nothing  -> lift (showError "Invalid command")
command _ = lift (showError "Invalid command") >> controlLoop

select :: String -> StateT [Result] IO ()
select input = do
  results <- get
  case readMaybe input of
    Nothing -> lift $ showError "Invalid selection, enter a number or :[cmd]"
    Just ix -> case lookup ix results of
      Nothing  -> lift $ showError "Selection out of bounds"
      Just res -> lift (expandResult res)
  controlLoop

-- Replaced by controlLoop
selection :: [(Int, MetaData)] -> IO ()
selection mds = do
  putChunk . bold . fore blue $ "> "
  input <- getLine
  let choice  = readMaybe input >>= flip lookup mds
      escapes = ["q", ":q", "quit", "exit"]
      invalid = putChunkLn . fore red . cp $ "Invalid command or selection"
  if | input `elem` escapes      -> pure ()
     | "new " `isPrefixOf` input -> searchSong . drop 4 $ input
     | "key " `isPrefixOf` input -> (showKey . drop 4) input >> selection mds
     | otherwise -> case choice of 
         Just md -> expandResult md >> selection mds
         Nothing -> invalid         >> selection mds

-- Replaced by searchSongST
searchSong :: String -> IO ()
searchSong name = do
  results <- findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)
  case results of
    Nothing  -> putChunkLn $ fore red "Error retrieving results"
    Just res -> presentResults . zip [1..] . nub $ res

searchSongST :: String -> StateT [Result] IO ()
searchSongST name = do
  results <- lift . findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)
  case results of
    Nothing  -> lift . putChunkLn $ fore red "Error retrieving results"
    Just res -> put (zip [1..] . nub $ res) >> presentResultsST
  pure ()

presentResultsST :: StateT [Result] IO ()
presentResultsST = do
  results <- get
  lift $ do
    putChunkLn . bold . fore yellow . cp $ "\tResults found: " ++ show (length results)
    mapM_ showResult results
  controlLoop

-- Replaced by presentResultsST
presentResults :: [(Int, MetaData)] -> IO ()
presentResults mds = do
  putChunkLn . bold . fore yellow . cp $ "\tResults found: " ++ show (length mds)
  mapM_ showResult mds
  selection mds

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

showKey :: String -> IO ()
showKey raw = do
  let invalid  = putChunkLn $ fore red "Error parsing key"
      parseKey [n,m] = maybe invalid draw key
        where draw = drawKeyboard . uncurry resolveScale
              key  = zipMaybe (readNote n, readMode m)
      parseKey _     = invalid
  parseKey . words $ raw
  putStrLn ""
