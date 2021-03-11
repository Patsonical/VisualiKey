{-# LANGUAGE OverloadedStrings #-}
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
main = getArgs >>= start . handleArgs

start :: StateT [s] IO a -> IO ()
start run = runStateT run [] >> pure ()

handleArgs :: [String] -> StateT [Result] IO ()
handleArgs args = do
  case args of
    []          -> pure ()
    ("-s":rest) -> searchSongST   . unwords $ rest
    ("-k":rest) -> lift . showKey . unwords $ rest
    other       -> searchSongST   . unwords $ other
  controlLoop

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

searchSongST :: String -> StateT [Result] IO ()
searchSongST name = do
  results <- lift . findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)
  case results of
    Nothing  -> lift . putChunkLn $ fore red "Error retrieving results"
    Just res -> put (zip [1..] . nub $ res) >> presentResultsST

presentResultsST :: StateT [Result] IO ()
presentResultsST = do
  results <- get
  lift $ do
    putChunkLn . bold . fore yellow . cp $ "\tResults found: " ++ show (length results)
    mapM_ showResult results

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
