{-# LANGUAGE OverloadedStrings #-}

module OldMain where

import Control.Monad.State
import Control.Monad.Zip  (mzip)
import Data.List          (intercalate, isPrefixOf, nub)
import Rainbow
import System.Environment (getArgs)
import Text.Read          (readMaybe)

import Keyboard
import Lib hiding (start)
import Music
import Scraper
import Types hiding (MetaData(..), Result(..))

type Result = (Int, MetaData)

main :: IO ()
main = getArgs >>= start . handleArgs

start :: StateT [s] IO a -> IO a
start = flip evalStateT []

handleArgs :: [String] -> StateT [Result] IO ()
handleArgs args =
  let search query = (searchSong     . unwords) query >> controlLoop
      key    query = (lift . showKey . unwords) query
      lucky  query = (imFeelingLucky . unwords) query
  in case args of
    []          -> controlLoop
    ("-s":rest) -> search rest
    ("-k":rest) -> key    rest
    ("-l":rest) -> lucky  rest
    other       -> search other

controlLoop :: StateT [Result] IO ()
controlLoop = do
  lift . putChunk . bold . fore blue $ "> "
  input   <- lift getLine
  case input of
    (':' : cmd) -> command cmd
    nonCmd      -> select nonCmd

command :: String -> StateT [Result] IO ()
command (cmd:rest) =
  let cmds = [ ('q', pure ()) 
             , ('s', search >> controlLoop) 
             , ('k', key    >> controlLoop)
             ]
      search = (searchSong     . strip) rest
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

searchSong :: String -> StateT [Result] IO ()
searchSong name = do
  results <- lift . findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)
  case results of
    Nothing  -> lift . showError $ "Error retrieving results"
    Just res -> put (zip [1..] . nub $ res) >> presentResults

imFeelingLucky :: String -> StateT [Result] IO ()
imFeelingLucky name = do
  results <- lift . findSong $ "https://tunebat.com/Search?q=" ++ intercalate "+" (words name)
  case results of
    Nothing  -> lift . showError $ "Error retrieving results"
    Just res -> case res of
      []    -> lift . putChunkLn . bold . fore yellow $ "No results found"
      (x:_) -> lift . expandResult $ x

presentResults :: StateT [Result] IO ()
presentResults = do
  let foundFormat = putChunkLn . bold . fore yellow . cp
  results <- get
  lift $ do
    case results of 
      []    -> foundFormat "\tNo results found"
      [res] -> foundFormat "\tOne result found" >> expandResult (snd res)
      _     -> foundFormat ("\tResults found: " ++ show (length results)) >> mapM_ showResult results

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
    Nothing  -> showError "Error parsing key"
    Just key -> drawKeyboard . uncurry resolveScale $ key
  putStrLn ""

showKey :: String -> IO ()
showKey raw = do
  let invalid  = showError "Error parsing key"
      parseKey [n,m] = maybe invalid draw key
        where draw = drawKeyboard . uncurry resolveScale
              key  = mzip (readNote n) (readMode m)
      parseKey _     = invalid
  parseKey . words $ raw
  putStrLn ""
