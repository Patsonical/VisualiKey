{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Keyboard
import Lib
import Music
import SpotifyAPI
import Types

import Control.Monad.Zip  (mzip)
import Rainbow
import Control.Monad.State
import Data.List (nub)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= start . handleArgs >>= handleError

handleError :: Either VKError () -> IO ()
handleError (Left e)   = putStrLn e >> pure ()
handleError (Right ()) = pure ()

handleArgs :: [String] -> VisualiKey ()
handleArgs args =
  let search query = (findSong . unwords) query >> controlLoop
      key    query = (liftIO . askKey . unwords) query
      lucky  query = (imFeelingLucky . unwords) query
  in case args of
    []          -> controlLoop
    ("-s":rest) -> search rest
    ("-k":rest) -> key    rest
    ("-l":rest) -> lucky  rest
    other       -> search other

controlLoop :: VisualiKey ()
controlLoop = do
  liftIO . putChunk . bold . fore blue $ "> "
  input   <- liftIO getLine
  case input of
    (':' : cmd) -> command cmd
    nonCmd      -> select nonCmd

command :: String -> VisualiKey ()
command (cmd:rest) = fromMaybe invalidCommand (lookup cmd cmds)
  where cmds = [ ('q', pure ())
               , ('s', search >> controlLoop)
               , ('k', key    >> controlLoop)
               ]
        search = (findSong        . strip) rest
        key    = (liftIO . askKey . strip) rest
command _ = invalidCommand

invalidCommand = liftIO (showError "Invalid command") >> controlLoop

select :: String -> VisualiKey ()
select input = do
  tracks <- gets results
  case readMaybe input of
    Nothing -> liftIO $ showError "Invalid selection, enter a number or :[cmd]"
    Just ix -> case lookup ix tracks of
      Nothing  -> liftIO $ showError "Selection out of bounds"
      Just res -> liftIO $ expandResult res
  controlLoop

findSong :: String -> VisualiKey ()
findSong name = do
  tracks <- zip [1..] . nub <$> (getAFFromResults =<< searchSong name)
  modify (\s -> s { results = tracks })
  presentResults

imFeelingLucky :: String -> VisualiKey ()
imFeelingLucky name = findSong name >> gets results >>= \case
  []    -> liftIO . putChunkLn . bold . fore yellow $ "No results found"
  (x:_) -> liftIO . expandResult . snd $ x

presentResults :: VisualiKey ()
presentResults = do
  let foundFormat = putChunkLn . bold . fore yellow . cp
  tracks <- gets results
  liftIO $ case tracks of
    []    -> foundFormat "\tNo results found"
    [res] -> foundFormat "\tOne result found" >> expandResult (snd res)
    _     -> do
      foundFormat ("\tResults found: " ++ show (length tracks))
      mapM_ showResult tracks

formatArtists :: [String] -> String
formatArtists artists = case length artists of
  0 -> ""
  1 -> head artists
  2 -> head artists ++ " and " ++ last artists
  _ -> fmt artists
    where fmt [] = ""
          fmt [x] = "and " ++ x
          fmt (x : y : xs) = x ++ ", " ++ fmt (y:xs)

showResult :: (Int, FinalTrack) -> IO ()
showResult (ix, FinalTrack name artists key) = do
  putChunk   . bold . fore blue  . cp $ '[' : show ix ++ "] "
  putChunk          . fore blue  . cp $ formatArtists artists ++ " - "
  putChunkLn        . fore green . cp $ name

keyHeader :: Key -> IO ()
keyHeader k = putChunkLn . bold . fore yellow . cp $ replicate 22 ' ' ++ "Key: " ++ formatKey k

expandResult :: FinalTrack -> IO ()
expandResult (FinalTrack name artists key) = do
  putChunk   . bold . fore blue   . cp $ '\t' : formatArtists artists ++ " - "
  putChunkLn . bold . fore green  . cp $ name
  keyHeader key
  drawKeyboard . uncurry resolveScale $ key
  putStrLn ""

askKey :: String -> IO ()
askKey raw = do
  let invalid  = showError "Error parsing key"
      parseKey [n,m] = maybe invalid draw key
        where draw k   = keyHeader k >> (drawKeyboard . uncurry resolveScale) k
              key      = readKey n m
      parseKey _       = invalid
  parseKey . words $ raw
  putStrLn ""
