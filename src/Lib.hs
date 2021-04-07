{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Text (pack)
import Rainbow
import Control.Exception
import Data.Either (Either(..))
import Control.Monad.Except

import Types

class (Bounded a, Enum a) => CycEnum a where
  toCycEnum         :: Int -> a
  fromCycEnum       :: a -> Int
  cycSucc           :: a -> a
  cycPred           :: a -> a
  cycEnumFrom       :: a -> [a]
  cycEnumFromThen   :: a -> a -> [a]
  cycEnumFromTo     :: a -> a -> [a]
  cycEnumFromThenTo :: a -> a -> a -> [a]

  toCycEnum               = toEnum
  fromCycEnum             = fromEnum
  cycSucc                 = succ
  cycPred                 = pred
  cycEnumFrom s           = enumFrom s ++ (cycle . enumFrom) minBound
  cycEnumFromThen s n     = [] -- TODO
  cycEnumFromTo s e       = [] -- TODO
  cycEnumFromThenTo s n e = [] -- TODO

zipMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
zipMaybe x = do
  a <- fst x
  b <- snd x
  pure (a,b)

cp :: String -> Chunk
cp = chunk . pack

showError :: String -> IO ()
showError = putChunkLn . fore red . cp

strip :: String -> String
strip = dropWhile (==' ')

catchIO :: IO a -> VisualiKey a
catchIO io = do
  result <- liftIO $ catch (Right <$> io) (\e -> 
    let str = show (e :: IOException) in pure . Left $ str)
  case result of
    Left e  -> throwError e
    Right a -> pure a

maybeToExcept :: VKError -> Maybe a -> VisualiKey a
maybeToExcept errorMsg = \case
  Nothing -> throwError errorMsg
  Just a  -> pure a
