{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Text (pack)
import Rainbow
import Control.Exception
import Data.Either (Either(..))
import Control.Monad.Except

import Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.State (evalStateT)

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

infixl 2 >>+=
(>>+=) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
a1 >>+= a2 = \v -> a1 v >> a2 v

-- As in Relude
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

cp :: String -> Chunk
cp = chunk . pack

showError :: String -> IO ()
showError = putChunkLn . fore red . cp

strip :: String -> String
strip = dropWhile (==' ')

runVK :: VKState -> VisualiKey a -> IO (Either VKError a)
runVK st = runExceptT . flip evalStateT st

start :: VisualiKey a -> IO (Either VKError a)
start vk = do
  manager <- newManager tlsManagerSettings
  let st = VKState manager (Token "" "" 0) []
  runVK st vk

safeLiftIO :: IO a -> VisualiKey a
safeLiftIO io = do
  result <- liftIO $ catch (Right <$> io) (\e -> 
    let str = show (e :: IOException) in pure . Left $ str)
  case result of
    Left e  -> throwError e
    Right a -> pure a

maybeToExcept :: VKError -> Maybe a -> VisualiKey a
maybeToExcept errorMsg = \case
  Nothing -> throwError errorMsg
  Just a  -> pure a
