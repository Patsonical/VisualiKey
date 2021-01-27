module Lib where

import Data.Text (pack)
import Rainbow   (Chunk, chunk)

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
