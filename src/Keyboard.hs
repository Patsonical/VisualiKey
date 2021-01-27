{-# LANGUAGE OverloadedStrings #-}
module Keyboard (drawKeyboard) where

import Control.Monad        (replicateM_, forM_)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans  (lift)
import Data.List            (intersperse)
import Data.Text            (pack)
import Rainbow

import Lib
import Music

noteChar :: Char -> Note -> Int -> ReaderT [Note] IO ()
noteChar c note count = do
  on   <- asks (elem note)
  root <- asks (\ scale -> note == head scale)
  let color = if root then blue else white
  lift $ if on
         then putChunk (fore black . back color . chunk . pack . replicate count $ c)
         else putChunk (fore green              . chunk . pack . replicate count $ c)

(~~) :: Note -> Int -> ReaderT [Note] IO ()
(~~) = noteChar ' '

(~-) :: Note -> Int -> ReaderT [Note] IO ()
(~-) = noteChar '_'

pc  :: Chunk -> ReaderT [Note] IO ()
pc  c = lift $ putChunk   (fore green c)
pcn :: Chunk -> ReaderT [Note] IO ()
pcn c = lift $ putChunkLn (fore green c)

row0 = lift $ putChunkLn (fore green . cp $ " " ++ replicate 55 '_')
row1 = do
  replicateM_ 2 . sequence_ . (pc "|" : ) . intersperse (pc "|") $ 
    [ C  ~~ 2
    , C' ~~ 1
    , D  ~~ 1
    , D' ~~ 1
    , E  ~~ 2
    , F  ~~ 2
    , F' ~~ 1
    , G  ~~ 1
    , G' ~~ 1
    , A  ~~ 1
    , A' ~~ 1
    , B  ~~ 2
    ]
  pcn "|"
row2 = do
  replicateM_ 2 . sequence_ . (pc "|" : ) . intersperse (pc "|") $ 
    [ C  ~~ 2
    , C' ~- 1
    , D  ~~ 1
    , D' ~- 1
    , E  ~~ 2
    , F  ~~ 2
    , F' ~- 1
    , G  ~~ 1
    , G' ~- 1
    , A  ~~ 1
    , A' ~- 1
    , B  ~~ 2
    ]
  pcn "|"
row3 = do
  forM_ [C,D,E,F,G,A,B,C,D,E,F,G,A,B] $ \note -> do
    pc "|"
    note ~~ 3
  pcn "|"

row4 = do
  forM_ [C,D,E,F,G,A,B,C,D,E,F,G,A,B] $ \note -> do
    pc "|"
    note ~- 3
  pcn "|"

drawKeyboard :: [Note] -> IO ()
drawKeyboard scale = flip runReaderT scale $ do
  row0
  replicateM_ 3 row1
  row2
  replicateM_ 2 row3
  row4
