module Music where

import Lib

data Note = A | A'
          | B -- B' == C
          | C | C'
          | D | D'
          | E -- E' == F
          | F | F'
          | G | G'
          deriving (Show, Eq, Enum, Bounded)

instance CycEnum Note where

data Mode = Major | Minor deriving Show

type Key = (Note, Mode)

(!!!) :: [a] -> [Int] -> [a]
xs !!! is = map (xs !!) is

resolveScale :: Note -> Mode -> [Note]
resolveScale root mode = case mode of
  Major -> cycEnumFrom root !!! [0,2,4,5,7,9,11]
  Minor -> cycEnumFrom root !!! [0,2,3,5,7,8,10]

readNote :: String -> Maybe Note
readNote text
  | text == "A"      = Just A
  | text == "B"      = Just B
  | text == "C"      = Just C
  | text == "D"      = Just D
  | text == "E"      = Just E
  | text == "F"      = Just F
  | text == "G"      = Just G
  | text == "A#"     = Just A'
  | text == "B#"     = Just C
  | text == "C#"     = Just C'
  | text == "D#"     = Just D'
  | text == "E#"     = Just F
  | text == "F#"     = Just F'
  | text == "G#"     = Just G'
  | text == "A\9837" = Just G'
  | text == "B\9837" = Just A'
  | text == "C\9837" = Just B
  | text == "D\9837" = Just C'
  | text == "E\9837" = Just D'
  | text == "F\9837" = Just E
  | text == "G\9837" = Just F'
  | otherwise        = Nothing

readMode :: String -> Maybe Mode
readMode text
  | text == "Major" = Just Major
  | text == "Minor" = Just Minor
  | otherwise       = Nothing
