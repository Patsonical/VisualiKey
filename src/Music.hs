module Music where

import Data.Char (toLower)

import Lib
import Types

instance CycEnum Note where

(!!!) :: [a] -> [Int] -> [a]
xs !!! is = map (xs !!) is

resolveScale :: Note -> Mode -> [Note]
resolveScale root mode = case mode of
  Major -> cycEnumFrom root !!! [0,2,4,5,7,9,11]
  Minor -> cycEnumFrom root !!! [0,2,3,5,7,8,10]

readMode :: String -> Maybe Mode
readMode raw = case map toLower raw of
  "major" -> Just Major
  "minor" -> Just Minor
  _       -> Nothing

readNote :: String -> Maybe Note
readNote raw = case map toLower raw of
  -- Pure notes
  "a"  -> Just A
  "b"  -> Just B
  "c"  -> Just C
  "d"  -> Just D
  "e"  -> Just E
  "f"  -> Just F
  "g"  -> Just G
  -- Sharps
  "a♯" -> Just A'
  "b♯" -> Just C
  "c♯" -> Just C'
  "d♯" -> Just D'
  "e♯" -> Just F
  "f♯" -> Just F'
  "g♯" -> Just G'
  "a#" -> Just A'
  "b#" -> Just C
  "c#" -> Just C'
  "d#" -> Just D'
  "e#" -> Just F
  "f#" -> Just F'
  "g#" -> Just G'
  -- Flats
  "a♭" -> Just G'
  "b♭" -> Just A'
  "c♭" -> Just B
  "d♭" -> Just C'
  "e♭" -> Just D'
  "f♭" -> Just E
  "g♭" -> Just F'
  "ab" -> Just G'
  "bb" -> Just A'
  "cb" -> Just B
  "db" -> Just C'
  "eb" -> Just D'
  "fb" -> Just E
  "gb" -> Just F'
  -- Other
  _    -> Nothing

formatNote :: String -> String
formatNote raw = case map toLower raw of
  -- Pure notes
  "a"  -> "A"
  "b"  -> "B"
  "c"  -> "C"
  "d"  -> "D"
  "e"  -> "E"
  "f"  -> "F"
  "g"  -> "G"
  -- Sharps
  "a♯" -> "A♯"
  "b♯" -> "B♯"
  "c♯" -> "C♯"
  "d♯" -> "D♯"
  "e♯" -> "E♯"
  "f♯" -> "F♯"
  "g♯" -> "G♯"
  "a#" -> "A♯"
  "b#" -> "B♯"
  "c#" -> "C♯"
  "d#" -> "D♯"
  "e#" -> "E♯"
  "f#" -> "F♯"
  "g#" -> "G♯"
  -- Flats
  "a♭" -> "A♭"
  "b♭" -> "B♭"
  "c♭" -> "C♭"
  "d♭" -> "D♭"
  "e♭" -> "E♭"
  "f♭" -> "F♭"
  "g♭" -> "G♭"
  "ab" -> "A♭"
  "bb" -> "B♭"
  "cb" -> "C♭"
  "db" -> "D♭"
  "eb" -> "E♭"
  "fb" -> "F♭"
  "gb" -> "G♭"
  -- Other
  _    -> ""
