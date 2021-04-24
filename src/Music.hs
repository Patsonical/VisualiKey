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

parseMode :: Int -> Maybe Mode
parseMode 0 = Just Minor
parseMode 1 = Just Major
parseMode _ = Nothing

parseNote :: Int -> Maybe Note
parseNote n = enumFrom minBound !!? n

getKey :: TuneableTrack -> Maybe Key
getKey (TuneableTrack _ k m) = (,) <$> parseNote k <*> parseMode m

formatKey :: Key -> String
formatKey (note, mode) = case mode of
  Major -> case note of
    C  -> "C Major"
    C' -> "D♭ Major"
    D  -> "D Major"
    D' -> "E♭ Major"
    E  -> "E Major"
    F  -> "F Major"
    F' -> "F♯ Major"
    G  -> "G Major"
    G' -> "A♭ Major"
    A  -> "A Major"
    A' -> "B♭ Major"
    B  -> "B Major"
  Minor -> case note of
    C  -> "C Minor"
    C' -> "C♯ Minor"
    D  -> "D Minor"
    D' -> "D♯ Minor"
    E  -> "E Minor"
    F  -> "F Minor"
    F' -> "F♯ Minor"
    G  -> "G Minor"
    G' -> "G♯ Minor"
    A  -> "A Minor"
    A' -> "B♭ Minor"
    B  -> "B Minor"

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

-- DEPRECATED {{{
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
-- }}}
