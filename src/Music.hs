module Music where

data Note = A | A'
          | B | B'
          | C | C'
          | D | D'
          | E | E'
          | F | F'
          | G | G'
          deriving (Show, Eq, Enum)

data Mode = Major | Minor

type Key = (Note, Mode)

(!!!) :: [a] -> [Int] -> [a]
xs !!! is = map (xs !!) is

--resolveScale :: Note -> Mode -> [Note]
--resolveScale root mode = case mode of
--  Major -> (cycle . enumFrom)
--  Minor -> []
