module Day13 where

import Data.List
import Data.Ord
import Data.List.Split

import Control.Arrow (second)
import Math.NumberTheory.Moduli.Chinese

waiting :: Integer -> Integer -> Integer
waiting s b = ((negate s `rem` b) + b) `rem` b

earliest :: Integer -> [Integer] -> Integer
earliest s = minimumBy (comparing (waiting s))

stamp = 1000303

busSource = "41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,541,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,983,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19"

type Bus = Integer

buslines :: [Bus]
buslines = 
      map read
    . filter (/= "x")
    . splitOn ","
    $ busSource


solution1 = waiting stamp (earliest stamp buslines)

type Time = Integer

constraints :: [(Time,Bus)]
constraints = map (second read) . filter (("x" /=).snd) . zip [0..] . splitOn "," $ busSource

solve2 :: [(Time,Bus)] -> Maybe Time
solve2 tbs = chineseRemainder [(b-t,b) | (t,b) <- tbs]

solution2 = solve2 constraints