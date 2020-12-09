module Day6 where

import Data.List.Split (splitOn)
import Data.List (nub, foldl1)
import Data.Set (Set)

parse :: String -> [[String]]
parse = splitOn [""] . lines

compute :: String -> Int
compute = sum . map (length . nub . concat) . parse
