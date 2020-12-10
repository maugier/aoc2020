module Day10 where

import Data.List
import Data.Map (Map, (!), fromList, lookup)
import Data.Maybe (maybeToList)
import Control.Applicative ((<$>), (<*>))

differences :: [Integer] -> [Integer]
differences =  (++[3]) . (zipWith subtract <*> tail) . (0:) . sort 

metric :: [Integer] -> Integer
metric xs = counts 1 * counts 3 where
    diffs = differences xs     
    counts k = toInteger . length . filter (k ==) $ diffs                                   
-- Yes, this is dynamic programming :)
chaincount :: [Integer] -> Integer
chaincount ps = count' ! maximum ps where
    count' = Data.Map.fromList $
        [(0, 1)] ++
        [ (i, sum [ x | o <- [3,2,1], x <- maybeToList $ Data.Map.lookup (i-o) count' ]) | i <- ps ]

main = do
    f <- readFile "input10.txt"
    let xs = (read <$>) . lines $ f
    print (metric xs)
    print (chaincount xs)
