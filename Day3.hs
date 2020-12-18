module Day3 where

import Data.List (transpose)

load :: FilePath -> IO [String]
load = (lines <$>) . readFile

route :: (Int, Int) -> [[a]] -> [a]
route (p,q) m = route' (map cycle m) where
    route' [] = []
    route' m = head (head m) : (route' . map (drop p) . drop q $ m)

hits = length . filter (== '#')

slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main = do
    m <- load "input3.txt"
    print . hits . route (3,1) $ m
    print . product $ [ hits (route s m) | s <- slopes ]
