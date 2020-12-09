module Day3 where

import Data.List (transpose)

load :: IO [String]
load = lines <$> readFile "input3.txt"

testmap = [ "..##......." 
          , "#...#...#.." 
          , ".#....#..#." 
          , "..#.#...#.#" 
          , ".#...##..#." 
          , "..#.##....." 
          , ".#.#.#....#" 
          , ".#........#" 
          , "#.##...#..." 
          , "#...##....#" 
          , ".#..#...#.#"
          ]

--route slope m = zipWith (!!) (map cycle m) (iterate (slope +) 0)
route s m = route' s (map cycle m) where
    route' _ [] = []
    route' (p,q) m = head (head m) : (route (p,q) . map (drop p) . drop q $ m)

hits = length . filter (== '#')

allslopes m = product [ hits (route s m) | s <- [(1,1), (3,1), (5,1), (7,1), (1,2)]] 