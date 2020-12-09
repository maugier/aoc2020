module Day9 where

import Data.List
import Data.Maybe

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [ (x,y) | y <- xs] ++ pairs xs

validate :: Int -> [Integer] -> [(Integer,Bool)]
validate n xs = map check windows where
    windows = zip (drop n xs) . map (take n) . tails $ xs
    check (x, ws) = (x, any (\(y1,y2) -> x == y1 + y2) (pairs ws))

testv = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
-- validate 5 testv

subseqs :: [a] -> [[a]]
subseqs = concatMap tails . inits

breakc :: Integer -> [Integer] -> Maybe [Integer]
breakc k = find ((k ==).sum) . filter ((1<).length) . subseqs

main = do
    f <- readFile "input9.txt"
    let xs = map read (lines f) :: [Integer]
    let Just (weak,_) = find (not.snd) . validate 25 $ xs
    print weak
    let Just win = breakc weak xs
    print $ maximum win + minimum win
