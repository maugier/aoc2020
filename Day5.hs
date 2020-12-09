module Day5 where

newtype Pass = Pass String
    deriving (Show, Eq)

pass :: String -> Maybe Pass
pass s | length s == 10 &&
         all (`elem` "FB") (take 7 s) &&
         all (`elem` "LR") (drop 7 s) = Just (Pass s)
       | otherwise = Nothing

pass' :: String -> Pass
pass' x = let Just p = pass x in p

fromBits :: [Int] -> Int
fromBits = foldl (\x y -> 2*x+y) 0

row :: Pass -> Int
row (Pass p) = fromBits (map bit $ take 7 p) where
    bit 'F' = 0
    bit 'B' = 1

column :: Pass -> Int
column (Pass p) = fromBits (map bit $ drop 7 p) where
    bit 'L' = 0
    bit 'R' = 1

seat :: Pass -> Int
seat p = row p * 8 + column p