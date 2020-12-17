module Day11 where


import Data.Array
import Data.Maybe (mapMaybe, isJust, catMaybes)
import Control.Monad (join)
import Control.Applicative
import Data.Foldable

data Seat = Free | Occupied
    deriving (Eq, Ord, Show)
type Loc = (Int,Int)
type Floor = Array Loc (Maybe Seat)



lfloor :: [String] -> Floor
lfloor grid = array ((1,1),(h,w)) [ ((x,y), seat s)
    | (x,row) <- zip [1..] grid
    , (y,s  ) <- zip [1..] row ] where
        seat '.' = Nothing
        seat '#' = Just Occupied
        seat 'L' = Just Free
        h = length grid
        w = length (head grid)

neighbors (x,y) = [ (x+a,y+b) | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0]

lookupa :: Ix i => Array i e -> i -> Maybe e
lookupa arr ix = if inRange (bounds arr) ix then Just (arr ! ix) else Nothing
    

rule :: [Seat] -> Seat -> Seat
rule ns Free | all (== Free) ns = Occupied
rule ns Occupied | length (filter (== Occupied) ns) >= 4 = Free
rule _ other = other


tick :: Floor -> Floor
tick old = array (bounds old) [ (l, s') | (l, s) <- assocs old
                              , let ns = mapMaybe join $ (old `lookupa`) <$> neighbors l
                              , let s' = rule ns <$> s ]

showFloor :: Floor -> [String]
showFloor fl = [ [ sym (fl ! (x,y)) | y <- [1..w] ] | x <- [1..h]] where
    (_, (h,w)) = bounds fl
    sym Nothing = '.'
    sym (Just Occupied) = '#'
    sym (Just Free) = 'L'

printFloor = mapM_ putStrLn . showFloor

stabilize :: Eq a => (a -> a) -> a -> a
stabilize f x = let y = f x
                in if x == y
                     then x
                     else stabilize f y

occupiedSeats :: Floor -> Int
occupiedSeats = length . filter (== Just Occupied) . toList

type Direction = Loc -> Loc

directions :: [Direction]
directions = [\(x,y) -> (x+h, y+w) | h <- [-1,0,1], w <- [-1,0,1], h /= 0 || w /= 0 ]

sight :: Floor -> Direction -> Loc -> Maybe Loc
sight f d l | not (bounds f `inRange` l') = Nothing
            | isJust (f ! l') = Just l'
            | otherwise = sight f d l'
    where l' = d l
    
los :: Floor -> Loc -> [Loc]
los f l =  mapMaybe (($ l) . sight f) directions

loscache :: Floor -> Array Loc [Loc]
loscache f = array (bounds f) [ (i, los f i) | i <- indices f]

ticks2 :: Floor -> [Floor]
ticks2 f0 = iterate tick2 f0 where
    lc = loscache f0
    tick2 f = array (bounds f0) [ (i, step i <$> (f ! i)) | i <- indices f0] where
        occupied s = length . filter (== Just Occupied) $ (f !) <$> (lc ! s)
        step i Free | occupied i == 0 = Occupied
        step i Occupied | occupied i >= 5 = Free
        step _ other = other

stabilize2 :: Floor -> Maybe Floor
stabilize2 = (fst <$>) . find (uncurry (==)) . (zip <*> tail) . ticks2