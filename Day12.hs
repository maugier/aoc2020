module Day12 where

import Control.Applicative
import Data.List (foldl')

type Command = (Char, Int)
type Ship = (Int,Int,Int)
type Waypoint = (Int,Int)

load :: IO [Command]
load = do
    f <- readFile "input12.txt"
    return $ (\l -> (head l, read (tail l))) <$> lines f


start :: Ship
start = (0,0,0)

step :: Ship -> Command -> Ship
step (x,y,d) (cmd, v) = case cmd of
    'N' -> (x,y+v,d)
    'S' -> (x,y-v,d)
    'E' -> (x+v,y,d)
    'W' -> (x-v,y,d)
    'L' -> (x,y,(d+v) `mod` 360)
    'R' -> (x,y,(d-v) `mod` 360)
    'F' -> let sub = case d of
                        0 -> 'E'
                        90 -> 'N'
                        180 -> 'W'
                        270 -> 'S'
            in step (x,y,d) (sub,v)

step2 :: (Ship,Waypoint) -> Command -> (Ship,Waypoint)
step2 (s,(x,y)) ('N', v) = (s,(x,y+v))
step2 (s,(x,y)) ('S', v) = (s,(x,y-v))
step2 (s,(x,y)) ('E', v) = (s,(x+v,y))
step2 (s,(x,y)) ('W', v) = (s,(x-v,y))
step2 s ('L', 0) = s
step2 (s,(x,y)) ('L', v) = step2 (s,(-y,x)) ('L', v-90)
step2 s ('R', v) = step2 s ('L', 360-v)
step2 ((sx,sy,d),(x,y)) ('F', v) = ((sx+v*x,sy+v*y,d),(x,y))


run :: [Command] -> Ship
run = foldl' step start

run2 :: [Command] -> Ship
run2 = fst . foldl' step2 (start, (10,1))

distance :: Ship -> Int
distance (x,y,_) = abs x + abs y