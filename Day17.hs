module Day17 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int,Int,Int,Int)

type Auto = S.Set Coord

neighbors :: Coord -> [Coord]
neighbors (x,y,z,w) = 
    [ (x+dx, y+dy, z+dz, w+dw)
    | dx <- [-1,0,1]
    , dy <- [-1,0,1]
    , dz <- [-1,0,1]
    , dw <- [-1,0,1]
    , not (dx == 0 && dy == 0 && dz == 0 && dw == 0)
    ]

tick :: Auto -> Auto
tick a = M.keysSet (M.filterWithKey f neighs) where
    neighs = M.fromListWith (+)
                [ (x', 1) 
                | x <- S.toList a 
                , x' <- neighbors x ]
    f _ 3 = True 
    f x 2 = x `S.member` a
    f _ _ = False

fromPlane :: String -> Auto
fromPlane s =
    S.fromList [ (x,y,0,0) 
               | (x,row) <- zip [0..] $ lines s
               , (y,cell) <- zip [0..] row
               , cell == '#' ]

main = do
    a <- fromPlane <$> readFile "input17.txt"
    print . S.size . (!! 6) . iterate tick $ a