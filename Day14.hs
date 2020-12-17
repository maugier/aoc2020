module Day14 where

import Data.List (unfoldr, foldl')
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Map as M
import Control.Monad (zipWithM)

toBinary :: Integer -> [Integer]
toBinary = take 36 . unfoldr (\x -> Just (x `mod` 2, x `div` 2))

fromBinary :: [Integer] -> Integer
fromBinary = foldr (\x r -> x + 2*r) 0

type Mask = String

mask :: Mask -> Integer -> Integer
mask s = fromBinary . zipWith mask' (reverse s) . toBinary where
    mask' '0' _ = 0
    mask' '1' _ = 1
    mask' 'X' b = b


type Parser p = Parsec () String p
type Address = Integer

pMask :: Parser Mask
pMask = string "mask = " >> many (oneOf "01X")

pMem :: Parser (Address,Integer)
pMem = (,) <$> (string "mem[" *> decimal) <*> (string "] = " *> decimal)

type Program = [Either Mask (Address,Integer)]

pProgram :: Parser Program
pProgram = eitherP pMask pMem `sepBy` char '\n'

type Memory = M.Map Address Integer

run :: Program -> Memory
run = fst . foldl' f (M.empty,undefined) where
    f (mem,_) (Left msk) = (mem,msk)
    f (mem,msk) (Right (a,d)) = ( M.insert a (mask msk d) mem, msk)

main = do
    f <- readFile "input14.txt"
    let Right p = parse pProgram "" f
    print (sum (run p))
    print (sum (run2 p))

--mask2 :: Mask -> Address -> [Address]
mask2 m = map fromBinary . zipWithM mask2' (reverse m) . toBinary where
    mask2' '0' x = [x]
    mask2' '1' _ = [1]
    mask2' 'X' _ = [0,1]

run2 :: Program -> Memory
run2 = fst . foldl' f (M.empty, undefined) where
    f (mem,_) (Left msk) = (mem,msk)
    f (mem,msk) (Right (a,d)) = (foldl' (\m' a' -> M.insert a' d m') mem (mask2 msk a), msk)
