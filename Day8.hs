module Day8 where

import Data.Array (Array, listArray, (!), bounds, (//), indices)
import Text.Read
import qualified Data.Set as S
import Data.Maybe

data Op = Nop | Acc | Jmp
    deriving (Show, Eq, Ord)

type Instr = (Op, Int)

type Program = Array Int Instr

(<?>) :: Maybe a -> String -> Either String a
Just a <?> _ = Right a
Nothing <?> s = Left s

load :: FilePath -> IO Program
load f = do
    txt <- readFile f
    case mapM parseInstr $ lines txt of
        Right instrs -> 
            return $ listArray (0, length instrs - 1) instrs
        Left err -> error err

parseInstr :: String -> Either String (Op, Int)
parseInstr s = do
    op <- case take 3 s of
            "nop" -> Right Nop
            "acc" -> Right Acc
            "jmp" -> Right Jmp
            _     -> Left $ "invalid opcode: " ++ s
    i <- case drop 3 s of
        (' ':'+':plus) -> readMaybe plus <?> "invalid offset"
        (' ':minus@('-':_)) -> readMaybe minus <?> "invalid offset"
    return (op, i)

trace :: Program -> [(Int,Int)]
trace p = steps (fst.bounds $ p , 0) where
    stop = snd (bounds p)
    steps s = s : if fst s == stop 
                    then []
                    else steps (next s)
    next (i,a) = case p ! i of
                    (Nop, _) -> (i+1, a)
                    (Acc, o) -> (i+1, a+o)
                    (Jmp, o) -> (i+o, a)


detectLoop :: Program -> Maybe (Int,Int)
detectLoop = flip (foldr tick (const Nothing)) S.empty . trace where
    tick (i,a) go s =
        if i `S.member` s
            then Just (i,a)
            else go (S.insert i s)


fuzz :: Int -> Program -> Maybe Program
fuzz n p = case p ! n of
    (Nop, x) -> Just $ p // [(n, (Jmp, x))]
    (Acc, x) -> Nothing
    (Jmp, x) -> Just $ p // [(n, (Nop, x))]

fuzzes :: Program -> [Program]
fuzzes p = catMaybes [fuzz n p | n <- indices p] 