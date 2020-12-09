module Day2 where

import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Text.Megaparsec
import Control.Applicative ((<$>))

data Policy = Policy Char Int Int
    deriving (Show)

policy :: Parsec () String Policy
policy = do
    a <- decimal
    char '-'
    b <- decimal
    spaceChar
    l <- letterChar
    return $ Policy l a b

passline :: Parsec () String (Policy, String)
passline = do
    pl <- policy
    char ':'
    spaceChar
    pass <- many letterChar
    return (pl, pass)

obeys :: String -> Policy -> Bool
--obeys pass (Policy l a b) = count >= a && count <= b where
--    count = length . filter (== l) $ pass
obeys pass (Policy l a b) = ((pass !! (a-1)) == l)/= ((pass !! (b-1)) == l)

goodline :: Parsec () String Bool
goodline = uncurry (flip obeys) <$> passline


countfile :: IO ()
countfile = do
    blob <- readFile "input2.txt"
    case mapM (parse goodline "") (lines blob) of
        Left e -> print e
        Right out -> print . length . filter id $ out
