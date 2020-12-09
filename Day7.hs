module Day7 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative ((<$))
import Data.List (sort, nub)
import Data.Map (Map, (!), fromList)

type Color = String

type Parser x = Parsec () String x

data Rule = Rule Color [(Int,Color)]
    deriving (Show)

color :: Parser Color
color = do
    adjective <- many alphaNumChar
    space1
    noun <- many alphaNumChar
    return $ adjective ++ " " ++ noun

bag :: Parser Color
bag = color <* do
        space1
        string "bag"
        option ' ' (char 's')

rule :: Parser Rule
rule = do
        root <- bag
        string " contain "
        Rule root <$> contents

contents :: Parser [(Int,Color)]
contents = no <|> pairs where
    no = [] <$ string "no other bags"
    pairs = amount `sepBy1` string ", "
    amount = do
        amt <- decimal
        space1
        b <- bag
        return (amt, b)

load = mapM (parse rule "") . lines

haz :: Rule -> Color -> Bool
haz (Rule _ xs) c = c `elem` map snd xs

canContain :: [Rule] -> [Color] -> [Color]
canContain rs cs = nub . sort $ [c' | r <- rs, let Rule c' _ = r, any (haz r) cs]

close :: [Rule] -> [Color] -> [Color]
close rs cs = let
                cs' = nub . sort $ cs ++ canContain rs cs 
              in if cs == cs'
                    then cs
                    else close rs cs'

count :: [Rule] -> Map Color Int
count rs = cost' where
    cost' = fromList [ (k, cost vs) | Rule k vs <- rs]
    cost [] = 1
    cost vs = 1 + sum [ n * (cost' ! c) | (n,c) <- vs ]
