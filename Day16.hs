module Day16 where

import Text.Megaparsec
    ( noneOf, parse, endBy, many, sepBy, Parsec )
import Text.Megaparsec.Char ( char, string )
import Text.Megaparsec.Char.Lexer ( decimal )

import Data.Map ( (!), keys, Map, fromList )
import Data.List ( transpose )

type Field = String 
type Constraint = [(Integer, Integer)]
type Ticket = [Integer]

obeys :: Integer -> Constraint -> Bool 
obeys x = any (\(a,b) -> x >= a && x <= b)

type Parser t = Parsec String String t

pRange :: Parser (Integer,Integer)
pRange = (,) <$> decimal <* char '-' <*> decimal

pConstraint :: Parser Constraint
pConstraint = pRange `sepBy` string " or "

pField :: Parser (Field, Constraint)
pField = (,) <$> many (noneOf "\n:") <* string ": " <*> pConstraint

pFields :: Parser (Map Field Constraint)
pFields = fromList <$> pField `endBy` char '\n'

pTicket :: Parser [Integer]
pTicket = decimal `sepBy` char ','

data Problem = Problem {
    constraints :: Map Field Constraint,
    mine :: Ticket,
    others :: [Ticket]
} deriving (Show,Eq,Ord)

pInput :: Parser Problem
pInput = do
    cs <- pFields
    string "\nyour ticket:\n"
    mine <- pTicket
    string "\n\nnearby tickets:\n"
    others <- pTicket `endBy` string "\n"
    return $ Problem cs mine others

parseFile p f = parse p f <$> readFile f

invalid :: Map Field Constraint -> Ticket -> Bool
invalid cs = not . all (flip any cs . obeys)

scanningErrors (Problem cs _ tickets) =
    sum [ x 
        | t <- tickets
        , x <- t
        , not (any (x `obeys`) cs)
        ]

clean :: Problem -> Problem
clean (Problem cs m o) = Problem cs m (Prelude.filter (not . invalid cs) o)


type FieldMap = [String]

pick :: [a] -> [(a,[a])]
pick [] = []
pick (x:xs) = (x, xs) : [ (y, x:ys) | (y,ys) <- pick xs]

possibleFields :: Problem -> [[String]]
possibleFields (Problem cs m os) =
    [ [ f 
      | f <- keys cs
      , all (`obeys` (cs ! f)) column
      ]
    | column <- transpose os
    ]
        
possibleMaps :: Problem -> [[String]]
possibleMaps p = go (possibleFields p) [] where
    go (fcs:rest) taken = [ f:fs 
                          | f <- fcs
                          , f `notElem` taken
                          , fs <- go rest (f:taken)
                          ]
    go [] _ = [[]]

main = do
    Right p <- parseFile pInput "input16.txt"
    let pm = possibleMaps (clean p)
    let answer = product [ v | (v,f) <- zip (mine p) (head pm)
            , take 9 f == "departure" ]
    print answer