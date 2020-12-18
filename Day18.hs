module Day18 where

import Text.Megaparsec

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad

type Parser a = Parsec () String a

token' :: Parser a -> Parser a
token' p = p <* space

pTerm :: Parser Integer
pTerm = token' decimal <|> (token' (char '(') *> pExpr <* token' (char  ')'))

pExpr :: Parser Integer
pExpr = pTerm >>= pOp

pOp :: Integer -> Parser Integer 
pOp x =     (((x +) <$> (token' (char '+') *> pTerm )) >>= pOp)
        <|> (((x *) <$> (token' (char '*') *> pTerm )) >>= pOp)
        <|> return x


pTerm2 :: Parser Integer
pTerm2 = token' decimal <|> (token' (char '(') *> pProd <* token' (char  ')'))

pProd :: Parser Integer
pProd = product <$> (pSum `sepBy` token' (char '*'))

pSum :: Parser Integer
pSum = sum <$> (pTerm2 `sepBy` token' (char '+'))


main = do
    f <- readFile "input18.txt"
    forM_ [pExpr, pProd] $ \p ->
        print . sum . map (either undefined id . parse p "") . lines $ f