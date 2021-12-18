{-# LANGUAGE BangPatterns #-}

module Main where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-18.txt"

  putStrLn "Part 1:"
  print input


-- * Parsing

data Tree a = Single a | Pair (Tree a) (Tree a)
  deriving (Show)
type Parser = Parsec String ()

parse :: String -> [Tree Int]
parse = fromRight' . Parsec.parse pTrees ""
  where
    fromRight' (Right a) = a
    fromRight' e         = error $ "I said LEFT! I mean RIGHT! " <> show e

pTrees :: Parser [Tree Int]
pTrees = sepEndBy pTree newline

pTree :: Parser (Tree Int)
pTree = between (char '[') (char ']') $ pTreePair <|> pTreeSingle

pTreePair :: Parser (Tree Int)
pTreePair = Pair <$> pTree' <*> (char ',' *> pTree')
  where
    pTree' = pTreeSingle <|> pTree

pTreeSingle :: Parser (Tree Int)
pTreeSingle = Single <$> pInt

pInt :: Parser Int
pInt = read <$> many1 (digit <|> char '-')
