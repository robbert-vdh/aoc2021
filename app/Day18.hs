{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Applicative as A
import Data.List
import Data.Maybe
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-18.txt"

  putStrLn "Part 1:"
  print . magnitude . sumReduce $ input

  putStrLn "\nPart 2:"
  print . maximum . map (magnitude . sumReduce) $ [[x, y] | x <- input, y <- input, x /= y]


-- * Part 1

data Tree a = Single a | Pair !(Tree a) !(Tree a)
  deriving (Eq)

instance Show a => Show (Tree a) where
  show (Single a) = "[" <> show a <> "]"
  show (Pair l r) = "[" <> show' l <> "," <> show' r <> "]"
    where
      show' (Single a) = show a
      show' p          = show p


magnitude :: Tree Int -> Int
magnitude (Single n) = n
magnitude (Pair l r) = (magnitude l * 3) + (magnitude r * 2)

-- | Add and reduce all trees in the list from left to right.
sumReduce :: [Tree Int] -> Tree Int
sumReduce = foldl1' addReduce

-- | Add the trees- I mean snailfish numbers and then perform the reduction.
addReduce :: Tree Int -> Tree Int -> Tree Int
addReduce !l !r = reduce (Pair l r)

data Action a
    -- | Nothing happened during reduction, stop when the final node returns this.
  = None
    -- | A reduction action has already been performed.
  | Reduced
    -- | Additions to the single elements to the left and the right of the
    -- exploded pair that still need to be performed. These are maybes because
    -- this may involve having to go up and down the tree.
  | Explode (Maybe a) (Maybe a)
  deriving (Show)

-- | Keep performing reductions on a tree until it doesn't change anymore.
reduce :: Tree Int -> Tree Int
reduce !tree
  | Just reduced <- maybeReduce tree = reduce reduced
  | otherwise                        = tree

-- | Perform a reduction on a tree. This will be 'Nothing' if the tree was not
-- changed. If this returns a new tree, keep repeating this until nothing
-- changes anymore.
--
-- Essentially, we manually implement a zipper here using the 'Explode'
-- constructor in 'Action'.
maybeReduce :: Tree Int -> Maybe (Tree Int)
maybeReduce = fromAction . go 0
  where
    -- dbg (output, action) = trace ("Input:  " <> show input <> "\nAction: " <> show action <> "\nOutput: " <> show output <> "\n") (output, action)

    fromAction (_, None) = Nothing
    fromAction (t, _)    = Just t

    -- | The boolean here indicates whether the tree explodes since that
    -- involves modifying siblings.
    go :: Int -> Tree Int -> (Tree Int, Action Int)
    go depth (Pair (Single l) (Single r))
      -- Explode
      | depth >= 4
      = (Single 0,                                                  Explode (Just l) (Just r))
    go _     (Single n)
      -- Split
      | n >= 10 -- Damnit, this being @> 10@ took an hour of my life
      = (Pair (Single $ n `div` 2) (Single $ n - (n `div` 2)),      Reduced)
      | otherwise
      = (Single n,                                                  None)
    go depth (Pair l r)
      -- When the leftmost pair has exploded, then the value of the right
      -- element should be added to the leftmost element of the right subtree,
      -- and the value of the left element needs to be passed up since we can't
      -- rewrite the neighbouring subtrees from here. @explodeR@ may be a
      -- 'Nothing' if the value has already been propagated. Pattern mating on
      -- @Just explodeR@ also took an hour of my life.
      | (l', Explode explodeL explodeR) <- go (depth + 1) l
      = (Pair l' (maybe r (\n -> modifyLeftmost (+ n) r) explodeR), Explode explodeL Nothing)
      -- Same thing for when the left element doesn't explode but the right one
      -- does.
      | (r', Explode explodeL explodeR) <- go (depth + 1) r
      = (Pair (maybe l (\n -> modifyRightmost (+ n) l) explodeL) r', Explode Nothing explodeR)
      -- When no reduction takes place in the left subtree, check the right one
      | (l', None)                      <- go (depth + 1) l
      , (r', action)                    <- go (depth + 1) r
      = (Pair l' r',                                                 action)
      -- Otherwise a reduction that we can't or don't need to handle here has
      -- happened in the left subtree, and we should just propagate that
      | (l', action)                    <- go (depth + 1) l
      = (Pair l' r,                                                  action)

-- | Modify the leftmost single element in the tree.
modifyLeftmost :: forall a. (a -> a) -> Tree a -> Tree a
modifyLeftmost f = fromJust . go
  where
    go :: Tree a -> Maybe (Tree a)
    go (Single x) = Just $ Single (f x)
    go (Pair l r) = (Pair <$> go l <*> pure r) A.<|> (Pair l <$> go r)

-- | The same as 'modifyLeftmost', but for the rightmost element in the tree.
modifyRightmost :: forall a. (a -> a) -> Tree a -> Tree a
modifyRightmost f = fromJust . go
  where
    go :: Tree a -> Maybe (Tree a)
    go (Single x) = Just $ Single (f x)
    go (Pair l r) = (Pair l <$> go r) A.<|> (Pair <$> go l <*> pure r)


-- * Parsing

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
