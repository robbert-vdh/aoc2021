{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List
import Data.List.Split
import Data.Ord


main :: IO ()
main = do
  (!template, !rules) <- parse <$> readFile "inputs/day-14.txt"

  putStrLn "Part 1:"
  let !result = doTimes (insertElements rules) 10 template
      !counts = sortBy (comparing snd) . M.toList $ tally result
  print $ snd (last counts) - snd (head counts)


doTimes :: (a -> a) -> Int -> a -> a
doTimes f n = (!! n) . iterate f


type Element = Char
type Rules   = HashMap (Element, Element) Element


-- | Insert elements in the polyer template according to the insertion rules.
insertElements :: Rules -> [Element] -> [Element]
insertElements rules template = go 0 template (insertions rules template)
  where
    -- Do the actual insertions according to the insertion list. The index
    -- parameter follows the /original/ index in the list, not the actual index
    -- in the new list. That way we can insert the elements at the correct
    -- places.
    go :: Int -> [Element] -> [(Int, Element)] -> [Element]
    go idx (x : xs) ins@((nextIdx, e) : es)
      | idx == nextIdx = e : x : go (idx + 1) xs es
      | otherwise      =     x : go (idx + 1) xs ins
    go _   xs           [] = xs
    go _   _            _  = error "Yeah uh what happened here"

-- | Compute the elements to insert in polymer template using the list of
-- insertion rules. The returned list contains pairs if indices in __the
-- original list__ at which the element has to be inserted (this thus has to be
-- shifted while doing the insertions). An index of 0 means that the element has
-- to be inserted at the very start of the list.
insertions :: Rules -> [Element] -> [(Int, Element)]
insertions rules = go 0
  where
    go :: Int -> [Element] -> [(Int, Element)]
    go idx (x : y : xs)
      | Just e <- M.lookup (x, y) rules = (idx + 1, e) : go (idx + 1) (y : xs)
      | otherwise                       =                go (idx + 1) (y : xs)
    go _   _ = []


tally :: [Element] -> HashMap Element Int
tally = M.fromListWith (+) . map (,1)


parse :: String -> ([Element], Rules)
parse = go . lines
  where
    go ((!template) : _ : rules) =
      let !ruleMap = M.fromList $ map ((\[[x, y], [r]] -> ((x, y), r)) . splitOn " -> ") rules
       in (template, ruleMap)
    go _ = error "Nope"
