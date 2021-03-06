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

  putStrLn "\nPart 2:"
  let !sparseResult = doTimes (sparseInsertElements rules) 40 (toSparse template)
      !sparseCounts = sortBy (comparing snd) . M.toList $ sparseTally (head template, last template) sparseResult
  print $ snd (last sparseCounts) - snd (head sparseCounts)


doTimes :: (a -> a) -> Int -> a -> a
doTimes f n = (!! n) . iterate f


type Element = Char
type Rules   = HashMap (Element, Element) Element


-- * Part 1
-- Because at this point linked lists still work great.

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


-- * Part 2
-- Thanks.

type SparsePolymer = HashMap (Element, Element) Int

-- | We need to make everything sparse _again_. And this time there's no getting
-- around it. This map contains the number of times these two-element sequences
-- occur.
toSparse :: [Element] -> SparsePolymer
toSparse = go M.empty
  where
    go !m (x : y : rest) = go (M.insertWith (+) (x, y) 1 m) (y : rest)
    go !m _              = m

-- | The same as 'insertElements', but for the sparse set.
sparseInsertElements :: Rules -> SparsePolymer -> SparsePolymer
sparseInsertElements rules template =
  M.unionWith (+) template (sparseInsertions rules template)

-- | The equivalent of 'insertions', but now returning deltas for the
-- 'SparsePolymer' map.
sparseInsertions :: Rules -> SparsePolymer -> SparsePolymer
sparseInsertions rules = foldl' go M.empty . M.toList
  where
    go :: SparsePolymer -> ((Element, Element), Int) -> SparsePolymer
    go !m ((x, y), count)
      | Just e <- M.lookup (x, y) rules
        -- This is idempotent when the pair is empty and the count is zero so it
        -- doesn't matter, but we'd still be wasting work
      , count > 0
      = foldl' (\m' (k, v) -> M.insertWith (+) k v m') m
          [ ((x, y), negate count)
          , ((x, e), count)
          , ((e, y), count)
          ]
      | otherwise
      = m


-- | Every character except for the first and the last character is duplicated,
-- so we just divide the total counts by two and then add the first and last
-- character back.
--
-- Git blame this for a more roundabout version that kept track of duplicate
-- character counts and compensated with that.
sparseTally :: (Char, Char) -> SparsePolymer -> HashMap Element Int
sparseTally (firstChar, lastChar)
  = M.unionWith (+) (M.fromList [(firstChar, 1), (lastChar, 1)])
  . M.map (`div` 2)
  . M.fromListWith (+)
  . concatMap (\((x, y), count) -> [(x, count), (y, count)])
  . M.toList


-- * Parsing

parse :: String -> ([Element], Rules)
parse = go . lines
  where
    go ((!template) : _ : rules) =
      let !ruleMap = M.fromList $ map ((\[[x, y], [r]] -> ((x, y), r)) . splitOn " -> ") rules
       in (template, ruleMap)
    go _ = error "Nope"
