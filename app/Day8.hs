{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Algorithms.Tim as MV

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-8.txt"

  print input

  putStrLn "Part 1:"
  undefined

-- * Segments
--
-- These represent each value of the seven segment display in different ways.

-- | The expected lit segments for each of the ten digits that can be
-- represented by the four segment display. The segment indices for each number
-- are in sorted.
expectedSegments :: B.Vector (U.Vector Char)
expectedSegments = B.fromList . map U.fromList $
  [ "abcefg"  -- 0
  , "cf"      -- 1
  , "acdeg"   -- 2
  , "acdeg"   -- 2
  , "acdfg"   -- 3
  , "bcdf"    -- 4
  , "abdfg"   -- 5
  , "abdefg"  -- 6
  , "acf"     -- 7
  , "abcdefg" -- 8
  , "abcdfg"  -- 9
  ]

-- | Sorted on the sequence length, stored as pairs of @(number, sequence)@.
sortedExpectedSegments :: B.Vector (Int, U.Vector Char)
sortedExpectedSegments
  = B.modify (MV.sortBy (comparing $ U.length . snd))
  $ B.indexed expectedSegments

-- | 'sortedExpectedSegments', filtered by which numbers have a unique number of
-- lit segments.
uniqueSegments :: B.Vector (Int, U.Vector Char)
uniqueSegments = B.ifilter isUnique sortedExpectedSegments

-- | The opposite of 'uniqueSegments'
duplicateSegments :: B.Vector (Int, U.Vector Char)
duplicateSegments = B.ifilter (\i s -> not $ isUnique i s) sortedExpectedSegments

-- | A predicate for filtering 'sortedExpectedSegments' based on uniqueness.
isUnique :: Int -> (Int, U.Vector Char) -> Bool
isUnique idx (_, segments)
  | l <- U.length segments
  ,    (idx /= 0                 && l == U.length (snd $ sortedExpectedSegments B.! (idx - 1)))
    || (idx /= (numSegments - 1) && l == U.length (snd $ sortedExpectedSegments B.! (idx + 1)))
  = False
  | otherwise
  = True
  where
    numSegments = B.length sortedExpectedSegments

-- * Solution

data Display = Display
  { -- | The ten digits from 'expectedSegments', but with the character ->
    -- segment assignments scrambled.
    patterns :: B.Vector (U.Vector Char)
    -- | A four digit output value, with the same scrambled assignments as those
    -- in 'patterns'.
  , outputs  :: B.Vector (U.Vector Char)
  }
  deriving (Show)

parse :: String -> [Display]
parse = map parseLine . lines
  where
    parseLine :: String -> Display
    parseLine l = case splitOn " | " l of
      [words -> ps, words -> os] -> Display (parseSegments ps) (parseSegments os)
      _                          -> error "What is this even!?"

    -- Make sure all segments are in the same order for readability reasons
    parseSegments :: [[Char]] -> B.Vector (U.Vector Char)
    parseSegments = B.fromList . map (U.modify MV.sort . U.fromList)
