{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char
import Data.Massiv.Array (Matrix, Vector, Ix1, Ix2(..), Sz(..), D, DS, U)
import qualified Data.Massiv.Array as A


main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-9.txt"

  let lowPoints = findLowPoints input

  putStrLn "Part 1:"
  print . computeSum . riskLevel . filterLowPoints $ lowPoints


-- | Mark each low point in the array.
findLowPoints :: (A.Manifest r Int) => Matrix r Int -> Matrix D (Bool, Int)
findLowPoints arr = A.setComp A.Par $ A.imap markLowPoint arr
  where
    Sz2 rows cols = A.size arr

    markLowPoint :: Ix2 -> Int -> (Bool, Int)
    markLowPoint (Ix2 y x) height =
      let leftBorder   = x == 0
          topBorder    = y == 0
          rightBorder  = x == cols - 1
          bottomBorder = y == rows - 1
          isLowPoint =
               (leftBorder   || height < (arr A.! Ix2 y       (x - 1)))
            && (topBorder    || height < (arr A.! Ix2 (y - 1) x      ))
            && (rightBorder  || height < (arr A.! Ix2 y       (x + 1)))
            && (bottomBorder || height < (arr A.! Ix2 (y + 1) x      ))
       in (isLowPoint, height)

-- | Get the values of the low points. This is sequential, so it's separated
-- from finding the low points.
filterLowPoints :: A.Source r (Bool, Int) => Matrix r (Bool, Int) -> Vector DS Int
filterLowPoints = A.smapMaybe keepLowPoints . A.toStreamArray
  where
    keepLowPoints (True, height) = Just height
    keepLowPoints _              = Nothing

-- | Convert height positions to risk levels.
riskLevel :: A.Stream r Ix1 Int => Vector r Int -> Vector DS Int
riskLevel = A.smap (+1)

-- | Because type inference on this doesn't really work without explicit types.
computeSum :: A.Stream r Ix1 Int => Vector r Int -> Int
computeSum = A.ssum

-- | Parse the input file to a heightmap
parse :: String -> Matrix U Int
parse = A.fromLists' A.Seq . map parseLine . lines
  where
    parseLine :: String -> [Int]
    parseLine = map digitToInt
