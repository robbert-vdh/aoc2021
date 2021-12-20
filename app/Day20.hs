{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Bits
import Data.Massiv.Array (Ix2 (..), Sz (..), Vector, Matrix, U)
import qualified Data.Massiv.Array as A

main :: IO ()
main = do
  (!lut, !input) <- parse <$> readFile "inputs/day-20.txt"

  putStrLn "Part 1:"
  print . A.sum . enhance lut $ enhance lut input


-- * Part 1

enhance :: (A.Manifest r1 Int, A.Manifest r2 Int) => Vector r1 Int -> Matrix r2 Int -> Matrix U Int
enhance lut
  = A.compute
  . A.map (lut A.!)
  . A.compute @U
    -- This padding means that everything grows by one pixel in each direction
  . A.applyStencil (A.Padding (Sz2 2 2) (Sz2 2 2) $ A.Fill 0) binarySumStencil
  where
    binarySumStencil = A.makeStencil (Sz2 3 3) (Ix2 1 1) $ \get ->
      -- These indices are in decending order because the bits should be read in
      -- big endian order
      sum $ zipWith (\offset idx -> get offset `shiftL` idx) offsets [8, 7 .. 0]

    offsets = [Ix2 y x | y <- [-1 .. 1], x <- [-1 .. 1]]


-- * Parsing

parse :: String -> (Vector U Int, Matrix U Int)
parse = go . lines
  where
    go :: [String] -> (Vector U Int, Matrix U Int)
    go (lut : _ : image) = (toInts $ A.fromList A.Seq lut, toInts $ A.fromLists' A.Seq image)
    go _ = error "This doesn't look quite right"

    toInts :: A.Index ix => A.Array U ix Char -> A.Array U ix Int
    toInts = A.compute @U . A.map \case
      '.' -> 0
      '#' -> 1
      _   -> error "Unexpected character in input"
