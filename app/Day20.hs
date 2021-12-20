{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Bits
import Data.Massiv.Array (Ix2 (..), Sz (..), Vector, Matrix, D, U)
import qualified Data.Massiv.Array as A

main :: IO ()
main = do
  (!lut, !input) <- parse <$> readFile "inputs/day-20.txt"

  putStrLn "Part 1:"
  print . A.sum $ applyTimes 2 lut input

  putStrLn "\nPart 2:"
  print . A.sum $ applyTimes 50 lut input

-- | So yeah uh...the problem with the input file is that the first character is
-- a @#@, so when still using '.' as padding in the 'enhance' function this will
-- cause the border to fold inwards every two iterations. The solution is to
-- either use the correct character for the padding, or to just add excess
-- padding and to chop it off after every second iteration. That's what we're
-- doing here.
applyTimes :: (A.Manifest r1 Int) => Int -> Vector r1 Int -> Matrix U Int -> Matrix U Int
applyTimes n lut input
  | n <= 2    = A.compute $ removePadding (n * n) (iterate (enhance lut) input !! n)
  | otherwise = applyTimes (n - 2) lut (applyTimes 2 lut input)


-- * Part 1

enhance :: (A.Manifest r1 Int, A.Manifest r2 Int) => Vector r1 Int -> Matrix r2 Int -> Matrix U Int
enhance lut
  = A.compute
  . A.map (lut A.!)
  . A.compute @U
    -- This padding means that everything grows by one pixel in each direction
  . A.applyStencil (A.Padding (Sz2 4 4) (Sz2 4 4) $ A.Fill 0) binarySumStencil
  where
    binarySumStencil = A.makeStencil (Sz2 3 3) (Ix2 1 1) $ \get ->
      -- These indices are in decending order because the bits should be read in
      -- big endian order
      sum $ zipWith (\offset idx -> get offset `unsafeShiftL` idx) offsets [8, 7 .. 0]

    offsets = [Ix2 y x | y <- [-1 .. 1], x <- [-1 .. 1]]

-- | Remove n rows of excess padding added in 'enhance'. This needs to be done
-- after the (sequential) enhancing.
removePadding :: (A.Source r a, A.Size r) => Int -> Matrix r a -> Matrix D a
removePadding n arr = A.extract' (Ix2 n n) (A.size arr - Sz2 (n * 2) (n * 2)) arr


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
