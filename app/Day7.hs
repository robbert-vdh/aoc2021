{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List.Split (splitOn)
import Data.Foldable
import Data.Monoid
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Ord (comparing)

import Criterion.Main

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-7.txt"
  let xMin  = minimum $ IM.keys input
      xMean = meanPosition input
      xMax  = maximum $ IM.keys input

  putStrLn "Part 1:"
  print . snd $! minimizeLS (`l1Loss` input)         1 (xMin, xMean, xMax)

  putStrLn "\nPart 2:"
  print . snd $! minimizeLS (`triangularLoss` input) 1 (xMin, xMean, xMax)

  putStrLn "\nBenchmarks below:\n"
  defaultMain
    [ bench "L1 loss"            $ nf (\i -> minimizeLS (`l1Loss` i)         1 (xMin, xMean, xMax)) input
    , bench "triangular L1 loss" $ nf (\i -> minimizeLS (`triangularLoss` i) 1 (xMin, xMean, xMax)) input
    ]

-- | The number of crabs at each position, where the key is the position and the
-- value is the number of crabs.
type Positions = IntMap Int

-- | The mean position for a group of horizontally-constrained crabs. We'll use
-- this for our initial guess.
meanPosition :: Positions -> Int
meanPosition positions = round (fromIntegral total / fromIntegral num :: Float)
  where
    total = getSum $ IM.foldMapWithKey (\pos n -> Sum $ pos * n) positions
    num   = sum positions

-- | A simple hill climbing based local search for minimising a function.
-- Because there's only a single local minimum, we can basically just do a
-- binary search for the target position that gives us the lower loss. The
-- proper way to solve this problem would be with linear programming, but
-- learning how the libraries for that work sounds like more effort than just
-- solving it this way. This takes a function that should be optimized that
-- takes a numeric argument and returns a loss value, an epsilon value for the
-- lowest significant difference in values we should consider, the minimum,
-- mean, and maximum input values (we can cheat here because we know these), and
-- it returns the input value for the minimum loss along with that loss value.
minimizeLS :: (Num l, Ord l) => (Int -> l) -> l -> (Int, Int, Int) -> (Int, l)
minimizeLS f epsilon startParams@(!xMin, !xMean, !xMax)
  -- The epsilon value is useful for floating point losses, for integer losses
  -- we should just compare the best guess to our current parameters.
  | sum guessLosses < epsilon || lowestGuess == startParams
  = (xMean, f xMean)
  | otherwise
  = minimizeLS f epsilon lowestGuess
  where
    -- Here the middle value is the actual guess, the other two values are the
    -- new min and max values in case that guess is the lowest of the bunch
    guesses =
      [ (xMin                  , (xMin + xMean) `div` 2      , xMean                 )
      , ((xMin + xMean) `div` 2, xMean                       , (xMean + xMax) `div` 2)
      , (xMean                 , ((xMean + xMax) `div` 2) + 1, xMax                  )
      ]
    guessLosses = map (\(_, x, _) -> f x) guesses
    lowestGuess = fst $ minimumBy (comparing snd) (zip guesses guessLosses)

-- | Compute the sum of the differences between each crab's position and the
-- predicted position.
l1Loss :: Int -> Positions -> Int
l1Loss prediction = getSum . IM.foldMapWithKey (\pos n -> Sum $ abs (prediction - pos) * n)

-- | Compute the sum of the differences between each crab's position and the
-- predicted position, when we should take the triangular number of the
-- distance. Meaning, @3@ becomes @1 + 2 + 3@.
triangularLoss :: Int -> Positions -> Int
triangularLoss prediction = getSum . IM.foldMapWithKey (\pos n -> Sum $ (triangularNumber . abs $ prediction - pos) * n)
  where
    -- Taken from https://hackage.haskell.org/package/DPutils-0.1.1.0/docs/src/Math.TriangularNumbers.html#triangularNumber
    triangularNumber :: Int -> Int
    triangularNumber n = (n * (n + 1)) `div` 2

-- | The horizontal positions for each crab as a mapping between positions and
-- number of crabs.
parse :: String -> Positions
parse = foldl' (\im pos -> IM.insertWith (+) pos 1 im) IM.empty . map read . splitOn ","
