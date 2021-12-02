{-# LANGUAGE BangPatterns #-}

module Main where

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-1.txt"

  putStrLn "Part 1:"
  print $! countIncrements input

  putStrLn "\nPart 2:"
  print . countIncrements . windowSums $ input

countIncrements :: [Int] -> Int
countIncrements (x : y : xs)
  | y > x     = countIncrements (y : xs) + 1
  | otherwise = countIncrements (y : xs)
countIncrements _ = 0

-- | Return the sums of all three element sliding windows in the list.
windowSums :: [Int] -> [Int]
windowSums (x : y : z : xs) = (x + y + z) : windowSums (y : z : xs)
windowSums _                = []

parse :: String -> [Int]
parse = map read . lines
