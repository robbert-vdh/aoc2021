{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-6.txt"

  putStrLn "Part 1:"
  print . length $ doTimes evalFish 80 input

doTimes :: (a -> a) -> Int -> a -> a
doTimes f n = (!! n) . iterate f

-- | The interval for lanternfish creating new lanternfish.
spawnRate :: Int
spawnRate = 7

-- | Run a single cycle of the fish spawning/updating algorithm. The numbers
-- represent the amount of time remaining until a fish spawns another fish. When
-- a fish's remaining time is 0, it will spawn another fish that starts with a
-- remaining time of 8 (so it will be set to 7 the day after this), and the fish
-- itself gets reset to 6 (since 0 is a valid value).
evalFish :: [Int] -> [Int]
evalFish (0 : ns) = 8 : 6 : evalFish ns
evalFish (n : ns) = n - 1 : evalFish ns
evalFish []       = []

parse :: String -> [Int]
parse = map read . splitOn ","
