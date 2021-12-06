{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-6.txt"

  putStrLn "Part 1:"
  print . length $ doTimes evalFish 80 input

  -- I was going to just wait for the linked list approach to do its thing but
  -- it took so long that I've already reimplemented it in the meantime
  putStrLn "\nPart 2:"
  print . sum . doTimes evalFishMap 256 $! toMap input

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

toMap :: [Int] -> IntMap Int
toMap = foldl' (\im n -> IM.insertWith (+) n 1 im) IM.empty

-- | The same as 'evalFish', but not using a linked list because they didn't
-- want us to use fancy pattern matching.
evalFishMap :: IntMap Int -> IntMap Int
evalFishMap = doSpawns . decrementTimer
  where
    decrementTimer = IM.mapKeys (subtract 1)
    -- Switching the order of spawning new fish and subtracting the timers
    -- compared to 'evalFish' makes this much simpler
    doSpawns im
      | Just nRespawn <- IM.lookup (-1) im
      = IM.insertWith (+) 8 nRespawn
      . IM.insertWith (+) 6 nRespawn
      . IM.delete (-1)
      $ im
      | otherwise
      = im

parse :: String -> [Int]
parse = map read . splitOn ","
