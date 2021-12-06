{-# LANGUAGE BangPatterns #-}

module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.List.Split (splitOn)

-- And a vector based implementation, because why not
import Control.Monad (replicateM_)
import Control.Monad.ST (ST)
import Criterion.Main
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-6.txt"

  putStrLn "Part 1:"
  print . length $ doTimes evalFish 80 input

  -- I was going to just wait for the linked list approach to do its thing but
  -- it took so long that I've already reimplemented it in the meantime
  putStrLn "\nPart 2:"
  print . sum . doTimes evalFishMap 256 $! toMap input

  -- Because why not
  putStrLn "\nPart 1 GOTTA GO FAST EDITION:"
  print . V.sum . evalVecSPEED 80  $! toVec input
  putStrLn "\nPart 2 GOTTA GO FAST EDITION:"
  print . V.sum . evalVecSPEED 256 $! toVec input

  -- And a faster, shift based vector implementation
  putStrLn "\nPart 1 GOTTA GO FAST EDITION redux:"
  print . V.sum . evalVecSPEEDRedux 80  $! toVecRedux input
  putStrLn "\nPart 2 GOTTA GO FAST EDITION redux:"
  print . V.sum . evalVecSPEEDRedux 256 $! toVecRedux input

  putStrLn "\nBenchmarks below:\n"
  defaultMain
    [ bench "linked list, 80 days"       $ nf (length . doTimes evalFish 80)       input
    , bench "intmap, 256 days"           $ nf (sum . doTimes evalFishMap 256)      $! toMap input
    , bench "I AM SPEED, 80 days"        $ nf (V.sum . evalVecSPEED 80)            $! toVec input
    , bench "I AM SPEED, 256 days"       $ nf (V.sum . evalVecSPEED 256)           $! toVec input
    , bench "I AM SPEED pt. 2, 80 days"  $ nf (V.sum . evalVecSPEEDRedux 80)       $! toVecRedux input
    , bench "I AM SPEED pt. 2, 256 days" $ nf (V.sum . evalVecSPEEDRedux 256)      $! toVecRedux input
    ]

doTimes :: (a -> a) -> Int -> a -> a
doTimes f n = (!! n) . iterate f

-- | The interval for lanternfish creating new lanternfish.
spawnRate :: Int
spawnRate = 7

-- * Using lists

-- | Run a single cycle of the fish spawning/updating algorithm. The numbers
-- represent the amount of time remaining until a fish spawns another fish. When
-- a fish's remaining time is 0, it will spawn another fish that starts with a
-- remaining time of 8 (so it will be set to 7 the day after this), and the fish
-- itself gets reset to 6 (since 0 is a valid value).
evalFish :: [Int] -> [Int]
evalFish (0 : ns) = 8 : 6 : evalFish ns
evalFish (n : ns) = n - 1 : evalFish ns
evalFish []       = []

-- * Using intmaps

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

-- * Using vectors

toVec :: [Int] -> Vector Int
toVec nums = V.modify (\mv -> mapM_ (MV.modify mv (+1)) nums) (V.replicate 10 0)

-- | This does everything in place as a single monadic action. The day 9 here is
-- used for the rollover from day 0 since we modify the vector in place and we
-- iterate over the days in sequence, see below.
evalVecSPEED :: Int -> Vector Int -> Vector Int
evalVecSPEED times = V.modify $ \mv -> replicateM_ times $ mapM_ (modDayInPlace mv) [0..9]
  where
    modDayInPlace :: MV.MVector s Int -> Int -> ST s ()
    modDayInPlace mv n = do
      oldCount <- MV.unsafeRead mv n
      MV.unsafeWrite mv n 0
      if n == 0
        then do
          -- These are one higher than they would need to be, because we'll iterate
          -- over them again, see above
          MV.unsafeModify mv (+ oldCount) 7
          MV.unsafeModify mv (+ oldCount) 9
        else
          MV.unsafeModify mv (+ oldCount) (n - 1)

toVecRedux :: [Int] -> Vector Int
toVecRedux nums = V.modify (\mv -> mapM_ (MV.modify mv (+1)) nums) (V.replicate 9 0)

-- | This also does everything in place as a single monadic action, but it just
-- shifts all elements and then handles the new spawns.
evalVecSPEEDRedux :: Int -> Vector Int -> Vector Int
evalVecSPEEDRedux times = V.modify $ \mv -> replicateM_ times (modDayInPlace mv)
  where
    modDayInPlace :: MV.MVector s Int -> ST s ()
    modDayInPlace mv = do
      nRespawn <- MV.unsafeRead mv 0

      -- Everyone grows one way older
      MV.unsafeCopy (MV.unsafeSlice 0 8 mv) (MV.unsafeSlice 1 8 mv)
      -- And day 0 fish will go back to day 6 and some new fish will spawn
      MV.unsafeModify mv (+ nRespawn) 6
      MV.unsafeWrite mv 8 nRespawn

parse :: String -> [Int]
parse = map read . splitOn ","
