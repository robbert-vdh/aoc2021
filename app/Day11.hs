{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.State.Strict
import Data.Char (digitToInt)
import Data.Function (on)
import Data.Massiv.Array (Matrix, Sz(..), Ix2(..), D, DW, U)
import qualified Data.Massiv.Array as A


main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-11.txt"

  putStrLn "Part 1:"
  print . numFlashes $! simulateSteps 100 input

  putStrLn "\nPart 2:"
  print . numSteps $! simulateUntilSync input


type Energy = Int

data SimulationState = SimulationState
  { numFlashes :: {-# UNPACK #-} !Int
  , numSteps   :: {-# UNPACK #-} !Int
    -- The grid is kept in a delayed representation so we can actually use the
    -- state monad to modify it without constantly having to recompute it to a
    -- manifest array
  , grid       :: Matrix D Energy
  }
  deriving (Show)

-- | Run the simulation for @steps@ steps, returning the updated energy levels
-- and the number of flashes that happened during the simulation.
simulateSteps :: A.Source r Energy => Int -> Matrix r Energy -> SimulationState
simulateSteps steps = execState (replicateM_ steps simulationStep) . SimulationState 0 0 . A.delay

-- | Run the simulation until every octopus has just flashed. In other words,
-- until all energy levels are 0.
simulateUntilSync :: A.Source r Energy => Matrix r Energy -> SimulationState
simulateUntilSync = execState go . SimulationState 0 0 . A.delay
  where
    go :: State SimulationState ()
    go = do
      simulationStep

      isSynchronized <- gets (A.all (== 0) . grid)
      unless isSynchronized go

simulationStep :: State SimulationState ()
simulationStep = do
  -- At the start of a simulation step the energy levels increase by 1
  modify $ \s -> s { grid = A.map (+ 1) $ grid s }

  -- After that, iteratively increase each octopus' energy levels by 1 for every
  -- flashing neighbour until nothing changes anymore. This boolean is used to
  -- keep track of whether a flashing octopus has already been counted or not.
  modify $ \s -> s
    { grid = A.map snd
           -- Iteration can only happen on manifest arrays because we need to
           -- compare the old and the new values, hence computing to unboxed
           -- arrays
           . A.iterateUntil (const $ A.eqArrays ((==) `on` snd)) (const $ A.computeS @U . doFlashes)
           . A.computeS @U
           . A.map (False,)
           $ grid s
    }

  -- Finally, count the number of flashes, and reset the flashing octopi back to
  -- energy level 0
  newFlashes <- gets (countFlashes . grid)
  finalGrid  <- gets (resetGrid . grid)
  modify $ \s -> s
    { numFlashes = numFlashes s + newFlashes
    , numSteps   = numSteps s + 1
    , grid       = finalGrid
    }
  where
    -- | When an adjacent octopus has an energy level greater than 9, increase
    -- that octopus' energy level by 1. This should be applied iteratively until
    -- nothing changes anymore. The boolean indicates whether a cell has already
    -- been processed, since only want to count each flash once. Each flashing
    -- octopus only spends a single iteration at @(False, 10)@ before going to
    -- @(True, 10)@.
    doFlashes :: A.Manifest r (Bool, Energy) => Matrix r (Bool, Energy) -> Matrix DW (Bool, Energy)
    doFlashes = A.mapStencil (A.Fill (True, 0)) $ A.makeStencil (Sz2 3 3) (Ix2 1 1) $ \rGet ->
      case rGet (Ix2 0 0) of
        -- Don't increase energy levels when they're already above 9, and also
        -- prevent counting a single flash more than once
        (_, energy) | energy > 9 -> (True, energy)
        -- If any of the neighbouring octopi is curring flashing (with energies
        -- above 9 and the boolean set to false since we don't want to count
        -- duplicates), then increase this octopus' levels by 1. If the energy
        -- levels exceed 9 then the boolean will be set to 'True' on the next
        -- iteration.
        (_, energy) ->
          let numFlashingNeighbours = length $ filter (isFlashing . rGet) neighbourOffsets
           in (False, energy + numFlashingNeighbours)

    -- | An octopus is flashing if it's energy level is greater than 9, and we
    -- have not yet counted it (i.e. the boolean being set to false).
    isFlashing :: (Bool, Int) -> Bool
    isFlashing (False, energy) | energy > 9 = True
    isFlashing _                            = False

    countFlashes :: A.Source r Energy => Matrix r Energy -> Int
    countFlashes = A.sum . A.map \energy ->
      if energy > 9
        then 1
        else 0

    resetGrid :: A.Source r Energy => Matrix r Energy -> Matrix D Energy
    resetGrid = A.map $ \energy ->
      if energy > 9
        then 0
        else energy

-- | Offsets for neighbouring coordinates when applying a stencil function, including diagonals.
neighbourOffsets :: [Ix2]
neighbourOffsets =
  [ Ix2 0    (-1)
  , Ix2 (-1) 0
  , Ix2 0    1
  , Ix2 1    0
  , Ix2 (-1) (-1)
  , Ix2 (-1) 1
  , Ix2 1    (-1)
  , Ix2 1    1
  ]

-- | Read the input file to a matrix of energy levels.
parse :: String -> Matrix U Energy
parse = A.fromLists' A.Seq . map (map digitToInt) . lines
