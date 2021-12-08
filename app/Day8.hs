{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Control.Parallel.Strategies
import Data.Char
import Data.Foldable
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as B
import GHC.Conc (numCapabilities)
import GHC.Generics (Generic)
import Numeric.GSL.SimulatedAnnealing
import qualified Numeric.LinearAlgebra as H

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-8.txt"

  -- The fun part is that doing this in parallel on a 12 core 24 thread CPU is
  -- only twice as fast as when not using any parallelization, and with 24
  -- threads it's 2.5 times slower. FFI calls require global locking, which is
  -- probably what's slowing this down.
  let chunkSize      = ceiling $ fromIntegral (length input) / (fromIntegral numCapabilities :: Float)
      mappings       = map (fromJust . solveMapping) input
      !mappedOutputs = map mapDisplayOutputs mappings `using` parListChunk chunkSize rdeepseq

  putStrLn "Part 1:"
  print . length . filter (`elem` [1, 4, 7, 8]) $ concat mappedOutputs

  putStrLn "\nPart 2:"
  print . sum $ map digitsToInt mappedOutputs

-- * Segments
--
-- These represent each value of the seven segment display in different ways.

-- | The expected lit segments for each of the ten digits that can be
-- represented by the four segment display. The segment indices for each number
-- are in sorted.
expectedSegments :: B.Vector (Set Char)
expectedSegments = B.fromList . map S.fromList $
  [ "abcefg"  -- 0
  , "cf"      -- 1
  , "acdeg"   -- 2
  , "acdfg"   -- 3
  , "bcdf"    -- 4
  , "abdfg"   -- 5
  , "abdefg"  -- 6
  , "acf"     -- 7
  , "abcdefg" -- 8
  , "abcdfg"  -- 9
  ]


-- * Solution

data Display = Display
  { -- | The ten digits from 'expectedSegments', but with the character ->
    -- segment assignments scrambled.
    patterns :: B.Vector (Set Char)
    -- | A four digit output value, with the same scrambled assignments as those
    -- in 'patterns'.
  , outputs  :: B.Vector (Set Char)
  }
  deriving (Show, Generic, NFData)

-- | The solver state for solving the mappings within a display.
data SolverState = SolverState
  { display        :: Display
    -- | The mapping from a pattern from 'patterns' to the character in
    -- 'expectedSegments'.
  , patternMapping :: !(Map Int Int)
    -- | The mapping from a character on the seven segment display as used in a
    -- 'Display' to the actual letter of the segment.
  , charMapping    :: !(Map Char Char)
  }
  deriving (Show, Generic, NFData)

-- | The actions we can take in our solver. Swapping a mapping means swapping
-- two values in either of the maps in 'SolverState'.
data SolverAction = SwapPatternMapping | SwapCharMapping
  deriving (Show, Enum, Bounded)

applyMapping :: Ord a => Map a a -> Set a -> Set a
applyMapping mapping = S.map (mapping M.!)

-- | The penalty/loss for a solver state. We want to minimize this.
solverPenalty :: SolverState -> Int
solverPenalty SolverState{..} = lengthPenalty + mappingPenalty
  where
    -- We need the pattern state from the solver permuted to match the expected
    -- segments, alongside the expected segments
    permutedPatterns = B.backpermute (patterns display) (B.fromList $ map (patternMapping M.!) [0..9])
    zippedPatterns   = B.zip expectedSegments permutedPatterns

    -- Because the set difference only tells you which elements from set A are not in set B
    symmetricDifference :: Ord a => Set a -> Set a -> Set a
    symmetricDifference a b = S.difference a b `S.union` S.difference b a

    -- | The sum of length errors when using the pattern mapping.
    lengthPenalty = getSum $
      foldMap (\(expected, actual) -> Sum . abs $ S.size expected - S.size actual)
              zippedPatterns

    -- | The number of difference characters in each segment set after applying
    -- the pattern mapping.
    mappingPenalty = getSum $
      foldMap (\(expected, actual) -> Sum . abs . S.size $ symmetricDifference expected (applyMapping charMapping actual))
              zippedPatterns

-- | Find the matching mapping from the scrambled pattern space to the expected
-- segments. There's a clever way to do this. This is not it.
solveMapping :: Display -> Maybe SolverState
solveMapping display'
  -- There should only be an exact solution
  | solverPenalty optimalSolution == 0 = Just optimalSolution
  | otherwise                          = Nothing
  where
    optimalSolution = simanSolve 420 3 solverParams initialState
      (fromIntegral . solverPenalty)
      -- This distance metric is not entirely correct
      (\x y -> fromIntegral . abs $ solverPenalty x - solverPenalty y)
      step
      Nothing -- (Just $ show . solverPenalty)

    -- This solver is WAY overkill, so the cooling rate and final temperature
    -- are relatively high to not waste too much time on the simpler to solve
    -- cases
    solverParams = SimulatedAnnealingParams
      100   -- Tries per step
      1000  -- Tries per temperature
      1     -- Maximum step size in random walk
      1.0   -- Boltzman constant for random walks
      1.0   -- Initial temperature
      2.0   -- Cooling rate
      0.069 -- Final temperature

    initialState = SolverState
      { display        = display'
      , patternMapping = M.fromList $ map (\idx -> (idx, idx)) [0..9]
      , charMapping    = M.fromList $ map (\c -> (c, c)) "abcdefg"
      }

    solverActionRange = fromIntegral (fromEnum (maxBound :: SolverAction))

    swapRange SwapPatternMapping = 9.0
    swapRange SwapCharMapping    = fromIntegral $ ord 'g' - ord 'a'

    swap :: Ord k => k -> k -> Map k v -> Map k v
    swap from to m = M.insert to (m M.! from) $ M.insert from (m M.! to) m

    -- Where n starts at 0, as generated by the RNG.
    char :: Int -> Char
    char n = chr (n + ord 'a')

    step :: H.Vector Double -> Double -> SolverState -> SolverState
    step rands _ state@SolverState{..} =
      let action   = toEnum (round $ (rands H.! 0) * solverActionRange) :: SolverAction
          -- These two can be the same
          swapFrom = round $ (rands H.! 1) * swapRange action
          swapTo   = round $ (rands H.! 2) * swapRange action
       in case action of
            SwapPatternMapping -> state { patternMapping = swap swapFrom swapTo patternMapping }
            SwapCharMapping    -> state { charMapping    = swap (char swapFrom) (char swapTo) charMapping }

-- | Grab the translated display numbers from a solver result.
mapDisplayOutputs :: SolverState -> [Int]
mapDisplayOutputs SolverState{display = Display{..}, ..}
  = map ((segmentsMapping M.!) . applyMapping charMapping)
  $ B.toList outputs
  where
    segmentsMapping :: Map (Set Char) Int
    segmentsMapping = M.fromList $ B.toList $ B.imap (\idx segment -> (segment, idx)) expectedSegments

-- | convert a list of digits to an integer.
digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\acc n -> (acc * 10) + n) 0

parse :: String -> [Display]
parse = map parseLine . lines
  where
    parseLine :: String -> Display
    parseLine l = case splitOn " | " l of
      [words -> ps, words -> os] -> Display (parseSegments ps) (parseSegments os)
      _                          -> error "What is this even!?"

    parseSegments :: [[Char]] -> B.Vector (Set Char)
    parseSegments = B.fromList . map S.fromList
