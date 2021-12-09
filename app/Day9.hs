{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Massiv.Array (Matrix, Vector, Ix1, Ix2(..), Sz(..), BN, D, DW, DS, U)
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A
import Data.Maybe
import Data.Ord
import GHC.Generics (Generic)


main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-9.txt"

  let lowPoints = A.computeP @U $ findLowPoints input

  putStrLn "Part 1:"
  print . computeSum . riskLevel . filterLowPoints $ lowPoints

  putStrLn "\nPart 2:"
  print . A.product . A.map snd . A.take 3 . basinSizes . markBasins $ lowPoints


-- * Low points
--
-- And how to find them with stencils.

-- | Mark each low point in the array.
findLowPoints :: (A.Manifest r Int) => Matrix r Int -> Matrix DW (Bool, Int)
findLowPoints = A.mapStencil (A.Fill 10) (A.makeStencil (Sz2 3 3) (Ix2 1 1) markLowPoint)
  where
    markLowPoint :: (Ix2 -> Int) -> (Bool, Int)
    markLowPoint get =
      let height     = get (Ix2 0 0)
          isLowPoint = all (\offset -> height < get offset) neighbourOffsets
       in (isLowPoint, height)

-- | Offsets for neighbouring coordinates when applying a stencil function.
neighbourOffsets :: [Ix2]
neighbourOffsets =
  [ Ix2 0    (-1)
  , Ix2 (-1) 0
  , Ix2 0    1
  , Ix2 1    0
  ]

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


-- * Flood filling basins
--
-- This is the overengineered data-parallel solution for part 2.

-- | Remove height 9 low points from the @(lowPoint, height)@ matrix since those
-- don't count as basins.
basinStarts :: A.Source r (Bool, Int) => Matrix r (Bool, Int) -> Matrix D (Bool, Int)
basinStarts = A.map (\(lowPoint, height) -> (lowPoint && height /= 9, height))

-- | Get the @(y, x)@ coordinates of every low point.
lowPointCoordinates :: A.Source r (Bool, Int) => Matrix r (Bool, Int) -> Vector DS Ix2
lowPointCoordinates
  = A.smapMaybe keepLowPoints
  . A.toStreamArray
  . A.imap (\idx (isLowPoint, _) -> (isLowPoint, idx))
  where
    keepLowPoints (True, idx) = Just idx
    keepLowPoints _           = Nothing

-- | Whether a place in the grid is part of a basin, does not belong to a basin
-- (because it's not a low point or there are multiple ways down), or whether
-- the point has not yet been reached.
data LocationStatus = Basin {-# UNPACK #-} !Int | Invalid | Unknown
  deriving (Eq, Show, Generic, NFData)

-- | Mark the basins in a vector of @(isLowPoint, height)@ with a number, in the
-- format @(basinIdx, height)@. These indices start at 1.
--
-- Doing a simple depth first search would be the sane way to do this. Instead,
-- we'll basically iteratively flood fill the @(status, height)@ matrix until
-- every element is filled.
markBasins :: forall r. (A.Source r (Bool, Int))
           => Matrix r (Bool, Int)
           -> Matrix BN (LocationStatus, Int)
markBasins
  = A.iterateUntil (const (==)) (const fillStep)
  . markBasinStarts
  where
    -- We want to assign a unique index to each coordinate so we can refer to
    -- the individual basins. We'll then write those to @initialStatus@ below.
    markBasinStarts :: Matrix r (Bool, Int) -> Matrix BN (LocationStatus, Int)
    markBasinStarts arr =
      let startCoordinates = A.simap (,) $ lowPointCoordinates (basinStarts arr)
          initialStatus    = A.map (\(_, height) -> (Unknown, height)) arr
       in runST $ A.withLoadMArrayS_ initialStatus $ \mArr -> do
            forM_ startCoordinates $ \(idx, coordinate) -> do
              A.unsafeModify mArr (\(_, height) -> return (Basin idx, height)) coordinate

    -- | Every step we'll apply a stencil to fill in unfilled parts adjacent to
    -- a basin.
    fillStep :: Matrix BN (LocationStatus, Int) -> Matrix BN (LocationStatus, Int)
    fillStep = A.computeP . A.mapStencil (A.Fill (Unknown, 10)) (A.makeStencil (Sz2 3 3) (Ix2 1 1) fillPoint)

    fillPoint :: (Ix2 -> (LocationStatus, Int)) -> (LocationStatus, Int)
    fillPoint get = case get (Ix2 0 0) of
      -- Heights of 9 are never part of a basin
      (Unknown, 9)       -> (Invalid, 9)
      (Unknown, height)  ->
        let neighbouringBasins = mapMaybe (validBasinIdx height . get) neighbourOffsets
         in case neighbouringBasins of
               -- NOTE: @[idx]@ gives the wrong result, so they probably expect
               --       you do to this in a depth first manner starting from the
               --       top left basin
               (idx : _) -> (Basin idx, height)
               _         -> (Unknown,   height)
      -- We only need to look at the unknown places, since we've already visited
      -- the other places in the matrix
      old                -> old

    validBasinIdx :: Int -> (LocationStatus, Int) -> Maybe Int
    validBasinIdx height (Basin idx, basinHeight) | basinHeight < height = Just idx
    validBasinIdx _      _                                               = Nothing

-- | Get the number of locations for each basin. The result is in the format
-- @(basinIdx, count)@, with the biggest basin coming first.
basinSizes :: A.Source r (LocationStatus, Int) => Matrix r (LocationStatus, Int) -> Vector U (Int, Int)
basinSizes
  = A.quicksortBy (comparing (Down . snd))
  . A.computeP @U
  . A.tally
  . A.computeP @U
  . A.smapMaybe keepBasins
  . A.toStreamArray
  where
    keepBasins :: (LocationStatus, Int) -> Maybe Int
    keepBasins (Basin idx, _) = Just idx
    keepBasins _              = Nothing


-- | Parse the input file to a heightmap
parse :: String -> Matrix U Int
parse = A.fromLists' A.Seq . map parseLine . lines
  where
    parseLine :: String -> [Int]
    parseLine = map digitToInt
