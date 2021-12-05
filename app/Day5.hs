{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM_)
import Data.List (unfoldr)
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.TypeLits

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-5.txt"

  putStrLn "Part 1:"
  print . overlapCount $! drawLines False input (initMatrix 0)

  putStrLn "\nPart 2:"
  print . overlapCount $! drawLines True input (initMatrix 0)

-- * Matrices
-- WARNING: Dangerous amounts of overengineering ahead

-- These ought to be large enough for our purposes.
type Matrix = MatrixBase 1000 1000
type Shape  = ShapeBase  1000 1000

-- | A \'matrix\' with dimensions known at compile time. These dimensions are
-- only used as hints to add some syntactic sugar for two dimensional indexing.
data MatrixBase (rows :: Nat) (cols :: Nat) a = Matrix { flatten :: V.Vector a }

instance forall rows cols a. (KnownNat cols, V.Unbox a, Show a) => Show (MatrixBase rows cols a) where
  show = unlines . map show . unfoldr splitRow . flatten
    where
      numCols = fromInteger $ natVal @cols undefined

      splitRow :: V.Vector a -> Maybe (V.Vector a, V.Vector a)
      splitRow remainder
        | V.null remainder = Nothing
        | otherwise        = Just $ V.splitAt numCols remainder

-- | Indexes a 'MatrixBase' of the same size.
data ShapeBase (rows :: Nat) (cols :: Nat) = Shape Int Int

-- | Initialize a matrix with a single elemnet.
initMatrix :: forall rows cols a. (KnownNat rows, KnownNat cols, V.Unbox a) => a -> MatrixBase rows cols a
initMatrix = Matrix . V.replicate size
  where
    size = fromInteger $ natVal @rows undefined * natVal @cols undefined

-- | We'll just hack our own abstraction for indexing linear vectors as if they
-- were matrices. There's no bounds checking here.
toLinear :: forall rows cols. KnownNat rows => ShapeBase rows cols -> Int
toLinear (Shape y x) = y * fromInteger (natVal @rows undefined) + x

-- | Convert a linear index to @(y, x)@. There's also no bounds checking here.
fromLinear :: forall rows cols. KnownNat rows => Int -> ShapeBase rows cols
fromLinear idx = Shape y x
  where
    (y, x) = idx `divMod` fromInteger (natVal @rows undefined)

-- | Index a matrix without bounds checking. Because speed.
(!) :: (KnownNat rows, V.Unbox a) => MatrixBase rows cols a -> ShapeBase rows cols -> a
(!) (Matrix vec) sh = vec `V.unsafeIndex` toLinear sh

-- * Solution

data Point = Point Int Int
-- The assumption here is that lines are always either horizontal or diagonal
data Line = Line Point Point

-- | Draw horizontal, vertical, and optionally also diagonal lines in the matrix
-- by incrementing the positions matching wit the line's points by one.
drawLines :: Bool -> [Line] -> Matrix Int -> Matrix Int
drawLines withDiagonals (Line (Point x1 y1) (Point x2 y2) : rest) (Matrix !arr) =
  drawLines withDiagonals rest $ Matrix (V.modify dewit arr)
  where
    -- The monadic action for actually incrementing the points, since there's no
    -- flipped version of 'V.modify' and doing that in place makes it pretty
    -- unreadable.
    dewit mv =
      forM_ linePoints $ \sh ->
        MV.unsafeModify mv (+ 1) (toLinear sh)

    xMin = min x1 x2
    xMax = max x1 x2
    yMin = min y1 y2
    yMax = max y1 y2

    linePoints :: [Shape]
    !linePoints = case () of
      -- Only consider horizontal lines...
      () | yMin == yMax -> map (\x -> Shape yMin x) [xMin .. xMax]
      -- ...and vertical lines...
      () | xMin == xMax -> map (\y -> Shape y xMin) [yMin .. yMax]
      -- ...and optionally also diagonal lines...
      () | withDiagonals && (xMax - xMin == yMax - yMin) ->
        let xDirection = signum (x2 - x1)
            yDirection = signum (y2 - y1)
            lineLength = xMax - xMin + 1
         in map (\offset -> Shape (y1 + (offset * yDirection)) (x1 + (offset * xDirection)))
                [0 .. lineLength - 1]
      -- ...and ignore the rest.
      _                 -> []
drawLines _ [] arr = arr

-- | Count the number of points where at least two lines overlap.
overlapCount :: Matrix Int -> Int
overlapCount = V.length . V.filter (>= 2) . flatten

parse :: String -> [Line]
parse = map parseArrow . lines
  where
    parseArrow :: String -> Line
    parseArrow line = case words line of
      [p1, _, p2] -> Line (parsePoint p1) (parsePoint p2)
      _           -> error "That doesn't look very arrow-like to me"

    parsePoint :: String -> Point
    parsePoint s = case map read (splitOn "," s) of
      [x, y] -> Point x y
      _      -> error "Point that to where the sun doesn't shine!"
