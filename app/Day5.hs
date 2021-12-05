{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.TypeLits

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-5.txt"

  putStrLn "Part 1:"
  undefined

-- * Matrices

-- These ought to be large enough for our purposes.
type Matrix a = MatrixBase 1000 1000
type Shape  a = ShapeBase  1000 1000

-- | A \'matrix\' with dimensions known at compile time. These dimensions are
-- only used as hints to add some syntactic sugar for two dimensional indexing.
data MatrixBase (rows :: Nat) (cols :: Nat) a = MatrixBase (V.Vector a)

-- | Indexes a 'MatrixBase' of the same size.
data ShapeBase (rows :: Nat) (cols :: Nat) = ShapeBase Int Int

-- | We'll just hack our own abstraction for indexing linear vectors as if they
-- were matrices. There's no bounds checking here.
toLinear :: forall rows cols. KnownNat rows => ShapeBase rows cols -> Int
toLinear (ShapeBase y x) = y * fromInteger (natVal @rows undefined) + x

-- | Convert a linear index to @(y, x)@. There's also no bounds checking here.
fromLinear :: forall rows cols. KnownNat rows => Int -> ShapeBase rows cols
fromLinear idx = ShapeBase y x
  where
    (y, x) = idx `divMod` fromInteger (natVal @rows undefined)

-- | Index a matrix without bounds checking. Because speed.
(!) :: (KnownNat rows, V.Unbox a) => MatrixBase rows cols a -> ShapeBase rows cols -> a
(!) (MatrixBase vec) sh = vec `V.unsafeIndex` toLinear sh

-- * Solution

parse :: String -> [String]
parse = map id . lines
