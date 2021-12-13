{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Control.Monad.ST.Strict
import Data.Massiv.Array (Matrix, Sz(..), Ix2(..), D, U)
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A


main :: IO ()
main = do
  (!initialDots, !folds) <- parse <$> readFile "inputs/day-13.txt"

  putStrLn "Part 1:"
  print . A.sum . A.map fromEnum $! doFolds initialDots [head folds]
  where
    _printFmt :: A.Source r Bool => Matrix r Bool -> IO ()
    _printFmt arr = do
      print (A.size arr)
      mapM_ putStrLn . A.toLists $ A.map fmt arr

    fmt True  = '#'
    fmt False = '.'


-- | Fold along an axis at this coordinate.
data Fold = FoldX Int | FoldY Int
  deriving (Show)


doFolds :: (A.Source r Bool) => Matrix r Bool -> [Fold] -> Matrix D Bool
doFolds a = foldl' go (A.delay a)
  where
    go :: Matrix D Bool -> Fold -> Matrix D Bool
    go arr fold =
      let (foldAxis, foldCoord) = case fold of
            FoldX c -> (1, c)
            FoldY c -> (2, c)

          -- The array needs to be split, and then the smallest array should be
          -- reversed and overlaid onto the larger array
          -- NOTE: The folding coordinate is increased by one because you
          --       apparently need to fold /on/ the coordinate so it's
          --       symmetrical
          (s1,       s2)       = A.splitAt' foldAxis (foldCoord + 1) arr
          (s1Size,   s2Size)   = (A.size s1,                          A.size s2)
          (s1Length, s2Length) = (A.getDim' (A.unSz s1Size) foldAxis, A.getDim' (A.unSz s2Size) foldAxis)

          -- @s1@ and @s2@ likely have different sizes, so either the start or
          -- the end of the overlay needs to be padded (along the fold axis)
          -- depending on whether @s2@ is being folded on top of @s1@ or the
          -- other way around. Massif doesn't seem to have an efficient built in
          -- function for padding (and concatenating a new array feels like a
          -- waste), so we'll use a generalized transformation to do this.
          -- XXX: Nope, they want you to __always__ fold towards the top/left so
          --      a lot of this isn't really needed. But I spent effort on it so
          --      I'll keep it in.
          (base, baseSize, overlay, overlaySize, padStart) =
            if s1Length >= s2Length
              then (s1, s1Size, s2, s2Size, True)
              else (s2, s2Size, s1, s1Size, False)

          reversedOverlay = A.reverse' foldAxis overlay

          startPadOffset = overlaySize - baseSize
          padOverlay :: Bool -> (Ix2 -> Bool) -> Ix2 -> Bool
          padOverlay _ get idx
            -- Padding at the start requires reading in the overlay array at earlier indices
            | padStart
            , paddedIdx <- idx - A.unSz startPadOffset
            , A.isSafeIndex overlaySize paddedIdx
            = get paddedIdx
            -- Padding at the end only requires a bounds check
            | A.isSafeIndex overlaySize idx
            = get idx
            -- And the padding should just be filled with 'False' values
            | otherwise
            = False
          paddedOverlay = A.transform' (const (baseSize, False)) padOverlay reversedOverlay

       in A.zipWith (||) base paddedOverlay

parse :: String -> (Matrix U Bool, [Fold])
parse = go . splitWhen (== "") . lines
  where
    go [dots, folds] =
      let folds'           = map pFold folds
          dotCoords        = map ((\[x, y] -> Ix2 (read y) (read x)) . splitOn ",") dots
          -- They don't specify the size of the paper, so I think you're
          -- supposed to figure this out yourself. Indices are 0 indexed while
          -- sizes obviously are not, hence the +1. Also, matrix sizes should
          -- always be odd even though they don't explicitly mention that in the
          -- puzzle.
          roundToOdd n     = ((n `div` 2) * 2) + 1
          height           = roundToOdd $ 1 + maximum (map (\(Ix2 y _) -> y) dotCoords)
          width            = roundToOdd $ 1 + maximum (map (\(Ix2 _ x) -> x) dotCoords)
          initialDots      = runST $ do
            mArr <- A.newMArray (Sz2 height width) False
            forM_ dotCoords $ \idx -> A.unsafeWrite mArr idx True
            A.unsafeFreeze A.Seq mArr
       in (initialDots, folds')
    go _ = error "Nope"

    pFold :: String -> Fold
    pFold s = case stripPrefix "fold along " s of
      Just ('x' : '=' : (read -> idx)) -> FoldX idx
      Just ('y' : '=' : (read -> idx)) -> FoldY idx
      _                                -> error ("Unexpected: " <> s)
