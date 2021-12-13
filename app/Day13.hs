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

  putStrLn "\nPart 2:"
  printFmt $! doFolds initialDots folds
  where
    printFmt :: A.Source r Bool => Matrix r Bool -> IO ()
    printFmt arr = do
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

          -- The array needs to be split, and then the bottom/right array should be
          -- reversed and overlaid onto the top/left ne
          -- NOTE: The folding coordinate is increased by one because you
          --       apparently need to fold /on/ the coordinate so it's
          --       symmetrical
          (base,     overlay)     = A.splitAt' foldAxis (foldCoord + 1) arr
          (baseSize, overlaySize) = (A.size base,                          A.size overlay)

          reversedOverlay = A.reverse' foldAxis overlay

          -- The reversed overlay may need to be realigned to match the other
          -- array. This is a lot more complicated than it needs to be because
          -- the folding is __on__ a coordinate instead of after, hence the +1
          -- again.
          (_, startPadOffset) = A.modifyDim' (A.unSz overlaySize - A.unSz baseSize) foldAxis (+ 1)
          padOverlay :: Bool -> (Ix2 -> Bool) -> Ix2 -> Bool
          padOverlay _ get idx
            | paddedIdx <- idx + startPadOffset
            , A.isSafeIndex overlaySize paddedIdx
            = get paddedIdx
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
          -- sizes obviously are not, hence the +1.
          height           = 1 + maximum (map (\(Ix2 y _) -> y) dotCoords)
          width            = 1 + maximum (map (\(Ix2 _ x) -> x) dotCoords)
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
