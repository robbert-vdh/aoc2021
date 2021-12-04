{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad (forM)
import Data.Array.Repa ((:.)(..), Array, DIM2, U, Z(..))
import qualified Data.Array.Repa as R
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List.Split
import System.IO

main :: IO ()
main = do
  !input <- parse "inputs/day-4.txt"

  print input
  putStrLn "Part 1:"
  undefined

-- | Because has time to write all of this.
pattern I2 :: Int -> Int -> DIM2
pattern I2 y x = Z :. y :. x

boardHeight :: Int
boardHeight = 5

boardShape :: DIM2
boardShape = I2 boardHeight boardHeight

data State = State
  { remainingNumbers :: ![Int]         -- ^ Numbers left to draw, in order
  , drawnNumbers     :: !(HashSet Int) -- ^ The numbers that have already been drawn
    -- | The boards, this could be a three dimensional array for maximum
    -- efficiency but this is nicer to work with.
  , boards           :: ![Array U DIM2 Int]
  } deriving (Show)

parse :: FilePath -> IO State
parse path = withFile path ReadMode $ \h -> do
  !numbers <- map read . splitOn "," <$> hGetLine h

  !boards <- whileM (not <$> hIsEOF h) $ do
    -- There's an empty line before every board
    _ <- hGetLine h

    elems <- forM [1..boardHeight] $ \_ ->
      -- Board lines are in the format @XX YY ZZ WW UU@
      map read . chunksOf 3 <$> hGetLine h :: IO [Int]

    return . R.fromListUnboxed boardShape . concat $ elems

  return $! State
    { remainingNumbers = numbers
    , drawnNumbers     = S.empty
    , boards           = boards
    }

  where
    whileM :: Monad m => m Bool -> m a -> m [a]
    whileM p a = do
      continue <- p
      if continue
        then (:) <$> a <*> whileM p a
        else return []
