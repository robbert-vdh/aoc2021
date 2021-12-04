{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State.Strict
import Data.Array.Repa ((:.)(..), Array, DIM2, U, Z(..))
import qualified Data.Array.Repa as R
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (find)
import Data.List.Split
import System.IO

main :: IO ()
main = do
  !input <- parse "inputs/day-4.txt"

  putStrLn "Part 1:"
  print $! evalState (findVictor >>= winningScore) input

-- | Because has time to write all of this.
pattern I2 :: Int -> Int -> DIM2
pattern I2 y x = Z :. y :. x

boardHeight :: Int
boardHeight = 5

boardShape :: DIM2
boardShape = I2 boardHeight boardHeight

type Bingo a = State BingoState a

type Board = Array U DIM2 Int

data BingoState = BingoState
  { remainingNumbers :: ![Int]              -- ^ Numbers left to draw, in order
  , drawnNumbers     :: !(HashSet Int)      -- ^ The numbers that have already been drawn
  , lastDrawnNumber  :: {-# UNPACK #-} !Int -- ^ The last draw number, needed for scoring
    -- | The boards, this could be a three dimensional array for maximum
    -- efficiency but this is nicer to work with.
  , boards           :: ![Board]
  } deriving (Show)

-- | Run the bingo game until someone wins the game (you've lost the game btw),
-- returning the victor. This doesn't consider ties.
findVictor :: Bingo Board
findVictor = do
  BingoState{..} <- get
  case find (isWinning drawnNumbers) boards of
    -- If someone has already won, great
    Just victor -> return victor
    -- Otherwise, keep drawing numbers until someone wins
    Nothing -> do
      modify' $ \s -> s
        -- This will diverge when we run out of numbers
        { remainingNumbers = tail remainingNumbers
        , drawnNumbers     = S.insert (head remainingNumbers) drawnNumbers
        , lastDrawnNumber  = head remainingNumbers
        }

      findVictor

-- | Calculate the score for the bingo game's winner.
winningScore :: Board -> Bingo Int
winningScore winner = do
  BingoState{..} <- get
  let remaining = filter (not . (`S.member` drawnNumbers)) $ R.toList winner

  return $! sum remaining * lastDrawnNumber

-- | Check if a bingo board has won with the given set of drawn numbers.
isWinning :: HashSet Int -> Board -> Bool
isWinning drawn board = any checkCandidate candidates
  where
    checkCandidate :: [DIM2] -> Bool
    checkCandidate = all $ \idx -> S.member (board `R.unsafeIndex` idx) drawn

    -- Indices for a single axis
    singleAxis    = [0 .. boardHeight - 1]
    rowCandidates = map (\y -> map (\x -> I2 y x) singleAxis) singleAxis
    colCandidates = map (\x -> map (\y -> I2 y x) singleAxis) singleAxis
    candidates    = rowCandidates ++ colCandidates

parse :: FilePath -> IO BingoState
parse path = withFile path ReadMode $ \h -> do
  !numbers <- map read . splitOn "," <$> hGetLine h

  !boards <- whileM (not <$> hIsEOF h) $ do
    -- There's an empty line before every board
    _ <- hGetLine h

    elems <- forM [1..boardHeight] $ \_ ->
      -- Board lines are in the format @XX YY ZZ WW UU@
      map read . chunksOf 3 <$> hGetLine h :: IO [Int]

    return . R.fromListUnboxed boardShape . concat $ elems

  return $! BingoState
    { remainingNumbers = numbers
    , drawnNumbers     = S.empty
    , lastDrawnNumber  = -1
    , boards           = boards
    }

  where
    whileM :: Monad m => m Bool -> m a -> m [a]
    whileM p a = do
      continue <- p
      if continue
        then (:) <$> a <*> whileM p a
        else return []
