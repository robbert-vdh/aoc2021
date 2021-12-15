{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.State.Strict
import Data.Char
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Massiv.Array (Ix2 (..), Matrix, U)
import qualified Data.Massiv.Array as A
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-15.txt"

  putStrLn "Part 1:"
  print $! evalState findPath (initialState input)


-- * A* search

data SearchState = SearchState
  { graph    :: !Graph
    -- | The nodes that have already been expanded. These should not be expanded
    -- again.
  , expanded :: !(HashSet Point)
    -- | A queue of paths to expand, ordered by weight. The weight is the
    -- traveled distance to get to the last point in the sequence, plus a
    -- heuristic for how long it would take to reach the exit (which is just the
    -- Manhattan distance multiplied by a scalar). The distance traveled is also
    -- stored alongside the path so it doesn't need to be recomputed all the
    -- time.
  , frontier :: MinPQueue Int Path
  }
  deriving (Show)

data Path = Path { distance :: Int, path :: Seq Point }
  deriving (Show)

-- Some fun orphan instances for our hashmap
deriving instance Generic Ix2
deriving instance Hashable Ix2

initialState :: Graph -> SearchState
initialState graph =
  SearchState graph (S.singleton start) (PQ.singleton 0 (Path 0 $ Seq.singleton start))
  where
    start = Ix2 0 0

-- | Find the shortest path to the bottom right node in the graph using the A*
-- path finding algorithm.
--
-- TODO: This is currently not an A* search because the heuristic is nasty and
--       it's practically instantaneous without one anyways
findPath :: State SearchState Path
findPath = do
  SearchState{..} <- get
  let ((_, Path currentDistance currentPath), otherPaths) = PQ.deleteFindMin frontier
      _ :|> currentNode@(Ix2 currentY currentX)           = currentPath
      goal@(Ix2 goalY goalX)                              = A.unSz (A.size graph) - Ix2 1 1

  if currentNode == goal
    then return $! Path currentDistance currentPath
    else do
      -- If we have not yet reached the end, expand the current node into new
      -- paths, enqueue those paths, and repeat this until we do reach the end
      let neighbours = [Ix2 y x | (y, x) <- [ (currentY,     currentX + 1), (currentY,     currentX - 1)
                                            , (currentY + 1, currentX),     (currentY - 1, currentX)]
                                  -- The neighbour needs to be in bounds
                                , y >= 0     && x >= 0
                                , y <= goalY && x <= goalX
                                  -- And no path should have expanded into that neighbour
                                , not (S.member (Ix2 y x) expanded)]

          -- This is an ugly monadic contraption instead of just mapping this list to a
          -- function because of all of the other state information you'd need
          -- to pass otherwise
          newPaths = PQ.fromList $ do
            node <- neighbours
            let newDistance   = currentDistance + (graph A.! node)
                -- TODO: This heuristic is not correct, so we're better off just doing a normal BFS
                -- Ix2 remY remX = goal - node
                -- -- Who is manhattanRem?
                -- manhattanRem  = abs remY + abs remX
                heuristic = 0
            return (newDistance + heuristic, Path newDistance (currentPath :|> node))

      modify $ \s -> s
        { expanded = S.union expanded (S.fromList neighbours)
        , frontier = PQ.union otherPaths newPaths
        }

      findPath


-- * Parsing

type Graph = Matrix U Int
type Point = Ix2

parse :: String -> Graph
parse = A.fromLists' A.Seq . map (map digitToInt) . lines
