{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List.Split
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import Data.String (IsString(..))
import GHC.Generics (Generic)


main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-12.txt"

  putStrLn "Part 1:"
  print . length . filter containsSmallCave . completePaths $ traverseAll expandUnique input

  putStrLn "\nPart 2:"
  print . length . completePaths $ traverseAll expandMaybeTwice input


-- | In the graph there are small caves (with lower case names) and big caves
-- (with upper case names). Small caves can only be visited once, large caves
-- can be visited as many times as necessary.
data Node = BigCave String | SmallCave String
  deriving (Eq, Ord, Show, Generic, Hashable)

instance IsString Node where
  fromString s@(c : _) | isUpper c = BigCave s
  fromString s                     = SmallCave s

-- | Mappings from each node to the other nodes they're connected to. This
-- contains the edges in both directions.
type Graph = HashMap Node [Node]


data TraversalState = TraversalState
  { graph         :: !Graph
  -- I assumed you'd also need to find the shortest path at some point. Apparently  not.
  --
  --   -- | The big caves that have already been visited.
  -- , visited       :: !(HashSet Node)
    -- | Paths that have not yet either died out or reached the end. These are
    -- traversed in breadth first order.
  , currentPaths  :: Seq (Seq Node)
    -- | Complete paths from the start to the end.
  , completePaths :: [Seq Node]
  }

-- | Traverse the graph from the end until all paths have been found.
traverseAll :: (TraversalState -> Seq Node -> [Node]) -> Graph -> TraversalState
traverseAll expandPath = execState go . initialState
  where
    go :: State TraversalState ()
    go = do
      traversalStep expandPath
      done <- gets (Seq.null . currentPaths)
      unless done go

    initialState :: Graph -> TraversalState
    initialState g =
      let startKey   = "start"
          startPaths = Seq.fromList . map (\n -> [startKey, n]) $ g M.! startKey
       in TraversalState g startPaths []

-- | Generate new nodes for a path for part 1, where small caves can only be
-- visited once. In this version we aren't looking for the shortest path, so
-- instead of using the visited set all nodes that are not already in the
-- current path should be considered.
expandUnique :: TraversalState -> Seq Node -> [Node]
expandUnique TraversalState{..} ~currentPath@(_ :|> currentNode) =
  filterNodes (S.fromList $ toList currentPath) (graph M.! currentNode)

-- | The same as 'expandUnique', but a path may visit a single small cave twice
-- unless this is the start cave.
expandMaybeTwice :: TraversalState -> Seq Node -> [Node]
expandMaybeTwice TraversalState{..} ~currentPath@(_ :|> currentNode) =
  filterNodes visitedNodes (graph M.! currentNode)
  where
    -- The visited nodes in this path. If the path does not already contain two
    -- visits of a single small cave, then this set will only contain the
    -- starting node since every other node can still be visited.
    visitedNodes =
      let visitedSet = S.fromList (toList currentPath)
       in if containsDuplicate (toList $ Seq.sort currentPath)
            then visitedSet
            else S.singleton "start"

    -- | Find a duplicate small cave in a sorted list of nodes.
    containsDuplicate :: [Node] -> Bool
    containsDuplicate ((SmallCave x) : (SmallCave y) : _ ) | x == y = True
    containsDuplicate (_             : y             : cs)          = containsDuplicate (y : cs)
    containsDuplicate _                                             = False

-- | Filter a list of nodes based on a set of visited nodes. Big caves can
-- always be revisited.
filterNodes :: HashSet Node -> [Node] -> [Node]
filterNodes visited = filter $ \case
  (BigCave _)     -> True
  n@(SmallCave _) -> not (S.member n visited)

-- | Perform a single breadth first search step, updating the current state.
-- This will only modify the first path in 'currentPaths', appending any new
-- uncovered paths to the end. The @expandPath@ function expands the current
-- node into new paths. This is needed because we want to compute _all_ paths
-- and in part 2 presumably also the shorted path. That means that in part 1 we
-- should ignore the global visited list and only include the nodes from the
-- current path.
traversalStep :: (TraversalState -> Seq Node -> [Node]) -> State TraversalState ()
traversalStep expandPath = do
  currentState@TraversalState{..} <- get
  let currentPath :<| otherPaths = currentPaths
      _ :|> currentNode          = currentPath

  -- Append the new (unvisited, where applicable) paths to the end of the
  -- current paths sequence, or don't do anything once we reach the end
  let reachedEnd = currentNode == "end"
      newPaths   = if reachedEnd
        then Seq.empty
        else (currentPath :|>) <$> Seq.fromList (expandPath currentState currentPath)

  modify $ \s -> s
    -- { visited       = S.insert currentNode visited
    { currentPaths  = otherPaths >< newPaths
    , completePaths = if reachedEnd
        then currentPath : completePaths
        else completePaths
    }


-- | For part 1 they only want to count paths containing small caves. Not sure
-- why this is specified explicitly since at least with my input it's not
-- possible to have a complete path that doesn't go through small caves.
containsSmallCave :: Seq Node -> Bool
containsSmallCave = any $ \case
  (SmallCave _) -> True
  _             -> False


parse :: String -> Graph
parse = foldl' insertEdge M.empty . lines
  where
    -- In the input edges only show up in a single direction, but in reality
    -- they are of course bidirectional
    insertEdge :: Graph -> String -> Graph
    insertEdge m (splitOn "-" -> [fromString -> n1, fromString -> n2]) =
      M.insertWith (++) n1 [n2] $ M.insertWith (++) n2 [n1] m
    insertEdge _ _ = error "I think you loaded the wrong file"
