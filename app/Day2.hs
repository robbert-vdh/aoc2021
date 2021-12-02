{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Main where

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-2.txt"

  putStrLn "Part 1:"
  print . mulState $! eval1 input initialState

  putStrLn "\nPart 2:"
  print . mulState $! eval2 input initialState

data State = State
  { position :: {-# UNPACK #-} !Int
  , depth    :: {-# UNPACK #-} !Int
  , aim      :: {-# UNPACK #-} !Int -- ^ Only used for part 0
  }

initialState :: State
initialState = State 0 0 0

-- Because they could only fit a single text box on the website
mulState :: State -> Int
mulState State{..} = position * depth

-- Yes, I know, you can skip this and just modify the state directly, but it's
-- fun just leave me alone
data Command
  = Forward Int -- ^ Increase horizontal position by n units
  | Up Int      -- ^ _Decrease_ depth by n units
  | Down Int    -- ^ _Increase_ depth by n units

-- | The evaluator for part 1, since the rules change in part 2. Oh and
-- MonadState has not been invented yet in whatever the current year is!
eval1 :: [Command] -> State -> State
eval1 (Forward n : cs) s@State{..} = eval1 cs $! s { position = position + n }
eval1 (Up n      : cs) s@State{..} = eval1 cs $! s { depth    = depth - n }
eval1 (Down n    : cs) s@State{..} = eval1 cs $! s { depth    = depth + n }
eval1 []               s           = s

-- | In part 2 Up/Down control the ship's bearing, and the depth is determined
-- by a combination of that and forwards movement.
eval2 :: [Command] -> State -> State
eval2 (Forward n : cs) s@State{..} = eval2 cs $! s { position = position + n, depth = depth + (aim * n) }
eval2 (Up n      : cs) s@State{..} = eval2 cs $! s { aim      = aim - n }
eval2 (Down n    : cs) s@State{..} = eval2 cs $! s { aim      = aim + n }
eval2 []               s           = s

parse :: String -> [Command]
parse = map (go . words) . lines
  where
    go ["forward", read -> n] = Forward n
    go ["up"     , read -> n] = Up n
    go ["down"   , read -> n] = Down n
    go xs                     = error "What are you even doing"
