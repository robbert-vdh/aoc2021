{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Main where

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-2.txt"

  putStrLn "Part 1:"
  print . mulState $! eval input initialState

data State = State
  { position :: {-# UNPACK #-} !Int
  , depth :: {-# UNPACK #-} !Int
  }

initialState :: State
initialState = State 0 0

-- Because they could only fit a single text box on the website
mulState :: State -> Int
mulState State{..} = position * depth

-- Yes, I know, you can skip this and just modify the state directly, but it's
-- fun just leave me alone
data Command
  = Forward Int -- ^ Increase horizontal position by n units
  | Up Int      -- ^ _Decrease_ depth by n units
  | Down Int    -- ^ _Increase_ depth by n units

-- MonadState has not been invented yet in whatever the current year is!
eval :: [Command] -> State -> State
eval (Forward n : cs) s@State{..} = eval cs $! s { position = position + n }
eval (Up n      : cs) s@State{..} = eval cs $! s { depth    = depth - n }
eval (Down n    : cs) s@State{..} = eval cs $! s { depth    = depth + n }
eval []               s           = s

parse :: String -> [Command]
parse = map (go . words) . lines
  where
    go ["forward", read -> n] = Forward n
    go ["up"     , read -> n] = Up n
    go ["down"   , read -> n] = Down n
    go xs                      = error "What are you even doing"
