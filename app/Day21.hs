{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.State.Strict
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH


-- | Positions and score are per-player.
data GameState = GameState
  { _pos         :: {-# UNPACK #-} !(Int, Int)
  , _score       :: {-# UNPACK #-} !(Int, Int)
    -- | True if it's currently player 1's turn.
  , _player1Turn :: !Bool
    -- | The next roll on the dice, goes back to 1 after rolling 100.
  , _nextRoll    :: {-# UNPACK #-} !Int
  }
  deriving (Show)

makeLenses ''GameState


main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-21.txt"

  putStrLn "Part 1:"
  print $! part1Score input


-- * Part 1

-- | Play the game until someone wins, then return the number of turns played
-- times the lowest score.
part1Score :: GameState -> Int
part1Score initialState =
  let (diceRolls, finalState) = runState playUntilWin initialState
      (p1Score, p2Score)      = _score finalState
   in diceRolls * min p1Score p2Score

-- | Keep playing the game (you lost it btw) until a player wins, and return the
-- number of times the dice was rolled (three times per turn).
playUntilWin :: State GameState Int
playUntilWin = do
  (p1Score, p2Score) <- use score
  if max p1Score p2Score >= 1000
    then return 0
    else gameStep >> (+ 3) <$> playUntilWin

gameStep :: State GameState ()
gameStep = do
  p1Turn <- use player1Turn

  -- We'll use lenses to make this function handle both players without any
  -- modifications
  let _player :: Lens (Int, Int) (Int, Int) Int Int
      _player = if p1Turn then _1 else _2

  -- The dice rolls are in the range [1, 100]
  firstRoll <- use nextRoll
  nextRoll .= ((firstRoll - 1 + 3) `mod` 100) + 1
  let rolls    = [(x - 1 `mod` 100) + 1 | x <- [firstRoll .. firstRoll + 2]]
      posDelta = sum rolls

  -- Move the player forward, and add their new position to their score
  pos . _player %= (\p -> ((p - 1 + posDelta) `mod` 10) + 1)
  newPos <- use (pos . _player)
  score . _player += newPos

  player1Turn %= not


-- * Parsing

parse :: String -> GameState
parse = go . map (read . drop (length  "Player N starting position: ")) . lines
  where
    go :: [Int] -> GameState
    go [p1Pos, p2Pos] = GameState (p1Pos, p2Pos) (0, 0) True 1
    go _              = error "You're supposed to play this game with two players"
