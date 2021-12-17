{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

import Data.List
import Data.Maybe
import Data.Ord
import Numeric.GSL.SimulatedAnnealing
import qualified Numeric.LinearAlgebra as H
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-17.txt"

  putStrLn "\nPart 1:"
  putStrLn $ "Target: " <> show input
  print . hitsTarget input $! fancySolve input
  -- print $! bruteforce input


-- | The target area.
data Target = Target { left :: Int, right :: Int, bottom :: Int, top :: Int }
  deriving (Show)

-- | Find the velocity with the highest y-coordinate that passes through the
-- target.
--
-- This doesn't actually work
fancySolve :: Target -> (Int, Int)
fancySolve target =
  simanSolve 420 2 solverParams (20, 20)
    (solverPenalty target)
    (\(oldX, oldY) (newX, newY) -> fromIntegral $ abs (newX - oldX) + abs (newY - oldY))
    step
    (Just $ \velo -> show velo <> ": dist = " <> show (overshootDistance target velo))
  where
    solverParams = SimulatedAnnealingParams
      1000   -- Tries per step
      1000   -- Tries per temperature
      1      -- Maximum step size in random walk
      1.0    -- Boltzman constant for random walks
      2.0    -- Initial temperature
      1.69   -- Cooling rate
      0.0069 -- Final temperature

    step :: H.Vector Double -> Double -> (Int, Int) -> (Int, Int)
    step rands _ velo@(veloX, veloY) =
      ( veloX + round (((rands H.! 0) - 0.5) * overshootDistance target velo * 10)
      , veloY + round (((rands H.! 1) - 0.5) * overshootDistance target velo * 10)
      )

-- | Just exhaustively try all options to find starting velocity that results in
-- highest reached Y-coordinate.
bruteforce :: Target -> ((Int, Int), Int)
bruteforce target@Target{..}
  = maximumBy (comparing snd)
  $ mapMaybe (\p -> (p,) <$> hitsTarget target p)
    [(x, y) | x <- [1 .. right], y <- [top .. abs bottom * 10]]

-- | The penalty for the solver. This is negative the maximum Y-coordinate
-- unless the trajectory would not hit the target area, in which case it is the
-- distance to the center of the target area.
solverPenalty :: Target -> (Int, Int) -> Double
solverPenalty target initialVelo
  | Just maxYPos <- hitsTarget target initialVelo = fromIntegral (negate maxYPos)
  | otherwise                                     = (overshootDistance target initialVelo + 100) ** 2

-- | Whether the given velocity causes the target to be hit. Returns the highest
-- Y-position if it does.
hitsTarget :: Target -> (Int, Int) -> Maybe Int
hitsTarget Target{..} = go (0, 0)
  where
    go :: (Int, Int) -> (Int, Int) -> Maybe Int
    go (posX, posY) (veloX, veloY)
      | posX > right || posY < bottom = Nothing
      | otherwise =
          let newPos  = (posX + veloX, posY + veloY)
              newVelo = (veloX - signum veloX, veloY - 1)
           in if posX >= left && posX <= right && posY >= bottom && posY <= top
                then Just posY
                else max posY <$> go newPos newVelo

-- | The minimal distance the target's center is overshot by while following the
-- trajectory, bounded by double the target's bottom right corner.
overshootDistance :: Target -> (Int, Int) -> Double
overshootDistance Target{..} = lowestDistance (0, 0)
  where
    centerX = (left + right) `div` 2
    centerY = (top + bottom) `div` 2
    boundX = right * 2
    boundY = bottom * 2

    lowestDistance :: (Int, Int) -> (Int, Int) -> Double
    lowestDistance (posX, posY) (veloX, veloY) =
      let distance                  = sqrt $ fromIntegral (centerX - posX) ** 2
                                           + fromIntegral (centerY - posY) ** 2
          newPos@(newPosX, newPosY) = (posX + veloX, posY + veloY)
          newVelo                   = (veloX - signum veloX, veloY - 1)
       in if newPosX > boundX || newPosY < boundY
            then distance
            else min distance (lowestDistance newPos newVelo)

parse :: String -> Target
parse = fromRight' . Parsec.parse pTarget ""
  where
    fromRight' (Right x) = x
    fromRight' _         = error "This wasn't in our agreement!"

    pTarget :: Parsec String () Target
    pTarget = Target <$> (string "target area: x=" *> pInt)     <*> (string ".." *> pInt)
                     <*> (string ", y=" *> pInt <* string "..") <*> pInt

    pInt :: Parsec String () Int
    pInt = read <$> many (digit <|> char '-')
