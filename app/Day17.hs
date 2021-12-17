{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Numeric.GSL.SimulatedAnnealing
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import qualified Numeric.LinearAlgebra as H

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-17.txt"

  putStrLn "So uh, this won't work because for my input the answer is unbounded, great"

  putStrLn "\nPart 1:"
  putStrLn $ "Target: " <> show input
  print $! solve input

-- | The target area.
data Target = Target { left :: Int, right :: Int, top :: Int, bottom :: Int }
  deriving (Show)

-- | Find the velocity with the highest y-coordinate that passes through the
-- target.
solve :: Target -> (Int, Int)
solve target =
  simanSolve 420 2 solverParams (0, 0)
    (solverPenalty target)
    (\(oldX, oldY) (newX, newY) -> fromIntegral $ abs (newX - oldX) + abs (newY - oldY))
    step
    -- (Just $ show . overshootDistance target)
    (Just $ \velo -> show velo <> ": dist = " <> show (overshootDistance target velo))
  where
    solverParams = SimulatedAnnealingParams
      50     -- Tries per step
      30000  -- Tries per temperature
      5      -- Maximum step size in random walk
      1.0    -- Boltzman constant for random walks
      20.0   -- Initial temperature
      4.20   -- Cooling rate
      0.0069 -- Final temperature

    step :: H.Vector Double -> Double -> (Int, Int) -> (Int, Int)
    step rands stepSize (veloX, veloY) =
      ( veloX + round ((rands H.! 0) * stepSize)
      , veloY + round ((rands H.! 1) * stepSize)
      )

-- | The penalty for the solver. This is negative the maximum Y-coordinate
-- unless the trajectory would not hit the target area, in which case it is the
-- distance to the center of the target area.
solverPenalty :: Target -> (Int, Int) -> Double
solverPenalty target initialVelo
  | Just maxYPos <- hitsTarget target initialVelo = fromIntegral (negate maxYPos)
  | otherwise                                     = overshootDistance target initialVelo + 100

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
    pTarget = Target <$> (string "target area: x=" *> pInt)      <*> (string ".." *> pInt)
                     <*> (string ", y=-" *> pInt <* string "..") <*> pInt

    pInt :: Parsec String () Int
    pInt = read <$> many (digit <|> char '-')
