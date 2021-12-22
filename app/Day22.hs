{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec


main :: IO ()
main = do
  !input <- inputM

  putStrLn "Part 1:"
  print . S.size $ naiveSolve input

  -- NOTE: Not to self: don't map directly within sets...I wasted an hour and a
  --       half debugging why my seemingly correct solution was giving me
  --       results that were too low
  putStrLn "\nPart 2:"
  print . sum . map cuboidSize . S.toList $ solve input

inputM :: IO [Instruction]
inputM = parse <$> readFile "inputs/day-22.txt"


-- * Part 1

data Instruction = TurnOn { cuboid :: !Cuboid } | TurnOff { cuboid :: !Cuboid }
  deriving (Show)
-- | All the ranges here are inclusive.
data Cuboid = Cuboid {-# UNPACK #-} !(Int, Int) -- ^ @[minX, maxX]@
                     {-# UNPACK #-} !(Int, Int) -- ^ @[minY, maxY]@
                     {-# UNPACK #-} !(Int, Int) -- ^ @[minZ, maxZ]@
  deriving (Show, Eq, Ord)

-- | Apply all instructions in sequence, turning the final set of enabled cubes.
naiveSolve :: [Instruction] -> Set (Int, Int, Int)
naiveSolve = foldl' apply S.empty
  where
    apply :: Set (Int, Int, Int) -> Instruction -> Set (Int, Int, Int)
    apply s (TurnOn  c) = S.union      s (S.fromList $ cuboidCoords part1Limit c)
    apply s (TurnOff c) = S.difference s (S.fromList $ cuboidCoords part1Limit c)

-- | The coordinate limit for part 1.
part1Limit :: Cuboid
part1Limit = Cuboid (-50, 50) (-50, 50) (-50, 50)

-- | The coordinates within a cuboid, restricted to another cuboid's bounds.
cuboidCoords :: Cuboid -> Cuboid -> [(Int, Int, Int)]
cuboidCoords (Cuboid (bMinX, bMaxX) (bMinY, bMaxY) (bMinZ, bMaxZ))
             (Cuboid (minX,  maxX)  (minY,  maxY)  (minZ,  maxZ))
  = [(x, y, z) | x <- [max minX bMinX .. min maxX bMaxX]
               , y <- [max minY bMinY .. min maxY bMaxY]
               , z <- [max minZ bMinZ .. min maxZ bMaxZ]]


-- * Part 2

-- | Of course they were going to do this. Instead of counting the cubes, keep
-- track of the active cuboids.
solve :: [Instruction] -> Set Cuboid
solve = foldl' apply S.empty
  where
    apply :: Set Cuboid -> Instruction -> Set Cuboid
    -- Extend existing cuboids by replacing them with this new cuboid, or just
    -- insert the new one if there's no overlap. Partially overlapping cuboids
    -- are split to new adjacent cuboids.
    apply s (TurnOn c) = S.insert c $ splitOverlapping c s
    -- And remove cuboids from the list that are contained within this cuboid
    -- that should be removed. Cuboids that partially overlap with the target
    -- region are again split.
    apply s (TurnOff c) = splitOverlapping c s

-- | Return a new cuboid set where the cuboids that intersect the target cuboid
-- are cut off and the cuboids that are contained fully within the target cuboid
-- are removed. For some reason Data.Set is not a monad instance, so this is a
-- bit roundabout.
splitOverlapping :: Cuboid -> Set Cuboid -> Set Cuboid
splitOverlapping (Cuboid (tMinX, tMaxX) (tMinY, tMaxY) (tMinZ, tMaxZ)) s =
  S.unions $ S.map (S.fromList . splitCuboid) s
  where
    -- Carve away the target cuboid from this cuboid, returning a list of new
    -- cuboids.
    splitCuboid :: Cuboid -> [Cuboid]
    splitCuboid c@(Cuboid (minX, maxX) (minY, maxY) (minZ, maxZ))
        -- Don't do anything to cuboids that don't intersect with the target
        -- Oh and all these off by one things! The conditions are all inverted
        -- now to account for that
      | (tMinX > maxX || tMaxX < minX) || (tMinY > maxY || tMaxY < minY) || (tMinZ > maxZ || tMaxZ < minZ) = [c]
      | otherwise = catMaybes
          [ if tMinX > minX then Just (Cuboid (minX, tMinX - 1)                (minY, maxY)                     (minZ, maxZ))      else Nothing
          , if tMaxX < maxX then Just (Cuboid (tMaxX + 1, maxX)                (minY, maxY)                     (minZ, maxZ))      else Nothing
            -- After this point we need to make sure that the new cuboids' X-coordinates don't overlap
          , if tMinY > minY then Just (Cuboid (max minX tMinX, min maxX tMaxX) (minY, tMinY - 1)                (minZ, maxZ))      else Nothing
          , if tMaxY < maxY then Just (Cuboid (max minX tMinX, min maxX tMaxX) (tMaxY + 1, maxY)                (minZ, maxZ))      else Nothing
            -- And now also the y-coordinates
          , if tMinZ > minZ then Just (Cuboid (max minX tMinX, min maxX tMaxX) (max minY tMinY, min maxY tMaxY) (minZ, tMinZ - 1)) else Nothing
          , if tMaxZ < maxZ then Just (Cuboid (max minX tMinX, min maxX tMaxX) (max minY tMinY, min maxY tMaxY) (tMaxZ + 1, maxZ)) else Nothing
          ]

-- | The number of cubes within a cuboid.
cuboidSize :: Cuboid -> Int
cuboidSize (Cuboid (minX, maxX) (minY, maxY) (minZ, maxZ)) =
  -- The ranges are inclusive
  (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)


-- * Parsing

type Parser = Parsec String ()

parse :: String -> [Instruction]
parse = fromRight' . Parsec.parse (sepEndBy pInstruction newline) ""
  where
    fromRight' (Right a) = a
    fromRight' e         = error $ "I said LEFT! I mean RIGHT! " <> show e

pInstruction :: Parser Instruction
pInstruction = (TurnOn <$ try (string "on ") <|> TurnOff <$ string "off ") <*> pCuboid

pCuboid :: Parser Cuboid
pCuboid = Cuboid <$> (string "x=" *> pRange) <*> (string ",y=" *> pRange) <*> (string ",z=" *> pRange)

pRange :: Parser (Int, Int)
pRange = (,) <$> pInt <*> (string ".." *> pInt)

pInt :: Parser Int
pInt = read <$> many1 (digit <|> char '-')
