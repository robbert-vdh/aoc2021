{-# LANGUAGE BangPatterns #-}

module Main where

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

inputM :: IO [Instruction]
inputM = parse <$> readFile "inputs/day-22.txt"


-- * Part 1

data Instruction = TurnOn { cuboid :: !Cuboid } | TurnOff { cuboid :: !Cuboid }
  deriving (Show)
-- | All the ranges here are inclusive.
data Cuboid = Cuboid {-# UNPACK #-} !(Int, Int) -- ^ @[minX, maxX]@
                     {-# UNPACK #-} !(Int, Int) -- ^ @[minY, maxY]@
                     {-# UNPACK #-} !(Int, Int) -- ^ @[minZ, maxZ]@
  deriving (Show)

-- | Apply all instructions in sequence, turning the final set of enabled cubes.
naiveSolve :: [Instruction] -> Set (Int, Int, Int)
naiveSolve = foldl' apply S.empty
  where
    apply :: Set (Int, Int, Int) -> Instruction -> Set (Int, Int, Int)
    apply s (TurnOn  cuboid) = S.union      s (S.fromList $ cuboidCoords part1Limit cuboid)
    apply s (TurnOff cuboid) = S.difference s (S.fromList $ cuboidCoords part1Limit cuboid)

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
