{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Control.Applicative as A
import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-19.txt"

  putStrLn "Part 1:"
  -- print input
  mapM_ (print . matchScanners (head input)) (tail input)
  -- TODO: Keep matching until the relative orientation and translation to the
  --       origin are known for all points


-- * Part 1

-- | Beacons detected by a scanner. These are relative to the scanner's
-- orientation, which can be one of 24 combinations of 90 degree rotations
-- around the three axes.
newtype ScannerData = ScannerData (Set Point)
  deriving (Show)
data Point = Point Int Int Int
  deriving (Show, Eq, Ord)

data Solution = Solution
  { scanners :: ScannerData
  , relativeOrientations :: IntMap ScannerConfig
  }
  deriving (Show)
data ScannerConfig = ScannerConfig
  { -- | THe orientation relative to scanner zero. This orientation consits of
    -- three integers in the range @[0, 3]@, which correspond to the multiples
    -- of 90 degrees the scanner was rotated around each axis compared to
    -- scanner 0. Scanner 0 thus always has orientation @(0, 0, 0)@.
    orientation :: Orientation
    -- | The offset, after applying both scanner's rotations, between
    -- 'overlappingScanner''s points and this scanner's. Transposing this
    -- scanner's points after rotation gets you the original locations relative
    -- to the first scanner.
  , offset :: Point
  }
  deriving (Show)
type Orientation = (Int, Int, Int)

-- | All 64 possible orientations.
orientations :: [Orientation]
orientations = [(rx, ry, rz) | rx <- [0 .. 3], ry <- [0 .. 3], rz <- [0 .. 3]]

-- | Rotate a point around the origin using the specified orientation. The
-- rotation happens in the order of the axes, but as long as this process is
-- deterministic how the rotation actually works doesn't matter.
rotate :: Orientation -> Point -> Point
rotate (rx, ry, rz) (Point x y z) =
    let (y', z')   = rotateTimes rx (y, z)   -- Rotate around the x-axis
        (x', z'')  = rotateTimes ry (x, z')  -- Rotate around the y-axis
        (x'', y'') = rotateTimes rz (x', y') -- Rotate around the z-axis
     in Point x'' y'' z''
  where
    -- | When rotating points 90 degrees around an axis, you change the values
    -- for the other two axes. Doing this four times gets you back the orignal
    -- values.
    rotate90 :: (Int, Int) -> (Int, Int)
    rotate90 (u, v) = (negate v, u)

    rotateTimes :: Int -> (Int, Int) -> (Int, Int)
    rotateTimes n p = iterate rotate90 p !! n

-- | Try to match rotate and transpose two scanners such that at least twelve
-- points overlap.
matchScanners :: ScannerData -> ScannerData -> Maybe ScannerConfig
matchScanners (ScannerData origin) (ScannerData target) = findMaybe matchWithOrientation orientations
  where
    findMaybe :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
    findMaybe f = foldl (\m x -> m A.<|> f x) Nothing

    -- | Rotate all of @target@'s points using an orientation, and then try to
    -- figure out which point in @target@ matches with the first point in
    -- @origin@.
    matchWithOrientation :: Orientation -> Maybe ScannerConfig
    matchWithOrientation rotation =
      let rotatedTarget = S.map (rotate rotation) target
       in ScannerConfig rotation <$> findMaybe (matchTargetPoint rotatedTarget) rotatedTarget

    -- | After rotating the target points and picking a point to focus on for
    -- the match, pair that point up with all origin points and check if they
    -- match.
    matchTargetPoint :: Set Point -> Point -> Maybe Point
    matchTargetPoint rotatedTarget targetPoint = findMaybe (yeeeeet rotatedTarget targetPoint) origin

    -- | For a set of rotated target points points from both the origin and the
    -- rotated target point sets, transpose the target points such that the
    -- target point matches the origin point. Then check if at least twelve
    -- other points match.
    yeeeeet :: Set Point -> Point -> Point -> Maybe Point
    yeeeeet rotatedTarget _targetPoint@(Point tx ty tz) _originPoint@(Point ox oy oz) =
      let (dx, dy, dz) = (ox - tx, oy - ty, oz - tz)
          translatedTarget = S.map (\(Point x y z) -> Point (x + dx) (y + dy) (z + dz)) rotatedTarget
       in if S.size (S.intersection origin translatedTarget) >= 12
             then Just (Point dx dy dz)
             else Nothing


-- * Parsing

type Parser = Parsec String ()

parse :: String -> [ScannerData]
parse = fromRight' . Parsec.parse (sepBy pScannerData newline) ""
  where
    fromRight' (Right a) = a
    fromRight' e         = error $ "I said LEFT! I mean RIGHT! " <> show e

pScannerData :: Parser ScannerData
pScannerData = ScannerData . S.fromList <$> (string "--- scanner " *> pInt *> string " ---" *> newline *> endBy pPoint newline)

pPoint :: Parser Point
pPoint = Point <$> pInt <*> (char ',' *> pInt) <*> (char ',' *> pInt)

pInt :: Parser Int
pInt = read <$> many1 (digit <|> char '-')
