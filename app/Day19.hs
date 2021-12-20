{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Main where

import qualified Control.Applicative as A
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  !scanners <- parse <$> readFile "inputs/day-19.txt"

  putStrLn "Part 1:"
  let (!mappings, !solvedPoints) = solve scanners
  print (S.size solvedPoints)

  putStrLn "\nPart 2:"
  print . maximum . pairwiseDistances $ backprojectScanners mappings


-- * Part 1

-- | Beacons detected by a scanner. These are relative to the scanner's
-- orientation, which can be one of 24 combinations of 90 degree rotations
-- around the three axes.
newtype ScannerData = ScannerData (Set Point)
  deriving (Show)
data Point = Point Int Int Int
  deriving (Show, Eq, Ord)

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

-- | All 64 possible orientations, filtered down to 24-non equivalent
-- orientations.
orientations :: [Orientation]
orientations = sort . M.elems . M.fromList . reverse $ map (\o -> (rotate o testPoint, o)) allOrientations
  where
    testPoint = Point 1 2 3
    allOrientations = [(rx, ry, rz) | rx <- [0 .. 3], ry <- [0 .. 3], rz <- [0 .. 3]]

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

translate :: Point -> Point -> Point
translate (Point dx dy dz) (Point x y z) = Point (x + dx) (y + dy) (z + dz)

findMaybe :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findMaybe f = foldl (\m x -> m A.<|> f x) Nothing

-- | Try to match rotate and transpose two scanners such that at least twelve
-- points overlap.
matchScanners :: ScannerData -> ScannerData -> Maybe ScannerConfig
matchScanners (ScannerData origin) (ScannerData target) = findMaybe matchWithOrientation orientations
  where
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
      let delta = Point (ox - tx) (oy - ty) (oz - tz)
          translatedTarget = S.map (translate delta) rotatedTarget
       in if S.size (S.intersection origin translatedTarget) >= 12
             then Just delta
             else Nothing

-- ** Solution

-- | Reconstruct a set of points relative to the first scanner. Also return the
-- mappings since we need them for part 2 and I'm too tired to refactor this.
solve :: [ScannerData] -> (IntMap (Int, ScannerConfig), Set Point)
solve [] = (IM.empty, S.empty)
solve scanners =
  let !mappings = iterativelyMatch 0 (IM.singleton 0 (0, ScannerConfig (0, 0, 0) (Point 0 0 0)))
   in (mappings, S.unions $ map (backproject scanners mappings) [0 .. numScanners - 1])
  where
    numScanners = length scanners

    -- Keep trying to match scanners to scanners in the map until all scanners
    -- have been matched.
    iterativelyMatch :: Int -> IntMap (Int, ScannerConfig) -> IntMap (Int, ScannerConfig)
    iterativelyMatch startIdx mappings
      | IM.size mappings == numScanners
      = mappings
      -- Try matching from the first scanner again after performing a cycle. We
      -- don't do this _every_ time to avoid unnecessarily rechecking the same
      -- scanner over and over again.
      | startIdx >= numScanners
      = trace ("Matched " <> show (IM.size mappings) <> " scanners after this cycle") iterativelyMatch 0 mappings
      | startIdx `IM.notMember` mappings
      = iterativelyMatch (startIdx + 1) mappings
      | otherwise
      = iterativelyMatch (startIdx + 1)
      . IM.union mappings
      . IM.fromList
      $ [(scannerIdx, (startIdx, config)) | scannerIdx <- [0 .. numScanners - 1]
                                          , scannerIdx `IM.notMember` mappings
                                          , Just config <- [matchScanners (scanners !! startIdx) (scanners !! scannerIdx)]]

-- | Rotate and translate the points from scanner @n@ such that their
-- orientation matches with scanner 0.
backproject :: [ScannerData] -> IntMap (Int, ScannerConfig) -> Int -> Set Point
backproject scanners mappings scannerIdx =
  let ScannerData scanner = scanners !! scannerIdx
      (_, backprojected) = until (\(mappedTo, _) -> mappedTo == 0) (uncurry (backproject' mappings)) (scannerIdx, scanner)
    in backprojected

-- | Backproject the set of points that are relative to scanner
-- @relativeIdx@ using the mappings, returning reoriented scanner data and
-- the index of the scanner this data is aligned to. This can be repeated
-- until they are aligned with scanner 0.
backproject' :: IntMap (Int, ScannerConfig) -> Int -> Set Point -> (Int, Set Point)
backproject' mappings relativeIdx scanner =
  let (mappedTo, ScannerConfig{..}) = mappings IM.! relativeIdx
    in (mappedTo, S.map (translate offset . rotate orientation) scanner)

-- * Part 2

-- | Calculate the pairwise Manhattan distances for all of the points in the
-- set.
pairwiseDistances :: Set Point -> [Int]
pairwiseDistances (S.toList -> points) =
  [abs (rx - lx) + abs (ry - ly) + abs (rz - lz) | p1@(Point lx ly lz) <- points
                                                 , p2@(Point rx ry rz) <- points
                                                 , p1 /= p2]

-- | Calculate the positions of all scanners relative to scanner 0.
--
-- Sorry for this, I just wanted to get this over with.
backprojectScanners :: IntMap (Int, ScannerConfig) -> Set Point
backprojectScanners mappings = S.fromList . IM.elems $ flip IM.map mappings $ \(mappedTo, config) ->
   head
   $ S.toList
   $ snd
   $ until (\(mappedTo', _) -> mappedTo' == 0) (uncurry (backproject' mappings))
           -- Backprojecting the offset to the first scanner's location gets you
           -- a set of all of the scanner's locations
           (mappedTo, S.singleton (offset config))


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
